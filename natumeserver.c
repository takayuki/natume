/*
 *  Natume -- an implementation of Kana-Kanji conversion in Haskell
 *  Copyright (C) 2006 2007 Takayuki Usui
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <sys/wait.h>
#include "canna.h"
#include "sig.h"
#include "natumeserver.h"

#define SOCKDIR    "/tmp/.iroha_unix"
#define SOCKPATH   "/tmp/.iroha_unix/IROHA"
static char* natumepath = NATUMEPATH;
static char* sockpath = SOCKPATH;
static int sock[2];
static int inet;
#ifdef sun
#undef sun
#endif
static struct sockaddr_un sun;
static struct sockaddr_in sin;
static int chldnum;
static int chldmax = 8;
static int debug;

void
version(void)
{
  fprintf(stderr,"%s\n",PACKAGE_VERSION);
  exit(0);
}

extern char** environ;
void
help(void)
{
  fprintf(stderr,"natumeserver [-dihv] [-n num] [-s path] [-e exec]\n");
  exit(0);
}

void
terminate(int sig)
{
  close(sock[0]);
  unlink(sun.sun_path);
  exit(0);
}

void
collect(int sig)
{
  int wstat;

  while (0 < waitpid(-1,&wstat,WNOHANG)) {
    if (!WIFSTOPPED(wstat) && 0 < chldnum)
      chldnum--;
  }
}

int
server_init(void)
{
  int root;
  struct stat st;
  int s,opt;

  root = 0;
  if (getuid() == 0) root = 1;
  if (!strcmp(sockpath,SOCKPATH)) {
    if (stat(SOCKDIR,&st) == -1) {
      if (root) {
	if(mkdir(SOCKDIR,0755)) { perror("mkdir"); return -1; }
      }	else {
	if(mkdir(SOCKDIR,0700)) { perror("mkdir"); return -1; }
      }
    }
    if (stat(SOCKDIR,&st) == -1) {
      perror("stat"); return -1;
    }
    if (!(S_IFDIR & st.st_mode)) return -1;
  }
  s = socket(PF_UNIX,SOCK_STREAM,0);
  if (s == -1) {
    perror("socket"); return -1;
  }
  strcpy(sun.sun_path,sockpath);
  sun.sun_family = AF_UNIX;
  if (bind(s,(struct sockaddr*)&sun,sizeof(sun)) == -1) {
    perror("bind"); return -1;
  }
  if (listen(s,5) == -1) {
    perror("listen"); return -1;
  }
  if (root) {
    if (chmod(sockpath,0777) == -1) { perror("chown"); return -1; }
  } else {
    if (chmod(sockpath,0700) == -1) { perror("chown"); return -1; }
  }
  sock[0] = s;

  if (inet == 1) {
    s = socket(PF_INET,SOCK_STREAM,0);
    if (s == -1) {
      perror("socket"); return -1;
    }
    opt = 1;
    if (setsockopt(s,SOL_SOCKET,SO_REUSEADDR,&opt,sizeof opt) == -1) {
      perror("setsockopt"); return -1;
    }
    sin.sin_family = AF_INET;
    if (inet_aton("0.0.0.0",&sin.sin_addr) == 0) return -1;
    sin.sin_port = htons(5680);
    if (bind(s,(struct sockaddr *)&sin,sizeof(sin)) == -1) {
      perror("bind"); return -1;
    }
    if (listen(s,5) == -1) {
      perror("listen"); return -1;
    }
    sock[1] = s;
  } else {
    sock[1] = -1;
  }
  return 0;
}

char* keyval(char* key,char* val)
{
  char* s = malloc(strlen(key)+strlen(val)+2);
  if (s == 0) return 0;
  sprintf(s,"%s=%s",key,val);
  return s;
}

int
child(int s,int t,int argc,char** argv,char** env)
{
  int newargc,envc;
  char** newargv,** newenv;
  int root;
  int id;
  int major,minor;
  int size,offset;
  char token[64],ac[64];
  struct passwd* pw;
  struct stat st;

  root = 0;
  if (getuid() == 0) root = 1;
  id = canna_init(0,s,s,0);
  if (id == -1)
    return -1;
  if (canna_accept(id,&major,&minor,&size,token,sizeof(token)) == -1)
    return -1;
  if (sscanf(token,"%*d.%*d:%n",&offset) != 0) return -1;
  pw = getpwnam(token+offset);
  if (pw == 0) return -1;
  if (pw->pw_uid == 0 || pw->pw_gid == 0) return -1;
  if (stat(pw->pw_dir,&st) == -1) return -1;
  if (!(S_IFDIR & st.st_mode)) return -1;
  if (root) {
    if (setgid(pw->pw_gid) == -1) { perror("setgid"); return -1; }
    if (setuid(pw->pw_uid) == -1) { perror("setuid"); return -1; }
  }
  if (chdir(pw->pw_dir) == -1) return -1;
  if (inet == 1 && t == 1) {
    struct sockaddr_in peer;
    socklen_t namelen = sizeof(peer);
    if (getpeername(s,(struct sockaddr *)&peer,&namelen) == -1)
      return -1;
    snprintf(ac,sizeof(ac),".natume/allow/%s",inet_ntoa(peer.sin_addr));
  } else {
    snprintf(ac,sizeof(ac),".natume/allow/%s","unix");
  }
  if (stat(ac,&st) == -1) return -1;
  if (!(S_IFREG & st.st_mode)) return -1;
  newargc = 0;
  newargv = (char**)malloc(sizeof(char*)*(argc+3));
  if (newargv == 0) return -1;
  newargv[newargc++] = "natume";
  newargv[newargc++] = "-S";
  while (*argv) newargv[newargc++] = *argv++;
  newargv[newargc] = 0;
  if (root) {
    envc = 5;
    newenv = (char**)malloc(sizeof(char*)*(envc+1));
    if (newenv == 0) return -1;
    newenv[0] = keyval("HOME",pw->pw_dir);
    newenv[1] = keyval("SHELL",pw->pw_shell);
    newenv[2] = keyval("LOGNAME",pw->pw_name);
    newenv[3] = keyval("USER",pw->pw_name);
    newenv[4] = keyval("CANNATOKEN",token);
    newenv[envc] = 0;
  } else {
    char** e = env; envc = 0;
    while (*e++) { envc++; }
    newenv = (char**)malloc(sizeof(*e)*(envc+2));
    if (newenv == 0) return -1;
    e = env; envc = 0;
    while (*e) { newenv[envc++] = *e++; }
    newenv[envc++] = keyval("CANNATOKEN",token);
    newenv[envc] = 0;
  }
  dup2(s,0); dup2(s,1); close(s);
  close(sock[0]); if (inet == 1 && t == 1) close(sock[1]);
  execve(natumepath,newargv,newenv);
  perror("execv");
  exit(1);
}

int
bg(void)
{
  const char* devnull = "/dev/null";
  pid_t pid;
  int fd;

  if ((pid = fork()) == -1) return -1;
  if (pid != 0)
    exit(0);
  if (setsid() == -1) return -1;
  if (chdir("/") == -1) return -1;
  fd = open(devnull,O_RDWR,0);
  if (fd == -1) return -1;
  dup2(fd,0); dup2(fd,1); dup2(fd,2); close(fd);
  return 0;
}

int
main(int argc,char** argv,char** env)
{
  int ch,s,t;
  fd_set fdset;
  pid_t pid;
  int status;

  while ((ch = getopt(argc,argv,"de:in:s:hv")) != -1) {
    switch (ch) {
    case 'e': natumepath = optarg; break;
    case 'd': debug = 1; break;
    case 'i': inet = 1; break;
    case 'n': chldmax = atoi(optarg); break;
    case 's': sockpath = optarg; break;
    case 'v': version(); break;
    case 'h':
    default: help();
    }
  }
  argc -= optind;
  argv += optind;

  if (server_init())
    return 1;
  if (!debug) {
    if (bg() == -1) {
      perror("daemon");
      return 1;
    }
  }
  sig_catch(SIGINT,terminate);
  sig_catch(SIGTERM,terminate);
  sig_block(SIGCHLD);
  sig_catch(SIGCHLD,collect);
  sig_ignore(SIGHUP);
  sig_ignore(SIGPIPE);

  while (1) {
    while (0 < chldmax && chldmax <= chldnum)
      sig_suspend(SIGCHLD);
    FD_ZERO(&fdset);
    FD_SET(sock[0],&fdset); if (inet == 1) FD_SET(sock[1],&fdset);
    sig_unblock(SIGCHLD);
    status = select((sock[0]<sock[1]?sock[1]:sock[0])+1,&fdset,0,0,0);
    sig_block(SIGCHLD);
    if (status == -1) {
      if (errno == EINTR) {
	continue;
      } else {
	perror("select"); break;
      }
    }
    t = 0;
    if (FD_ISSET(sock[0],&fdset)) {
      s = accept(sock[0],0,0);
      if (s == -1) {
	perror("accept"); break;
      }
    } else if (inet == 1 && FD_ISSET(sock[1],&fdset)) {
      s = accept(sock[1],0,0);
      if (s == -1) {
	perror("accept"); break;
      }
      t = 1;
    } else {
      continue;
    }
    if ((pid = fork()) == -1) {
      perror("fork"); break;
    }
    if (pid == 0) {
      if (child(s,t,argc,argv,env) == -1)
	exit(1);
    }
    close(s);
    chldnum++;
  }
  terminate(0);
  return 1;
}
