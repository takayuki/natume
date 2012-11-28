/*
 * Natume -- an implementation of Kana-Kanji conversion in Haskell
 * Copyright (C) 2006-2012 Takayuki Usui
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
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
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <sys/wait.h>
#include "canna.h"
#include "sig.h"
#include "socketlist.h"
#include "natumeserver.h"

#define SOCKDIR    "/tmp/.iroha_unix"
#define SOCKPATH   "/tmp/.iroha_unix/IROHA"
static char* natumepath = NATUMEPATH;
static char* sockpath = SOCKPATH;
static struct socketlist sock;
static int inet;
static int inet6;
#ifdef sun
#undef sun
#endif
static struct sockaddr_un sun;
static int chldnum;
static int chldmax = 8;
static int debug;

void
version(void)
{
  fprintf(stderr,"%s\n",PACKAGE_VERSION);
  exit(0);
}

void
help(void)
{
  fprintf(stderr,"natumeserver [-46dihv] [-n num] [-s path] [-e exec]\n");
  exit(0);
}

void
terminate(int sig)
{
  socketlist_free(&sock);
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
listen_unix(struct addrinfo* ai)
{
  int root = 0;
  int s;
  struct stat st;

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
  s = socket(AF_UNIX,SOCK_STREAM,0);
  if (s == -1) {
    perror("socket"); return -1;
  }
  strcpy(sun.sun_path,sockpath);
  sun.sun_family = AF_UNIX;
  if (bind(s,(struct sockaddr*)&sun,sizeof(sun)) == -1) {
    perror("bind"); goto err;
  }
  if (listen(s,5) == -1) {
    perror("listen"); goto err;;
  }
  if (root) {
    if (chmod(sockpath,0777) == -1) { perror("chown"); goto err; }
  } else {
    if (chmod(sockpath,0700) == -1) { perror("chown"); goto err; }
  }
  socketlist_register(&sock,s,ai);
  return s;
 err:
  close(s);
  return -1;
}

int
listen_inet(struct addrinfo* ai)
{
  int s,on = 1;

  s = socket(ai->ai_family,ai->ai_socktype,ai->ai_protocol);
  if (s == -1) {
    perror("socket"); return -1;
  }
  if (setsockopt(s,SOL_SOCKET,SO_REUSEADDR,&on,sizeof(on)) == -1) {
    perror("setsockopt"); goto err;
  }
  if (ai->ai_family == AF_INET6) {
    if (setsockopt(s,IPPROTO_IPV6, IPV6_V6ONLY,&on,sizeof(on)) == -1) {
      perror("setsockopt"); goto err;
    }
  }
  if (bind(s,(struct sockaddr *)ai->ai_addr,ai->ai_addrlen) == -1) {
    perror("bind"); goto err;
  }
  if (listen(s,5) == -1) {
    perror("listen");  goto err;
  }
  socketlist_register(&sock,s,ai);
  return 0;
 err:
  close(s);
  return -1;
}

int
server_init(void)
{
  struct addrinfo hints;
  struct addrinfo *rs;
  struct addrinfo *r;
  int res;

  memset(&hints,0,sizeof(hints));
  hints.ai_family = AF_UNIX;
  hints.ai_socktype = SOCK_STREAM;
  if (listen_unix(&hints) < 0)
    return -1;
  if (!inet && !inet6)
    return 0;
  memset(&hints,0,sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;
  hints.ai_flags = (AI_PASSIVE | AI_NUMERICSERV);
  res = getaddrinfo(0,"5680",&hints,&rs);
  if (res < 0) {
    fprintf(stderr,"getaddrinfo: %s\n",gai_strerror(res)); goto err;
  }
  for (r = rs; r != 0; r = r->ai_next) {
    if (inet && r->ai_family == AF_INET)
      listen_inet(r);
    else if (inet6 && r->ai_family == AF_INET6)
      listen_inet(r);
  }
  freeaddrinfo(rs);
  return 0;
 err:
  freeaddrinfo(rs);
  return -1;
}

char* keyval(char* key,char* val)
{
  char* s = malloc(strlen(key)+strlen(val)+2);
  if (s == 0) return 0;
  sprintf(s,"%s=%s",key,val);
  return s;
}

int
child(int s,struct addrinfo* ai,int argc,char** argv,char** env)
{
  int newargc,envc;
  char** newargv,** newenv;
  int root = 0;
  int id;
  int major,minor;
  int size,offset;
  char token[64],ac[64];
  struct passwd* pw;
  struct stat st;

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
  if (ai->ai_family == AF_UNIX) {
    snprintf(ac,sizeof(ac),".natume/allow/%s","unix");
  } else {
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    if (getpeername(s,(struct sockaddr *)&addr,&addrlen) == -1)
      return -1;
    char peer[64];
    if (getnameinfo((struct sockaddr *)&addr,addrlen,peer,sizeof(peer),
                    0,0,NI_NUMERICHOST) != 0)
      return -1;
    snprintf(ac,sizeof(ac),".natume/allow/%s",peer);
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
  socketlist_free(&sock);
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
  int ch,s;
  fd_set fdset;
  pid_t pid;
  int status;

  while ((ch = getopt(argc,argv,"46de:in:s:hv")) != -1) {
    switch (ch) {
    case 'e': natumepath = optarg; break;
    case 'd': debug = 1; break;
    case 'i':
    case '4': inet = 1; break;
    case '6': inet6 = 1; break;
    case 'n': chldmax = atoi(optarg); break;
    case 's': sockpath = optarg; break;
    case 'v': version(); break;
    case 'h':
    default: help();
    }
  }
  argc -= optind;
  argv += optind;

  socketlist_init(&sock);
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
    socketlist_fdset(&sock,&fdset);
    sig_unblock(SIGCHLD);
    status = select(sock.nfds,&fdset,0,0,0);
    sig_block(SIGCHLD);
    if (status == -1) {
      if (errno == EINTR) {
	continue;
      } else {
	perror("select"); break;
      }
    }
    struct socket* r  = socketlist_accept(&sock,&fdset);
    if (r == 0)
      continue;
    s = accept(r->fd,0,0);
    if (s == -1) {
      perror("accept"); break;
    }
    if ((pid = fork()) == -1) {
      perror("fork"); break;
    }
    if (pid == 0) {
      if (child(s,&r->ai,argc,argv,env) == -1)
	exit(1);
    }
    close(s);
    chldnum++;
  }
  terminate(0);
  return 1;
}
