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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/queue.h>
#include <sys/select.h>
#include <unistd.h>
#include "socketlist.h"

struct socket*
socket_new(int fd,struct addrinfo* ai) {
  struct socket* s = malloc(sizeof(struct socket));
  if (s == 0)
    return 0;
  s->fd = fd;
  if (ai == 0) {
    memset(&s->ai,0,sizeof(s->ai));
  } else {
    s->ai.ai_family = ai->ai_family;
    s->ai.ai_socktype = ai->ai_socktype;
    s->ai.ai_protocol = ai->ai_protocol;
    void* addr = malloc(ai->ai_addrlen);
    if (addr == 0) {
      free(s);
      return 0;
    }
    memcpy(addr,ai->ai_addr,ai->ai_addrlen);
    s->ai.ai_addr = addr;
    s->ai.ai_addrlen = ai->ai_addrlen;
  }
  return s;
}

void
socket_free(struct socket* s) {
  if (s->ai.ai_addr != 0) {
    close(s->fd);
    free(s->ai.ai_addr);
  }
  free(s);
}

void
socketlist_init(struct socketlist* head) {
  head->next = 0;
  head->nfds = -1;
}

void
socketlist_put(struct socketlist* head, struct socket* s) {
  s->next = head->next;
  head->next = s;
  if (head->nfds <= s->fd)
    head->nfds = s->fd + 1;
}

struct socket*
socketlist_remove(struct socketlist* head) {
  struct socket* s = head->next;
  if (s != 0)
    head->next = s->next;
  return s;
}

struct socket*
socketlist_register(struct socketlist* head,int fd,struct addrinfo* ai) {
  struct socket* s = socket_new(fd,ai);
  if (s == 0)
    return 0;
  socketlist_put(head,s);
  return s;
}

struct socket*
socketlist_accept(struct socketlist* head,fd_set* fdset) {
  struct socket* s;
  for (s = head->next; s != 0; s = s->next)
    if (FD_ISSET(s->fd,fdset))
      return s;
  return 0;
}

void
socketlist_fdset(struct socketlist* head,fd_set* fdset) {
  struct socket* s;
  for (s = head->next; s != 0; s = s->next)
    FD_SET(s->fd,fdset);
}

void
socketlist_free(struct socketlist* head) {
  struct socket* s;
  while ((s = socketlist_remove(head)) != 0)
    socket_free(s);
}
