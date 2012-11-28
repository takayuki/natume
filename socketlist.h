/*
 *  Natume -- an implementation of Kana-Kanji conversion in Haskell
 *  Copyright (C) 2006-2012 Takayuki Usui
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
#ifndef _SOCKLIST_H
#define _SOCKLIST_H

#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>

struct socket {
  int fd;
  struct addrinfo ai;
  struct socket* next;
};

struct socketlist {
  struct socket* next;
  int nfds;
};

void socketlist_init(struct socketlist*);
struct socket* socketlist_register(struct socketlist*,int,struct addrinfo*);
struct socket* socketlist_accept(struct socketlist*,fd_set*);
void socketlist_fdset(struct socketlist*,fd_set*);
void socketlist_free(struct socketlist*);
  
#endif
