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
#ifndef _CONNECT_H
#define _CONNECT_H

#include <stdio.h>
#include "array.h"
#include "da.h"

typedef struct {
  na_array key;
  na_array hit;
  da d;
  char* idx;
  char* dat;
  FILE* fp;
} da_connect;

int connect_init(char*,char*);
void connect_free(int);
int connect_start(int);
void connect_stop(int);
int connect_add(int,int*,int,int);
int connect_build(int);
int connect_load(int);
void connect_unload(int);
int connect_search(int,int*,int);
int connect_fetch(int,int,int*,int,int*);
void connect_dump(int);

#endif
