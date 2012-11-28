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
#ifndef _ARRAY_H
#define _ARRAY_H

#include <stdlib.h>

typedef struct {
  size_t len;
  size_t size;
  void*  data;
  size_t num;
  void (*init)(void*,void*);
  void (*final)(void*,void*);
  size_t bot;
  size_t top;
  void* private;
} na_vector;

typedef struct _na_array_cell {
  na_vector head;
  struct _na_array_cell* tail;
} na_array_cell;

typedef struct {
  size_t shot;
  size_t len;
  size_t size;
  na_array_cell vector;
  size_t num;
  na_array_cell* win;
  void (*init)(void*,void*);
  void (*final)(void*,void*);
  int (*cmp)(void*,void*,void*);
  void* private;
} na_array;

int na_vector_init(na_vector*,size_t,size_t,size_t,
		   void (*)(void*,void*),void (*)(void*,void*),void*);
int na_vector_cinit(na_vector*,na_vector*);
void na_vector_free(na_vector*);
void na_vector_reset(na_vector*);
int na_vector_count(na_vector*);
void* na_vector_fetch(na_vector*,int);
void* na_vector_update(na_vector*,int,void*);
void* na_vector_append(na_vector*,void*);
  
int na_array_init(na_array*,size_t,size_t,void (*)(void*,void*),
		  void (*)(void*,void*),int (*)(void*,void*,void*),void*);
int na_array_cinit(na_array*,na_array*);
void na_array_free(na_array*);
void na_array_reset(na_array*);
void* na_array_flatten(na_array*);
int na_array_cmp(na_array*,void* dat,int len);
int na_array_sort(na_array*);
int na_array_count(na_array*);
void* na_array_fetch(na_array*,int);
void* na_array_update(na_array*,int,void*);
void* na_array_append(na_array*,void*);
void* na_array_last(na_array*);

#endif
