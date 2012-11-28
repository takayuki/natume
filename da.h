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
#ifndef _DA_H
#define _DA_H

#include "array.h"
#include <sys/types.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

typedef struct {
  int base;
  int check;
} da_unit;

typedef struct {
  uint16_t code;
  int val;
  size_t left;
  size_t right;
} da_node;

typedef struct {
  na_array unit;
  int dat;
  int pos;
} da;

int da_init(da*);
void da_free(da*);
int da_create_header(int);
int da_data_attach(da*,char*,int (*)(int));
void da_data_detach(da*);
int da_sort(da*,na_array*);
int da_build(da*,na_array*,int (*)(da*,off_t,int,uint16_t*,int));
int da_save(da*,char*);
int da_load(da*,char*);
int da_search(da*,char*,na_array*,int);
int da_search16(da*,uint16_t*,int,na_array*,int);
void da_dump(da*,void (*)(da*,int));
int da_insert(da*,char*,int);

#endif

