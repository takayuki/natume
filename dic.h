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
#ifndef _DIC_H
#define _DIC_H

#include "array.h"
#include "da.h"

typedef struct {
  na_array key;
  na_array hit;
  da d;
  char* idx;
  char* dat;
  char* sta;
  struct { int id; FILE* fp; } bld;
  int st;
  int endpnt;
} da_dic;

typedef struct {
  int16_t count:12,style:4;
  int16_t reserve;
  int32_t mtime;
} __attribute__ ((packed)) dic_stat;

typedef struct {
  uint16_t len;
  char yomi[64];
  uint16_t cnt;
} dic_yomi;

typedef struct {
  uint16_t len;
  char word[64];
  uint16_t tbl;
  uint16_t cst;
  uint32_t pnt;
} dic_word;

int dic_init(char*,char*,char*,char*);
void dic_free(int);
int dic_start(int);
void dic_stop(int);
int dic_add(int,char*,char*,int,int,int);
int dic_build(int);
int dic_load(int);
void dic_unload(int);
int dic_update(int,int,int);
int dic_search(int,char*,int);
int dic_fetch(int,int n,char**,char**,int*,int*,int*,int*,int*,int,int);
void dic_dump(int);
int dic_insert(int,char*,char*,int,int,int);

#endif
