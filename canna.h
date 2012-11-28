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
#ifndef _CANNA_H
#define _CANNA_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <sys/types.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

typedef struct {
  int32_t maj;
  int32_t size;
  uint8_t* data;
} canna_request_type0;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
} canna_request_type1;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t data;
} canna_request_type2;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t cxt;
  uint16_t data;
} canna_request_type3;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t cxt;
  int16_t idx;
  uint16_t data;
} canna_request_type6;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t cxt;
  int16_t idx;
  int16_t data;
} canna_request_type7;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t cxt;
  int16_t idx;
  int16_t cnd;
  uint16_t data;
} canna_request_type8;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t cxt;
  int16_t idx;
  int16_t cnd;
  int16_t data;
} canna_request_type9;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t cxt;
  int16_t num;
  int32_t mode;
  int16_t* data;
} canna_request_type10;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t cxt;
  uint8_t* data;
} canna_request_type12;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int32_t mode;
  int16_t cxt;
  uint16_t* data;
} canna_request_type14;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int32_t mode;
  int16_t cxt;
  uint8_t* data;
} canna_request_type15;

typedef union {
  struct {
    uint16_t min;
    int16_t cxt;
  } ok;
  int32_t err;
} canna_response_type0;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int8_t data;
} canna_response_type2;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int8_t stat;
  uint16_t* data;
} canna_response_type3;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int8_t stat;
  uint32_t* data;
} canna_response_type4;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t data;
} canna_response_type5;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t num;
  uint8_t* data;
} canna_response_type6;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t num;
  uint16_t* data;
} canna_response_type7;

typedef struct {
  uint8_t maj;
  uint8_t min;
  uint16_t size;
  int16_t num;
  int32_t* data;
} canna_response_type9;


int canna_init(char*,int,int,int);
void canna_free(int);
int canna_accept(int,int*,int*,int*,char*,int);
int canna_establish(int,int*,int*,int*,int*);
int canna_request(int,int*,int*,int*,int*,int*,int*,char*,int);
int canna_response(int,int*,int*,int*,int*,int*,int*,char**);

#endif
