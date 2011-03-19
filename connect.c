/*
 *  Natume -- an implementation of Kana-Kanji conversion in Haskell
 *  Copyright (C) 2006-2011 Takayuki Usui
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
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "connect.h"

static da_connect connect[1];

static int
fetch_key16(da* d,off_t offset,int more,uint16_t* buf,int max)
{
  int fd = d->dat;
  uint16_t len;
  int i;
  uint16_t c;

  if (lseek(fd,offset + more,SEEK_SET) == -1)
    return 0;
  if (read(fd,&len,2) != 2)
    return 0;
  len = len < (--max) ? len : max;
  for (i = 0; i < len; i++) {
    if (read(fd,&c,2) != 2)
      return 0;
    buf[i] = c;
  }
  buf[len] = 0;
  return len;
}

static int
fetch_uint16(da* d,off_t offset,uint16_t more,uint16_t* val)
{
  int fd = d->dat;
  if (lseek(fd,offset + more,SEEK_SET) == -1)
    return 0;
  if (read(fd,val,2) != 2)
    return 0;
  return 2;
}

static int
fetch(da* d,int pos,uint16_t* store,int max,uint16_t* cost)
{
  int len,offset;

  len = fetch_key16(d,pos,0,store,max);
  offset = 2 + 2 * len;
  fetch_uint16(d,pos,offset,cost);
  return len;
}

static void
show(da* d,int pos)
{
  int len,offset;
  uint16_t s[8];
  uint16_t cost;

  len = fetch_key16(d,pos,0,s,8);
  offset = 2 + 2 * len;
  fetch_uint16(d,pos,offset,&cost);
  if (len == 1) {
    printf("([%d],%d)\n",s[0],cost);
  } else if (len == 2) {
    printf("([%d,%d],%d)\n",s[0],s[1],cost);
  } else if (len == 3) {
    printf("([%d,%d,%d],%d)\n",s[0],s[1],s[2],cost);
  }
}

static int
cmp(void* _p,void* _q,void* _d)
{
  off_t* p = _p;
  off_t* q = _q;
  da* d = _d;
  int i,m,n,x;
  uint16_t s[8],t[8];

  i = 0;
  m = fetch_key16(d,*p,0,s,8);
  n = fetch_key16(d,*q,0,t,8);
  while (i < m && i < n) {
    x = s[i] - t[i];
    if (x != 0) return x;
    i++;
  }
  x = (m - i) - (n - i);
  return x;
}

int
connect_init(char* idx,char* dat)
{
  int id = 0;
  da_connect* con = &connect[id];

  if (da_init(&con->d))
    return -1;
  if (na_array_init(&con->key,1024,sizeof(off_t),0,0,cmp,&con->d) == -1)
    return -1;
  con->idx = strdup(idx);
  con->dat = strdup(dat);
  na_array_init(&con->hit,256,sizeof(int),0,0,0,0);
  return id;
}

void
connect_free(int id)
{
  da_connect* con = &connect[id];
  na_array_free(&con->hit);
  free(con->dat);
  free(con->idx);
  na_array_free(&con->key);
  da_free(&con->d);
}

int
connect_start(int id)
{
  da_connect* con = &connect[id];
  con->fp = fopen(con->dat,"w+");
  if (con->fp == 0)
    return -1;
  return 0;
}

void
connect_stop(int id)
{
  da_connect* con = &connect[id];
  fclose(con->fp);
}

int
connect_add(int id,int* rule,int length,int cost)
{
  da_connect* con = &connect[id];
  off_t pos;
  uint16_t ren,len,cst;

  len = length;
  pos = ftello(con->fp);
  if (fwrite(&len,sizeof(uint16_t),1,con->fp) != 1)
    return -1;
  while (0 < len) {
    ren = (uint16_t)(*rule);
    if (fwrite(&ren,sizeof(uint16_t),1,con->fp) != 1)
      return -1;
    rule++; len--;
  }
  cst = cost;
  if (fwrite(&cst,sizeof(uint16_t),1,con->fp) != 1)
    return -1;
  na_array_append(&con->key,&pos);
  return 0;
}

int
connect_build(int id)
{
  da_connect* con = &connect[id];

  if (da_data_attach(&con->d,con->dat,0) == -1)
    return -1;
  if (da_sort(&con->d,&con->key) == -1)
    return -1;
  if (da_build(&con->d,&con->key,fetch_key16) == -1)
    return -1;
  da_data_detach(&con->d);
  if (da_save(&con->d,con->idx) == -1)
    return -1;
  return 0;
}

int
connect_load(int id)
{
  da_connect* con = &connect[id];

  if (da_data_attach(&con->d,con->dat,0) == -1)
    return -1;
  if (da_load(&con->d,con->idx) == -1)
    return -1;
  return 0;
}

void
connect_unload(int id)
{
  da_connect* con = &connect[id];
  da_data_detach(&con->d);
}

int
connect_search(int id,int* key,int length)
{
  da_connect* con = &connect[id];
  int i;
  uint16_t prefix[length];

  for (i = 0; i < length; i++)
    prefix[i] = key[i];
  na_array_reset(&con->hit);
  da_search16(&con->d,prefix,length,&con->hit,0);
  return na_array_count(&con->hit);
}

int
connect_fetch(int id,int n,int* rule,int max,int* cost)
{
  da_connect* con = &connect[id];
  uint16_t st[max];
  uint16_t cst;
  int pos,num;
  int i;

  if (na_array_count(&con->hit) <= n)
    return 0;
  pos = *(int*)na_array_fetch(&con->hit,n);
  num = fetch(&con->d,pos,st,max,&cst);
  for (i = 0; i <= num; i++)
    rule[i] = st[i];
  *cost = cst;
  return num;
}

void
connect_dump(int id)
{
  da_connect* con = &connect[id];
  da_dump(&con->d,show);
}
