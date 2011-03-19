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
#include <fcntl.h>
#include <stdio.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include "dic.h"

static da_dic dict[16];
static int dic_max = sizeof(dict)/sizeof(da_dic);
static int dic_num;

static time_t birth = 1142262000;

static int
align(int x)
{
  return (x % 2) == 0 ? x : x + 1;
}

static uint16_t
align16(uint16_t x)
{
  return (x % 2) == 0 ? x : x + 1;
}

static int
fetch_key(da* d,off_t offset,uint16_t more,char* buf,int max)
{
  int fd = d->dat;
  uint16_t len;
  
  if (lseek(fd,offset + more,SEEK_SET) == -1)
    return 0;
  if (read(fd,&len,2) != 2)
    return 0;
  len = len < (--max) ? len : max;
  if (read(fd,buf,len) != len)
    return 0;
  buf[len] = 0;
  return len;
}

static int
fetch_key16(da* d,off_t offset,int more,uint16_t* buf,int max)
{
  int fd = d->dat;
  uint16_t len;
  int i;
  unsigned char c;
  
  if (lseek(fd,offset + more,SEEK_SET) == -1)
    return 0;
  if (read(fd,&len,2) != 2)
    return 0;
  len = len < (--max) ? len : max;
  for (i = 0; i < len; i++) {
    if (read(fd,&c,1) != 1)
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
fetch_uint32(da* d,off_t offset,uint16_t more,uint32_t* val)
{
  int fd = d->dat;

  if (lseek(fd,offset + more,SEEK_SET) == -1)
    return 0;
  if (read(fd,val,4) != 4)
    return 0;
  return 4;
}

static void
show(da* d,int pos)
{
  int length,offset;
  int i;
  dic_yomi y;
  dic_word w;

  length = fetch_key(d,pos,0,y.yomi,sizeof(y.yomi));
  offset = 2 + align(length);
  fetch_uint16(d,pos,offset,&y.cnt);
  offset += 2;
  for (i = 0; i < y.cnt; i++) {
    fetch_uint16(d,pos,offset,&w.tbl);
    offset += 2;
    fetch_uint16(d,pos,offset,&w.cst);
    offset += 2;
    fetch_uint32(d,pos,offset,&w.pnt);
    offset += 4;
    length = fetch_key(d,pos,offset,w.word,sizeof(w.word));
    offset += 2 + align(length);
    printf("%s %s %d %d %d\n",y.yomi,w.word,w.tbl,w.cst,w.pnt);
  }
}

static int
cmp2(void* _p,void* _q,void* _d)
{
  off_t* p = _p;
  off_t* q = _q;
  da* d = _d;
  int m,n,x;
  char s[64],t[64];
  uint16_t u,v;

  m = fetch_key(d,*p,0,s,sizeof(s));
  n = fetch_key(d,*q,0,t,sizeof(t));
  if ((x = strcmp(s,t)) == 0) {
    m = align(m);
    n = align(n);
    fetch_uint16(d,*p,m+2,&u);
    fetch_uint16(d,*q,n+2,&v);
    if ((x = u - v) == 0) {
      fetch_uint16(d,*p,m+4,&u);
      fetch_uint16(d,*q,n+4,&v);
      if ((x = u - v) == 0) {
	fetch_key(d,*p,m+10,s,sizeof(s));
	fetch_key(d,*q,n+10,t,sizeof(t));
	x = strcmp(s,t);
      }
    }
  }
  return x;
}

static int
cmp1(void* _p,void* _q,void* _d)
{
  off_t* p = _p;
  off_t* q = _q;
  da* d = _d;
  char s[64],t[64];

  fetch_key(d,*p,0,s,sizeof(s));
  fetch_key(d,*q,0,t,sizeof(t));
  return strcmp(s,t);
}

int
dic_init(char* idx,char* dat,char* sta,char* tmp)
{
  int id;
  da_dic* dic;
  da_dic* bld;

  id = dic_num; 
  if (dic_max <= id) return -1;
  dic = &dict[id];
  dic_num++;
  memset(dic,0,sizeof(*dic));
  if (tmp == 0) {
    dic->bld.id = 0;
  } else {
    if (dic_max <= (id + 1)) return -1;
    dic->bld.id = id + 1;
    bld = &dict[dic->bld.id];
    dic_num++;
    memset(bld,0,sizeof(*bld));
    if (da_init(&bld->d) == -1)
      return -1;
    if (na_array_init(&bld->key,1024,sizeof(off_t),0,0,cmp2,&bld->d) == -1)
      return -1;
    bld->dat = strdup(tmp);
  }
  if (da_init(&dic->d))
    return -1;
  if (na_array_init(&dic->key,1024,sizeof(off_t),0,0,cmp1,&dic->d) == -1)
    return -1;
  dic->idx = strdup(idx);
  dic->dat = strdup(dat);
  if (sta) dic->sta = strdup(sta);
  na_array_init(&dic->hit,256,sizeof(int),0,0,0,0);
  return id;
}

void
dic_free(int id)
{
  da_dic* dic = &dict[id];

  if (dic->bld.id) dic_free(dic->bld.id);
  na_array_free(&dic->hit);
  if (dic->sta) free(dic->sta);
  if (dic->dat) free(dic->dat);
  if (dic->idx) free(dic->idx);
  na_array_free(&dic->key);
  da_free(&dic->d);
}

int
dic_start(int id)
{
  da_dic* dic = &dict[id];
  da_dic* bld = &dict[dic->bld.id];

  bld->bld.fp = fopen(bld->dat,"w+");
  if (bld->bld.fp == 0)
    return -1;
  bld->endpnt = 0;
  return 0;
}

void
dic_stop(int id)
{
  da_dic* dic = &dict[id];
  da_dic* bld = &dict[dic->bld.id];
  fclose(bld->bld.fp);
}

int
dic_add(int id,char* yomi,char* word,int table,int cost,int point)
{
  da_dic* dic = &dict[id];
  da_dic* bld = &dict[dic->bld.id];
  FILE* tmp = bld->bld.fp;
  off_t pos;
  dic_yomi y;
  dic_word w;

  pos = ftello(tmp);
  if (pos == -1)
    return -1;
  y.len = strlen(yomi);
  if (fwrite(&y.len,sizeof(y.len),1,tmp) != 1)
    return -1;
  y.len = align16(y.len);
  if (fwrite(yomi,1,y.len,tmp) != y.len)
    return -1;
  w.tbl = table;
  w.cst = cost;
  w.pnt = (uint32_t)point;
  if (fwrite(&w.tbl,sizeof(w.tbl),1,tmp) != 1)  
    return -1;
  if (fwrite(&w.cst,sizeof(w.cst),1,tmp) != 1)
    return -1;
  if (fwrite(&w.pnt,sizeof(w.pnt),1,tmp) != 1)
    return -1;
  w.len = strlen(word);
  if (fwrite(&w.len,sizeof(w.len),1,tmp) != 1)
    return -1;
  w.len = align16(w.len);
  if (fwrite(word,1,w.len,tmp) != w.len)
    return -1;
  na_array_append(&bld->key,&pos);
  if (bld->endpnt < point)
    bld->endpnt = point;
  return 0;
}

static int
create_header(int fd)
{
  uint16_t zero16 = 0;
  uint32_t zero32 = 0;

  if (write(fd,&zero16,sizeof(zero16)) != sizeof(zero16))
    return -1;
  if (write(fd,&zero16,sizeof(zero16)) != sizeof(zero16))
    return -1;
  if (write(fd,&zero16,sizeof(zero16)) != sizeof(zero16))
    return -1;
  if (write(fd,&zero16,sizeof(zero16)) != sizeof(zero16))
    return -1;
  if (write(fd,&zero32,sizeof(zero32)) != sizeof(zero32))
    return -1;
  if (write(fd,&zero16,sizeof(zero16)) != sizeof(zero16))
    return -1;
  return 0;
}

int
dic_build(int id)
{
  da_dic* dic = &dict[id];
  da_dic* bld = &dict[dic->bld.id];
  FILE* dat,* sta;
  off_t pos,key,last;
  int length,offset;
  int i;
  dic_yomi y;
  dic_word w;
  struct _prev {
    dic_yomi y;
    dic_word w;
  } prev;

  if (da_data_attach(&bld->d,bld->dat,0) == -1)
    return -1;
  if (da_sort(&bld->d,&bld->key) == -1)
    return -1;
  dat = fopen(dic->dat,"w+");
  if (dat == 0)
    return -1;
  memset(&prev,0,sizeof(prev));
  last = 0; y.cnt = 0;
  for (i = 0; i < na_array_count(&bld->key); i++) {
    key = *(off_t*)na_array_fetch(&bld->key,i);
    pos = ftello(dat);
    length = fetch_key(&bld->d,key,0,y.yomi,sizeof(y.yomi));
    if (length == -1)
      return -1;
    if (strcmp(prev.y.yomi,y.yomi)) {
      if (last) {
        fseeko(dat,last,SEEK_SET);
        if (fwrite(&y.cnt,sizeof(y.cnt),1,dat) != 1)
	  return -1;
        fseeko(dat,pos,SEEK_SET);
      }
      na_array_append(&dic->key,&pos);
      y.len = strlen(y.yomi);
      if (fwrite(&y.len,sizeof(y.len),1,dat) != 1)
	return -1;
      y.len = align16(y.len);
      if (fwrite(y.yomi,1,y.len,dat) != y.len)
	return -1;
      last = ftello(dat);
      y.cnt = 0;
      if (fwrite(&y.cnt,sizeof(y.cnt),1,dat) != 1)
	return -1;
      strcpy(prev.y.yomi,y.yomi);
      prev.w.tbl = 0;
    }
    offset = 2 + align(length);
    fetch_uint16(&bld->d,key,offset,&w.tbl);
    offset += 2;
    fetch_uint16(&bld->d,key,offset,&w.cst);
    offset += 2;
    fetch_uint32(&bld->d,key,offset,&w.pnt);
    offset += 4;
    length = fetch_key(&bld->d,key,offset,w.word,sizeof(w.word));
    if (length == -1)
      return -1;
    if (prev.w.tbl == w.tbl && strcmp(prev.w.word,w.word) == 0)
      continue;
    prev.w.tbl = w.tbl; strcpy(prev.w.word,w.word);
    y.cnt++;
    if (fwrite(&w.tbl,sizeof(w.tbl),1,dat) != 1)
      return -1;
    if (fwrite(&w.cst,sizeof(w.cst),1,dat) != 1)
      return -1;
    if (fwrite(&w.pnt,sizeof(w.pnt),1,dat) != 1)
      return -1;
    w.len = strlen(w.word);
    if (fwrite(&w.len,sizeof(w.len),1,dat) != 1)
      return -1;
    w.len = align16(w.len);
    if (fwrite(w.word,1,w.len,dat) != w.len)
      return -1;
  }
  if (last) {
    if (fseeko(dat,last,SEEK_SET) == -1)
      return -1;
    if (fwrite(&y.cnt,sizeof(y.cnt),1,dat) != 1)
      return -1;
  }
  fclose(dat);
  da_data_detach(&bld->d);

  if (da_data_attach(&dic->d,dic->dat,create_header) == -1)
    return -1;
  if (da_sort(&dic->d,&dic->key) == -1)
    return -1;
  if (da_build(&dic->d,&dic->key,fetch_key16) == -1)
    return -1;
  if (da_save(&dic->d,dic->idx) == -1)
    return -1;
  da_data_detach(&dic->d);

  sta = fopen(dic->sta,"w+");
  if (dat == 0)
    return -1;
  fclose(sta);
  return 0;
}

static int
dic_rebuild(int id)
{
  da_dic* dic = &dict[id];
  da* d = &dic->d;
  int i;
  off_t pos;
  int length,offset;
  dic_yomi y;
  dic_word w;

  pos = 14; offset = 0;
  while (1) {
    pos += offset;
    length = fetch_key(d,pos,0,y.yomi,sizeof(y.yomi));
    if (length == -1 || length == 0)
      break;
    offset = 2 + align(length);
    fetch_uint16(d,pos,offset,&y.cnt);
    offset += 2;
    for (i = 0; i < y.cnt; i++) {
      fetch_uint16(d,pos,offset,&w.tbl);
      offset += 2;
      fetch_uint16(d,pos,offset,&w.cst);
      offset += 2;
      fetch_uint32(d,pos,offset,&w.pnt);
      offset += 4;
      length = fetch_key(d,pos,offset,w.word,sizeof(w.word));
      offset += 2 + align(length);
    }
    na_array_append(&dic->key,&pos);
  }
  if (na_array_count(&dic->key) == 0)
    return 0;
  if (da_sort(&dic->d,&dic->key) == -1)
    return -1;
  if (da_build(&dic->d,&dic->key,fetch_key16) == -1)
    return -1;
  return 0;
}

int
dic_load(int id)
{
  da_dic* dic = &dict[id];
  int fd;

  if (da_data_attach(&dic->d,dic->dat,create_header) == -1)
    return -1;
  if (da_load(&dic->d,dic->idx) == -1 && dic_rebuild(id) == -1)
    return -1;
  if (dic->sta) {
    fd = open(dic->sta,O_RDWR|O_CREAT,0644);
    if (fd == -1)
      return -1;
    dic->st = fd;
  }
  return 0;
}

void
dic_unload(int id)
{
  da_dic* dic = &dict[id];

  if (dic->sta) close(dic->st);
  da_data_detach(&dic->d);
}

static int
lock_stat(int id,int point)
{
  da_dic* dic = &dict[id];
  struct flock lck;

  if (dic->sta == 0)
    return 1;
  lck.l_start = sizeof(dic_stat) * point;
  lck.l_len = sizeof(dic_stat);
  lck.l_type = F_RDLCK;
  lck.l_whence = SEEK_SET;
  return fcntl(dic->st,F_SETLK,&lck);
}

static int
unlock_stat(int id,int point)
{
  da_dic* dic = &dict[id];
  struct flock lck;

  if (dic->sta == 0)
    return 1;
  lck.l_start = sizeof(dic_stat) * point;
  lck.l_len = sizeof(dic_stat);
  lck.l_type = F_UNLCK;
  lck.l_whence = SEEK_SET;
  return fcntl(dic->st,F_SETLK,&lck);
}

int
dic_get_stat(int id,int point,dic_stat* st)
{
  da_dic* dic = &dict[id];
  int fd;

  if (point <= 0)
    return -1;
  if (dic->sta == 0) {
    memset(st,0,sizeof(*st));
    return 0;
  }
  lock_stat(id,point);
  fd = dic->st;
  if (lseek(fd,sizeof(dic_stat) * point,SEEK_SET) == -1)
    return 0;
  if (read(fd,st,sizeof(dic_stat)) != sizeof(dic_stat))
    return 0;
  unlock_stat(id,point);
  return 0;
}

int
dic_update(int id,int point,int style)
{
  da_dic* dic = &dict[id];
  int fd;
  dic_stat st;

  if (point <= 0)
    return -1;
  if (dic->sta == 0)
    return 0;
  lock_stat(id,point);
  fd = dic->st;
  if (lseek(fd,sizeof(st) * point,SEEK_SET) == -1)
    return 0;
  if (read(fd,&st,sizeof(st)) != sizeof(st))
    memset(&st,0,sizeof(st));
  if  (st.count < 2047) st.count += 1;
  st.style = style;
  st.mtime = (uint32_t)(time(0) - birth);
  if (lseek(fd,sizeof(st) * point,SEEK_SET) == -1)
    return 0;
  if (write(fd,&st,sizeof(st)) != sizeof(st))
    return 0;
  fsync(fd);
  unlock_stat(id,point);
  return 0;
}

int
dic_search(int id,char* key,int exact)
{
  da_dic* dic = &dict[id];
  na_array_reset(&dic->hit);
  da_search(&dic->d,key,&dic->hit,exact);
  return na_array_count(&dic->hit);
}

static int
fetch(int id,int pos,char** yomi,char** word,
      uint16_t* table,uint16_t* cost,
      int32_t* point,int32_t* style,int32_t* last,int width,int height)
{
  da_dic* dic = &dict[id];
  da* d = &dic->d;
  char buf[width];
  uint16_t cnt;
  int length,offset,i,diff;
  dic_stat st;

  length = fetch_key(d,pos,0,buf,width);
  offset = 2 + align(length);
  fetch_uint16(d,pos,offset,&cnt);
  offset += 2;
  cnt = cnt < height ? cnt : height;
  for (i = 0; i < cnt; i++) {
    strcpy(*(yomi+i),buf);
    fetch_uint16(d,pos,offset,table+i);
    offset += 2;
    fetch_uint16(d,pos,offset,cost+i);
    offset += 2;
    fetch_uint32(d,pos,offset,(uint32_t*)point+i);
    offset += 4;
    length = fetch_key(d,pos,offset,*(word+i),width);
    offset += 2 + align(length);
    if (dic_get_stat(id,point[i],&st) == -1)
      return 0;
    style[i] = st.style;
    diff = time(0) - (st.mtime + birth);
    last[i] = 0 < diff ? diff : 0;
  }
  return cnt;
}

int
dic_fetch(int id,int n,char** yomi,char** word,
	  int* table,int* cost,
	  int* point,int* style,int* last,int width,int height)
{
  da_dic* dic = &dict[id];
  uint16_t tbl[height],cst[height];
  int i;
  int pos,num;

  if (na_array_count(&dic->hit) <= n)
    return 0;
  pos = *(int*)na_array_fetch(&dic->hit,n);
  num = fetch(id,pos,yomi,word,tbl,cst,point,style,last,width,height);
  for (i = 0; i < num; i++) {
    table[i] = tbl[i]; cost[i] = cst[i];
  }
  return num;
}

void
dic_dump(int id)
{
  da_dic* dic = &dict[id];
  da_dump(&dic->d,show);
}

static int
lock_dic(int id)
{
  da_dic* dic = &dict[id];
  struct flock lck;

  lck.l_start = 0;
  lck.l_len = 0;
  lck.l_type = F_RDLCK;
  lck.l_whence = SEEK_SET;
  return fcntl(dic->d.dat,F_SETLK,&lck);
}

static int
unlock_dic(int id)
{
  da_dic* dic = &dict[id];
  struct flock lck;

  lck.l_start = 0;
  lck.l_len = 0;
  lck.l_type = F_UNLCK;
  lck.l_whence = SEEK_SET;
  return fcntl(dic->d.dat,F_SETLK,&lck);
}

static uint32_t
dic_point(int id)
{
  da_dic* dic = &dict[id];
  da* d = &dic->d;
  off_t pos;
  int length,offset;
  char null[64];
  uint16_t null16;
  uint32_t point;

  pos = 0; offset = 0;
  length = fetch_key(d,pos,offset,null,sizeof(null));
  if (length != 0)
    return -1;
  offset += 2;
  fetch_uint16(d,pos,offset,&null16);
  offset += 2;
  fetch_uint16(d,pos,offset,&null16);
  offset += 2;
  fetch_uint16(d,pos,offset,&null16);
  offset += 2;
  fetch_uint32(d,pos,offset,&point);
  return point;
}

int
dic_insert(int id,char* yomi,char* word,int table,int cost,int style)
{
  da_dic* dic = &dict[id];
  int fd;
  int val;
  dic_yomi y;
  dic_word w;
  dic_stat st; 
  int result;

  y.cnt = 1; w.tbl = table; w.cst = cost;
  lock_dic(id);
  w.pnt = dic_point(id);
  if (w.pnt == -1)
    return -1;
  w.pnt += 1;
  fd = dic->d.dat;
  if ((val = lseek(fd,0,SEEK_END)) == -1)
    return -1;
  y.len = strlen(yomi);
  if (write(fd,&y.len,sizeof(y.len)) != sizeof(y.len))
    return -1;
  y.len = align16(y.len);
  if (write(fd,yomi,y.len) != y.len)
    return -1;
  if (write(fd,&y.cnt,sizeof(y.cnt)) != sizeof(y.cnt))
    return -1;
  if (write(fd,&w.tbl,sizeof(w.tbl)) != sizeof(w.tbl))
    return -1;
  if (write(fd,&w.cst,sizeof(w.cst)) != sizeof(w.cst))
    return -1;
  if (write(fd,&w.pnt,sizeof(w.pnt)) != sizeof(w.pnt))
    return -1;
  w.len = strlen(word);
  if (write(fd,&w.len,sizeof(w.len)) != sizeof(w.len))
    return -1;
  w.len = align16(w.len);
  if (write(fd,word,w.len) != w.len)
    return -1;
  result = da_insert(&dic->d,yomi,val);
  if (result == 0) {
    if (lseek(fd,8,SEEK_SET) == -1)
      return -1;
    if (write(fd,&w.pnt,sizeof(w.pnt)) != sizeof(w.pnt))
      return -1;
    if (dic->sta) {
      fd = dic->st;
      if (lseek(fd,sizeof(st) * w.pnt,SEEK_SET) == -1)
	return -1;
      memset(&st,0,sizeof(st));
      st.count = 1;
      st.style = style;
      st.mtime = (uint32_t)(time(0) - birth);
      if (write(fd,&st,sizeof(st)) != sizeof(st))
	return -1;
      fsync(fd);
    }
    /* if (da_save(&dic->d,dic->idx) == -1) return -1; */
  }
  unlock_dic(id);
  return result;
}

