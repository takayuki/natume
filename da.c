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
/* 
 * This file contains an enhanced re-implementation of "darts".
 * The key ideas and internal data structures are basically the same.
 * But, it enables a new word to be added dynamically,
 * whereas the original implementation provides read-only functionalities.
 *
 * Darts: Double-ARray Trie System
 * http://www.chasen.org/~taku/software/darts/
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include "da.h"

int
da_init(da* d)
{
  if (na_array_init(&d->unit,512*1024,sizeof(da_unit),0,0,0,d) == -1)
    return -1;
  return 0;
}

void
da_free(da* d)
{
  na_array_free(&d->unit);
}

int
da_data_attach(da* d,char* path,int (*create_header)(int))
{
  struct stat st;
  int fd;

  if (stat(path,&st) == -1) {
    fd = open(path,O_RDWR|O_CREAT,0644);
    if (fd == -1)
      return -1;
    if (create_header) {
      if (create_header(fd) == -1)
	return -1;
    }
  } else {
    fd = open(path,O_RDWR);
    if (fd == -1) {
      fd = open(path,O_RDONLY);
      if (fd == -1)
	return -1;
    }
  }
  d->dat = fd;
  return 0;
}

void
da_data_detach(da* d)
{
  close(d->dat);
}

int
da_sort(da* d,na_array* key)
{
  return na_array_sort(key);
}

static int
da_build_do(da* d,int parent,int depth,na_array* prefix,
	    na_array* key,int left,int right,
	    int (*fetch)(da*,off_t,int,uint16_t*,int))
{
  na_array sibling;
  int num;
  da_node* s;
  da_node n;
  na_array* unit = &d->unit;
  da_unit* u;
  off_t* p;
  int i;
  int c;
  uint16_t ch;
  int pos;
  int base;

  if (na_array_init(&sibling,128,sizeof(da_node),0,0,0,0) == -1)
    return -1;
  for (i = left; i <= right; i++) {
    uint16_t buf[256];
    int len; int x;

    p = (off_t*)na_array_fetch(key,i);
    len = fetch(d,*p,0,buf,sizeof(buf)/sizeof(uint16_t));
    if (len == -1)
      abort();
    if (len < depth)
      continue;
    x = na_array_cmp(prefix,buf,len);
    if (x == 0) {
      n.code = 0;
      n.val = -((int)(*p))-1;
      n.left = i;
      n.right = i;
      na_array_append(&sibling,&n);
    } else if (x < 0) {
      da_node* prev;
      prev = (da_node*)na_array_last(&sibling);
      ch = buf[depth];
      ch++;
      if (ch < buf[depth])
	return -1;
      if (prev == 0 || prev->code != ch) {
	n.code = ch;
	n.val = 0;
	n.left = i;
	n.right = i;
	na_array_append(&sibling,&n);
      } else {
	prev->right = i;
      }
    }
  }
  num = na_array_count(&sibling);
  if (num == 0)
    return -1;

  s = (da_node*)na_array_fetch(&sibling,0);
  pos = d->pos - s->code;
  if (pos < 2)
    pos = d->pos;
  while (1) {
    int first = 1;
  retry:
    for (i = 0; i < num; i++) {
      s = (da_node*)na_array_fetch(&sibling,i);
      u = (da_unit*)na_array_fetch(unit,pos+s->code);
      if (u->check != 0) {
	if (first)
	  d->pos++;
	pos++;
	goto retry;
      }
      first = 0;
    }
    break;
  }

  base = pos;
  for (i = 0; i < num; i++) {
    s = (da_node*)na_array_fetch(&sibling,i);
    u = (da_unit*)na_array_fetch(unit,base+s->code);
    u->check = parent;
  }
  for (i = 0; i < num; i++) {
    na_array expand;

    s = (da_node*)na_array_fetch(&sibling,i);
    ch = s->code;
    if (ch == 0) {
      u = (da_unit*)na_array_fetch(unit,base);
      u->base = s->val;
    } else {
      na_array_cinit(&expand,prefix);
      c = ch;
      u = (da_unit*)na_array_fetch(unit,base+c);
      ch--;
      na_array_append(&expand,&ch);
      u->base = da_build_do(d,base+c,depth+1,&expand,
			    key,s->left,s->right,fetch);
      if (u->base == -1)
	return -1;
      na_array_free(&expand);
    }
  }
  na_array_free(&sibling);
  return base;
}

int
da_build(da* d,na_array* key,int (*fetch)(da*,off_t,int,uint16_t*,int))
{
  da_unit start = { 0,0 };
  da_unit* u;
  na_array prefix;
  int cmp(void* x,void* y,void* private)
  {
    return *(uint16_t*)x - *(uint16_t*)y;
  };

  if (na_array_count(key) == 0)
    return 0;
  if (na_array_init(&prefix,256,sizeof(uint16_t),0,0,cmp,0) == -1)
    return -1;
  u = na_array_update(&d->unit,1,&start);
  d->pos = 2;
  u->base = da_build_do(d,1,0,&prefix,key,0,na_array_count(key)-1,fetch);
  if (u->base == -1)
    return -1;
  na_array_free(&prefix);
  return 0;
}

int
da_save(da* d,char* path)
{
  FILE* fp;
  void* p;

  fp = fopen(path,"w");
  if (fp == 0)
    return -1;
  p = na_array_flatten(&d->unit);
  if (p == 0)
    return -1;
  fwrite(p,sizeof(da_unit),na_array_count(&d->unit),fp);
  fclose(fp);
  return 0;
}

int
da_load(da* d,char* path)
{
  FILE* fp;
  void* p;
  da_unit u;

  fp = fopen(path,"r");
  if (fp == 0)
    return -1;
  while (fread(&u,sizeof(da_unit),1,fp)) {
    na_array_append(&d->unit,&u);
  }
  fclose(fp);
  p = na_array_flatten(&d->unit);
  if (p == 0)
    return -1;
  return 0;
}

int
da_search_do(da* d,int parent,int depth,
	     na_array* key,na_array* hit,int exact)
{
  na_array* unit = &d->unit;
  da_unit* u;
  int num;
  int pos;
  uint16_t ch;
  int next_parent;

  num = 0;
  u = (da_unit*)na_array_fetch(unit,parent);
  if (u->base < 0) {
    pos = -u->base-1;
    na_array_append(hit,&pos);
    num++;
  } else if (0 < u->base) {
    next_parent = u->base;
    u = (da_unit*)na_array_fetch(unit,next_parent);
    ch = *(uint16_t*)na_array_fetch(key,depth);
    if ((exact == 0 || ch == 0) && parent == u->check)
      num += da_search_do(d,next_parent,depth+1,key,hit,exact);
    if (0 < ch) {
      next_parent = next_parent + (ch + 1);
      u = (da_unit*)na_array_fetch(unit,next_parent);
      if (parent == u->check)
	num += da_search_do(d,next_parent,depth+1,key,hit,exact);
    }
  }
  return num;
}

int
da_search(da* d,char* key,na_array* hit,int exact)
{
  na_array prefix;
  int num;
  uint16_t ch;

  na_array_init(&prefix,256,sizeof(uint16_t),0,0,0,0);
  while (*key) {
    ch = *(unsigned char*)key++;
    na_array_append(&prefix,&ch);
  }
  num = da_search_do(d,1,0,&prefix,hit,exact);
  na_array_free(&prefix);
  return num;
}

int
da_search16(da* d,uint16_t* key,int length,na_array* hit,int exact)
{
  na_array prefix;
  int num;

  na_array_init(&prefix,256,sizeof(uint16_t),0,0,0,0);
  while (0 < length) {
    na_array_append(&prefix,key++); length--;
  }
  num = da_search_do(d,1,0,&prefix,hit,exact);
  na_array_free(&prefix);
  return num;
}

static void
da_dump_do(da* d,int parent,na_array* prefix,void (*show)(da*,int))
{
  na_array* unit = &d->unit;
  da_unit* u;
  int c;
  uint16_t ch;
  int next_parent;
  na_array next_prefix;
  int range = 4096;

  u = (da_unit*)na_array_fetch(unit,parent);
  next_parent = u->base;
  for (c = 0; c <= range; c++) {
    u = (da_unit*)na_array_fetch(unit,next_parent + c);
    if (parent != u->check)
      continue;
    if (u->base < 0) {
      show(d,-u->base-1);
    } else if (0 < u->base) {
      na_array_cinit(&next_prefix,prefix);
      ch = c - 1;
      na_array_append(&next_prefix,&ch);
      da_dump_do(d,next_parent+c,&next_prefix,show);
      na_array_free(&next_prefix);
    }
  }
}

void
da_dump(da* d,void (*show)(da*,int))
{
  na_array prefix;
  na_array_init(&prefix,256,sizeof(uint16_t),0,0,0,0);
  da_dump_do(d,1,&prefix,show);
  na_array_free(&prefix);
  return;		
}

static int
da_alloc_node(da* d,int depth,na_array* key)
{
  na_array* unit = &d->unit;
  da_unit* u;
  uint16_t ch;
  int i;
  int offset;

  ch = *(uint16_t*)na_array_fetch(key,depth);
  offset = ch == 0 ? 0 : ch + 1;
  i = 2; u = (da_unit*)na_array_fetch(unit,i+offset);
  while (u->check != 0) {
    i++; u = (da_unit*)na_array_fetch(unit,i+offset);
  }
  return i;
}

static int
da_shift_unit(da* d,int parent,int hole)
{
  na_array* unit = &d->unit;
  na_array sibling;
  da_unit s;
  da_unit* u;
  da_unit* v;
  int base_old,base_new;
  int i,j;
  int bot,pos;
  int range = 256;

  if (na_array_init(&sibling,128,sizeof(da_unit),0,0,0,0) == -1)
    return -1;
  u = (da_unit*)na_array_fetch(unit,parent);
  base_old = u->base;
  bot = -1;
  for (i = 0; i <= range; i++) {
    u = (da_unit*)na_array_fetch(unit,base_old+i);
    if (u->check == parent) {
      if (bot == -1) bot = i;
      na_array_update(&sibling,i,u);
    } else if ((base_old+i) == hole) {
      if (bot == -1) bot = i;
      s.base = 0; s.check = parent;
      na_array_update(&sibling,i,&s);
    }
  }
  if (bot == -1)
    return -1;

  pos = base_old - bot;
  if (pos < 2)
    pos = base_old;
  while (1) {
  retry:
    for (i = 0; i <= range; i++) {
      u = (da_unit*)na_array_fetch(&sibling,i);
      if (u->check == parent) {
	v = (da_unit*)na_array_fetch(unit,pos+i);
	if (v->check != 0) {
	  pos++;
	  goto retry;
	}
      }
    }
    break;
  }

  base_new = pos;
  u = (da_unit*)na_array_fetch(unit,parent);
  u->base = base_new;
  for (i = 0; i <= range; i++) {
    u = (da_unit*)na_array_fetch(&sibling,i);
    if (0 < u->base) {
      for (j = 0; j <= range; j++) {
	v = (da_unit*)na_array_fetch(unit,u->base+j);
	if (v->check == (base_old + i))
	  v->check = base_new + i;
      }
    }
    if (u->base != 0) {
      v = (da_unit*)na_array_fetch(unit,base_new+i);
      v->check = u->check; v->base = u->base;
      u = (da_unit*)na_array_fetch(unit,base_old+i);
      u->check = 0; u->base = 0;
    }
  }
  return base_new;
}


static int
da_insert_do(da* d,int parent,int depth,na_array* key,int val)
{
  na_array* unit = &d->unit;
  da_unit* u;
  uint16_t ch;
  int next_parent;

  u = (da_unit*)na_array_fetch(unit,parent);
  if (u->base < 0) {
    u->base = -val-1;  /* overwrite duplicate records */
  } else if (0 < u->base) {
    next_parent = u->base;
    u = (da_unit*)na_array_fetch(unit,next_parent);
    ch = *(uint16_t*)na_array_fetch(key,depth);
    if (ch == 0) {
      if (parent != u->check) {
	if (u->check == 0) {
	  u->check = parent;
	  u->base = -val-1;
	} else { 
	  next_parent = da_shift_unit(d,parent,next_parent);
	  u = (da_unit*)na_array_fetch(unit,next_parent);
	  u->check = parent;
	  u->base = -val-1;
	}
      } else {
	if (da_insert_do(d,next_parent,depth+1,key,val) == -1)
	  return -1;
      }
    } else {
      next_parent = next_parent + (ch + 1);
      u = (da_unit*)na_array_fetch(unit,next_parent);
      if (parent != u->check) {
	if (u->check == 0) {
	  u->check = parent;
	  u->base = da_alloc_node(d,depth+1,key);
	} else {
	  next_parent = da_shift_unit(d,parent,next_parent) + (ch + 1);
	  u = (da_unit*)na_array_fetch(unit,next_parent);
	  u->check = parent;
	  u->base = da_alloc_node(d,depth+1,key);
	}
      }
      if (da_insert_do(d,next_parent,depth+1,key,val) == -1)
	return -1;
    }
  }
  return 0;
}

int
da_insert(da* d,char* key,int val)
{
  na_array prefix;
  uint16_t ch;
  da_unit* u;
  int result;

  na_array_init(&prefix,256,sizeof(uint16_t),0,0,0,0);
  while (*key) {
    ch = *(unsigned char*)key++;
    na_array_append(&prefix,&ch);
  }
  u = (da_unit*)na_array_fetch(&d->unit,1);
  if (u->base == 0) {
    da_unit start = { 2,0 };
    na_array_update(&d->unit,1,&start);
  }
  result = da_insert_do(d,1,0,&prefix,val);
  na_array_free(&prefix);
  return result;
}
