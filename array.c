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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include "array.h"

int
na_vector_init(na_vector* s,size_t base,size_t len,size_t size,
	       void (*init)(void*,void*),void (*final)(void*,void*),
	       void* private)
{
  if (s == 0) return -1;
  s->len = len;
  s->size = size;
  s->data = calloc(s->len,s->size);
  if (s->data == 0) abort();
  s->num = 0;
  s->init = init;
  s->final = final;
  s->private = private;
  s->bot = base;
  s->top = s->bot + s->len;
  return 0;
}

int
na_vector_cinit(na_vector* s,na_vector* t)
{
  if (s == 0 || t == 0) return -1;
  s->len = t->len;
  s->size = t->size;
  s->data = calloc(s->len,s->size);
  if (s->data == 0) abort();
  memcpy(s->data,t->data,(t->len * t->size));
  s->num = t->num;
  s->init = t->init;
  s->final = t->final;
  s->private = t->private;
  s->bot = t->bot;
  s->top = s->bot + s->len;
  return 0;
}

void
na_vector_free(na_vector* s)
{
  int i;

  if (s == 0) return;
  if (s->data) {
    if (s->final) {
      for (i = 0; i < s->num; i++)
	s->final(s->private,s->data + s->size * i);
    }
    free(s->data);
  }
}

void
na_vector_reset(na_vector* s)
{
  int i;

  if (s == 0) return;
  if (s->data) {
    if (s->final) {
      for (i = 0; i < s->num; i++)
	s->final(s->private,s->data + s->size * i);
    }
  }
  s->num = 0;
}

int
na_vector_count(na_vector* s)
{
  if (s == 0) return -1;
  return s->num;
}

void*
na_vector_fetch(na_vector* s,int idx)
{
  if (s == 0) return 0;
  if (s->len <= idx)
    return 0;
  while (s->num <= idx) {
    if (s->init)
      s->init(s->private,s->data + (s->size * s->num));
    else
      memset(s->data + (s->size * s->num),0,s->size);
    s->num++;
  }
  return s->data + (s->size * idx);
}

void*
na_vector_update(na_vector* s,int idx,void* member)
{
  void* p = na_vector_fetch(s,idx);

  if (p == 0) return 0;
  if (s->final)
    s->final(s->private,p);
  memcpy(p,member,s->size);
  return p;
}

void*
na_vector_append(na_vector* s,void* member)
{
  return na_vector_update(s,s->num,member);
}

int
na_array_init(na_array* a,size_t shot,size_t size,
	      void (*init)(void*,void*),void (*final)(void*,void*),
	      int (*cmp)(void*,void*,void*),void* private)
{
  if (a == 0) return -1;
  a->shot = shot;
  a->len = a->shot;
  a->size = size;
  if (na_vector_init(&a->vector.head,0,a->len,a->size,
		     init,final,private) == -1)
    return -1;
  a->vector.tail = 0;
  a->num = 0;
  a->win = &a->vector;
  a->init = init;
  a->final = final;
  a->cmp = cmp;
  a->private = private;
  return 0;
}

int
na_array_cinit(na_array* a,na_array* b)
{
  void* bp;
  if (a == 0 || b == 0) return -1;
  bp = na_array_flatten(b);
  if (bp == 0) return -1;
  a->shot = b->shot;
  a->len = b->len;
  a->size = b->size;
  if (na_vector_cinit(&a->vector.head,&b->vector.head) == -1)
    return -1;
  a->vector.tail = 0;
  a->num = b->num;
  a->win = &a->vector;
  a->init = b->init;
  a->final = b->final;
  a->cmp = b->cmp;
  a->private = b->private;
  return 0;
}

void
na_array_free(na_array* a)
{
  na_array_cell* u;
  na_array_cell* v;

  if (a == 0) return;
  u = &a->vector;
  na_vector_free(&u->head);
  u = a->vector.tail;
  while (u) {
    na_vector_free(&u->head);
    v = u->tail;
    free(u);
    u = v;
  }
}

void
na_array_reset(na_array* a)
{
  na_array_cell* u;
  na_array_cell* v;

  if (a == 0) return;
  u = &a->vector;
  na_vector_reset(&u->head);
  u = a->vector.tail;
  while (u) {
    na_vector_free(&u->head);
    v = u->tail;
    free(u);
    u = v;
  }
  a->vector.tail = 0;
  a->len = a->vector.head.len;
  a->num = a->vector.head.num;
  a->win = &a->vector;
}

void*
na_array_flatten(na_array* a)
{
  na_array b;
  void* g;
  na_array_cell* p;
  int s;

  if (a == 0) return 0;
  if (a->vector.tail == 0)
    return a->vector.head.data;
  b.shot = a->shot;
  b.len = a->len;
  b.size = a->size;
  if (na_vector_init(&b.vector.head,0,b.len,b.size,
		     a->init,a->final,a->private) == -1)
    return 0;
  g = b.vector.head.data;
  p = &a->vector;
  while (p) {
    s = p->head.size * p->head.num;
    memcpy(g,p->head.data,s);
    g += s;
    b.vector.head.num += p->head.num;
    p = p->tail;
  }
  b.vector.tail = 0;
  b.num = b.vector.head.num;
  b.win = &a->vector;
  b.init = a->init;
  b.final = a->final;
  b.cmp = a->cmp;
  b.private = a->private;
  na_array_free(a);
  *a = b;
  return a->vector.head.data;
}

int
na_array_cmp(na_array* a,void* dat,int len)
{
  int i;
  int m,n;
  int x;

  i = 0; m = na_array_count(a); n = len;
  while (i < m && i < n) {
    x = a->cmp(na_array_fetch(a,i),(dat + a->size * i),a->private);
    if (x != 0) return x;
    i++;
  }
  x = (m - i) - (n - i);
  return x;
}

int
na_array_sort(na_array* a)
{
  int cmp(off_t* p,off_t* q) { return a->cmp(p,q,a->private); }
  void* p = na_array_flatten(a);

  if (p == 0) return -1;
  if (a->num == 0)
    return 0;
#ifdef HAVE_MERGESORT
  mergesort(a->vector.head.data,a->num,a->size,(int (*)())cmp);
#else
  qsort(a->vector.head.data,a->num,a->size,(int (*)())cmp);
#endif
  return 0;
}

int
na_array_count(na_array* a)
{
  if (a == 0) return -1;
  return a->num;
}

void*
na_array_fetch(na_array* a,int idx)
{
  void* p;
  na_array_cell* u;
  na_array_cell* v;
  int i;

  if (a == 0) return 0;
  if (a->win->head.bot <= idx && idx < a->win->head.top) {
    a->num -= a->win->head.num;
    p = na_vector_fetch(&a->win->head,idx - a->win->head.bot);
    a->num += a->win->head.num;
    return p;
  } else {
    if (idx < a->win->head.bot)
      a->win = &a->vector;
    while (1) {
      while (a->win) {
	if (a->win->head.bot <= idx && idx < a->win->head.top) {
	  a->num -= a->win->head.num;
	  p = na_vector_fetch(&a->win->head,idx - a->win->head.bot);
	  a->num += a->win->head.num;
	  return p;
	}
	a->win = a->win->tail;
      }

      u = &a->vector;
      while (u->tail)
	u = u->tail;
      a->num -= u->head.num;
      for (i = u->head.num; i < u->head.len; i++)
	na_vector_fetch(&u->head,i);
      a->num += u->head.num;

      v = malloc(sizeof(*v));
      if (v == 0)
	abort();
      if (na_vector_init(&v->head,a->len,a->shot,a->size,
			 a->init,a->final,a->private) == -1)
	return 0;
      v->tail = 0;
      u->tail = v;
      u = v;
      a->len += u->head.len;
      a->win = u;
    }
  }
}

void*
na_array_update(na_array* a,int idx,void* member)
{
  void* p = na_array_fetch(a,idx);

  if (p == 0) return 0;
  if (a->final)
    a->final(a->private,p);
  memcpy(p,member,a->size);
  return p;
}

void*
na_array_append(na_array* a,void* member)
{
  return na_array_update(a,a->num,member);
}

void*
na_array_last(na_array* a)
{
  if (a->num == 0)
    return 0;
  else
    return na_array_fetch(a,a->num-1);
}
