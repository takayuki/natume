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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_SYS_ENDIAN_H
#include <sys/endian.h>
#endif
#include <sys/socket.h>
#include <sys/un.h>
#include "array.h"
#include "canna.h"

typedef struct {
  int in,out;
  na_array buf;
  int sock;
  struct sockaddr_un un;
  int mode;
} canna_server;

static canna_server server[1];

static void
reset(canna_server* s)
{
  na_array_reset(&s->buf);
}

static int
emit(canna_server* s,void* p,int size)
{
  int done = 0;
  while (done < size) {
    na_array_append(&s->buf,p+done);
    done += 1;
  }
  return 0;
}

static int
flush(canna_server* s)
{
  void* p; int size;
  int done = 0,status;

  p = na_array_flatten(&s->buf);
  if (p == 0)
    return -1;
  size = na_array_count(&s->buf);
  while (done < size) {
    status = write(s->out,p+done,size-done);
    if (status == -1) {
      perror("write");
      return -1;
    }
    if (status == 0)
      return -1;
    done += status;
  }
  return 0;
}

static int
retr(canna_server* s,void* p,int size)
{
  int done = 0,status;

  while (done < size) {
    status = read(s->in,p+done,size-done);
    if (status == -1) {
      perror("read");
      return -1;
    }
    if (status == 0)
      return -1;
    done +=  status;
  }
  return 0;
}

static int
widen(uint16_t* dst,int max,char* src,int length)
{
  uint8_t* p = (uint8_t*)src;
  uint8_t* q = (uint8_t*)dst;
  int cnt = 0;

  if (q != 0) {
    while (cnt < max && 0 < length) {
      if (*p <= 0x7F) {
        *q++ = 0; *q++ = *p++; length--; cnt++;
      } else if ((0xA1 <= *p && *p <= 0xFE) || *p == 0x8E) {
        *q++ = *p++; length--;
        if (0 < length && 0xA1 <= *p && *p <= 0xFE) {
          *q++ = *p++; length--; cnt++;
        } else {
          return -1;
        }
      } else {
        return -1;
      }
    }
  } else {
    while (0 < length) {
      if (*p <= 0x7F) {
        p++; length--; cnt++;
      } else if ((0xA1 <= *p && *p <= 0xFE) || *p == 0x8E) {
        p++; length--;
        if (0 < length && 0xA1 <= *p && *p <= 0xFE) {
          p++; length--; cnt++;
        } else {
          return -1;
        }
      } else {
        return -1;
      }
    }
  }
  return cnt;
}

static int
narrow(char* dst,int max,uint16_t* src,int length)
{
  uint8_t* p = (uint8_t*)src;
  uint8_t* q = (uint8_t*)dst;
  int cnt = 0;

  if (q != 0) {
    while (cnt < max && 0 < length) {
      if (*p == 0 && *(p + 1) <= 0x7F) {
        p++; *q++ = *p++; length--; cnt++; 
      } else if ((0xA1 <= *p && *p <= 0xFE) || *p == 0x8E) {
        *q++ = *p++;
        if (0xA1 <= *p && *p <= 0xFE) {
          *q++ = *p++; cnt += 2; length--;
        } else {
          return -1;
        }
      } else {
        return -1;
      }
    }
  } else {
    while (0 < length) {
      if (*p == 0 && *(p + 1) <= 0x7F) {
        p++; p++; length--; cnt++; 
      } else if ((0xA1 <= *p && *p <= 0xFE) || *p == 0x8E) {
        p++;
        if (0xA1 <= *p && *p <= 0xFE) {
          p++; cnt += 2; length--;
        } else {
          return -1;
        }
      } else {
        return -1;
      }
    }
  }
  return cnt;
}

int
canna_init(char* path,int in,int out,int mode)
{
  int id = 0;
  canna_server* svr = &server[0];

  if (na_array_init(&svr->buf,1024*4,sizeof(char),0,0,0,0) == -1)
    return -1;
  if (path == 0 || *path == 0) {
    svr->sock = -1;
    svr->in = in; svr->out = out;
  } else {
    svr->sock = socket(PF_UNIX,SOCK_STREAM,0);
    if (svr->sock == -1) {
      perror("socket");
      return -1;
    }
    strcpy(svr->un.sun_path,path);
    svr->un.sun_family = AF_UNIX;
    if (bind(svr->sock,(struct sockaddr*)&svr->un,sizeof(svr->un)) == -1) {
      perror("bind");
      return -1;
    }
    if (listen(svr->sock,5) == -1) {
      perror("listen");
      return -1;
    }
  }
  svr->mode = mode;
  return id;
}

void
canna_free(int id)
{
  canna_server* svr = &server[id];

  if (svr->sock != -1) {
    close(svr->sock);
    unlink(svr->un.sun_path);
  }
  na_array_free(&svr->buf);
}

int
canna_accept(int id,int* major,int* minor,int* aux0,char* buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_request_type0 req;
  int maj;
  int size;
  char* token;

  if (svr->sock == -1) {
    svr->in = svr->in; svr->out = svr->out;
  } else {
    svr->in = svr->out = accept(svr->sock,0,0);
    if (svr->in == -1) {
      perror("accept");
      return -1;
    }
  }
  if (svr->mode == 0) {
    if (retr(svr,&req.maj,4)) return -1;
    maj = ntohl(req.maj);
    if (maj != 1) return -1;
    if (retr(svr,&req.size,4)) return -1;
    size = ntohl(req.size);
    if (bufsize < size) return -1;
    req.data = (uint8_t*)buf;
    if (retr(svr,req.data,size)) return -1;
    *major = maj;
    *minor = 0;
    *aux0 = size;
  } else {
    *major = 1;
    *minor = 0;
    token = getenv("CANNATOKEN");
    if (token == 0 || *token == 0)
      return -1;
    strcpy(buf,token);
    *aux0  = strlen(buf) + 1;
  }
  return 0;
}

int
canna_establish(int id,int* major,int* minor,int* cxt,int* aux0)
{
  canna_server* svr = &server[id];
  canna_response_type0 res;

  reset(svr);
  if (*cxt == -1) {
    res.err = htonl(*cxt);
    if (emit(svr,&res.err,4)) return -1;
  } else {
    res.ok.min = ntohs(*aux0);
    res.ok.cxt = ntohs(*cxt);
    if (emit(svr,&res.ok.min,2)) return -1;
    if (emit(svr,&res.ok.cxt,2)) return -1;
  }
  flush(svr);
  return 0;
}


int
request_type1(int id,int major,int minor)
{
  canna_server* svr = &server[id];
  canna_request_type1 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  if (size != 0) return -1;
  return 0;
}

int
request_type2(int id,int major,int minor,int* cxt)
{
  canna_server* svr = &server[id];
  canna_request_type2 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  if (size != 2) return -1;
  if (retr(svr,&req.data,2)) return -1;
  *cxt = (int16_t)ntohs(req.data);
  return 0;
}

int
request_type3(int id,int major,int minor,int* cxt,int* aux0)
{
  canna_server* svr = &server[id];
  canna_request_type3 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  if (size != 4) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  if (retr(svr,&req.data,2)) return -1;
  *aux0 = ntohs(req.data);
  return 0;
}

int
request_type6(int id,int major,int minor,int* cxt,int* aux0,int* aux1)
{
  canna_server* svr = &server[id];
  canna_request_type6 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  if (size != 6) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  if (retr(svr,&req.idx,2)) return -1;
  if (retr(svr,&req.data,2)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  *aux0 = (int16_t)ntohs(req.idx);
  *aux1 = ntohs(req.data);
  return 0;
}

int
request_type7(int id,int major,int minor,int* cxt,int* aux0,int* aux1)
{
  canna_server* svr = &server[id];
  canna_request_type7 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  if (size != 6) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  if (retr(svr,&req.idx,2)) return -1;
  if (retr(svr,&req.data,2)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  *aux0 = (int16_t)ntohs(req.idx);
  *aux1 = (int16_t)ntohs(req.data);
  return 0;
}

int
request_type8(int id,int major,int minor,int* cxt,
	      int* aux0,int* aux1,int* aux2)
{
  canna_server* svr = &server[id];
  canna_request_type8 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  if (size != 8) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  if (retr(svr,&req.idx,2)) return -1;
  if (retr(svr,&req.cnd,2)) return -1;
  if (retr(svr,&req.data,2)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  *aux0 = (int16_t)ntohs(req.idx);
  *aux1 = (int16_t)ntohs(req.cnd);
  *aux2 = ntohs(req.data);
  return 0;
}

int
request_type9(int id,int major,int minor,int* cxt,
	      int* aux0,int* aux1,int* aux2)
{
  canna_server* svr = &server[id];
  canna_request_type9 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  if (size != 8) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  if (retr(svr,&req.idx,2)) return -1;
  if (retr(svr,&req.cnd,2)) return -1;
  if (retr(svr,&req.data,2)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  *aux0 = (int16_t)ntohs(req.idx);
  *aux1 = (int16_t)ntohs(req.cnd);
  *aux2 = (int16_t)ntohs(req.data);
  return 0;
}

int
request_type10(int id,int major,int minor,int* cxt,
	       int* aux0,int* aux1,char* buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_request_type10 req;
  int32_t* data = (int32_t*)buf;
  int size;
  int i;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  size -= 8;
  if (size < 0) return -1;
  req.data = malloc(size);
  if (req.data == 0) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  if (retr(svr,&req.num,2)) return -1;
  if (retr(svr,&req.mode,4)) return -1;
  if (retr(svr,req.data,size)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  *aux0 = (int16_t)ntohs(req.num);
  *aux1 = ntohl(req.mode);
  for (i = 0; i < *aux0; i++)
    data[i] = (int16_t)ntohs(req.data[i]);
  free(req.data);
  return 0;
}

int
request_type12(int id,int major,int minor,int* cxt,
	       char* buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_request_type12 req;
  int size;
  int i;
  int init,last,sub;
  uint16_t* p;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  size -= 2;
  if (size < 0) return -1;
  req.data = malloc(size);
  if (req.data == 0) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  if (retr(svr,req.data,size)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  init = 0; last = size;
  p = (uint16_t*)req.data;
  for (i = 0; i < (size / 2); i++) {
    init += 2; last -= 2;
    if (*p == 0) break; p++;
  }
  if (*p != 0) return -1;
  sub = narrow(0,0,(uint16_t*)req.data,init/2);
  if (sub == -1) return -1;
  if (bufsize < (sub + last)) return -1;
  sub = narrow(buf,bufsize,(uint16_t*)req.data,init/2);
  if (*(buf+sub-1) != 0) return -1;
  *(buf+sub-1) = '\t';
  memcpy(buf+sub,req.data+init,last);
  if (*(buf+sub+last-1) != 0) return -1;
  free(req.data);
  return 0;
}

int
request_type14(int id,int major,int minor,int* cxt,
	       int* aux0,char* buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_request_type14 req;
  int size;
  int sub;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  size -= 6;
  if (size < 2 || bufsize < size) return -1;
  req.data = malloc(size);
  if (req.data == 0) return -1;
  if (retr(svr,&req.mode,4)) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  *aux0 = ntohl(req.mode);
  if (retr(svr,req.data,size)) return -1;
  sub = narrow(buf,bufsize,req.data,size/2);
  if (sub == -1) return -1;
  if (*(buf+sub-1) != 0) return -1;
  free(req.data);
  return 0;
}

int
request_type15(int id,int major,int minor,int* cxt,
	       int* aux0,int* aux1,char* buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_request_type15 req;
  int size;

  if (retr(svr,&req.size,2)) return -1;
  size = ntohs(req.size);
  size -= 6;
  if (size <= 0 || bufsize < size) return -1;
  req.data = (uint8_t*)buf;
  if (retr(svr,&req.mode,4)) return -1;
  if (retr(svr,&req.cxt,2)) return -1;
  if (retr(svr,req.data,size)) return -1;
  if (*(buf+size-1) != 0) return -1;
  *cxt = (int16_t)ntohs(req.cxt);
  *aux0 = ntohl(req.mode);
  *aux1 = size;
  return 0;
}

int
response_type2(int id,int major,int minor,int data)
{
  canna_server* svr = &server[id];
  canna_response_type2 res;

  reset(svr);
  res.maj = major;
  res.min = minor;
  res.size = htons(1);
  res.data = htons(data);
  if (emit(svr,&res.maj,1)) return -1;
  if (emit(svr,&res.min,1)) return -1;
  if (emit(svr,&res.size,2)) return -1;
  if (emit(svr,&res.data,1)) return -1;
  flush(svr);
  return 0;
}

int
response_type3(int id,int major,int minor,int stat,char** buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_response_type3 res;
  int total,sub,size,offset;
  int i;

  reset(svr);
  if (stat == -1) {
    res.maj = major;
    res.min = minor;
    res.size = htons(1);
    res.stat = -1;

    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.stat,1)) return -1;
  } else {
    total = 0;
    for (i = 0; i < bufsize; i++) {
      sub = widen(0,0,buf[i],strlen(buf[i])+1);
      if (sub == -1) return -1;
      total += sub;
    }
    size = total * 2;

    res.maj = major;
    res.min = minor;
    res.size = htons(1+size);
    res.stat = 1;
    res.data = malloc(size);
    offset = 0;
    for (i = 0; i < bufsize; i++) {
      sub = widen(((uint16_t*)res.data) + offset,
		  total - offset,buf[i],strlen(buf[i]) + 1);
      if (sub == -1) break;
      offset += sub;
    }
    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.stat,1)) return -1;
    if (emit(svr,res.data,size)) return -1;
    free(res.data);
  }
  flush(svr);
  return 0;
}

int
response_type4(int id,int major,int minor,int idx,int cand,int num)
{
  canna_server* svr = &server[id];
  canna_response_type4 res;
  int32_t stat[7];

  reset(svr);
  if (num == -1) {
    res.maj = major;
    res.min = minor;
    res.size = htons(1);
    res.stat = -1;

    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.stat,1)) return -1;
  } else {
    stat[0] = htonl(idx);
    stat[1] = htonl(cand);
    stat[2] = htonl(num);
    stat[3] = 0;
    stat[4] = 0;
    stat[5] = 0;
    stat[6] = 0;

    res.maj = major;
    res.min = minor;
    res.size = htons(1+28);
    res.stat = 0;
    res.data = (uint32_t*)stat;
    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.stat,1)) return -1;
    if (emit(svr,res.data,28)) return -1;
  }
  flush(svr);
  return 0;
}

int
response_type5(int id,int major,int minor,int data)
{
  canna_server* svr = &server[id];
  canna_response_type5 res;

  reset(svr);
  res.maj = major;
  res.min = minor;
  res.size = htons(2);
  res.data = htons(data);
  if (emit(svr,&res.maj,1)) return -1;
  if (emit(svr,&res.min,1)) return -1;
  if (emit(svr,&res.size,2)) return -1;
  if (emit(svr,&res.data,2)) return -1;
  flush(svr);
  return 0;
}

int
response_type6(int id,int major,int minor,int num,char** buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_response_type6 res;
  int total,sub,size,offset;
  int i;

  reset(svr);
  if (num == -1) {
    res.maj = major;
    res.min = minor;
    res.size = htons(2);
    res.num = htons(-1);

    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.num,2)) return -1;
  } else {
    total = 0;
    for (i = 0; i < bufsize; i++) {
      sub = strlen(buf[i]) + 1;
      total += sub;
    }
    size = total;

    res.maj = major;
    res.min = minor;
    res.size = htons(2+size);
    res.num = htons(num);
    res.data = malloc(size);
    offset = 0;
    for (i = 0; i < bufsize; i++) {
      sub = strlen(buf[i]) + 1;
      memcpy(res.data + offset,buf[i],sub);
      offset += sub;
    }
    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.num,2)) return -1;
    if (emit(svr,res.data,size)) return -1;
    free(res.data);
  }
  flush(svr);
  return 0;
}

int
response_type7(int id,int major,int minor,int num,char** buf,int bufsize)
{
  canna_server* svr = &server[id];
  canna_response_type7 res;
  int total,sub,size,offset;
  int i;

  reset(svr);
  if (num == -1) {
    res.maj = major;
    res.min = minor;
    res.size = htons(2);
    res.num = htons(-1);

    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.num,2)) return -1;
  } else {
    total = 0;
    for (i = 0; i < bufsize; i++) {
      sub = widen(0,0,buf[i],strlen(buf[i])+1);
      if (sub == -1) return -1;
      total += sub;
    }
    size = total * 2;

    res.maj = major;
    res.min = minor;
    res.size = htons(2+size);
    res.num = htons(num);
    res.data = malloc(size);
    offset = 0;
    for (i = 0; i < bufsize; i++) {
      sub = widen(((uint16_t*)res.data) + offset,
		  total - offset,buf[i],strlen(buf[i]) + 1);
      if (sub == -1) break;
      offset += sub;
    }
    if (emit(svr,&res.maj,1)) return -1;
    if (emit(svr,&res.min,1)) return -1;
    if (emit(svr,&res.size,2)) return -1;
    if (emit(svr,&res.num,2)) return -1;
    if (emit(svr,res.data,size)) return -1;
    free(res.data);
  }
  flush(svr);
  return 0;
}

int
response_type9(int id,int major,int minor,int num)
{
  canna_server* svr = &server[id];
  canna_response_type9 res;

  reset(svr);
  res.maj = major;
  res.min = minor;
  res.size = htons(2);
  res.num = htons(num);
  if (emit(svr,&res.maj,1)) return -1;
  if (emit(svr,&res.min,1)) return -1;
  if (emit(svr,&res.size,2)) return -1;
  if (emit(svr,&res.num,2)) return -1;
  flush(svr);
  return 0;
}

int
canna_request(int id,int* major,int* minor,int* cxt,
	      int* aux0,int* aux1,int* aux2,char* buf,int bufsize)
{
  canna_server* svr = &server[id];
  uint8_t maj,min;

  if (retr(svr,&maj,1)) return -1;
  if (retr(svr,&min,1)) return -1;
  *major = maj; *minor = min;

  if (maj == 0x02 && min == 0) {
    /* Finalization */
    return request_type1(id,maj,min);
  } else if (maj == 0x03 && min == 0) {
    /* CreateContext */
    return request_type1(id,maj,min);
  } else if (maj == 0x03 && min == 1) {
    /* CreateDictionary */
    return request_type15(id,maj,min,cxt,aux0,aux1,buf,bufsize);
  } else if (maj == 0x04 && min == 0) {
    /* DuplicateContext */
    return request_type2(id,maj,min,aux0);
  } else if (maj == 0x05 && min == 0) {
    /* CloseContext */
    return request_type2(id,maj,min,aux0);
  } else if (maj == 0x06 && min == 0) {
    /* GetDictionaryList */
    return request_type3(id,maj,min,cxt,aux0);
  } else if (maj == 0x07 && min == 0) {
    /* GetDirectoryList */
    return request_type3(id,maj,min,cxt,aux0);
  } else if (maj == 0x08 && min == 0) {
    /* MountDictionary */
    return request_type15(id,maj,min,cxt,aux0,aux1,buf,bufsize);
  } else if (maj == 0x08 && min == 1) {
    /* Sync */
    return request_type15(id,maj,min,cxt,aux0,aux1,buf,bufsize);
  } else if (maj == 0x09 && min == 0) {
    /* UnmountDictionary */
    return request_type15(id,maj,min,cxt,aux0,aux1,buf,bufsize);
  } else if (maj == 0x0b && min == 0) {
    /* GetMountDictionaryList */
    return request_type3(id,maj,min,cxt,aux0);
  } else if (maj == 0x0d && min == 0) {
    /* DefineWord */
    return request_type12(id,maj,min,cxt,buf,bufsize);
  } else if (maj == 0x0e && min == 0) {
    /* DeleteWord */
    return request_type12(id,maj,min,cxt,buf,bufsize);
  } else if (maj == 0x0f && min == 0) {
    /* BeginConvert */
    return request_type14(id,maj,min,cxt,aux0,buf,bufsize);
  } else if (maj == 0x10 && min == 0) {
    /* EndConvert */
    return request_type10(id,maj,min,cxt,aux0,aux1,buf,bufsize);
  } else if (maj == 0x11 && min == 0) {
    /* GetCandidacyList */
    return request_type6(id,maj,min,cxt,aux0,aux1);
  } else if (maj == 0x12 && min == 0) {
    /* GetYomi */
    return request_type6(id,maj,min,cxt,aux0,aux1);
  } else if (maj == 0x1a && min == 0) {
    /* ResizePause */
    return request_type7(id,maj,min,cxt,aux0,aux1);
  } else if (maj == 0x1b && min == 0) {
    /* GetHinsi */
    return request_type8(id,maj,min,cxt,aux0,aux1,aux2);
  } else if (maj == 0x1c && min == 0) {
    /* GetLex */
    return request_type9(id,maj,min,cxt,aux0,aux1,aux2);
  } else if (maj == 0x1d && min == 0) {
    /* GetStatus */
    return request_type7(id,maj,min,cxt,aux0,aux1);
  } else if (maj == 0x21 && min == 0) {
    /* SetApplicationName */
    return request_type15(id,maj,min,cxt,aux0,aux1,buf,bufsize);
  } else if (maj == 0x22 && min == 0) {
    /* NoticeGroupName */
    return request_type15(id,maj,min,cxt,aux0,aux1,buf,bufsize);
  } else {
    return -1;
  }
}

int
canna_response(int id,int* major,int* minor,int* cxt,
	       int* aux0,int* aux1,int* aux2,char** buf2)
{
  int maj = *major,min = *minor;

  if (maj == 0x02 && min == 0) {
    /* Finalization */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x03 && min == 0) {
    /* CreateContext */
    return response_type5(id,maj,min,*aux0);
  } else if (maj == 0x03 && min == 1) {
    /* CreateDictionary */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x04 && min == 0) {
    /* DuplicateContext */
    return response_type5(id,maj,min,*aux0);
  } else if (maj == 0x05 && min == 0) {
    /* CloseContext */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x06 && min == 0) {
    /* GetDictionaryList */
    return response_type6(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x07 && min == 0) {
    /* GetDirectoryList */
    return response_type6(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x08 && min == 0) {
    /* MountDictionary */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x08 && min == 1) {
    /* Sync */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x09 && min == 0) {
    /* UnmountDictionary */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x0b && min == 0) {
    /* GetMountDictionaryList */
    return response_type6(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x0d && min == 0) {
    /* DefineWord */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x0e && min == 0) {
    /* DeleteWord */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x0f && min == 0) {
    /* BeginConvert */
    return response_type7(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x10 && min == 0) {
    /* EndConvert */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x11 && min == 0) {
    /* GetCandidacyList */
    return response_type7(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x12 && min == 0) {
    /* GetYomi */
    return response_type7(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x1a && min == 0) {
    /* ResizePause */
    return response_type7(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x1b && min == 0) {
    /* GetHinsi */
    return response_type3(id,maj,min,*aux0,buf2,*aux1);
  } else if (maj == 0x1c && min == 0) {
    /* GetLex */
    return response_type9(id,maj,min,*aux0);
  } else if (maj == 0x1d && min == 0) {
    /* GetStatus */
    return response_type4(id,maj,min,*aux0,*aux1,*aux2);
  } else if (maj == 0x21 && min == 0) {
    /* SetApplicationName */
    return response_type2(id,maj,min,*aux0);
  } else if (maj == 0x22 && min == 0) {
    /* NoticeGroupName */
    return response_type2(id,maj,min,*aux0);
  } else {
    return -1;
  }
}
