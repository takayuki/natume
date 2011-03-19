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
#include <signal.h>
#include <string.h>
#include "sig.h"

void
sig_catch(int sig,void (*f)(int))
{
  struct sigaction sa;

  memset(&sa,0,sizeof(sa));
  sa.sa_handler = f;
  sigemptyset(&sa.sa_mask);
  sigaction(sig,&sa,0);
}

void
sig_ignore(int sig)
{
  sig_catch(sig,SIG_IGN);
}

void
sig_block(int sig)
{
  sigset_t ss;

  sigemptyset(&ss);
  sigaddset(&ss,sig);
  sigprocmask(SIG_BLOCK,&ss,0);
}

void
sig_unblock(int sig)
{
  sigset_t ss;

  sigemptyset(&ss);
  sigaddset(&ss,sig);
  sigprocmask(SIG_UNBLOCK,&ss,0);
}

void
sig_suspend(int sig)
{
  struct sigaction sa;
  sigset_t ss;

  memset(&sa,0,sizeof(sa));
  sigaction(sig,0,&sa);
  ss = sa.sa_mask;
  sigdelset(&ss,sig);
  sigsuspend(&ss);
}
