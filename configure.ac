#
# Natume -- an implementation of Kana-Kanji conversion in Haskell
# Copyright (C) 2006-2012 Takayuki Usui
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
#
AC_PREREQ(2.59)
AC_INIT([Natume], [0.12], [strscr@users.sourceforge.jp], [natume])

AC_CONFIG_SRCDIR([natume.hs])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_AUX_DIR([tools])

AC_PROG_CC
AC_CHECK_PROG(AR, [ar], [ar])
AC_PROG_INSTALL
XX_PROG_GHC
if test -z "$GHC"; then
  AC_MSG_ERROR([install GHC (the Glasgow Haskell Compiler) on your system])
fi

AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([fcntl.h stdint.h string.h unistd.h])
AC_CHECK_HEADERS([arpa/inet.h netinet/in.h sys/endian.h])
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_FUNC_FORK
AC_FUNC_FSEEKO
AC_FUNC_MALLOC
AC_TYPE_SIGNAL
AC_FUNC_STAT
AC_FUNC_SELECT_ARGTYPES
AC_CHECK_FUNCS([memset mergesort mkdir select setenv unsetenv])
AC_CHECK_LIB([nsl], [gethostbyname])
AC_CHECK_LIB([socket], [socket])
AC_SEARCH_LIBS([inet_ntoa], [resolv])
AC_SEARCH_LIBS([inet_aton], [resolv])

AC_CONFIG_FILES([Config.hs natumeserver.h])
AC_CONFIG_FILES([initnatume.sh],[chmod +x initnatume.sh])
AC_CONFIG_FILES([natume.sh],[chmod +x natume.sh])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([dist/natume.spec dist/pkgsrc/inputmethod/natume/Makefile])
AC_OUTPUT
