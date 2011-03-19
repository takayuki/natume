AC_DEFUN([XX_PROG_GHC],
[AC_PATH_PROG([GHC], [ghc])
if test -n "$GHC"; then
  AC_PATH_PROG([GHCI], [ghci])
  AC_SUBST([GHC])
  AC_SUBST([GHCI])
fi
])dnl
