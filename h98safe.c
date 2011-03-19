#include <stdio.h>
int
main(int argc,char* argv[])
{
  char* in,* out;
  int c;
  const int eof = EOF;

  if (argc < 4) return 1;
  in = argv[2]; out = argv[3];
  if (freopen(in,"r",stdin) == 0) return 1;
  if (freopen(out,"w",stdout) == 0) return 1;
  printf("{-# LINE 1 \"%s\" #-}\n",in);
  while((c = getchar()) != eof)
    if (c <= 127) putchar(c); else printf("\\x%02x",c);
  fflush(stdout);
  return (!feof(stdin));
}
