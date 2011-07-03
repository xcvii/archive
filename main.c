#include "ecpp.h"

#include <stdio.h>

void dump_result(unsigned long n)
{
  fprintf(stdout, "\n%lu is %s\n", n,
      ecpp_test(n) ? "prime" : "composite");
}

int main(int argc, char *argv[])
{
  if (1 < argc)
  {
    unsigned long n;

    sscanf(argv[1], "%lu", &n);

    ecpp_init();

    dump_result(n);

    return 0;
  }

  fprintf(stderr, "Usage: `%s INTEGER'\n", argv[0]);
  return 1;
}

