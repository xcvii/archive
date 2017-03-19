#include "sieve.h"

#include "bitarray.h"

#ifdef DEBUG
#include <stdio.h>
#endif /* DEBUG */

int
sieve_test(unsigned short n)
{
  int i, j;
  int is_prime;
  bitarray ar;

#ifdef DEBUG
  fprintf(stderr, "\nTesting %d with sieve test... ", n);
#endif /* DEBUG */

  if (n < 2) return 0;

  bitarray_init(&ar, n + 1);

  for (i = 2; i * i <= n; ++i)
  {
    if (!bitarray_get(&ar, i))
    {
      for (j = 2 * i; j <= n; j += i)
      {
        if (j == n)
        {
          is_prime = 0;
          goto cleanup_and_return;
        }

        bitarray_set(&ar, j, 1);
      }
    }
  }

  is_prime = !bitarray_get(&ar, n);

cleanup_and_return:
  bitarray_free(&ar);

#ifdef DEBUG
  fprintf(stderr, "\t%s\n", is_prime ? "prime!" : "composite!");
#endif /* DEBUG */

  return is_prime;
}

