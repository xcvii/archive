#include "bitarray.h"

#include <stdlib.h>
#include <string.h>

void
bitarray_init(bitarray *ar, size_t sz)
{
  size_t bc = sz / 8 + !!(sz % 8);

  ar->size = sz;
  ar->bits = malloc(bc);
  memset(ar->bits, 0, bc);
}

void
bitarray_free(bitarray *ar)
{
  free(ar->bits);
}

int
bitarray_get(bitarray *ar, size_t ix)
{
  size_t offset = ix / 8;
  char mask = 0x80 >> (ix % 8);

  return !!(ar->bits[offset] & mask);
}

void
bitarray_set(bitarray *ar, size_t ix, int val)
{
  size_t offset = ix / 8;
  char mask = 0x80 >> (ix % 8);

  if (val)
  {
    ar->bits[offset] |= mask;
  }
  else
  {
    ar->bits[offset] &= ~mask;
  }
}

void bitarray_setall(bitarray *ar, int val)
{
  size_t bc = ar->size / 8 + !!(ar->size % 8);

  memset(ar->bits, val ? 0xff : 0, bc);
}

