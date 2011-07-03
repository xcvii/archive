#ifndef BITARRAY_H
#define BITARRAY_H

#include <stddef.h>

typedef struct
{
  size_t size;
  char *bits;
} bitarray;

void bitarray_init(bitarray *ar, size_t sz);

void bitarray_free(bitarray *ar);

int bitarray_get(bitarray *ar, size_t ix);

void bitarray_set(bitarray *ar, size_t ix, int val);

void bitarray_setall(bitarray *ar, int val);

#endif /* BITARRAY_H */
