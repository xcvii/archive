#include "ecpp.h"

#include "sieve.h"

#include <gmp.h>

#include <limits.h>
#include <stdlib.h>
#include <time.h>

#ifdef DEBUG
#include <stdio.h>
#endif /* DEBUG */

/* get the discriminant of a curve, given by
 *   delta(E(a, b)) = 4 * a^3 + 27 * b^2
 */
static void ec_discriminant(mpz_t delta, mpz_t a, mpz_t b);

/* get the number of points in GF(n) lying on an elliptic curve E(a, b)
 * the algorithm might fail when n is composite -- this is indicated by the
 * return value
 */
static int ec_order(mpz_t order, mpz_t a, mpz_t b, unsigned long n);

/* add two points on an elliptic curve E(a, _) over GF(n) using the special
 * addition defined by the elliptic curve group law:
 *   (xr, yr, zr) = (x1, y1, z1) + (x2, y2, z2)
 * where zr is given as the function return value
 */
static int ec_add(mpz_t xr, mpz_t yr, mpz_t x1, mpz_t y1, int z1,
    mpz_t x2, mpz_t y2, int z2, unsigned long n, mpz_t a);

/* multiply a point on an elliptic curve E(a, _) over GF(n) by a scalar:
 *   (xr, yr, zr) = factor * (x, y, z)
 * where zr is given as the function return value
 */
static int ec_times(mpz_t xr, mpz_t yr, mpz_t x, mpz_t y, int z, mpz_t factor,
    unsigned long n, mpz_t a);

void ecpp_init()
{
  srand(time(NULL));
}

int ecpp_test(unsigned long n)
{
  mpz_t a, b,
        x0, y0,
        xt, yt,
        tmp;
  int z;
  int is_prime = 0;

  if (n <= USHRT_MAX)
  {
    return sieve_test(n);
  }

  mpz_init(a);
  mpz_init(b);
  mpz_init(x0);
  mpz_init(y0);
  mpz_init(xt);
  mpz_init(yt);
  mpz_init(tmp);

#ifdef DEBUG
  gmp_fprintf(stderr, "\nTesting %d with ECPP...\n", n);
#endif /* DEBUG */

  for (;;) /* keep trying while the curve order factoring fails */
  {
    for (;;) /* keep trying while n divides curve discriminant */
    {
      /* initialise a random point P = (x0, y0)
       * and a random elliptic curve E(a, b): y^2 = x^3 + a*x + b
       * with b expressed in terms of (a, x0, y0) so the point lies on the curve
       */

      mpz_set_ui(a, rand() % n);
      mpz_set_ui(x0, rand() % n);
      mpz_set_ui(y0, rand() % n);
      mpz_init(b);

      mpz_mul(b, y0, y0);
      mpz_mul(tmp, x0, x0);
      mpz_submul(b, tmp, x0);
      mpz_submul(b, a, x0);
      mpz_mod_ui(b, b, n);

#ifdef DEBUG
      gmp_fprintf(stderr, "\n\tE: y^2 = x^3 + %Zd * x + %Zd\n", a, b);
      gmp_fprintf(stderr, "\tP = (%Zd,%Zd,1)\n", x0, y0);
#endif /* DEBUG */

      /* the discriminant of the curve and n are required to be coprimes
       * -- if not, then either
       *   A) n divides the discriminant -- a new curve must be generated
       *   B) n is composite and a proper factor is found -- the algorithm can
       *      terminate
       */

      ec_discriminant(tmp, a, b);

#ifdef DEBUG
      mpz_mod_ui(tmp, tmp, n);
      gmp_fprintf(stderr, "\tdelta(E/GF(%d)) = 4*a^3 + 27*b^2 = %Zd\n",
          n, tmp);
#endif /* DEBUG */

      mpz_gcd_ui(tmp, tmp, n);

#ifdef DEBUG
      gmp_fprintf(stderr, "\tgcd(delta, %d) = %Zd\n", n, tmp);
#endif /* DEBUG */

      if (0 == mpz_cmp_ui(tmp, 1))
      {
        break;
      }
      else if (0 != mpz_cmp_ui(tmp, n))
      {
        is_prime = 0;
        goto cleanup_and_return;
      }
    }

    /* the curve order algorithm failing indicates n is composite
     */
    if (!(ec_order(tmp, a, b, n)))
    {
      is_prime = 0;
      break;
    }

#ifdef DEBUG
    gmp_fprintf(stderr, "\t|E/GF(%d)| = %Zd\n", n, tmp);
#endif /* DEBUG */

    /* the curve order should be the multiple of 2 and a "probable prime" n --
     * if the order is not even, a new curve is generated
     */
    if (!mpz_even_p(tmp))
    {
      continue;
    }

    /* P + P != 0, or a new curve is generated
     */
    z = ec_add(xt, yt, x0, y0, 1, x0, y0, 1, n, a);

#ifdef DEBUG
    gmp_fprintf(stderr, "\t2 * P = (%Zd,%Zd,%d)\n", xt, yt, z);
#endif /* DEBUG */

    if (0 != z)
    {
      /* order * P = 0, or n is composite
       */
      z = ec_times(xt, yt, x0, y0, 1, tmp, n, a);

#ifdef DEBUG
      gmp_fprintf(stderr, "\t|E| * P = (%Zd,%Zd,%d)\n", xt, yt, z);
#endif /* DEBUG */

      if (0 != z)
      {
        is_prime = 0;
        break;
      }

      /* at this point, order/2 being a prime implies n is a prime --
       * a recursive call to ecpp_test is used to test order/2 for primality
       */
      mpz_div_ui(tmp, tmp, 2);
      if (ecpp_test(mpz_get_ui(tmp)))
      {
        is_prime = 1;
        break;
      }
    }
  }

cleanup_and_return:
  mpz_clear(a);
  mpz_clear(b);
  mpz_clear(x0);
  mpz_clear(y0);
  mpz_clear(xt);
  mpz_clear(yt);
  mpz_clear(tmp);

  return is_prime;
}

static void ec_discriminant(mpz_t delta, mpz_t a, mpz_t b)
{
  /*   delta(E(a, b)) = 4 * a^3 + 27 * b^2
   */

  mpz_t tmp;
  mpz_init(tmp);

  mpz_mul(delta, a, a);
  mpz_mul(delta, delta, a);
  mpz_mul_ui(delta, delta, 4);

  mpz_mul(tmp, b, b);

  mpz_addmul_ui(delta, tmp, 27);

  mpz_clear(tmp);
}

static int ec_order(mpz_t order, mpz_t a, mpz_t b, unsigned long n)
{
  /* use the inefficient naive approach to count the points of an elliptic
   * curve E(a, b) over finite field GF(n):
   *   |(E(a, b))/GF(n)| =
   *           n + 1 + sum (x in GF(n)) (jacobi_symbol((x^3 + a*x + b), n))
   */

  unsigned long i;
  mpz_t tmp;
  int order_exists;

  if (!(n & 1))
  {
    order_exists = 0;
  }
  else
  {
    mpz_init(tmp);

    mpz_set_ui(order, n);
    mpz_add_ui(order, order, 1);

    for (i = 0; i < n; ++i)
    {
      mpz_set_ui(tmp, i);
      mpz_mul_ui(tmp, tmp, i);
      mpz_mul_ui(tmp, tmp, i);
      mpz_addmul_ui(tmp, a, i);
      mpz_add(tmp, tmp, b);

      mpz_set_si(tmp, mpz_kronecker_ui(tmp, n));
      mpz_add(order, order, tmp);
    }

    order_exists = 1;

    mpz_clear(tmp);
  }

  return order_exists;
}

static int ec_add(mpz_t xr, mpz_t yr, mpz_t x1, mpz_t y1, int z1,
    mpz_t x2, mpz_t y2, int z2, unsigned long n, mpz_t a)
{
  /* add two points of an elliptic curve by matching an addition rule,
   * in non-trivial cases producing a secant connecting the points (or a
   * tangent in case of overlapping points), finding the third intersection
   * point and mirroring it to the y axis
   */

  if (0 == z1 && 0 == z2) /* 0 + 0 = 0 */
  {
    mpz_set_ui(xr, 0);
    mpz_set_ui(yr, 1);

    return 0;
  }
  else if (0 == z2) /* P + 0 = P */
  {
    mpz_set(xr, x1);
    mpz_set(yr, y1);

    return 1;
  }
  else if (0 == z1) /* 0 + Q = Q */
  {
    mpz_set(xr, x2);
    mpz_set(yr, y2);

    return 1;
  }
  else if (0 != mpz_cmp(x1, x2)) /* P + Q, P != Q */
  {
    /*   m = (y2 - y1) / (x2 - x1)
     *   xr = m^2 - x1 - x2
     *   yr = m * (x1 - x3) - y1
     */
    mpz_t xt, yt, tmp;

    mpz_init(xt);
    mpz_init(yt);
    mpz_init_set_ui(tmp, n);

    mpz_sub(yt, y2, y1);
    mpz_sub(xt, x2, x1);

    {
      /* multiplicative inverse over GF(n) using the Extended Euclidean
       * Algorithm
       *   1 = g = s * (x2 - x1) + t * n = s * (x2 - x1)
       */

      mpz_t s, t, g;

      mpz_init(s);
      mpz_init(t);
      mpz_init(g);

      mpz_gcdext(g, s, t, xt, tmp);
      mpz_mul(xt, s, yt);
      mpz_mod_ui(xt, xt, n);
      mpz_set(yt, xt);

      mpz_clear(s);
      mpz_clear(t);
      mpz_clear(g);
    }

    mpz_mul(xt, xt, xt);
    mpz_sub(xt, xt, x1);
    mpz_sub(xt, xt, x2);
    mpz_mod_ui(xt, xt, n);

    mpz_sub(tmp, x1, xt);
    mpz_mul(yt, yt, tmp);
    mpz_sub(yt, yt, y1);
    mpz_mod_ui(yt, yt, n);

    mpz_set(xr, xt);
    mpz_set(yr, yt);

    mpz_clear(xt);
    mpz_clear(yt);
    mpz_clear(tmp);

    return 1;
  }
  else if (0 != mpz_cmp(y1, y2)) /* P + (-P) = 0 */
  {
    mpz_set_ui(xr, 0);
    mpz_set_ui(yr, 1);

    return 0;
  }
  else /* P + P */
  {
    /*   m = (3 * x1^2 + a) / (2 * y1)
     *   xr = m^2 - 2 * x1
     *   yr = m * (x1 - x3) - y1
     */
    mpz_t xt, yt, tmp;

    mpz_init(xt);
    mpz_init(yt);

    mpz_init_set_ui(tmp, n);

    mpz_mul(xt, x1, x1);
    mpz_mul_ui(xt, xt, 3);
    mpz_add(xt, xt, a);
    mpz_add(yt, y1, y1);

    {
      /* multiplicative inverse over GF(n) using the Extended Euclidean
       * Algorithm
       *   1 = g = s * (2 * y1) + t * n = s * (2 * y1)
       */

      mpz_t s, t, g;

      mpz_init(s);
      mpz_init(t);
      mpz_init(g);

      mpz_gcdext(g, s, t, yt, tmp);
      mpz_mul(xt, s, xt);
      mpz_mod_ui(xt, xt, n);
      mpz_set(yt, xt);

      mpz_clear(s);
      mpz_clear(t);
      mpz_clear(g);
    }

    mpz_mul(xt, xt, xt);
    mpz_sub(xt, xt, x1);
    mpz_sub(xt, xt, x1);
    mpz_mod_ui(xt, xt, n);

    mpz_sub(tmp, x1, xt);
    mpz_mul(yt, yt, tmp);
    mpz_sub(yt, yt, y1);
    mpz_mod_ui(yt, yt, n);

    mpz_set(xr, xt);
    mpz_set(yr, yt);

    mpz_clear(xt);
    mpz_clear(yt);
    mpz_clear(tmp);

    return 1;
  }
}

static int ec_times(mpz_t xr, mpz_t yr, mpz_t x, mpz_t y, int z, mpz_t factor,
    unsigned long n, mpz_t a)
{
  /* multiply a point of an elliptic curve by a scalar with an efficient
   * algorithm that uses point addition and a decomposition of the scalar to
   * the sum of powers of two
   */
  int sign;
  mpz_t x1, y1,
        xt, yt,
        k;
  int z1 = 0, zt;

  mpz_init(x1);
  mpz_init(y1);
  mpz_init(xt);
  mpz_init(yt);
  mpz_init(k);

  mpz_set_ui(xt, 0);
  mpz_set_ui(yt, 1);
  zt = 0;

  if (0 != z || mpz_cmp_ui(factor, 0) == 0) /*   k * 0 = 0 * P = 0   */
  {
    sign = mpz_sgn(factor);
    mpz_abs(k, factor);

    while (mpz_cmp_ui(k, 0) > 0)
    {
      int pow;

      mpz_set(x1, x);
      mpz_set(y1, y);

      for (pow = 0; mpz_cmp_ui(k, (2 << pow)) >= 0; ++pow)
      {
        z1 = ec_add(x1, y1, x1, y1, z1, x1, y1, z1, n, a);
      }

      mpz_sub_ui(k, k, 1 << pow);

      zt = ec_add(xt, yt, xt, yt, zt, x1, y1, z1, n, a);
    }

    if (-1 == sign)
    {
      mpz_neg(yt, yt);
    }
  }

  mpz_set(xr, xt);
  mpz_set(yr, yt);

  mpz_clear(x1);
  mpz_clear(y1);
  mpz_clear(xt);
  mpz_clear(yt);
  mpz_clear(k);

  return zt;
}

