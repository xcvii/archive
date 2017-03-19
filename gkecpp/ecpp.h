#ifndef ECPP_H
#define ECPP_H

/* initialise the random number generator used in the ECPP algorithm
 */
void ecpp_init();

/* test an integer n for primality using the Goldwasser-Kilian ECPP algorithm
 */
int ecpp_test(unsigned long n);

#endif /* ECPP_H */
