/*
 * "w.h", Pjotr '87.
 */

extern COMPLEX *W_factors;
extern unsigned Nfactors;

/*
 * W gives the (already computed) Wn ^ k (= e ^ (2pi * i * k / n)).
 * Notice that the powerseries of Wn has period Nfactors.
 */
#define	W(n, k)		(W_factors [((k) * (Nfactors / (n))) % Nfactors])
