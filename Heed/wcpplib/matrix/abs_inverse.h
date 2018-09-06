#ifndef ABS_INVERSE_H
#define ABS_INVERSE_H
/*
Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

namespace Heed {

// X abstract_determinant(M& mi, long q, int& serr)
// The array mi is changed.
// If can not calc (diagonal elements get zero) serr=1, if all OK - serr=0
// M any matrix class supporting access to elements by the function
// M.ac(nrow, ncol), both indexes start from 0.
// X - class of type of element if this array.
// It should support fabs(X) and arithmetic operations.

#define ALWAYS_USE_TEMPLATE_PAR_AS_FUN_PAR  // required by sun Solaris
#ifndef ALWAYS_USE_TEMPLATE_PAR_AS_FUN_PAR

template <class M, class X>
X abstract_determinant(M& mi, long q) {

#else
template <class M, class X>
X abstract_determinant(M& mi, long q, X /*dummy*/) {
#endif

  if (q == 1) {
    return mi.ac(0, 0);
  } else if (q == 2) {
    return mi.ac(0, 0) * mi.ac(1, 1) - mi.ac(0, 1) * mi.ac(1, 0);
  } else if (q == 3) {
    return mi.ac(0, 0) * mi.ac(1, 1) * mi.ac(2, 2) +
           mi.ac(0, 2) * mi.ac(1, 0) * mi.ac(2, 1) +
           mi.ac(0, 1) * mi.ac(1, 2) * mi.ac(2, 0) -
           mi.ac(0, 2) * mi.ac(1, 1) * mi.ac(2, 0) -
           mi.ac(0, 0) * mi.ac(1, 2) * mi.ac(2, 1) -
           mi.ac(0, 1) * mi.ac(1, 0) * mi.ac(2, 2);
  }
  X koef = 1;
  for (long nr = 0; nr < q; nr++) {
    long nmax = 0;
    double d = 0;
    for (long nr1 = nr; nr1 < q; nr1++) {
      if (fabs(mi.ac(nr1, nr)) > d) {
        d = fabs(mi.ac(nr1, nr));
        nmax = nr1;
      }
    }
    // mcout<<"d="<<d<<'\n';
    // mcout<<"nmax="<<nmax<<'\n';
    if (d == 0) {
      // serr = 1;
      return koef * mi.ac(nmax, nr);
    }
    if (nmax > nr) {
      for (long nc = nr; nc < q; nc++) {
        X t(mi.ac(nr, nc));
        mi.ac(nr, nc) = mi.ac(nmax, nc);
        mi.ac(nmax, nc) = t;
      }
      // transposition of rows: determinant changes sign
      koef *= -1;  
    }
    X t = mi.ac(nr, nr);
    for (long nr1 = nr + 1; nr1 < q; nr1++) {
      X k(mi.ac(nr1, nr) / t);
      // mcout<<"nr1="<<nr1<<" nr="<<nr<<'\n';
      // mcout<<"k="<<k<<'\n';
      // add elements of another row: the main value of
      // determinant is not affected (proven in linear algebra)
      // But the resolution gets worser.
      for (long nc = nr; nc < q; nc++) {
        mi.ac(nr1, nc) -= k * mi.ac(nr, nc);
      }  
    }
    for (long nc = nr; nc < q; nc++) {
      mi.ac(nr, nc) /= t;
    }
    koef *= t;
  }
  return koef;
}

}

#endif
