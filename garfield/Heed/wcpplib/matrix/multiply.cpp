/*
Copyright (c) 2001 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/
#include "wcpplib/matrix/multiply.h"
#include "wcpplib/util/FunNameStack.h"

namespace Heed {

DynLinArr<DoubleAc> operator*(const DynArr<DoubleAc>& mt,
                              const DynLinArr<double>& vc) {
  mfunname(
      "DynLinArr<DoubleAc> operator*(const DynArr<DoubleAc>& mt, const "
      "DynLinArr<double>& vc)");

  const DynLinArr<long>& qel_mt(mt.get_qel());
  check_econd11(qel_mt.get_qel(), != 2, mcerr);
  const long q = vc.get_qel();
  check_econd12(q, !=, qel_mt[1], mcerr);
  DoubleAc s(0);  // assumes that this clears the element
  DynLinArr<DoubleAc> res(qel_mt[0], s);
  for (long n1 = 0; n1 < qel_mt[0]; n1++) {
    for (long n2 = 0; n2 < q; n2++) {
      res.acu(n1) += mt.acu(n1, n2) * vc.acu(n2);
    }
  }
  return res;
}

DynLinArr<DoubleAc> operator*(const DynArr<double>& mt,
                              const DynLinArr<DoubleAc>& vc) {
  mfunname(
      "DynLinArr<DoubleAc> operator*(const DynArr<double>& mt, const "
      "DynLinArr<DoubleAc>& vc)");

  const DynLinArr<long>& qel_mt(mt.get_qel());
  check_econd11(qel_mt.get_qel(), != 2, mcerr);
  const long q = vc.get_qel();
  check_econd12(q, !=, qel_mt[1], mcerr);
  DoubleAc s(0);  // assumes that this clears the element
  DynLinArr<DoubleAc> res(qel_mt[0], s);
  for (long n1 = 0; n1 < qel_mt[0]; n1++) {
    for (long n2 = 0; n2 < q; n2++) {
      res.acu(n1) += mt.acu(n1, n2) * vc.acu(n2);
    }
  }
  return res;
}

DoubleAc operator*(const DynLinArr<DoubleAc>& vc1,
                   const DynLinArr<double>& vc2) {
  mfunname(
      "DoubleAc operator*(const DynLinArr<DoubleAc>& vc1, const "
      "DynLinArr<double>& vc2)");

  const long q1 = vc1.get_qel();
  const long q2 = vc2.get_qel();
  check_econd12(q1, !=, q2, mcerr);
  DoubleAc s(0);  // assumes that this clears the element
  for (long n = 0; n < q1; n++) {
    s += vc1.acu(n) * vc2.acu(n);
  }
  return s;
}

DoubleAc operator*(const DynLinArr<double>& vc1,
                   const DynLinArr<DoubleAc>& vc2) {
  mfunname(
      "DoubleAc operator*(const DynLinArr<double>& vc1, const "
      "DynLinArr<DoubleAc>& vc2)");
  const long q1 = vc1.get_qel();
  const long q2 = vc2.get_qel();
  check_econd12(q1, !=, q2, mcerr);
  DoubleAc s(0);  // assumes that this clears the element
  for (long n = 0; n < q1; n++) {
    s += vc1.acu(n) * vc2.acu(n);
    // mcout<<"vc1[n]="<<vc1[n]<<'\n';
    // mcout<<"vc2[n]="<<vc2[n]<<'\n';
    // mcout<<"vc1[n] * vc2[n]="<<vc1[n] * vc2[n]<<'\n';
    // mcout<<"s="<<s<<'\n';
  }
  return s;
}

DynLinArr<DoubleAc> operator+(const DynLinArr<DoubleAc>& vc1,
                              const DynLinArr<double>& vc2) {
  mfunname(
      "DoubleAc operator+(const DynLinArr<DoubleAc>& vc1, const "
      "DynLinArr<double>& vc2)");
  const long q1 = vc1.get_qel();
  const long q2 = vc2.get_qel();
  check_econd12(q1, !=, q2, mcerr);
  DynLinArr<DoubleAc> s(q1);
  for (long n = 0; n < q1; n++) {
    s.acu(n) = vc1.acu(n) + vc2.acu(n);
  }
  return s;
}
DynLinArr<DoubleAc> operator-(const DynLinArr<DoubleAc>& vc1,
                              const DynLinArr<double>& vc2) {
  mfunname(
      "DoubleAc operator-(const DynLinArr<DoubleAc>& vc1, const "
      "DynLinArr<double>& vc2)");
  const long q1 = vc1.get_qel();
  const long q2 = vc2.get_qel();
  check_econd12(q1, !=, q2, mcerr);
  DynLinArr<DoubleAc> s(q1);
  for (long n = 0; n < q1; n++) {
    s.acu(n) = vc1.acu(n) - vc2.acu(n);
  }
  return s;
}

DynLinArr<DoubleAc> operator+(const DynLinArr<double>& vc1,
                              const DynLinArr<DoubleAc>& vc2) {
  mfunname(
      "DoubleAc operator+(const DynLinArr<double>& vc1, const "
      "DynLinArr<DoubleAc>& vc2)");
  const long q1 = vc1.get_qel();
  const long q2 = vc2.get_qel();
  check_econd12(q1, !=, q2, mcerr);
  DynLinArr<DoubleAc> s(q1);
  for (long n = 0; n < q1; n++) {
    s.acu(n) = vc1.acu(n) + vc2.acu(n);
  }
  return s;
}
DynLinArr<DoubleAc> operator-(const DynLinArr<double>& vc1,
                              const DynLinArr<DoubleAc>& vc2) {
  mfunname(
      "DoubleAc operator-(const DynLinArr<double>& vc1, const "
      "DynLinArr<DoubleAc>& vc2)");
  const long q1 = vc1.get_qel();
  const long q2 = vc2.get_qel();
  check_econd12(q1, !=, q2, mcerr);
  DynLinArr<DoubleAc> s(q1);
  for (long n = 0; n < q1; n++) {
    s.acu(n) = vc1.acu(n) - vc2.acu(n);
  }
  return s;
}

DynArr<DoubleAc> operator+(const DynArr<DoubleAc>& mt1,
                           const DynArr<double>& mt2) {
  mfunname(
      "DynArr<DoubleAc> operator+(const DynArr<DoubleAc>& mt1, const "
      "DynArr<double>& mt2)");
  const long qdim1 = mt1.get_qdim();
  const long qdim2 = mt2.get_qdim();
  check_econd12(qdim1, !=, qdim2, mcerr);
  const DynLinArr<long>& qe1 = mt1.get_qel();
  const DynLinArr<long>& qe2 = mt2.get_qel();
  check_econd12(qe1, !=, qe2, mcerr);
  DynArr<DoubleAc> ms(mt1);
  IterDynArr<DoubleAc> iter(&ms);
  DoubleAc* at;
  while ((at = iter.more()) != NULL) {
    (*at) = (*at) + mt2.acu(iter.get_ncur());
  }
  return ms;
}
DynArr<DoubleAc> operator-(const DynArr<DoubleAc>& mt1,
                           const DynArr<double>& mt2) {
  mfunname(
      "DynArr<DoubleAc> operator-(const DynArr<DoubleAc>& mt1, const "
      "DynArr<double>& mt2)");
  long qdim1 = mt1.get_qdim();
  long qdim2 = mt2.get_qdim();
  check_econd12(qdim1, !=, qdim2, mcerr);
  const DynLinArr<long>& qe1 = mt1.get_qel();
  const DynLinArr<long>& qe2 = mt2.get_qel();
  check_econd12(qe1, !=, qe2, mcerr);
  DynArr<DoubleAc> ms(mt1);
  IterDynArr<DoubleAc> iter(&ms);
  DoubleAc* at;
  while ((at = iter.more()) != NULL) {
    (*at) = (*at) - mt2.acu(iter.get_ncur());
  }
  return ms;
}

DynArr<DoubleAc> operator+(const DynArr<double>& mt1,
                           const DynArr<DoubleAc>& mt2) {
  mfunname(
      "DynArr<DoubleAc> operator+(const DynArr<double>& mt1, const "
      "DynArr<DoubleAc>& mt2)");
  long qdim1 = mt1.get_qdim();
  long qdim2 = mt2.get_qdim();
  check_econd12(qdim1, !=, qdim2, mcerr);
  const DynLinArr<long>& qe1 = mt1.get_qel();
  const DynLinArr<long>& qe2 = mt2.get_qel();
  check_econd12(qe1, !=, qe2, mcerr);
  DynArr<DoubleAc> ms(mt2);
  IterDynArr<DoubleAc> iter(&ms);
  DoubleAc* at;
  while ((at = iter.more()) != NULL) {
    (*at) = (*at) + mt1.acu(iter.get_ncur());
  }
  return ms;
}

DynArr<DoubleAc> operator-(const DynArr<double>& mt1,
                           const DynArr<DoubleAc>& mt2) {
  mfunname(
      "DynArr<DoubleAc> operator-(const DynArr<double>& mt1, const "
      "DynArr<DoubleAc>& mt2)");
  long qdim1 = mt1.get_qdim();
  long qdim2 = mt2.get_qdim();
  check_econd12(qdim1, !=, qdim2, mcerr);
  const DynLinArr<long>& qe1 = mt1.get_qel();
  const DynLinArr<long>& qe2 = mt2.get_qel();
  check_econd12(qe1, !=, qe2, mcerr);
  DynArr<DoubleAc> ms(mt2);
  IterDynArr<DoubleAc> iter(&ms);
  DoubleAc* at;
  while ((at = iter.more()) != NULL) {
    (*at) = -(*at) + mt1.acu(iter.get_ncur());
  }
  return ms;
}

}
