#include "wcpplib/matrix/abs_inverse.h"
#include "wcpplib/matrix/inverse.h"
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

namespace Heed {

void inverse_DynArr_prot(const DynArr<DoubleAc>& mi, DynArr<DoubleAc>& mr,
                         int& szero, int& serr, int s_stop) {
  mfunname(
      "void inverse_DynArr_prot(const DynArr<DoubleAc>& mi, "
      "DynArr<DoubleAc>& mr, int& s_zero, int& serr, int s_stop)");
  // mcout<<"inverse_DynArr_prot:\n";
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  check_econd12(miqel[0], !=, miqel[1], mcerr);
  serr = 0;
  szero = 0;
  long q = miqel[0];
  mr = DynArr<DoubleAc>(q, q);
  if (q == 1) {
    if (mi.ac(0, 0).get() == 0.0) {
      szero = 1;
      serr = 1;
      return;
    }
    mr.ac(0, 0) = 1.0 / mi.ac(0, 0);
    if (fabs(mr.ac(0, 0)).left_limit() == 0) {
      serr = 1;
    }
    return;
  }
  DynArr<DoubleAc> mii(mi);
  mr.assignAll(0.0);
  for (long n = 0; n < q; n++) mr.ac(n, n) = DoubleAc(1.0);

  for (long nr = 0; nr < q; nr++) {
    // Iprintn(mcout, nr);
    long nmax = 0;
    DoubleAc d(0.0);
    for (long nr1 = nr; nr1 < q; nr1++) {
      if (fabs(mii.ac(nr1, nr)) > d) {
        d = fabs(mii.ac(nr1, nr));
        nmax = nr1;
      }
    }
    // mcout<<"d="<<d<<'\n';
    // mcout<<"nmax="<<nmax<<'\n';
    if (d.get() == 0.0) {
      szero = 1;
      serr = 1;
      return;
    }
    if (d.left_limit() == 0) {
      serr = 1;
      if (s_stop == 1) return;
    }
    if (nmax > nr) {
      for (long nc = nr; nc < q; nc++) {
        DoubleAc t(mii.ac(nr, nc));
        mii.ac(nr, nc) = mii.ac(nmax, nc);
        mii.ac(nmax, nc) = t;
      }
      for (long nc = 0; nc < q; nc++) {
        DoubleAc t(mr.ac(nr, nc));
        mr.ac(nr, nc) = mr.ac(nmax, nc);
        mr.ac(nmax, nc) = t;
      }
    }
    DoubleAc t = mii.ac(nr, nr);
    for (long nr1 = 0; nr1 < q; nr1++) {
      if (nr1 != nr) {
        DoubleAc k(mii.ac(nr1, nr) / t);
        // mcout<<"nr1="<<nr1<<" nr="<<nr<<'\n';
        // mcout<<"k="<<k<<'\n';
        for (long nc = nr; nc < q; nc++) {
          mii.ac(nr1, nc) -= k * mii.ac(nr, nc);
        }
        for (long nc = 0; nc < q; nc++) {
          mr.ac(nr1, nc) -= k * mr.ac(nr, nc);
        }
      }
    }
    for (long nc = nr; nc < q; nc++) mii.ac(nr, nc) /= t;
    for (long nc = 0; nc < q; nc++) mr.ac(nr, nc) /= t;
  }
}

void inverse_DynArr(const DynArr<double>& mi, DynArr<double>& mr, int& serr) {
  mfunname(
      "void inverse_DynArr(const DynArr<double>& mi, DynArr<double>& mr, "
      "int& serr)");
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  check_econd12(miqel[0], !=, miqel[1], mcerr);
  serr = 0;
  long q = miqel[0];
  mr = DynArr<double>(q, q);
  if (q == 1) {
    if (mi.ac(0, 0) == 0.0) {
      serr = 1;
      return;
    }
    mr.ac(0, 0) = 1.0 / mi.ac(0, 0);
    return;
  }
  DynArr<DoubleAc> mii;
  DynArr<DoubleAc> mrr(q, q);
  copy_DynArr(mi, mii);
  mrr.assignAll(0.0);
  for (long n = 0; n < miqel[0]; n++) mrr.ac(n, n) = 1.0;
  int szero;
  inverse_DynArr_prot(mii, mrr, szero, serr);
  copy_DynArr(mrr, mr);
}

void inverse_DynArr(const DynArr<DoubleAc>& mi, DynArr<DoubleAc>& mr1,
                    int& szero, int& serr1, DynArr<DoubleAc>& mr2, int& serr2) {
  mfunname(
      "void inverse_DynArr(const DynArr<DoubleAc>& mi, DynArr<DoubleAc>& "
      "mr1, int& szero, int& serr1, DynArr<DoubleAc>& mr2, int& serr2)");
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  check_econd12(miqel[0], !=, miqel[1], mcerr);
  serr1 = 0;
  serr2 = 0;
  long q = miqel[0];
  // mr = DynArr<DoubleAc>(miqel[0], miqel[0]);
  if (q == 1) {
    if (mi.ac(0, 0).get() == 0.0) {
      serr1 = 1;
      return;
    }
    mr1 = DynArr<DoubleAc>(q, q);
    mr2 = DynArr<DoubleAc>(q, q);
    mr1.ac(0, 0) = 1.0 / mi.ac(0, 0).get();
    mr2.ac(0, 0) = DoubleAc(1.0) / mi.ac(0, 0);
    if (fabs(mr2.ac(0, 0)).left_limit() == 0.0) serr2 = 1;
    return;
  }

  DynArr<DoubleAc> mii(q, q);
  for (long n1 = 0; n1 < q; n1++) {
    for (long n2 = 0; n2 < q; n2++) {
      mii.ac(n1, n2) = double(mi.ac(n1, n2));
    }
  }
  DynArr<DoubleAc> mrr(q, q);
  mrr.assignAll(0.0);
  for (long n = 0; n < q; n++) mrr.ac(n, n) = 1.0;
  inverse_DynArr_prot(mii, mrr, szero, serr1, 0);
  copy_DynArr(mrr, mr1);
  if (szero != 0) return;
  mii = mi;
  mrr.assignAll(0.0);
  for (long n = 0; n < q; n++) mrr.ac(n, n) = DoubleAc(1.0);
  inverse_DynArr_prot(mii, mrr, szero, serr2, 0);
  check_econd11(szero, != 0, mcerr);
  copy_DynArr(mrr, mr2);
}

void inverse_DynArr(const DynArr<double>& mi,
                    const DynLinArr<int>& s_var,  // 1 if variable
                    DynArr<double>& mr, int& serr) {
  mfunname(
      "void inverse_DynArr(const DynArr<double>& mi, const "
      "DynLinArr<long>& s_var, DynArr<double>& mr, int& serr)");
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  check_econd12(miqel[0], !=, miqel[1], mcerr);
  check_econd12(s_var.get_qel(), !=, miqel[0], mcerr);

  long q = s_var.get_qel();
  long qvar = 0;
  int s = 1;  // sign that all parameters are varied
  for (long n = 0; n < q; n++) {
    if (s_var[n] == 0)
      s = 0;
    else
      qvar++;
  }
  if (s == 1) {
    inverse_DynArr(mi, mr, serr);
    return;
  }
  check_econd11(qvar, <= 0, mcerr);
  DynArr<double> mi1(qvar, qvar);
  int nv1 = 0;
  for (int n1 = 0; n1 < q; n1++) {
    if (s_var[n1] != 1) continue;
    int nv2 = 0;
    for (int n2 = 0; n2 < q; n2++) {
      if (s_var[n2] == 1) {
        mi1.ac(nv1, nv2) = mi.ac(n1, n2);
        nv2++;
      }
    }
    nv1++;
  }
  DynArr<double> mr1;
  inverse_DynArr(mi1, mr1, serr);
  mr = DynArr<double>(q, q);
  mr.assignAll(0.0);
  nv1 = 0;
  for (int n1 = 0; n1 < q; n1++) {
    if (s_var[n1] != 1) continue;
    int nv2 = 0;
    for (int n2 = 0; n2 < q; n2++) {
      if (s_var[n2] == 1) {
        mr.ac(n1, n2) = mr1.ac(nv1, nv2);
        nv2++;
      }
    }
    nv1++;
  }
}

void inverse_DynArr_prot(const DynArr<DoubleAc>& mi,
                         const DynLinArr<int>& s_var,  // 1 if variable
                         DynArr<DoubleAc>& mr, int& szero, int& serr,
                         int s_stop) {
  mfunname(
      "void inverse_DynArr_prot(const DynArr<DoubleAc>& mi, const "
      "DynLinArr<long>& s_var, DynArr<DoubleAc>& mr, int& szero, int& "
      "serr, int s_stop=1)");
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  check_econd12(miqel[0], !=, miqel[1], mcerr);
  check_econd12(s_var.get_qel(), !=, miqel[0], mcerr);

  long q = s_var.get_qel();

  long qvar = 0;
  int s = 1;  // sign that all parameters are varied
  for (long n = 0; n < q; n++) {
    if (s_var[n] == 0)
      s = 0;
    else
      qvar++;
  }
  if (s == 1) {
    inverse_DynArr_prot(mi, mr, szero, serr, s_stop);
    return;
  }
  check_econd11(qvar, <= 0, mcerr);
  DynArr<DoubleAc> mi1(qvar, qvar);
  int nv1 = 0;
  for (int n1 = 0; n1 < q; n1++) {
    if (s_var[n1] != 1) continue;
    int nv2 = 0;
    for (int n2 = 0; n2 < q; n2++) {
      if (s_var[n2] == 1) {
        mi1.ac(nv1, nv2) = mi.ac(n1, n2);
        nv2++;
      }
    }
    nv1++;
  }
  DynArr<DoubleAc> mr1;
  inverse_DynArr_prot(mi1, mr1, szero, serr, s_stop);
  mr = DynArr<DoubleAc>(q, q);
  mr.assignAll(0.0);
  nv1 = 0;
  for (int n1 = 0; n1 < q; n1++) {
    if (s_var[n1] != 1) continue;
    int nv2 = 0;
    for (int n2 = 0; n2 < q; n2++) {
      if (s_var[n2] == 1) {
        mr.ac(n1, n2) = mr1.ac(nv1, nv2);
        nv2++;
      }
    }
    nv1++;
  }
}

void inverse_DynArr(const DynArr<DoubleAc>& mi,
                    const DynLinArr<int>& s_var,  // 1 if variable
                    DynArr<DoubleAc>& mr1, int& szero, int& serr1,
                    DynArr<DoubleAc>& mr2, int& serr2) {
  mfunname(
      "void inverse_DynArr(const DynArr<DoubleAc>& mi, const "
      "DynLinArr<long>& s_var, DynArr<DoubleAc>& mr1, int& szero, int& "
      "serr1, DynArr<DoubleAc>& mr2, int& serr2 )");
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  check_econd12(miqel[0], !=, miqel[1], mcerr);
  check_econd12(s_var.get_qel(), !=, miqel[0], mcerr);

  long q = s_var.get_qel();
  long qvar = 0;
  int s = 1;  // sign that all parameters are varied
  for (long n = 0; n < q; n++) {
    if (s_var[n] == 0)
      s = 0;
    else
      qvar++;
  }
  if (s == 1) {
    inverse_DynArr(mi, mr1, szero, serr1, mr2, serr2);
    return;
  }
  check_econd11(qvar, <= 0, mcerr);
  DynArr<DoubleAc> mi1(qvar, qvar);
  int nv1 = 0;
  for (int n1 = 0; n1 < q; n1++) {
    if (s_var[n1] != 1) continue;
    int nv2 = 0;
    for (int n2 = 0; n2 < q; n2++) {
      if (s_var[n2] == 1) {
        mi1.ac(nv1, nv2) = mi.ac(n1, n2);
        nv2++;
      }
    }
    nv1++;
  }
  DynArr<DoubleAc> mrr1, mrr2;
  inverse_DynArr(mi1, mrr1, szero, serr1, mrr2, serr2);
  mr1 = DynArr<DoubleAc>(q, q);
  mr1.assignAll(0.0);
  if (serr1 != 1) {
    mr2 = DynArr<DoubleAc>(q, q);
    mr2.assignAll(0.0);
  }
  nv1 = 0;
  for (int n1 = 0; n1 < q; n1++) {
    if (s_var[n1] == 1) {
      int nv2 = 0;
      for (int n2 = 0; n2 < q; n2++) {
        if (s_var[n2] == 1) {
          mr1.ac(n1, n2) = mrr1.ac(nv1, nv2);
          if (serr1 != 1) {
            mr2.ac(n1, n2) = mrr2.ac(nv1, nv2);
          }
          nv2++;
        }
      }
      nv1++;
    }
  }
}

DoubleAc determinant_DynArr(const DynArr<DoubleAc>& mi, long q) {
  mfunname("DoubleAc determinant_DynArr(const DynArr<DoubleAc>& mi, long q)");
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  if (q == 0) {
    check_econd12(miqel[0], !=, miqel[1], mcerr);
    q = miqel[0];
  } else {
    check_econd11(miqel[0], < q, mcerr);
    check_econd11(miqel[1], < q, mcerr);
  }
  // serr=0;
  DynArr<DoubleAc> mii(mi);
#ifndef ALWAYS_USE_TEMPLATE_PAR_AS_FUN_PAR
  return abstract_determinant<DynArr<DoubleAc>, DoubleAc>(mii, q);
#else
  DoubleAc fict;
  return abstract_determinant(mii, q, fict);
#endif
}

DoubleAc determinant_DynArr(const DynArr<DoubleAc>& mi,
                            const DynLinArr<int>& s_var,  // 1 if variable
                            long q) {
  mfunname("DoubleAc determinant_DynArr(...)");
  const DynLinArr<long>& miqel(mi.get_qel());
  check_econd11(miqel.get_qel(), != 2, mcerr);
  check_econd11(miqel[0], <= 0, mcerr);
  // check_econd12(miqel[0] , != , miqel[1] , mcerr);
  if (q == 0) {
    check_econd12(miqel[0], !=, miqel[1], mcerr);
    q = miqel[0];
  } else {
    check_econd11(miqel[0], < q, mcerr);
    check_econd11(miqel[1], < q, mcerr);
  }
  check_econd12(q, >, s_var.get_qel(), mcerr);
  long miq = std::min(miqel[0], miqel[1]);
  long qvar = 0;
  int s = 1;  // sign that all parameters are varied
  for (long n = 0; n < s_var.get_qel(); n++) {
    if (s_var[n] == 0) {
      s = 0;
    } else {
      qvar++;
      if (qvar == q) break;
    }
  }
  if (s == 1) return determinant_DynArr(mi, q);

  check_econd11(qvar, <= 0, mcerr);
  check_econd11(qvar, < q, mcerr);
  DynArr<DoubleAc> mi1(q, q);
  int nv1 = 0;
  for (int n1 = 0; n1 < miq; n1++) {
    if (s_var[n1] != 1) continue;
    int nv2 = 0;
    for (int n2 = 0; n2 < miq; n2++) {
      if (s_var[n2] == 1) {
        mi1.ac(nv1, nv2) = mi.ac(n1, n2);
        nv2++;
        if (nv2 >= q) break;
      }
    }
    nv1++;
    if (nv1 >= q) break;
  }
  return determinant_DynArr(mi1, q);
}
}
