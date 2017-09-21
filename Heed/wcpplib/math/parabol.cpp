/*
Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/math/parabol.h"
#include "wcpplib/math/DoubleAc.h"
#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/matrix/multiply.h"
#include "wcpplib/matrix/inverse.h"

namespace Heed {

Parabol::Parabol(const Parabol& f) { *this = f; }

Parabol::Parabol(double x[3], double y[3]) : s_det(0), s_dxzero(0) {
  mfunname("Parabol::Parabol(double x[3], double y[3])");

  check_econd12a(x[0], ==, x[1], "x[2]=" << x[2] << " y[0]=" << y[0] << " y[1]="
                                         << y[1] << " y[2]=" << y[2] << '\n',
                 mcerr);
  check_econd12a(x[0], ==, x[2], "x[1]=" << x[1] << " y[0]=" << y[0] << " y[1]="
                                         << y[1] << " y[2]=" << y[2] << '\n',
                 mcerr);
  check_econd12a(x[1], ==, x[2], "x[0]=" << x[0] << " y[0]=" << y[0] << " y[1]="
                                         << y[1] << " y[2]=" << y[2] << '\n',
                 mcerr);
  DynArr<DoubleAc> mat(3, 3);
  DynLinArr<DoubleAc> par(3);
  DynLinArr<DoubleAc> f(3);
  for (int i = 0; i < 3; ++i) {
    f[i] = y[i];
    mat.ac(i, 2) = 1.0;
    mat.ac(i, 1) = x[i];
    mat.ac(i, 0) = x[i] * x[i];
  }
  int ierr;
  int szero;
  DynArr<DoubleAc> mat_inv;
  inverse_DynArr_prot(mat, mat_inv, szero, ierr);
  // check_econd11a( ierr, != 0 , "should never happen\n", mcerr );
  if (ierr == 0) {
    par = mat_inv * f;
    // Iprintdla_DoubleAc(mcout, par, 3);
    // if(fabs(par[0]) == 0.0)
    //  da=0.0;
    // else
    //  da=par[0];
    da = par[0];
    db = par[1];
    dc = par[2];
  } else {
    da = 0.0;
    DynLinArr<int> s_var(3);
    s_var[0] = 0;
    s_var[1] = 1;
    s_var[2] = 1;
    DynArr<DoubleAc> mat_inv1(3, 3);
    // int ierr1;
    // inverse_DynArr(mat, s_var, mat_inv, ierr, mat_inv1, ierr1);
    inverse_DynArr_prot(mat, s_var, mat_inv, szero, ierr);
    if (ierr != 0) {
      // what if x1 and x2 are the same but the both differ from x0
      // Then the following should help:
      mat.ac(1, 1) = mat.ac(0, 1);
      mat.ac(1, 2) = mat.ac(0, 2);
      f[1] = f[0];
      s_var[0] = 0;
      s_var[1] = 1;
      s_var[2] = 1;
      inverse_DynArr_prot(mat, s_var, mat_inv, szero, ierr);
      check_econd11a(ierr, != 0,
                     "should never happen\nmat=" << mat << "\ns_var=" << s_var
                                                 << "\nmat_inv=" << mat_inv,
                     mcerr);
    }
    par = mat_inv * f;
    db = par[1];
    dc = par[2];
  }
}

Parabol::Parabol(double x1, double x2, double x3, double y1, double y2,
                 double y3)
    : s_det(0), s_dxzero(0) {
  mfunname("Parabol::Parabol(double x[3], double y[3])");

  check_econd12a(x1, ==, x2, "x3=" << x3 << " y1=" << y1 << " y2=" << y2
                                   << " y3=" << y3 << '\n',
                 mcerr);
  check_econd12a(x1, ==, x3, "x2=" << x2 << " y1=" << y1 << " y2=" << y2
                                   << " y3=" << y3 << '\n',
                 mcerr);
  check_econd12a(x2, ==, x3, "x1=" << x1 << " y1=" << y1 << " y2=" << y2
                                   << " y3=" << y3 << '\n',
                 mcerr);
  DynArr<DoubleAc> mat(3, 3);
  DynLinArr<DoubleAc> par(3);
  DynLinArr<DoubleAc> f(3);
  f[0] = y1;
  mat.ac(0, 2) = 1.0;
  mat.ac(0, 1) = x1;
  mat.ac(0, 0) = x1 * x1;
  f[1] = y2;
  mat.ac(1, 2) = 1.0;
  mat.ac(1, 1) = x2;
  mat.ac(1, 0) = x2 * x2;
  f[2] = y3;
  mat.ac(2, 2) = 1.0;
  mat.ac(2, 1) = x3;
  mat.ac(2, 0) = x3 * x3;

  int ierr;
  int szero;
  DynArr<DoubleAc> mat_inv;
  inverse_DynArr_prot(mat, mat_inv, szero, ierr);
  // check_econd11a( ierr, != 0 , "should never happen\n", mcerr );
  if (ierr == 0) {
    par = mat_inv * f;
    // Iprintdla_DoubleAc(mcout, par, 3);
    // if(fabs(par[0]) == 0.0)
    //  da=0.0;
    // else
    //  da=par[0];
    da = par[0];
    db = par[1];
    dc = par[2];
  } else {
    da = 0.0;
    DynLinArr<int> s_var(3);
    s_var[0] = 0;
    s_var[1] = 1;
    s_var[2] = 1;
    DynArr<DoubleAc> mat_inv1(3, 3);
    // int ierr1;
    // inverse_DynArr(mat, s_var, mat_inv, ierr, mat_inv1, ierr1);
    inverse_DynArr_prot(mat, s_var, mat_inv, szero, ierr);
    if (ierr != 0) {
      // what if x1 and x2 are the same but the both differ from x0
      // Then the following should help:
      mat.ac(1, 1) = mat.ac(0, 1);
      mat.ac(1, 2) = mat.ac(0, 2);
      f[1] = f[0];
      s_var[0] = 0;
      s_var[1] = 1;
      s_var[2] = 1;
      inverse_DynArr_prot(mat, s_var, mat_inv, szero, ierr);
      check_econd11a(ierr, != 0,
                     "should never happen\nmat=" << mat << "\ns_var=" << s_var
                                                 << "\nmat_inv=" << mat_inv,
                     mcerr);
    }
    par = mat_inv * f;
    db = par[1];
    dc = par[2];
  }
}

Parabol::Parabol(double x[3], double y[3], int /*ii*/) : s_det(0), s_dxzero(0) {
  mfunname("Parabol::Parabol(double x[3], double y[3], int)");

  check_econd12(x[0], ==, x[1], mcerr);
  // check_econd12( x[0] , == , x[2] , mcerr);
  // check_econd12( x[1] , == , x[2] , mcerr);

  DynArr<DoubleAc> mat(3, 3);
  DynLinArr<DoubleAc> par(3);
  DynLinArr<DoubleAc> f(3);
  for (int i = 0; i < 3; ++i) f[i] = y[i];
  for (int i = 0; i < 2; ++i) {
    mat.ac(i, 2) = 1.0;
    mat.ac(i, 1) = x[i];
    // Iprintdan(mcout, mat.ac(i,1));
    mat.ac(i, 0) = x[i] * x[i];
  }
  mat.ac(2, 2) = 0.0;
  mat.ac(2, 1) = 1.0;
  mat.ac(2, 0) = 2.0 * x[2];
  int ierr;
  int szero;
  DynArr<DoubleAc> mat_inv;
  inverse_DynArr_prot(mat, mat_inv, szero, ierr);
  check_econd11a(ierr, != 0, "should never happen\n", mcerr);
  /*
  if (ierr != 0) {
    da = 0.0;
    DynLinArr<int> s_var(3);
    s_var[0] = 0;
    s_var[1] = 1;
    s_var[2] = 1;
    inverse_DynArr(mat, mat_inv, ierr);
    check_econd11a( ierr, != 0 , "should never happen\n", mcerr );
    par = mat_inv * f;
    db=par[1]; dc=par[2];
  } else {
  */
  par = mat_inv * f;
  if (fabs(par[0]) == 0.0) {
    da = 0.0;
  } else {
    da = par[0];
    db = par[1];
    dc = par[2];
  }
  /*
  HepMatrix mat(3,3,0);
  HepVector par(3,0);
  HepVector f(3,0);
  for (int i = 0; i < 3; i++) f[i]=y[i];
  for (int i = 0; i < 2; i++) {
    mat[i][2] = 1.0;
    mat[i][1] = x[i];
    mat[i][0] = x[i] * x[i];
  }
  mat[2][2] = 0.0;
  mat[2][1] = 1.0;
  mat[2][0] = 2.0 * x[2];
  par = solve(mat, f);
  da = par[0]; db = par[1]; dc = par[2];
  */
}

int Parabol::find_zero(double xzero[2]) const {
  mfunnamep("int Parabol::find_zero(double xzero[2]) const");
  const Parabol& t = (*this);
  if (s_dxzero == 0) {
    // mcout<<"Parabol::find_zero: s_dxzero == 0\n";
    t.s_dxzero = 1;
    if (da == 0.0) {
      if (db == 0.0) {
        funnw.ehdr(mcerr);
        mcerr << "can not find zero\n";
        spexit(mcerr);
      } else {
        t.qdxzero = 1;
        t.dxzero[0] = -dc / db;
      }
    } else {
      if (determinant() < 0.0) {
        t.qdxzero = 0;
        t.dxzero[0] = 0;
        t.dxzero[1] = 0;
      } else if (determinant() == 0.0) {
        t.qdxzero = 1;
        t.dxzero[0] = -db / (2.0 * da);
      } else {
        const double sq = sqrt(determinant());
        t.qdxzero = 2;
        if (da > 0.0) {
          t.dxzero[0] = (-db - sq) / (2.0 * da);
          t.dxzero[1] = (-db + sq) / (2.0 * da);
        } else {
          t.dxzero[1] = (-db - sq) / (2.0 * da);
          t.dxzero[0] = (-db + sq) / (2.0 * da);
        }
        // mcout<<"Parabol::find_zero: t.dxzero[0]="<<t.dxzero[0]
        //     <<" dxzero[0]="<<dxzero[0]<<'\n';
        // mcout<<"Parabol::find_zero: t.dxzero[1]="<<t.dxzero[1]
        //     <<" dxzero[1]="<<dxzero[1]<<'\n';
      }
    }
  }
  xzero[0] = dxzero[0];
  xzero[1] = dxzero[1];
  return qdxzero;
}

double Parabol::find_maxmin(void) {
  mfunname("double Parabol::find_maxmin(void)");
  check_econd11(da, == 0, mcerr);
  return -db / (2.0 * da);
}

std::ostream& operator<<(std::ostream& file, const Parabol& f) {
  double xz[2];
  int q = f.find_zero(xz);
  Ifile << "Parabol: a=" << f.a() << " b=" << f.b() << " c=" << f.c()
        << " qxzero=" << q;
  if (q > 0) file << " xzero=" << xz[0];
  if (q > 1) file << ' ' << xz[1];
  file << '\n';
  return file;
}
}
