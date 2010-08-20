#include <cmath>

#include "Numerics.hh"

namespace Garfield {

namespace Numerics {

void
Dfact(const int n, std::vector<std::vector<double> >& a, std::vector<int>& ir,
      int& ifail, double& det, int& jfail) {
    
  const double g1 = 1.e-19;
  const double g2 = 1.e-19;
  
  double tf, p, q, t, s11, s12;
  int k;
  
  ifail = jfail = 0;
  
  int nxch = 0;
  det = 1.;
  
  for (int j = 1; j <= n; ++j) {
    k  =  j;
    p  =  fabs(a[j - 1][j - 1]);
    if (j == n) {
      if (p <= 0.) {
        det = 0.;
        ifail = -1;
        jfail = 0;
        return;
      }
      det *= a[j - 1][j - 1];
      a[j - 1][j - 1]  =  1. / a[j - 1][j - 1];
      t  =  fabs(det);
      if (t < g1) {
        det = 0.;
        if (jfail == 0) jfail = -1;
      } else if (t > g2) {
        det = 1.;
        if (jfail == 0) jfail = +1;
      }
      continue;
    }
    for (int i = j + 1; i <= n; ++i) {
      q  =  fabs(a[i - 1][j - 1]);
      if (q <= p) continue;
      k = i;
      p = q;
    }
    if (k != j) {
      for (int l = 1; l <= n; ++l) {
        tf = a[j - 1][l - 1];
        a[j - 1][l - 1] = a[k - 1][l - 1];
        a[k - 1][l - 1] = tf;
      }
      ++nxch;
      ir[nxch - 1] = j * 4096 + k;
    } else if (p <= 0.) {
      det = 0.;
      ifail = -1;
      jfail = 0;
      return;
    }
    det *= a[j - 1][j - 1];
    a[j - 1][j - 1]  =  1. / a[j - 1][j - 1];
    t  =  fabs(det);
    if (t < g1) {
      det = 0.;
      if (jfail == 0) jfail = -1;
    } else if (t > g2) {
      det = 1.;
      if (jfail == 0) jfail = +1;
    }
    for (k = j + 1; k <= n; ++k) {
      s11  =  -a[j - 1][k - 1];
      s12  =  -a[k - 1][j];
      if (j == 1) {
        a[j - 1][k - 1] = -s11 * a[j - 1][j - 1];
        a[k - 1][j] = -(s12 + a[j - 1][j] * a[k - 1][j - 1]);
        continue;
      }
      for (int i = 1; i <= j - 1; ++i) {
        s11 += a[i - 1][k - 1] * a[j - 1][i - 1];
        s12 += a[i - 1][j]     * a[k - 1][i - 1];
      }
      a[j - 1][k - 1]  =  -s11 * a[j - 1][j - 1];
      a[k - 1][j]      =  -a[j - 1][j] * a[k - 1][j - 1] - s12;
    }
  }
  
  if (nxch % 2 != 0) det = -det;
  if (jfail != 0) det = 0.;
  ir[n - 1] = nxch;
  
}

void
Dfeqn(const int n, std::vector<std::vector<double> >& a,
      std::vector<int>& ir, std::vector<double>& b) {
     
  double te;
  double s21,s22;
  
  if (n <= 0) return;
  
  int nxch = ir[n - 1];
  if (nxch != 0) {
    for (int m = 1; m <= nxch; ++m) {
      int ij = ir[m - 1];
      int i = ij / 4096;
      int j = ij % 4096;
      te = b[i - 1];
      b[i - 1] = b[j - 1];
      b[j - 1] = te;
    }
  }
    
  b[0] *= a[0][0];
  if (n == 1) return;
  
  
  for (int i = 2; i <= n; ++i) {
    s21 = -b[i - 1];
    for (int  j = 1; j <= i - 1; ++j) {
      s21 += a[i - 1][j - 1] * b[j - 1];
    }
    b[i - 1] = -a[i - 1][i - 1] * s21;
  }
    
  for (int i = 1; i <= n - 1; ++i) {    
    s22 = -b[n - i - 1];
    for (int j = 1; j <= i; ++j) {
      s22 += a[n - i - 1][n - j] * b[n - j];
    }
    b[n - i - 1] = -s22;
  }
  
}

void
Dfinv(const int n, std::vector<std::vector<double> >& a,
      std::vector<int>& ir) {
      
  double ti;
  double s31, s32, s33, s34;
  
  if (n <= 1) return;
  a[1][0] = - a[1][1] * a[0][0] * a[1][0];
  a[0][1] = -a[0][1];
  if (n > 2) {
    for (int i = 3; i <= n; ++i) {
      for (int j = 1; j <= i - 2; ++j) {
        s31 = 0.;
        s32 = a[j - 1][i - 1];
        for (int k = j; k <= i - 2; ++k) {
          s31 += a[k - 1][j - 1] * a[i - 1][k - 1];
          s32 += a[j - 1][k]     * a[k][i - 1];
        }
        a[i - 1][j - 1] = -a[i - 1][i - 1] * 
                          (s31 + a[i - 2][j - 1] * a[i - 1][i - 2]);
        a[j - 1][i - 1] = -s32;
      }
      a[i - 1][i - 2] = -a[i - 1][i - 1] * a[i - 2][i - 2] * a[i - 1][i - 2];
      a[i - 2][i - 1] = -a[i - 2][i - 1];
    }
  }
  
  for (int i = 1; i <= n - 1; ++i) {
    for (int j = 1; j <= i; ++j) {
      s33 = a[i - 1][j - 1];
      for (int k = 1; k <= n - i; ++k) { 
        s33 += a[i + k - 1][j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][j - 1] = s33;
    }
    for (int j = 1; j <= n - i; ++j) {
      s34 = 0.;
      for (int k = j; k <= n - i; ++k) {
        s34 += a[i + k - 1][i + j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][i + j - 1] = s34;
    }
  }
  
  int nxch = ir[n - 1];
  if (nxch == 0) return;
  
  for (int m = 1; m <= nxch; ++m) {
    int k = nxch - m + 1;
    int ij = ir[k - 1];
    int i = ij / 4096;
    int j = ij % 4096;
    for (k = 1; k <= n; ++k) {
      ti = a[k - 1][i - 1];
      a[k - 1][i - 1] = a[k - 1][j - 1];
      a[k - 1][j - 1] = ti;
    }
  }
  
}

//   ******************************************************************
//
//   REPLACES B BY THE SOLUTION X OF A*X=B, AND REPLACES A BY ITS IN-
//   VERSE.
//
//   n            ORDER OF THE SQUARE MATRIX IN ARRAY A.
//   A            (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY CONTAINING
//                AN n BY n MATRIX.
//
//   IFAIL        OUTPUT PARAMETER.   IFAIL= 0 ... NORMAL EXIT.
//                                    IFAIL=-1 ... SINGULAR MATRIX.
//
//   B            (DOUBLE PRECISION) ONE-DIMENSIONAL ARRAY
//
//   CALLS ... DFACT, DFINV.
//
//   ******************************************************************

void
Deqinv(const int n, std::vector<std::vector<double> >& a, 
       int& ifail, std::vector<double>& b) {
         
  double t1,t2,t3;
  double det,temp,s;
  double b1,b2,c11,c12,c13,c21,c22,c23,c31,c32,c33;
  
  std::vector<int> ir; ir.clear(); ir.resize(n);
  for (int i = 0; i < n; ++i) ir[i] = 0;
  
  //TEST FOR PARAMETER ERRORS.
  if (n < 1) {
    ifail = +1;
    return;
  }
  
  ifail = 0;
  int jfail = 0;
  
  if (n > 3) {
    // n > 3 CASES.
    // FACTORIZE MATRIX, INVERT AND SOLVE SYSTEM.
    Dfact(n, a, ir, ifail, det, jfail);
    if (ifail != 0) return;
    Dfeqn(n, a, ir, b);
    Dfinv(n, a, ir);
  } else if (n == 3) {
    // n=3 CASE.
    // COMPUTE COFACTORS.
    c11=a[1][1]*a[2][2]-a[1][2]*a[2][1];
    c12=a[1][2]*a[2][0]-a[1][0]*a[2][2];
    c13=a[1][0]*a[2][1]-a[1][1]*a[2][0];
    c21=a[2][1]*a[0][2]-a[2][2]*a[0][1];
    c22=a[2][2]*a[0][0]-a[2][0]*a[0][2];
    c23=a[2][0]*a[0][1]-a[2][1]*a[0][0];
    c31=a[0][1]*a[1][2]-a[0][2]*a[1][1];
    c32=a[0][2]*a[1][0]-a[0][0]*a[1][2];
    c33=a[0][0]*a[1][1]-a[0][1]*a[1][0];
    t1=fabs(a[0][0]);
    t2=fabs(a[1][0]);
    t3=fabs(a[2][0]);

    // SET temp=PIVOT AND det=PIVOT*det.
    if (t1 >= t2) {
      if (t3 >= t1) {
        // PIVOT IS A31
        temp=a[2][0];
        det=c23*c12-c22*c13;
      } else {
        // PIVOT IS A11
        temp=a[0][0];
        det=c22*c33-c23*c32;
      }
    } else {
      if (t3 >= t2) {
        // PIVOT IS A31
        temp=a[2][0];
        det=c23*c12-c22*c13;
      } else {
        // PIVOT IS A21
        temp=a[1][0];
        det=c13*c32-c12*c33;
      }
    }
    
    // SET ELEMENTS OF INVERSE IN A.
    if (det == 0.) {
      ifail = -1;
      return;
    }
    s=temp/det;
    a[0][0]=s*c11;
    a[0][1]=s*c21;
    a[0][2]=s*c31;
    a[1][0]=s*c12;
    a[1][1]=s*c22;
    a[1][2]=s*c32;
    a[2][0]=s*c13;
    a[2][1]=s*c23;
    a[2][2]=s*c33;
    
    // REPLACE B BY AINV*B.
    b1=b[0];
    b2=b[1];
    b[0]=a[0][0]*b1+a[0][1]*b2+a[0][2]*b[2];
    b[1]=a[1][0]*b1+a[1][1]*b2+a[1][2]*b[2];
    b[2]=a[2][0]*b1+a[2][1]*b2+a[2][2]*b[2];
  } else if (n == 2) {
    // n=2 CASE BY CRAMERS RULE.
    det=a[0][0]*a[1][1]-a[0][1]*a[1][0];
    if (det == 0.) {
      ifail = -1;
      return;
    }
    s=1./det;
    c11   =s*a[1][1];
    a[0][1]=-s*a[0][1];
    a[1][0]=-s*a[1][0];
    a[1][1]=s*a[0][0];
    a[0][0]=c11;

    b1=b[0];
    b[0]=c11*b1+a[0][1]*b[1];
    b[1]=a[1][0]*b1+a[1][1]*b[1];
  } else {
    //n=1 CASE.
    if (a[0][0] == 0.) {
      ifail = -1;
      return;
    }
    a[0][0]=1./a[0][0];
    b[0]=a[0][0]*b[0];
  }
  
}

void
Cfact(const int n, std::vector<std::vector<std::complex<double> > >& a, 
      std::vector<int>& ir,
      int& ifail, std::complex<double>& det, int& jfail) {
    
  const double g1 = 1.e-19;
  const double g2 = 1.e-19;
  
  std::complex<double> tf;
  double p, q, t;
  std::complex<double> s11, s12;
  int k;
  
  ifail = jfail = 0;
  
  int nxch = 0;
  det = std::complex<double>(1., 0.);
  
  for (int j = 1; j <= n; ++j) {
    k  =  j;
    p  =  std::max(fabs(real(a[j - 1][j - 1])), 
                   fabs(imag(a[j - 1][j - 1])));
    if (j == n) {
      if (p <= 0.) {
        det = std::complex<double>(0., 0.);
        ifail = -1;
        jfail = 0;
        return;
      }
      det *= a[j - 1][j - 1];
      a[j - 1][j - 1]  =  std::complex<double>(1.,0.) / a[j - 1][j - 1];
      t  =  std::max(fabs(real(det)), fabs(imag(det)));
      if (t < g1) {
        det = std::complex<double>(0., 0.);
        if (jfail == 0) jfail = -1;
      } else if (t > g2) {
        det = std::complex<double>(1., 0.);
        if (jfail == 0) jfail = +1;
      }
      continue;
    }
    for (int i = j + 1; i <= n; ++i) {
      q  =  std::max(fabs(real(a[i - 1][j - 1])),
                     fabs(imag(a[i - 1][j - 1])));
      if (q <= p) continue;
      k = i;
      p = q;
    }
    if (k != j) {
      for (int l = 1; l <= n; ++l) {
        tf = a[j - 1][l - 1];
        a[j - 1][l - 1] = a[k - 1][l - 1];
        a[k - 1][l - 1] = tf;
      }
      ++nxch;
      ir[nxch - 1] = j * 4096 + k;
    } else if (p <= 0.) {
      det = std::complex<double>(0., 0.);
      ifail = -1;
      jfail = 0;
      return;
    }
    det *= a[j - 1][j - 1];
    a[j - 1][j - 1]  =  1. / a[j - 1][j - 1];
    t  =  std::max(fabs(real(det)), fabs(imag(det)));
    if (t < g1) {
      det = std::complex<double>(0., 0.);
      if (jfail == 0) jfail = -1;
    } else if (t > g2) {
      det = std::complex<double>(1., 0.);
      if (jfail == 0) jfail = +1;
    }
    for (k = j + 1; k <= n; ++k) {
      s11  =  -a[j - 1][k - 1];
      s12  =  -a[k - 1][j];
      if (j == 1) {
        a[j - 1][k - 1] = -s11 * a[j - 1][j - 1];
        a[k - 1][j] = -(s12 + a[j - 1][j] * a[k - 1][j - 1]);
        continue;
      }
      for (int i = 1; i <= j - 1; ++i) {
        s11 += a[i - 1][k - 1] * a[j - 1][i - 1];
        s12 += a[i - 1][j]     * a[k - 1][i - 1];
      }
      a[j - 1][k - 1]  =  -s11 * a[j - 1][j - 1];
      a[k - 1][j]      =  -a[j - 1][j] * a[k - 1][j - 1] - s12;
    }
  }
  
  if (nxch % 2 != 0) det = -det;
  if (jfail != 0) det = std::complex<double>(0., 0.);
  ir[n - 1] = nxch;
  
}

void
Cfinv(const int n, std::vector<std::vector<std::complex<double> > >& a,
      std::vector<int>& ir) {
      
  std::complex<double> ti;
  std::complex<double> s31, s32, s33, s34;
  
  if (n <= 1) return;
  a[1][0] = - a[1][1] * a[0][0] * a[1][0];
  a[0][1] = -a[0][1];
  if (n > 2) {
    for (int i = 3; i <= n; ++i) {
      for (int j = 1; j <= i - 2; ++j) {
        s31 = std::complex<double>(0., 0.);
        s32 = a[j - 1][i - 1];
        for (int k = j; k <= i - 2; ++k) {
          s31 += a[k - 1][j - 1] * a[i - 1][k - 1];
          s32 += a[j - 1][k]     * a[k][i - 1];
        }
        a[i - 1][j - 1] = -a[i - 1][i - 1] * 
                          (s31 + a[i - 2][j - 1] * a[i - 1][i - 2]);
        a[j - 1][i - 1] = -s32;
      }
      a[i - 1][i - 2] = -a[i - 1][i - 1] * a[i - 2][i - 2] * a[i - 1][i - 2];
      a[i - 2][i - 1] = -a[i - 2][i - 1];
    }
  }
  
  for (int i = 1; i <= n - 1; ++i) {
    for (int j = 1; j <= i; ++j) {
      s33 = a[i - 1][j - 1];
      for (int k = 1; k <= n - i; ++k) { 
        s33 += a[i + k - 1][j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][j - 1] = s33;
    }
    for (int j = 1; j <= n - i; ++j) {
      s34 = std::complex<double>(0., 0.);
      for (int k = j; k <= n - i; ++k) {
        s34 += a[i + k - 1][i + j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][i + j - 1] = s34;
    }
  }
  
  int nxch = ir[n - 1];
  if (nxch == 0) return;
  
  for (int m = 1; m <= nxch; ++m) {
    int k = nxch - m + 1;
    int ij = ir[k - 1];
    int i = ij / 4096;
    int j = ij % 4096;
    for (k = 1; k <= n; ++k) {
      ti = a[k - 1][i - 1];
      a[k - 1][i - 1] = a[k - 1][j - 1];
      a[k - 1][j - 1] = ti;
    }
  }
  
}


//    ******************************************************************
//
//     REPLACES A BY ITS INVERSE.
//
//     (PARAMETERS AS FOR CEQINV.)
//
//     CALLS ... CFACT, CFINV.
//
//     ******************************************************************

void
Cinv(const int n, 
     std::vector<std::vector<std::complex<double> > >& a, int& ifail) {
     
  double t1,t2,t3;
  std::complex<double> det,temp,s;
  std::complex<double> c11,c12,c13,c21,c22,c23,c31,c32,c33;
   
  std::vector<int> ir; ir.clear(); ir.resize(n);
  for (int i = 0; i < n; ++i) ir[i] = 0;
  
  // TEST FOR PARAMETER ERRORS.
  if (n < 1) {
    ifail = 1;
    return;
  }
  
  ifail = 0;
  int jfail = 0;
 
  if (n > 3) {
    // n > 3 CASES.
    // FACTORIZE MATRIX AND INVERT.
    Cfact(n, a, ir, ifail, det, jfail);
    if (ifail != 0) return;
    Cfinv(n, a, ir);
  } else if (n == 3) {
    // n=3 CASE.
    // COMPUTE COFACTORS.
    c11=a[1][1]*a[2][2]-a[1][2]*a[2][1];
    c12=a[1][2]*a[2][0]-a[1][0]*a[2][2];
    c13=a[1][0]*a[2][1]-a[1][1]*a[2][0];
    c21=a[2][1]*a[0][2]-a[2][2]*a[0][1];
    c22=a[2][2]*a[0][0]-a[2][0]*a[0][2];
    c23=a[2][0]*a[0][1]-a[2][1]*a[0][0];
    c31=a[0][1]*a[1][2]-a[0][2]*a[1][1];
    c32=a[0][2]*a[1][0]-a[0][0]*a[1][2];
    c33=a[0][0]*a[1][1]-a[0][1]*a[1][0];
    t1=fabs(real(a[0][0])) + fabs(imag(a[0][0]));
    t2=fabs(real(a[1][0])) + fabs(imag(a[1][0]));
    t3=fabs(real(a[2][0])) + fabs(imag(a[2][0]));

    // SET temp=PIVOT AND det=PIVOT*det.
    if (t1 >= t2) {
      if (t3 >= t1) {
        // PIVOT IS A31
        temp=a[2][0];
        det=c23*c12-c22*c13;
      } else {
        // PIVOT IS A11
        temp=a[0][0];
        det=c22*c33-c23*c32;
      }
    } else {
      if (t3 >= t2) {
        // PIVOT IS A31
        temp=a[2][0];
        det=c23*c12-c22*c13;
      } else {
        // PIVOT IS A21
        temp=a[1][0];
        det=c13*c32-c12*c33;
      }
    }
    // SET ELEMENTS OF INVERSE IN A.
    if (real(det) == 0. && imag(det) == 0.) {
      ifail = -1;
      return;
    }
    s=temp/det;
    a[0][0]=s*c11;
    a[0][1]=s*c21;
    a[0][2]=s*c31;
    a[1][0]=s*c12;
    a[1][1]=s*c22;
    a[1][2]=s*c32;
    a[2][0]=s*c13;
    a[2][1]=s*c23;
    a[2][2]=s*c33;
  } else if (n == 2) {
    // n=2 CASE BY CRAMERS RULE.
    det=a[0][0]*a[1][1]-a[0][1]*a[1][0];
    if (real(det) == 0. && imag(det) == 0.) {
      ifail = -1;
      return;
    }
    s=std::complex<double>(1., 0.)/det;
    c11   =s*a[1][1];
    a[0][1]=-s*a[0][1];
    a[1][0]=-s*a[1][0];
    a[1][1]=s*a[0][0];
    a[0][0]=c11;
  } else {
    //n=1 CASE.
    if (real(a[0][0]) == 0. && imag(a[0][0]) == 0.) {
      ifail = -1;
      return;
    }
    a[0][0]=std::complex<double>(1.,0.)/a[0][0];
  }

}

// Numerical integration using 15-point Gauss-Kronrod algorithm
// Origin: QUADPACK
double 
GaussKronrod15(double (*f)(const double), 
               const double a, const double b) {

  // Abscissae of the 15-point Kronrod rule
  // xGK[1], xGK[3], ... abscissae of the 7-point Gauss rule
  // xGK[0], xGK[2], ... abscissae which are optimally added 
  //                     to the 7-point Gauss rule
  const double xGK[8] = {9.914553711208126e-01, 9.491079123427585e-01, 
                         8.648644233597691e-01, 7.415311855993944e-01, 
                         5.860872354676911e-01, 4.058451513773972e-01,
                         2.077849550078985e-01, 0.0e+00};
  // Weights of the 15-point Kronrod rule
  const double wGK[8] = {2.293532201052922e-02, 6.309209262997855e-02,
                         1.047900103222502e-01, 1.406532597155259e-01, 
                         1.690047266392679e-01, 1.903505780647854e-01, 
                         2.044329400752989e-01, 2.094821410847278e-01};                                            
  // Weights of the 7-point Gauss rule
  const double wG[4] = {1.294849661688697e-01, 2.797053914892767e-01, 
                        3.818300505051189e-01, 4.179591836734694e-01};
  
  // Mid-point of the interval
  const double center = 0.5 * (a + b);
  // Half-length of the interval
  const double halfLength = 0.5 * (b - a);

  double fC = f(center);
  // Result of the 7-point Gauss formula
  double resG = fC * wG[3];
  // Result of the 15-point Kronrod formula
  double resK = fC * wGK[7];
  
  for (int j = 0; j < 3; ++j) {
    const int i = j * 2 + 1;
    // Abscissa
    const double x = halfLength * xGK[i];
    // Function value
    const double fSum = f(center - x) + f(center + x);
    resG += wG[j] * fSum;
    resK += wGK[i] * fSum;
  }
  
  for (int j = 0; j < 4; ++j) {
    const int i = j * 2;
    const double x = halfLength * xGK[i];
    const double fSum = f(center - x) + f(center + x);
    resK += wGK[i]* fSum;
  }

  return resK * halfLength;
  
}


}

}
