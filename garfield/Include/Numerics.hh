// Collection of numerical routines

#ifndef G_NUMERICS_H
#define G_NUMERICS_H

#include <vector>
#include <complex>

namespace Garfield {

namespace Numerics {

// Linear algebra routines from CERNLIB
void Dfact(const int n, std::vector<std::vector<double> >& a,
           std::vector<int>& ir, int& ifail, double& det, int& jfail);
void Dfeqn(const int n, std::vector<std::vector<double> >& a,
           std::vector<int>& ir, std::vector<double>& b);
void Dfinv(const int n, std::vector<std::vector<double> >& a,
           std::vector<int>& ir);
void Deqinv(const int n, std::vector<std::vector<double> >& a, int& ifail,
            std::vector<double>& b);

void Cfact(const int n, std::vector<std::vector<std::complex<double> > >& a,
           std::vector<int>& ir, int& ifail, std::complex<double>& det,
           int& jfail);
void Cfinv(const int n, std::vector<std::vector<std::complex<double> > >& a,
           std::vector<int>& ir);
void Cinv(const int n, std::vector<std::vector<std::complex<double> > >& a,
          int& ifail);

// Numerical integration using 15-point Gauss-Kronrod algorithm
double GaussKronrod15(double (*f)(const double), const double a,
                      const double b);

// Modified Bessel functions.
// Series expansions from Abramowitz and Stegun.
inline double BesselI0S(const double xx) {
  return 1. + 3.5156229 * pow(xx / 3.75, 2) + 3.0899424 * pow(xx / 3.75, 4) +
         1.2067492 * pow(xx / 3.75, 6) + 0.2659732 * pow(xx / 3.75, 8) +
         0.0360768 * pow(xx / 3.75, 10) + 0.0045813 * pow(xx / 3.75, 12);
}

inline double BesselI1S(const double xx) {
  return xx *
         (0.5 + 0.87890594 * pow(xx / 3.75, 2) +
          0.51498869 * pow(xx / 3.75, 4) + 0.15084934 * pow(xx / 3.75, 6) +
          0.02658733 * pow(xx / 3.75, 8) + 0.00301532 * pow(xx / 3.75, 10) +
          0.00032411 * pow(xx / 3.75, 12));
}

inline double BesselK0S(const double xx) {
  return -log(xx / 2.) * BesselI0S(xx) - 0.57721566 +
         0.42278420 * pow(xx / 2., 2) + 0.23069756 * pow(xx / 2., 4) +
         0.03488590 * pow(xx / 2., 6) + 0.00262698 * pow(xx / 2., 8) +
         0.00010750 * pow(xx / 2., 10) + 0.00000740 * pow(xx / 2., 12);
}

inline double BesselK0L(const double xx) {
  return (exp(-xx) / sqrt(xx)) *
         (1.25331414 - 0.07832358 * (2. / xx) + 0.02189568 * pow(2. / xx, 2) -
          0.01062446 * pow(2. / xx, 3) + 0.00587872 * pow(2. / xx, 4) -
          0.00251540 * pow(2. / xx, 5) + 0.00053208 * pow(2. / xx, 6));
}

inline double BesselK1S(const double xx) {
  return log(xx / 2.) * BesselI1S(xx) +
         (1. / xx) *
             (1. + 0.15443144 * pow(xx / 2., 2) - 0.67278579 * pow(xx / 2., 4) -
              0.18156897 * pow(xx / 2., 6) - 0.01919402 * pow(xx / 2., 8) -
              0.00110404 * pow(xx / 2., 10) - 0.00004686 * pow(xx / 2., 12));
}

inline double BesselK1L(const double xx) {
  return (exp(-xx) / sqrt(xx)) *
         (1.25331414 + 0.23498619 * (2. / xx) - 0.03655620 * pow(2. / xx, 2) +
          0.01504268 * pow(2. / xx, 3) - 0.00780353 * pow(2. / xx, 4) +
          0.00325614 * pow(2. / xx, 5) - 0.00068245 * pow(2. / xx, 6));
}

double Divdif(const std::vector<double>& f, const std::vector<double>& a,
              int nn, double x, int mm);

bool Boxin3(const std::vector<std::vector<std::vector<double> > >& value,
            const std::vector<double>& xAxis, 
            const std::vector<double>& yAxis,
            const std::vector<double>& zAxis, 
            const int nx, const int ny, const int nz, 
            const double xx, const double yy, const double zz, 
            double& f, const int iOrder);
}
}

#endif
