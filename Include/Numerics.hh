// Collection of numerical routines

#ifndef G_NUMERICS_H
#define G_NUMERICS_H

#include <vector>
#include <complex>

namespace Garfield {

namespace Numerics {

  void Dfact(const int n, std::vector<std::vector<double> >& a,
             std::vector<int>& ir, int& ifail, double& det, int& jfail);
  void Dfeqn(const int n, std::vector<std::vector<double> >& a,
             std::vector<int>& ir, std::vector<double>& b);
  void Dfinv(const int n, std::vector<std::vector<double> >& a,
             std::vector<int>& ir);
  void Deqinv(const int n, std::vector<std::vector<double> >& a,
              int& ifail, std::vector<double>& b);

  void Cfact(const int n, std::vector<std::vector<std::complex<double> > >& a,
             std::vector<int>& ir, int& ifail, 
             std::complex<double>& det, int& jfail);
  void Cfinv(const int n, std::vector<std::vector<std::complex<double> > >& a,
             std::vector<int>& ir);
  void Cinv(const int n, std::vector<std::vector<std::complex<double> > >& a,
            int& ifail);

}

}

#endif
