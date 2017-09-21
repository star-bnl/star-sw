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
#include <iomanip>
#include "wcpplib/math/minmax.h"
#include "wcpplib/math/DoubleAc.h"

namespace Heed {

DoubleAc::DoubleAc(double f, double ffmin, double ffmax) {
  mfunname("DoubleAc::DoubleAc(double f, double ffmin, double ffmax)");
  check_econd12a(f, <, ffmin, "f - ffmin=" << f - ffmin << '\n', mcerr);
  check_econd12a(f, >, ffmax, "f - ffmax=" << f - ffmax << '\n', mcerr);
  di = ffmin;
  d = f;
  da = ffmax;
}

DoubleAc::DoubleAc(double f, double relative_prec) {
  mfunname("DoubleAc::DoubleAc(double f, double relative_prec)");
  check_econd11(relative_prec, < 0, mcerr);
  check_econd11(relative_prec, >= 1, mcerr);
  d = f;
  if (f >= 0) {
    da = f * (1.0 + relative_prec);
    di = f / (1.0 + relative_prec);
  } else {
    di = f * (1.0 + relative_prec);
    da = f / (1.0 + relative_prec);
  }
}

DoubleAc& DoubleAc::operator*=(DoubleAc f) {
  mfunnamep("DoubleAc& DoubleAc::operator*=(const DoubleAc& f)");
#ifdef DEBUG_OPERATOR_MULT_PRINT
  mcout.precision(17);
  mcout << "d=" << d << '\n';
  mcout << "di=" << di << '\n';
  mcout << "da=" << da << '\n';
  mcout << "f.d=" << f.d << '\n';
  mcout << "f.di=" << f.di << '\n';
  mcout << "f.da=" << f.da << '\n';
#endif
#ifndef VISUAL_STUDIO
  if (std::isnan(di) == 1) di = -DBL_MAX;
  if (std::isnan(da) == 1) da = DBL_MAX;
  if (std::isnan(f.di) == 1) f.di = -DBL_MAX;
  if (std::isnan(f.da) == 1) f.da = DBL_MAX;
#else
  if (_isnan(di) == 1) di = -DBL_MAX;
  if (_isnan(da) == 1) da = DBL_MAX;
  if (_isnan(f.di) == 1) f.di = -DBL_MAX;
  if (_isnan(f.da) == 1) f.da = DBL_MAX;
#endif
#ifdef POSSIBLE_FAILURE_ISNAN
  if (!(di < d) && !(di >= d)) di = -DBL_MAX;
  if (!(da < d) && !(da >= d)) da = DBL_MAX;
  if (!(f.di < f.d) && !(f.di >= f.d)) f.di = -DBL_MAX;
  if (!(f.da < f.d) && !(f.da >= f.d)) f.da = DBL_MAX;
#endif
#ifdef DEBUG_OPERATOR_MULT
  DoubleAc temp(*this);  // for debug
#endif
  d *= f.d;
  if (di >= 0) {
    if (f.di >= 0) {
      di *= f.di;
      da *= f.da;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 1\n";
#endif
    } else if (f.da >= 0)  // assumed that f.di < 0
    {
      di = f.di * da;
      da *= f.da;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 2\n";
#endif
    } else  // assumed that f.da < 0
    {
      double ti = da * f.di;
      da = di * f.da;
      di = ti;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 3\n";
#endif
    }
  } else if (da >= 0)  // assumed that di < 0
  {
    if (f.di >= 0) {
      di *= f.da;
      da *= f.da;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 4\n";
#endif
    } else if (f.da >= 0) {
      double ti = std::min(di * f.da, da * f.di);
      da = std::max(di * f.di, da * f.da);
      di = ti;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 5\n";
#endif
    } else {
      double ti = da * f.di;
      da = di * f.di;
      di = ti;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 6\n";
#endif
    }
  } else  // assumed that di < 0 and da < 0
  {
    if (f.di >= 0) {
      di *= f.da;
      da *= f.di;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 7\n";
#endif
    } else if (f.da >= 0) {
      double ti = di * f.da;
      da = di * f.di;
      di = ti;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 8\n";
#endif
    } else {
      double ti = da * f.da;
      da = di * f.di;
      di = ti;
#ifdef DEBUG_OPERATOR_MULT_PRINT
      mcout << "case 8\n";
#endif
    }
  }
// mcout<<"d="<<d<<'\n';
// mcout<<"di="<<di<<'\n';
// mcout<<"da="<<da<<'\n';
#ifdef CHECK_CORRECTNESS_AT_MULT
  if (d < di) {
    if (d - di == 0.0)  // strange but at Sc. Linux 4.0 with -O2 this happens
      goto mend;        // but this precation does not cure!
    funnw.ehdr(mcerr);
    mcerr << "d  <  di at the end of computations\n";
    mcerr << "This number:\n";
    print(mcerr, 6);
    mcerr << "Argument:\n";
    f.print(mcerr, 6);
    if (d > di) mcerr << "if(d > di) is also positive\n";
    if (d == di) mcerr << "if(d == di) is also positive\n";
    Iprintn(mcerr, d - di);
#ifdef DEBUG_OPERATOR_MULT
    // for debug:
    mcerr << "old value:\n";
    temp.print(mcerr, 6);
#endif
    spexit(mcerr);
  }
  if (d > da) {
    if (d == da)  // strange but at Sc. Linux 4.0 with O2 this happens
      goto mend;
    funnw.ehdr(mcerr);
    mcerr << "d  >  di at the end of computations\n";
    mcerr << "This number:\n";
    print(mcerr, 6);
    mcerr << "Argument:\n";
    f.print(mcerr, 6);
    if (d < da) mcerr << "if(d < da) is also positive\n";
    if (d == da) mcerr << "if(d == da) is also positive\n";
    Iprintn(mcerr, d - da);
#ifdef DEBUG_OPERATOR_MULT
    // for debug:
    mcerr << "old value:\n";
    temp.print(mcerr, 6);
#endif
    spexit(mcerr);
  }
mend:
#endif
  return *this;
}

DoubleAc& DoubleAc::operator/=(DoubleAc f) {
  mfunnamep("DoubleAc& DoubleAc::operator/=(const DoubleAc& f)");
  check_econd11(f.d, == 0, mcerr);
  check_econd11(f.d, == 0.0, mcerr);
#ifndef VISUAL_STUDIO
  if (std::isnan(di) == 1) di = -DBL_MAX;
  if (std::isnan(da) == 1) da = DBL_MAX;
  if (std::isnan(f.di) == 1) f.di = -DBL_MAX;
  if (std::isnan(f.da) == 1) f.da = DBL_MAX;
#else
  if (_isnan(di) == 1) di = -DBL_MAX;
  if (_isnan(da) == 1) da = DBL_MAX;
  if (_isnan(f.di) == 1) f.di = -DBL_MAX;
  if (_isnan(f.da) == 1) f.da = DBL_MAX;
#endif
#ifdef POSSIBLE_FAILURE_ISNAN
  if (!(di < d) && !(di >= d)) di = -DBL_MAX;
  if (!(da < d) && !(da >= d)) da = DBL_MAX;
  if (!(f.di < f.d) && !(f.di >= f.d)) f.di = -DBL_MAX;
  if (!(f.da < f.d) && !(f.da >= f.d)) f.da = DBL_MAX;
#endif

  d /= f.d;
  if (f.di < 0 && f.da > 0) {
    di = -DBL_MAX;
    da = DBL_MAX;
  } else if (di >= 0) {
    if (f.di > 0) {
      di /= f.da;
      da /= f.di;
    } else if (f.di == 0) {
      di /= f.da;
      da = DBL_MAX;
    } else {
      if (f.da == 0) {
        da = di / f.di;
        di = -DBL_MAX;
      } else {
        double ti = da / f.da;
        // mcout<<"d="<<d<<" ti="<<ti<<'\n';
        da = di / f.di;
        di = ti;
      }
    }
  } else if (da >= 0) {
    if (f.di > 0) {
      di /= f.di;
      da /= f.di;
    } else if (f.di == 0) {
      di = -DBL_MAX;
      da = DBL_MAX;
    } else {
      if (f.da == 0) {
        da = DBL_MAX;
        di = -DBL_MAX;
      } else {
        double ti = da / f.da;
        da = di / f.da;
        di = ti;
      }
    }
  } else {
    // assumed da < 0
    if (f.di > 0) {
      di /= f.di;
      da /= f.da;
    } else if (f.di == 0) {
      di = -DBL_MAX;
      // check_econd11(f.da ,  == 0 , mcerr); // otherwise f.d == 0 which
      // was already rejected
      if (f.da == 0) {
        funnw.ehdr(mcerr);
        mcerr << "f.da  == 0\n";
        mcerr << "This means that f.d == 0 which should been already "
              << "rejected.\n";
        mcerr << "If the program reaches this point, this means that\n"
              << "f.d is not between f.di and f.da, which is prohibited\n";
        mcerr << "f       :\n";
        f.print(mcerr, 6);
        spexit(mcerr);
      }
      da /= f.da;
    } else {
      if (f.da == 0) {
        di = da / f.di;
        da = DBL_MAX;
      } else {
        double ti = da / f.di;
        da = di / f.da;
        di = ti;
      }
    }
  }
#ifdef CHECK_CORRECTNESS_AT_MULT
  if (d < di) {
    funnw.ehdr(mcerr);
    mcerr << "d  <  di at the end of computations\n";
    mcerr << "This number:\n";
    print(mcerr, 6);
    mcerr << "Argument:\n";
    f.print(mcerr, 6);
    // for debug:
    // mcerr<<"old value:\n";
    // temp.print(mcerr, 6);
    spexit(mcerr);
  }
  if (d > da) {
    funnw.ehdr(mcerr);
    mcerr << "d  >  di at the end of computations\n";
    mcerr << "This number:\n";
    print(mcerr, 6);
    mcerr << "Argument:\n";
    f.print(mcerr, 6);
    // for debug:
    // mcerr<<"old value:\n";
    // temp.print(mcerr, 6);
    spexit(mcerr);
  }
#endif
  // check_econd12(d , < , di , mcerr);
  // check_econd12(d , > , da , mcerr);
  return *this;
}

DoubleAc sqrt(const DoubleAc& f) {
  if (f.get() < 0) {
    mcerr << "error in DoubleAc sqrt(const DoubleAc& f): f.get() < 0, f.get()="
          << f.get() << '\n';
    spexit(mcerr);
  }
  return DoubleAc(std::sqrt(double(f.get())),
                  std::sqrt(double(std::max(f.get_left_limit(), 0.0))),
                  std::sqrt(double(f.get_right_limit())));
}

DoubleAc square(const DoubleAc& f) {
  if (f.left_limit() >= 0.0) {
    return DoubleAc(f.get() * f.get(), f.left_limit() * f.left_limit(),
                    f.right_limit() * f.right_limit());
  } else if (f.right_limit() >= 0.0) {
    double t = std::max(-f.left_limit(), f.right_limit());
    return DoubleAc(f.get() * f.get(), 0.0, t * t);
  }
  return DoubleAc(f.get() * f.get(), f.right_limit() * f.right_limit(),
                  f.left_limit() * f.left_limit());
}

DoubleAc pow(const DoubleAc& f, double p) {
  if (p == 1) return f;
  if (p == 0) return DoubleAc(1.0);
  if (p > 0) {
    double d = std::pow(f.get(), p);
    double di = std::pow(f.left_limit(), p);
    double da = std::pow(f.right_limit(), p);
    if (f.left_limit() >= 0.0) {
      return DoubleAc(d, di, da);
    } else if (f.right_limit() >= 0.0) {
      if (di < 0.0)
        return DoubleAc(d, di, da);
      else  // the power is even
        return DoubleAc(d, 0.0, std::max(di, da));
    } else {
      if (di < 0.0)
        return DoubleAc(d, di, da);
      else  // the power is even
        return DoubleAc(d, da, di);
    }
  } else {
    double d = std::pow(f.get(), -p);
    double di = std::pow(f.left_limit(), -p);
    double da = std::pow(f.right_limit(), -p);
    if (f.left_limit() >= 0.0) {
      return 1.0 / DoubleAc(d, di, da);
    } else if (f.right_limit() >= 0.0) {
      if (di < 0.0)
        return 1.0 / DoubleAc(d, di, da);
      else  // the power is even
        return 1.0 / DoubleAc(d, 0.0, std::max(di, da));
    } else {
      if (di < 0.0)
        return 1.0 / DoubleAc(d, di, da);
      else  // the power is even
        return 1.0 / DoubleAc(d, da, di);
    }
  }
}

DoubleAc exp(const DoubleAc& f) {
  double d = std::exp(f.get());
  double di = std::exp(f.left_limit());
  double da = std::exp(f.right_limit());
  return DoubleAc(d, di, da);
}

DoubleAc sin(const DoubleAc& f) {
  // mcout<<"DoubleAc sin is starting, f="; f.print(mcout, 3);
  double d = std::sin(f.get());
  double di = std::sin(f.left_limit());
  double da = std::sin(f.right_limit());
  // Iprintn(mcout, d);
  // Iprintn(mcout, di);
  // Iprintn(mcout, da);
  long n = left_round(f.get() / M_PI + 0.5);
  long ni = left_round(f.left_limit() / M_PI + 0.5);
  long na = left_round(f.right_limit() / M_PI + 0.5);
  // Iprintn(mcout, n);
  // Iprintn(mcout, ni);
  // Iprintn(mcout, na);
  if (n % 2 == 0) {
    // Even number.
    if (ni < n) {
      di = -1.0;
      da = std::max(di, da);
      if (na > n) {
        da = 1.0;
      }
    } else if (na > n) {
      da = 1.0;
      di = std::min(di, da);
    }
  } else {
    // Odd number.
    double temp = di;
    di = da;
    da = temp;
    if (ni < n) {
      da = 1.0;
      di = std::min(di, da);
      if (na > n) {
        di = -1.0;
      }
    } else if (na > n) {
      di = -1.0;
      da = std::max(di, da);
    }
  }
  // Iprintn(mcout, d);
  // Iprintn(mcout, di);
  // Iprintn(mcout, da);
  return DoubleAc(d, di, da);
}

DoubleAc cos(const DoubleAc& f) {
  double d = std::cos(f.get());
  double di = std::cos(f.left_limit());
  double da = std::cos(f.right_limit());
  long n = left_round(f.get() / M_PI - 1.0);
  long ni = left_round(f.left_limit() / M_PI - 1.0);
  long na = left_round(f.right_limit() / M_PI - 1.0);
  if (n % 2 == 0) {
    // Even number.
    if (ni < n) {
      di = -1.0;
      da = std::max(di, da);
      if (na > n) {
        da = 1.0;
      }
    } else if (na > n) {
      da = 1.0;
      di = std::min(di, da);
    }
  } else {
    // Odd number.
    double temp = di;
    di = da;
    da = temp;
    if (ni < n) {
      da = 1.0;
      di = std::min(di, da);
      if (na > n) {
        di = -1.0;
      }
    } else if (na > n) {
      di = -1.0;
      da = std::max(di, da);
    }
  }
  return DoubleAc(d, di, da);
}

DoubleAc asin(const DoubleAc& f) {
  if (fabs(f.get()) > 1) {
    mcerr << "ERROR in inline DoubleAc asin(const DoubleAc& f):\n"
          << "fabs(f.get()) > 1: f.get()=" << f.get() << '\n';
    spexit(mcerr);
  }
  double d = std::asin(f.get());
  double di;
  if (f.left_limit() < -1.0)
    di = std::asin(-1.0);
  else
    di = std::asin(f.left_limit());
  double da;
  if (f.right_limit() > 1.0)
    da = std::asin(1.0);
  else
    da = std::asin(f.right_limit());
  return DoubleAc(d, di, da);
}

DoubleAc acos(const DoubleAc& f) {
  if (fabs(f.get()) > 1) {
    mcerr << "ERROR in inline DoubleAc acos(const DoubleAc& f):\n"
          << "fabs(f.get()) > 1: f.get()=" << f.get() << '\n';
    spexit(mcerr);
  }
  double d = std::acos(f.get());
  double da;
  if (f.left_limit() < -1.0)
    da = std::acos(-1.0);
  else
    da = std::acos(f.left_limit());
  double di;
  if (f.right_limit() > 1.0)
    di = std::acos(1.0);
  else
    di = std::acos(f.right_limit());
  return DoubleAc(d, di, da);
}

void DoubleAc::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  if (l == 1) {
    file << d;
  } else if (l == 2) {
    file << d << " [ " << di << " , " << da << " ] ";
  } else if (l == 3) {
    file << d;
    int t = file.precision(2);
    file << " [" << std::setw(8) << d - di << "," << std::setw(8) << da - d
         << "] ";
    file.precision(t);
  } else if (l == 4) {
    file << d << " [ " << di << " , " << da << " ] \n";
  } else if (l == 5) {
    file << d;
    int t = file.precision(2);
    file << " [" << std::setw(8) << d - di << "," << std::setw(8) << da - d
         << "] \n";
    file.precision(t);
  } else {
    int t = file.precision(16);
    file << "DoubleAc: d=" << std::setw(20) << d << " di=" << std::setw(20)
         << di << " da=" << std::setw(20) << da << '\n';
    file.precision(t);
  }
}

DoubleAc pow(const DoubleAc& /*f*/, const DoubleAc& /*p*/) {
  mcerr << "ERROR in inline DoubleAc pow(const DoubleAc& f, const DoubleAc& "
           "p):\n";
  mcerr << "not implemented yet\n";
  spexit(mcerr);
  return 0.0;
}

std::ostream& operator<<(std::ostream& file, const DoubleAc& f) {
  f.print(file, 1);
  return file;
}

}
