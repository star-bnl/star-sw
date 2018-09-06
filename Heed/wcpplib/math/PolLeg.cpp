#include "wcpplib/math/PolLeg.h"
#include "wcpplib/util/FunNameStack.h"

/*
Copyright (c) 2003 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

namespace Heed {

double polleg(const int l, const double x) {
  mfunname("double polleg(int l, double x)");
  check_econd11(l, < 0, mcerr);
  check_econd11a(l, > 6, "not implemented", mcerr);
  switch (l) {
    case 0:
      return 1.0;
    case 1:
      return x;
    case 2:
      return 0.5 * (3.0 * x * x - 1.0);
    case 3:
      return 0.5 * (5.0 * x * x * x - 3.0 * x);
    case 4: {
      const double x2 = x * x;
      return 1.0 / 8.0 * (35.0 * x2 * x2 - 30.0 * x2 + 3.0);
    }
    case 5: {
      const double x2 = x * x;
      const double x3 = x2 * x;
      return 1.0 / 8.0 * (63.0 * x3 * x2 - 70.0 * x3 + 15.0 * x);
    }
    case 6: {
      const double x2 = x * x;
      const double x4 = x2 * x2;
      return 1.0 / 16.0 * (231.0 * x4 * x2 - 315.0 * x4 + 105.0 * x2 - 5.0);
    }
    default:
      return 0.0;
  }
  return 0.0;  // should never happen
}
}
