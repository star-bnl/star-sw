#ifndef MINMAX_H
#define MINMAX_H
/*
Copyright (c) 2001 Igor Borisovich Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
This software is distributed in the hope that it will be useful,
but the author makes no representations about the suitability
of this software for any particular purpose.
It is provided "as is" without express or implied warranty.
*/

#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <cmath>
#include <cfloat>
#include "wcpplib/util/FunNameStack.h"

namespace Heed {

/// Minimal power of e the result of which can be represented.
static const double dbl_min_e_exp = (DBL_MIN_EXP - 1) * log(double(FLT_RADIX));
/// Maximal power of e the result of which can be represented.
/// Similar numbers for a != e can be obtained in the user's program by
/// double dbl_max_a_exp = dbl_max_e_exp / log(a);
static const double dbl_max_e_exp = (DBL_MAX_EXP - 1) * log(double(FLT_RADIX));
/// Largest number the square of which can be represented.
static const double dbl_max_square = sqrt(DBL_MAX);  

static const double flt_min_e_exp = (FLT_MIN_EXP - 1) * log(double(FLT_RADIX));
static const double flt_max_e_exp = (FLT_MAX_EXP - 1) * log(double(FLT_RADIX));
/// Largest number the square of which can be represented.
static const double flt_max_square = sqrt(FLT_MAX);  

inline long left_round(double f) {
  return f >= 0 ? long(f) : -long(-f) - 1;
}

template <class T>
inline T tabs(const T& x) {
  return x >= 0 ? x : -x;
}

template <class T>
int apeq_mant(const T& x1, const T& x2, T prec) {
  if (x1 == x2) return 1;
  if (prec == 0) return 0;
  if (x1 == 0 && x2 == 0) return 1;
  if ((x1 < 0 && x2 > 0) || (x1 > 0 && x2 < 0)) return 0;
  if (tabs((x1 - x2) / (x1 + x2)) <= prec) return 1;
  return 0;
}

}

#endif
