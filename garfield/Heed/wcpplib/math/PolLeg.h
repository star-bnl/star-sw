#ifndef POLLEG_H
#define POLLEG_H

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

/// Simple function for Legendre polynomials.
/// Implemented only l = 0, 1, 2, 3, 4, 5, 6.
double polleg(const int l, const double x);
}

#endif
