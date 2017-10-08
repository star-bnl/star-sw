#ifndef INVERSE_H
#define INVERSE_H
/*
Various matrix inversions performed upon arrays of DynLinArr and
DynArr classes.


Copyright (c) 2001 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/math/DoubleAc.h"

namespace Heed {

/// Matrix inversion
/// szero == 1 indicates that the calculations are terminated 
/// owing to an attempt to divide by 0. The final matrix is not correct.
/// serr == 1 indicates that the interval precision is broken
/// (but the final matrix may be provided if szero=0).
/// s_stop is a directive to stop if the interval precision is broken.
void inverse_DynArr_prot(const DynArr<DoubleAc>& mi, DynArr<DoubleAc>& mr,
                         int& szero, int& serr, int s_stop = 1);

void inverse_DynArr(const DynArr<double>& mi, DynArr<double>& mr, int& serr);

/// Call inverse_DynArr_prot two times, first with inbuilt precision
/// and the second time with the given precision.
/// serr1 means the inversion cannot be done in a numerically precise way.
/// In this case mr2 and serr2 are not initialised.
/// If serr1 == 0, mr2 and serr2 are always calculated. 
/// If szero == 1, serr1 == 1 too.
void inverse_DynArr(const DynArr<DoubleAc>& mi, DynArr<DoubleAc>& mr1,
                    int& szero, int& serr1, DynArr<DoubleAc>& mr2, int& serr2);

void inverse_DynArr(const DynArr<double>& mi,
                    const DynLinArr<int>& s_var,  // 1 if variable
                    DynArr<double>& mr, int& serr);
void inverse_DynArr_prot(const DynArr<DoubleAc>& mi,
                         const DynLinArr<int>& s_var,  // 1 if variable
                         DynArr<DoubleAc>& mr, int& szero, int& serr,
                         int s_stop = 1);

/// Pack the matrix, call the same function declared without s_var
/// and unpack the result.
void inverse_DynArr(const DynArr<DoubleAc>& mi,
                    const DynLinArr<int>& s_var,  // 1 if variable
                    DynArr<DoubleAc>& mr1, int& szero, int& serr1,
                    DynArr<DoubleAc>& mr2, int& serr2);

DoubleAc determinant_DynArr(const DynArr<DoubleAc>& mi,
                            long q = 0);  // default means total matrix

DoubleAc determinant_DynArr(const DynArr<DoubleAc>& mi,
                            const DynLinArr<int>& s_var,  // 1 if variable
                            long q = 0);  // default means total matrix
                                          // counts active rows and columns
}
#endif
