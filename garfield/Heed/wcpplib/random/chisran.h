#ifndef CHISRAN_H
#define CHISRAN_H
#include <vector>

// Remake of CERN's subroutine hisran for generation of random numbers
// according to histogram
// I. B. Smirnov, 2003.

// This is old programs.
// The new ones are in wcpplib/math/tline.h
// They are more generic, although the old ones should be working as well.

namespace Heed {

float chispre(float *x, float *p, float *f, long q);
// here x is a left side of interval on which
// function p is a constant.
// last point x[q] is the end of last interval.
// p and f must have dimensions q.
// x must have dimension q + 1
// f is return array.
// f[n] is normalized integral till the end of n'th interval or till
// x[n+1].
// Normalization is such that f[q-1]=1.0;
// Return value is the true integral.

float chisran(float flat_random_number, float *x, float *f, long q);

double chispre(std::vector<double> &f, int s_allow_zero_f = 0);
// here intervals are unit
// function p is a constant along these intervals.
// last point x[q] is the end of last interval.
// p and f must have dimensions q.
// f is return array.
// f[n] is normalized integral till the end of n'th interval or till
// x[n+1].
// Normalization is such that f[q-1]=1.0;
// Return value is the true integral.
// Sun of f should be always more than zero.
// if s_allow_zero_f = 1, some values of f are allowed to be
// negative and they are made zero with diagnostic printed.
// (this have sence only for debug.)

double chisran(double flat_random_number, const std::vector<double> &f);
}

#endif
