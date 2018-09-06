#ifndef POINTSRAN_H
#define POINTSRAN_H

#include <vector>

/*
Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

namespace Heed {

/// Generates random numbers according to array.
/// This class generates random numbers
/// according to a pointwise distribution, with linear interpolation
/// between given points and also with linear extrapolation.

class PointsRan {
 private:
  double xmin;            // is changed internally
  double xmax;            // is changed internally

  // x[0] is changed to xmin or that of zero interpolated y,
  // x[q-1] to xmax or similarly zero y.
  // y[0] and y[q-1] are recalculated correspondingly
  std::vector<double> x;  
  std::vector<double> y;  

  /// Integrated minus initial level at xmin
  std::vector<double> iy;
  std::vector<double> a;
  double integ_total;
  double integ_active;
  double integ_start;
  int n_start;
  double integ_finish;
  int n_finish;

 public:
  PointsRan() {}

  /** Constructor
    * \param fx
             x values
    * \param fy
             y values
    * \param fxmin
             Minimum of generated distribution. If less then x[0], 
             extend the distribution by linear extrapolation.
             If the extrapolated line crosses zero, the extension stops there.
             Otherwise it stops at fxmin.
    * \param fxmax
             Maximum of generated distribution. If greater than x[q-1], 
             extend the distribution by linear extrapolation.
    **/
  PointsRan(const std::vector<double>& fx, const std::vector<double>& fy,
            double fxmin, double fxmax);  

  double ran(double flat_ran) const;
  double get_integ_total() const { return integ_total; }
  // actual integral of active distribution
  double get_integ_active() const { return integ_active; }
  void print(std::ostream& file) const;
};
}

#endif
