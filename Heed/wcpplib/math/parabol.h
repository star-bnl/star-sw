#ifndef PARABOL_H
#define PARABOL_H

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

/// Solution of quadratic equation.
/// The main class is colled with a cut of the last character: Parabol
/// to assure absence of coincidences with any other libraries and user files.
/// This was inpired by a significant problem with link. The programs
/// containig this class often don't want to be linked due to missing of
/// references to functions from this class, if the file compiled from
/// parabol.c is just included in library. The class parabol does not
/// contain anything special, but this often happens. Why - unknown.
/// The mentioned change of name did not solve this problem.
/// The only solution is the inclusion of the object file parabol.o in the list
/// of command supplied to linker.
/// In general, this is very simple class, see definition.

class Parabol {
 public:
  double a() const { return da; }
  double b() const { return db; }
  double c() const { return dc; }
  void put_a(const double fa) {
    da = fa;
    s_det = 0;
    s_dxzero = 0;
  }
  void put_b(const double fb) {
    db = fb;
    s_det = 0;
    s_dxzero = 0;
  }
  void put_c(const double fc) {
    dc = fc;
    s_det = 0;
    s_dxzero = 0;
  }

  /// Default constructor.
  Parabol(void) : da(0.0), db(0.0), dc(0.0), s_det(0), s_dxzero(0) {}
  Parabol(const Parabol& f);
  Parabol(double fa, double fb, double fc)
      : da(fa), db(fb), dc(fc), s_det(0), s_dxzero(0) {}
  /// Constructor from three points.
  Parabol(double x[3], double y[3]);
  Parabol(double x[3], double y[3], int);
  // creates parabola by 3 points, in one of each ,
  // in the third one, the  derivative of the function is supplied instead of
  // the function. int is any, to differ from previous constructor
  /// Constructor from 3 points
  Parabol(double x1, double x2, double x3, double y1, double y2, double y3);

  /// Evaluate the function.
  double eval(const double x) const { return da * x * x + db * x + dc; }

  // Returns number of solutions. First is the least.
  int find_zero(double xzero[2]) const;
  double find_maxmin();

  double determinant() const {
    const Parabol& t = (*this);
    if (s_det == 0) {
      t.s_det = 1;
      t.det = db * db - 4 * da * dc;
    }
    return det;
  }

 private:
  double da, db, dc;
  mutable int s_det;
  mutable double det;
  mutable int s_dxzero;
  mutable int qdxzero;
  mutable double dxzero[2];
};

std::ostream& operator<<(std::ostream& file, const Parabol& f);
}

#endif
