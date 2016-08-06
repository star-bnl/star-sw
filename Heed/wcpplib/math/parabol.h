#ifndef PARABOL_H
#define PARABOL_H
/*
Manipulating with parabola and solution of square equation.
The main class is colled with a cut of the last character: Parabol
to assure absence of coincidences with any other libraries and user files.
THis was inpired by a significant problem with link. The programs
contating this class oftenly don't want to be linked due to missing of
references to functions from this class, if the file compiled from
parabol.c is just included in library. The class parabol does not
contain anything special, but this often happens. Why - unknown.
The mentioned change of name did nto solve this problem.
The only solution is the inclusion of the object file parabol.o in the list
of command supplied to linker.
In general, this is very simple class, see definition.


Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"

class Parabol  // a is omited to avoid coincidences with any other libraries
    {
 public:
  inline double a(void) const { return da; }
  inline double b(void) const { return db; }
  inline double c(void) const { return dc; }
  inline void put_a(double fa) {
    da = fa;
    s_det = 0;
    s_dxzero = 0;
  }
  inline void put_b(double fb) {
    db = fb;
    s_det = 0;
    s_dxzero = 0;
  }
  inline void put_c(double fc) {
    dc = fc;
    s_det = 0;
    s_dxzero = 0;
  }

  Parabol(void) : da(0.0), db(0.0), dc(0.0), s_det(0), s_dxzero(0) {}
  Parabol(const Parabol& f);
  Parabol(double fa, double fb, double fc)
      : da(fa), db(fb), dc(fc), s_det(0), s_dxzero(0) {}
  Parabol(double x[3], double y[3]);  // creates parabola by 3 points
  Parabol(double x[3], double y[3], int);
  // creates parabola by 3 points, in one of each ,
  // in the third one, the  derivative of the function is supplied instead of
  // the function. int is any, to differ from previous constructor
  Parabol(double x1, double x2, double x3, double y1, double y2,
          double y3);  // creates parabola by 3 points

  inline double y(double x) { return da * x * x + db * x + dc; }

  int find_zero(double xzero[2]) const;  // returns number of solutions
                                         // first is the least.
  double find_maxmin(void);

  inline double determinant(void) const {
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

#endif
