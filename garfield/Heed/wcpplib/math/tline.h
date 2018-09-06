#ifndef TLINE_H
#define TLINE_H

/*
Copyright (c) 2005 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include <vector>
#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/math/minmax.h"

//#define TLINE_REDUCE_TO_RAW_ARR  // useful for acceleration of PointCoorMesh
// if the type D keeps elements in consecutive array
// whose address can be obtained as address of the first element.
// In PointCoorMesh this all is switched by the following way:
//#ifndef TLINE_REDUCE_TO_RAW_ARR
//  D* amesh;
//#else
//  T* amesh;
//#endif
// In constructors the assignment is switched by the following way:
//#ifndef TLINE_REDUCE_TO_RAW_ARR
//  amesh = famesh;
//  xmin = (*amesh)[0];
//  xmax = (*amesh)[q-1];
//#else
//  amesh = &((*famesh)[0]);
//  xmin = amesh[0];
//  xmax = amesh[q-1];
//#endif
// Note that in both cases only the address is kept in this class and the
// corresponding object is not copied.
// If Copying is necessary, one can use CopiedPointCoorMesh.
// Note that class CopiedPointCoorMesh, based on DynLinArr,
// also provides acceleration based on the use of raw array
// address and switched on by TLINE_REDUCE_TO_RAW_ARR.
// Note that this class does not use .acu() functions of DynLinArr,
// allowing for the use of unchecked fast access
// (since these functions were written after tline was done).

//#define CHECK_POINT_MESH  //verifies that the points are in increasing order.
// It is indeed long and may be inacceptable for some applications.

namespace Heed {

/// Mesh with equal steps.
/// Determined by the number of "bins", minimum and maximum.
/// The object of this class keeps all ingredients in it.
/// It can be therefore copied and deleted freely.
/// T is the type of returned value.
/// T cannot be const.
/// At construction q has meaning of number of intervals.

template <class T>
class EqualStepCoorMesh {
 public:
  /// Get number of intervals.
  inline long get_qi(void) const { return q; }

  inline T get_xmin(void) const { return xmin; }
  inline T get_xmax(void) const { return xmax; }

  /// Get single coordinate of the point in the mesh.
  /// It can be last point of the last interval.
  inline void get_scoor(long n, T& b) const { b = xmin + n * step; }
  /// Get interval. Return 1 if interval is found.
  inline int get_interval(long n, T& b1, T& b2) const {
    if (n < 0 || n >= q) return 0;
    b1 = xmin + n * step;
    b2 = b1 + step;
    return 1;
  }

  /** Get interval.
    * \param x coordinate
    * \param n1 bin number
    */
  virtual int get_interval(T x, long& n1) const;

  // The same as above, but returns more information:
  // n1, b1: the bin number and left border,
  // n2, b2: the next bin number and its left
  virtual int get_interval(T x, long& n1, T& b1, long& n2, T& b2) const;

  virtual int get_interval_extrap(T x, long& n1, T& b1, long& n2, T& b2) const;
  // returns 1 if x inside xmin and xmax and therefore b1 and b2,
  // returns 0 if x < xmin
  //   In this case n1 = 0, n2 = 1, b1 and b2 are corresponding.
  // returns 2 if x > xmax
  //   In this case n1 = q-1, n2 = q, b1 and b2 are corresponding.

  inline int get_step(long n, T& fstep) const {
    if (n < 0 || n >= q) return 0;
    fstep = step;
    return 1;
  }

  EqualStepCoorMesh<T>() : q(0), xmin(0), xmax(0), step(0) {}
  EqualStepCoorMesh<T>(long fq, T fxmin, T fxmax);
  void print(std::ostream& file) const;

 private:
  // Number of steps or intervals.
  /// Attention: if you count the number of points with the
  /// last point of the last step there will be q+1 points.
  long q;  
  T xmin;
  T xmax;
  T step;
};

template <class T>
EqualStepCoorMesh<T>::EqualStepCoorMesh(long fq, T fxmin, T fxmax)
    : q(fq), xmin(fxmin), xmax(fxmax) {
  mfunname(
      "template<class T> EqualStepCoorMesh<T>::EqualStepCoorMesh<T>(long "
      "fq, T fxmin, T fxmax)");
  check_econd11(q, < 0, mcerr);
  check_econd24(q, ==, 0, &&, xmin, <, xmax, mcerr);
  check_econd12(xmin, >, xmax, mcerr);
  step = (fxmax - fxmin) / q;
  check_econd11(step, == 0, mcerr);
}

template <class T>
int EqualStepCoorMesh<T>::get_interval(T x, long& n1) const {
  if (x < xmin || x >= xmax) {
    n1 = 0;
    return 0;
  }
  n1 = long((x - xmin) / step);
  if (n1 < 0) {
    mcerr << "ERROR in EqualStepCoorMesh<T>::get_interval:\n"
          << "n1 < 0 \n";
    print(mcerr);
    Iprintn(mcerr, x);
    Iprintn(mcerr, n1);
    spexit(mcerr);
  }
  return 1;
}

template <class T>
int EqualStepCoorMesh<T>::get_interval(T x, long& n1, T& b1, long& n2,
                                       T& b2) const {
  if (x < xmin || x >= xmax) {
    n1 = 0;
    n2 = 0;
    b1 = 0;
    b2 = 0;
    return 0;
  }
  n1 = long((x - xmin) / step);
  n2 = n1 + 1;
  b1 = xmin + step * n1;
  b2 = b1 + step;
  if (n1 < 0 || n2 > q || b2 > xmax) {
    mcerr << "ERROR in EqualStepCoorMesh<T>::get_interval:\n"
          << "n1 < 0 || n2 > q || b2 > xmax\n";
    print(mcerr);
    Iprintn(mcerr, x);
    Iprint4n(mcerr, n1, n2, b1, b2);
    spexit(mcerr);
  }
  return 1;
}

template <class T>
int EqualStepCoorMesh<T>::get_interval_extrap(T x, long& n1, T& b1, long& n2,
                                              T& b2) const {
  int i_ret = 1;

  if (x < xmin) {
    i_ret = 0;
    n1 = 0;
    n2 = 1;
    b1 = xmin;
    b2 = xmin + step;
  } else if (x >= xmax) {
    i_ret = 2;
    n1 = q - 1;
    n2 = q;
    b1 = xmax - step;
    b2 = xmax;
  } else {
    n1 = long((x - xmin) / step);
    n2 = n1 + 1;
    if (n2 == q) {
      b2 = xmax;
      b1 = b2 - step;
    } else {
      b1 = xmin + step * n1;
      b2 = b1 + step;
      if (n1 < 0 || n2 > q || b2 > xmax) {
        mcerr << "ERROR in EqualStepCoorMesh<T>::get_interval_extrap:\n"
              << "n1 < 0 || n2 > q || b2 > xmax\n";
        print(mcerr);
        Iprint4n(mcerr, n1, n2, b1, b2);
        spexit(mcerr);
      }
    }
  }
  return i_ret;
}

template <class T>
void EqualStepCoorMesh<T>::print(std::ostream& file) const {
  Ifile << "EqualStepCoorMesh<T>:\n";
  indn.n += 2;
  Ifile << "Type of T is (in internal notations) " << typeid(T).name() << '\n';
  Iprint4n(file, q, xmin, xmax, step);
  indn.n -= 2;
}

template <class T>
std::ostream& operator<<(std::ostream& file, const EqualStepCoorMesh<T>& f) {
  f.print(file);
  return file;
}

template <class T>
std::istream& operator>>(std::istream& file, EqualStepCoorMesh<T>& f) {
  mfunname("istream& operator>>(istream& file, EqualStepCoorMesh<T>& f)");
  definp_endpar dep(&file, 0, 1, 0);
  set_position("Type of T is (in internal notations)", *dep.istrm, dep.s_rewind,
               dep.s_req_sep);
  long q;
  T xmin;
  T xmax;
  DEFINPAP(q);
  DEFINPAP(xmin);
  DEFINPAP(xmax);
  f = EqualStepCoorMesh<T>(q, xmin, xmax);
  return file;
}

template <class T>
int operator==(const EqualStepCoorMesh<T>& f1, const EqualStepCoorMesh<T>& f2) {
  if (f1.get_qi() != f2.get_qi() || f1.get_xmin() != f2.get_xmin() ||
      f1.get_xmax() != f2.get_xmax())
    return 0;
  else
    return 1;
}

template <class T>
int apeq_mant(const EqualStepCoorMesh<T>& f1, const EqualStepCoorMesh<T>& f2,
              T prec) {
  if (f1.get_qi() != f2.get_qi() ||
      !apeq_mant(f1.get_xmin(), f2.get_xmin(), prec) ||
      !apeq_mant(f1.get_xmax(), f2.get_xmax(), prec)) {
    Iprintn(mcout, !apeq_mant(f1.get_xmin(), f2.get_xmin(), prec));
    Iprintn(mcout, !apeq_mant(f1.get_xmax(), f2.get_xmax(), prec));
    return 0;
  } else
    return 1;
}

template <class T>
int operator!=(const EqualStepCoorMesh<T>& f1, const EqualStepCoorMesh<T>& f2) {
  if (f1.get_qi() != f2.get_qi() || f1.get_xmin() != f2.get_xmin() ||
      f1.get_xmax() != f2.get_xmax())
    return 1;
  else
    return 0;
}

template <class T, class D>
// D is anything allowing indexing
long t_find_interval(double x, long q, const D& coor) {
  long n1, n2, n3;
#ifndef TLINE_REDUCE_TO_RAW_ARR
  if (q <= 1) return -1;
  if (x < coor[0] || x > coor[q - 1]) return -1;
  if (x < coor[1]) return 0;
  if (x >= coor[q - 2]) return q - 2;
  n1 = 0;
  n2 = q - 1;
  while (n2 - n1 > 1) {
    n3 = n1 + (n2 - n1) / 2;
    if (x < coor[n3])
      n2 = n3;
    else
      n1 = n3;
  }
  return n1;
#else
  T* arr = &(coor[0]);  // take the address of the first element
  if (q <= 1) return -1;
  if (x < arr[0] || x > arr[q - 1]) return -1;
  if (x < arr[1]) return 0;
  if (x >= arr[q - 2]) return q - 2;
  n1 = 0;
  n2 = q - 1;
  while (n2 - n1 > 1) {
    n3 = n1 + (n2 - n1) / 2;
    if (x < arr[n3])
      n2 = n3;
    else
      n1 = n3;
  }
  return n1;

#endif
}

// Use (scan) only end of the array starting from index n_start
// as if it is 0
// The return index:
// -1 if less than corr[n_start] or more than coor[q-1]
// Index if inside

template <class T, class D>
long t_find_interval_end(double x, long q, const D& coor, long n_start) {
  long n1, n2, n3;
  if (n_start < 0 || n_start > q - 1) {
    mcerr << " ERROR in t_find_interval_end(...):\n";
    mcerr << "n_start < 0 || n_start > q-1\n";
    Iprint2n(mcout, n_start, q);
    spexit(mcerr);
  }
#ifndef TLINE_REDUCE_TO_RAW_ARR
  // if(q <= 1) return -1;
  if (q - n_start <= 1) return -1;
  if (x < coor[n_start] || x > coor[q - 1]) return -1;
  if (x < coor[n_start + 1]) return n_start;
  if (x >= coor[q - 2]) return q - 2;
  n1 = n_start;
  n2 = q - 1;
  while (n2 - n1 > 1) {
    n3 = n1 + (n2 - n1) / 2;
    if (x < coor[n3])
      n2 = n3;
    else
      n1 = n3;
  }
  return n1;
#else
  T* arr = &(coor[0]);  // take the address of the first element
  // if(q <= 1) return -1;
  if (q - n_start <= 1) return -1;
  if (x < arr[n_start] || x > arr[q - 1]) return -1;
  if (x < arr[n_start + 1]) return n_start;
  if (x >= arr[q - 2]) return q - 2;
  n1 = n_start;
  n2 = q - 1;
  while (n2 - n1 > 1) {
    n3 = n1 + (n2 - n1) / 2;
    if (x < arr[n3])
      n2 = n3;
    else
      n1 = n3;
  }
  return n1;

#endif
}

/// Generic mesh with arbitrary steps.
/// The array determining the step edges is located somewhere outside.
/// In object of this class only the raw pointer is contained with consequences:

/*
Attention, here there is a raw pointer to mesh.
It is not possible to use the passive pointer since
if D is plain array, the reference cannot be registered.
This is deviation from methodology, but it is not clear what can
be done here. The mesh cannot be copyed or deleted after the PointCoorMesh
is initialized. Obviously, the latter should be initialized immidiately
before the use and not keept permanentely.
Obviously again, that sooner or later the user can forget about this
(the author also once almost forgot and was at the edge of error)
and try to initialize it permanantely and then copy together with the mesh.
This would be an error, which again confirms the correctness of the
object management methodology, which forbids to place the raw pointers
in classes. This class (below) may be understood as a temporary exclusion,
which should be treated with great care.
At construction q has meaning of number of points.
 */

template <class T, class D>
class PointCoorMesh {
 public:
  inline long get_qi(void) const { return q - 1; }
  inline T get_xmin(void) const { return xmin; }
  inline T get_xmax(void) const { return xmax; }
  inline void get_scoor(long n, T& b) const {
#ifndef TLINE_REDUCE_TO_RAW_ARR
    b = (*amesh)[n];
#else
    b = amesh[n];
#endif
  }
  virtual int get_interval(long n, T& b1, T& b2) const {
    if (n < 0 || n >= q - 1) return 0;
#ifndef TLINE_REDUCE_TO_RAW_ARR
    b1 = (*amesh)[n];
    b2 = (*amesh)[n + 1];
#else
    b1 = amesh[n];
    b2 = amesh[n + 1];
#endif
    return 1;
  }

  int get_interval(T x, long& n1) const;

  int get_interval(T x, long& n1, T& b1, long& n2, T& b2) const;

  int get_interval_extrap(T x, long& n1, T& b1, long& n2, T& b2) const;

  inline int get_step(long n, T& fstep) const {
    if (n < 0 || n >= q - 1) return 0;
    T b1, b2;
    get_interval(n, b1, b2);
    fstep = b2 - b1;
    return 1;
  }

  PointCoorMesh<T, D>(void)
      : q(0), xmin(0), xmax(0), x_old(0), n_old(-1), amesh(NULL) {
    ;
  }
  PointCoorMesh<T, D>(long fq,  // number of points, number of intervals
                      // is fq - 1.
                      D* famesh);  // dimension is fq and the last index is fq-1
                                   // This is the end point of the last interval
  virtual ~PointCoorMesh<T, D>() {}
  void check(void);  // check that the points are sequencial.
                     // This is also done in constructor above provided that
                     // macro CHECK_POINT_MESH is initialized.

  virtual void print(std::ostream& file) const;

 private:
  long q;  // the number of points
           // The number of intervals is q-1.
           // Therefore q has to be 2 or more
#ifndef TLINE_REDUCE_TO_RAW_ARR
  D* amesh;
#else
  T* amesh;
#endif
  T xmin;
  T xmax;
  // auxiliary thing to accelerate finding intervals
  mutable T x_old;     // previous x for finding interval
  mutable long n_old;  // -1 if there is nothing
};

template <class T, class D>
PointCoorMesh<T, D>::PointCoorMesh(long fq, D* famesh)
    : q(fq), x_old(0), n_old(-1) {
  if (q <= 1) {
    mcerr << "ERROR in PointCoorMesh<T,D>::PointCoorMesh<T,D>:\n"
          << "q <= 1\n";
    Iprintn(mcerr, q);
    spexit(mcerr);
  }
#ifndef TLINE_REDUCE_TO_RAW_ARR
  // amesh.put( famesh );
  amesh = famesh;
  xmin = (*amesh)[0];
  xmax = (*amesh)[q - 1];
#else
  amesh = &((*famesh)[0]);
  xmin = amesh[0];
  xmax = amesh[q - 1];
#endif
  // check consistence
  if (xmin > xmax) {
    mcerr << "ERROR in PointCoorMesh<T,D>::PointCoorMesh<T,D>:\n"
          << "xmin > xmax\n";
    Iprint2n(mcerr, xmin, xmax);
    spexit(mcerr);
  }
#ifdef CHECK_POINT_MESH
  long n;
  for (n = 0; n < q - 1; n++) {
#ifndef TLINE_REDUCE_TO_RAW_ARR
    if ((*amesh)[n] >= (*amesh)[n + 1])
#else
    if (amesh[n] >= amesh[n + 1])
#endif
    {
      mcerr << "ERROR in PointCoorMesh<T,D>::PointCoorMesh<T,D>:\n"
            << "amesh[n] >= amesh[n+1]\n";
#ifndef TLINE_REDUCE_TO_RAW_ARR
      Iprint3n(mcerr, n, (*amesh)[n], (*amesh)[n + 1]);
#else
      Iprint3n(mcerr, n, amesh[n], amesh[n + 1]);
#endif
      spexit(mcerr);
    }
  }
#endif
}

template <class T, class D>
void PointCoorMesh<T, D>::check(void) {
  long n;
  for (n = 0; n < q - 1; n++) {
#ifndef TLINE_REDUCE_TO_RAW_ARR
    if ((*amesh)[n] >= (*amesh)[n + 1])
#else
    if (amesh[n] >= amesh[n + 1])
#endif
    {
      mcerr << "ERROR in PointCoorMesh<T,D>::check(void):\n"
            << "amesh[n] >= amesh[n+1]\n";
#ifndef TLINE_REDUCE_TO_RAW_ARR
      Iprint3n(mcerr, n, (*amesh)[n], (*amesh)[n + 1]);
#else
      Iprint3n(mcerr, n, amesh[n], amesh[n + 1]);
#endif
      spexit(mcerr);
    }
  }
}

template <class T, class D>
int PointCoorMesh<T, D>::get_interval(T x, long& n1) const {
  if (x < xmin || x >= xmax) {
    n1 = 0;
    return 0;
  }
#ifndef TLINE_REDUCE_TO_RAW_ARR
  if (n_old >= 0 && x_old <= x) {
    n1 = t_find_interval_end<T, D>(x, q, *amesh, n_old);
  } else {
    n1 = t_find_interval<T, D>(x, q, *amesh);
  }
// n1 = t_find_interval< T , D >(x, q, amesh);
#else
  if (n_old >= 0 && x_old <= x) {
    n1 = t_find_interval_end<T, T*>(x, q, amesh, n_old);
  } else {
    n1 = t_find_interval<T, T*>(x, q, amesh);
  }
// n1 = t_find_interval< T , T* >(x, q, &amesh);
#endif

  if (n1 < 0) {
    mcerr << "ERROR in PointCoorMesh<T,D>::get_interval:\n"
          << "n1 < 0\n";
    print(mcerr);
    Iprintn(mcerr, n1);
    spexit(mcerr);
  }
  n_old = n1;
  x_old = x;
  return 1;
}

template <class T, class D>
int PointCoorMesh<T, D>::get_interval(T x, long& n1, T& b1, long& n2,
                                      T& b2) const {
  if (x < xmin || x >= xmax) {
    n1 = 0;
    n2 = 0;
    b1 = 0;
    b2 = 0;
    return 0;
  }
#ifndef TLINE_REDUCE_TO_RAW_ARR
  if (n_old >= 0 && x_old <= x) {
    n1 = t_find_interval_end<T, D>(x, q, *amesh, n_old);
  } else {
    n1 = t_find_interval<T, D>(x, q, *amesh);
  }
// n1 = t_find_interval< T , D >(x, q, amesh);
#else
  if (n_old >= 0 && x_old <= x) {
    n1 = t_find_interval_end<T, T*>(x, q, amesh, n_old);
  } else {
    n1 = t_find_interval<T, T*>(x, q, amesh);
  }
// n1 = t_find_interval< T , T* >(x, q, &amesh);
#endif
  n2 = n1 + 1;
#ifndef TLINE_REDUCE_TO_RAW_ARR
  b1 = (*amesh)[n1];
  b2 = (*amesh)[n2];
#else
  b1 = amesh[n1];
  b2 = amesh[n2];
#endif
  if (n1 < 0 || n1 >= q || n2 < 0 || n2 >= q || b1 < xmin || b1 > xmax ||
      b2 < xmin || b2 > xmax) {
    mcerr << "ERROR in PointCoorMesh<T,D>::get_interval:\n"
          << "n1 < 0 || n1 >= q || n2 < 0 || n2 >= q || b1 < xmin || b1 > xmax "
             "|| b2 < xmin || b2 > xmax\n";
    print(mcerr);
    Iprint4n(mcerr, n1, n2, b1, b2);
    spexit(mcerr);
  }
  n_old = n1;
  x_old = x;
  return 1;
}

template <class T, class D>
int PointCoorMesh<T, D>::get_interval_extrap(T x, long& n1, T& b1, long& n2,
                                             T& b2) const {
  int i_ret = 1;

  if (x < xmin) {
    i_ret = 0;
    n1 = 0;
    n2 = 1;
    b1 = xmin;
#ifndef TLINE_REDUCE_TO_RAW_ARR
    b2 = (*amesh)[1];
#else
    b2 = amesh[1];
#endif
  } else if (x >= xmax) {
    i_ret = 2;
    n1 = q - 2;
    n2 = q - 1;
#ifndef TLINE_REDUCE_TO_RAW_ARR
    b1 = (*amesh)[q - 2];
#else
    b1 = amesh[q - 2];
#endif
    b2 = xmax;
  } else {
#ifndef TLINE_REDUCE_TO_RAW_ARR
    if (n_old >= 0 && x_old <= x) {
      n1 = t_find_interval_end<T, D>(x, q, *amesh, n_old);
    } else {
      n1 = t_find_interval<T, D>(x, q, *amesh);
    }
// n1 = t_find_interval< T , D >(x, q, amesh);
#else
    if (n_old >= 0 && x_old <= x) {
      n1 = t_find_interval_end<T, T*>(x, q, amesh, n_old);
    } else {
      n1 = t_find_interval<T, T*>(x, q, amesh);
    }
// n1 = t_find_interval< T , T* >(x, q, &amesh);
#endif
    n2 = n1 + 1;
#ifndef TLINE_REDUCE_TO_RAW_ARR
    b1 = (*amesh)[n1];
    b2 = (*amesh)[n2];
#else
    b1 = amesh[n1];
    b2 = amesh[n2];
#endif
    if (n1 < 0 || n1 >= q || n2 < 0 || n2 >= q || b1 < xmin || b1 > xmax ||
        b2 < xmin || b2 > xmax) {
      mcerr << "ERROR in PointCoorMesh<T,D>::get_interval:\n"
            << "n1 < 0 || n1 >= q || n2 < 0 || n2 >= q || b1 < xmin || b1 > "
               "xmax || b2 < xmin || b2 > xmax\n";
      print(mcerr);
      Iprint4n(mcerr, n1, n2, b1, b2);
      spexit(mcerr);
    }
    n_old = n1;
    x_old = x;
  }
  return i_ret;
}

template <class T, class D>
void PointCoorMesh<T, D>::print(std::ostream& file) const {
  Ifile << "PointCoorMesh<T,D>:\n";
  indn.n += 2;
  Ifile << "Type of T is (in internal notations) " << typeid(T).name() << '\n';
  Ifile << "Type of D is (in internal notations) " << typeid(D).name() << '\n';
  Iprint3n(file, q, xmin, xmax);
  Iprint2n(file, n_old, x_old);
#ifndef TLINE_REDUCE_TO_RAW_ARR
  // Ifile << "(*amesh)=" << (*amesh) << '\n';
  Ifile << "(*amesh)=" << (*amesh)[0] << '\n';
#else
  Ifile << "amesh:" << '\n';
  long n;
  indn.n += 2;
  for (n = 0; n < q; n++) {
    Ifile << "n=" << n << " amesh[n]=" << noindent << amesh[n] << yesindent
          << '\n';
  }
  file << yesindent;
  indn.n -= 2;
#endif
  indn.n -= 2;
}

template <class T, class D>
std::ostream& operator<<(std::ostream& file, const PointCoorMesh<T, D>& f) {
  f.print(file);
  return file;
}

// ---------------------------------------------------------------------
// The generic mesh which has arbitrary steps.
// The array determining the step edges is located right in this object.
// Note that it is difficult to make this class derived from previous one
// due to possibility of it without ifndef TLINE_REDUCE_TO_RAW_ARR.
// Then the previous class keeps address of D, not necessary
// raw array or DynLinArr.
// Note also that TLINE_REDUCE_TO_RAW_ARR works here too.

//#define TLINE_COPIED_USE_ADDRESS  // doublfull option.
// If TLINE_REDUCE_TO_RAW_ARR is defined, it allows to access to content
// of DynLinArr as to raw array.
// If TLINE_COPIED_USE_ADDRESS is not defined, access goes through object,
// and with doundary checks if they are activated for DynLinArr.
// Perhaps the latter might be slower.

//-------------------------------------------------------------

// Step array is like a histogram.
// Each value of y represents constant height in each interval
// If mesh is defined by points,
// its size should be longer by unity than the number of y-points,
// the last x-point being represent the end of the last bin.

/*
// Extract value defined by this array for abscissa x
template <class T, class D, class M>
T t_value_step_ar(const M& mesh, const D& y,  // array of function values
                  T x, int s_include_last_point = 0)
    // 0 - not include, 1 - include
{
  mfunname("double t_value_step_ar(...)");
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x < xmin) return 0;
  if (s_include_last_point == 0) {
    if (x >= xmax) return 0;
  } else {
    if (x > xmax) return 0;
  }
  long n1, n2;
  T b1, b2;
  int i_ret = 0;
  i_ret = mesh.get_interval(x, n1, b1, n2, b2);
  check_econd11(i_ret, != 1, mcerr);
  return y[n1];
}

// The same for two-dimensional array D
template <class T, class D, class M1, class M2>
T t_value_step_ar(const M1& mesh1, const M2& mesh2,
                  const D& y,  // array of function values
                  T x1, T x2, int s_include_last_point = 0)
    // 0 - not include, 1 - include
{
  mfunname("double t_value_step_ar(...)");
  double x1min = mesh1.get_xmin();
  double x1max = mesh1.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x1 < x1min) return 0;
  if (s_include_last_point == 0) {
    if (x1 >= x1max) return 0;
  } else {
    if (x1 > x1max) return 0;
  }
  double x2min = mesh2.get_xmin();
  double x2max = mesh2.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x2 < x2min) return 0;
  if (s_include_last_point == 0) {
    if (x2 >= x2max) return 0;
  } else {
    if (x2 > x2max) return 0;
  }
  long n11, n12;
  long n21, n22;
  T b1, b2;
  int i_ret = 0;

  i_ret = mesh1.get_interval(x1, n11, b1, n12, b2);
  check_econd11(i_ret, != 1, mcerr);

  i_ret = mesh2.get_interval(x2, n21, b1, n22, b2);

  check_econd11(i_ret, != 1, mcerr);
  return y[n11][n21];
}
*/

// Fill the array y like a histogram adding value val (or 1) for bin
// corresponding to abscissa x
/*
template <class T, class D, class M>
void t_hfill_step_ar(const M& mesh, const D& y,  // array of function values
                     T x, T val = 1, int s_include_last_point = 0)
    // 0 - not include, 1 - include
{
  mfunname("double t_hfill_step_ar(...)");
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x < xmin) return;
  if (s_include_last_point == 0) {
    if (x >= xmax) return;
  } else {
    if (x > xmax) return;
  }
  long n1;
  int i_ret = 0;
  i_ret = mesh.get_interval(x, n1);
  check_econd11(i_ret, != 1, mcerr);
  y[n1] += val;
  return;
}

// The same as above, but with "ac" access instead of "[]".
// Useful if D is DynArr.

template <class T, class D, class M>
void t_hfill_step_ar_ac(const M& mesh, const D& y,  // array of function values
                        T x, T val = 1, int s_include_last_point = 0)
    // 0 - not include, 1 - include
{
  mfunname("double t_hfill_step_ar(...)");
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x < xmin) return;
  if (s_include_last_point == 0) {
    if (x >= xmax) return;
  } else {
    if (x > xmax) return;
  }
  long n1;
  int i_ret = 0;
  i_ret = mesh.get_interval(x, n1);
  check_econd11(i_ret, != 1, mcerr);
  y.ac(n1) += val;
  return;
}

// The same but for two-dimensional array:
template <class T, class D, class M1, class M2>
void t_hfill_step_ar_ac(const M1& mesh1, const M2& mesh2,
                        const D& y,  // array of function values
                        T x1, T x2, T val = 1, int s_include_last_point = 0)
    // 0 - not include, 1 - include
{
  mfunname("double t_hfill_step_ar(...)");
  double x1min = mesh1.get_xmin();
  double x1max = mesh1.get_xmax();
  double x2min = mesh2.get_xmin();
  double x2max = mesh2.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x1 < x1min) return;
  if (s_include_last_point == 0) {
    if (x1 >= x1max) return;
  } else {
    if (x1 > x1max) return;
  }
  if (x2 < x2min) return;
  if (s_include_last_point == 0) {
    if (x2 >= x2max) return;
  } else {
    if (x2 > x2max) return;
  }
  long n1;
  int i_ret1 = 0;
  i_ret1 = mesh1.get_interval(x1, n1);
  check_econd11(i_ret1, != 1, mcerr);
  long n2;
  int i_ret2 = 0;
  i_ret2 = mesh2.get_interval(x2, n2);
  check_econd11(i_ret2, != 1, mcerr);
  y.ac(n1, n2) += val;
  return;
}
*/

/*
Integrate the function represented by array y (interpreted as
rectangular bins with height determined by the values y[n])
from x1 to x2, taking either pure integral by x (for xpower = 0,
that is it will be sum of function values multiplied by
bin width)
or int(f * x * dx) (for xpower = 1,
the integration of product of function by x).
*/

template <class T, class D, class M>
T t_integ_step_ar(const M& mesh, const D& y,  // array of function values
                  T x1, T x2, int xpower)     // currently 0 or 1
{
  mfunname("double t_integ_step_ar(...)");

  check_econd21(xpower, != 0 &&, != 1, mcerr);
  check_econd12(x1, >, x2, mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0;
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  if (x2 <= xmin) return 0;
  if (x1 >= xmax) return 0;
  if (x1 == x2) return 0;
  long istart, iafterend;  // indexes to sum total intervals
  T s(0);
  if (x1 <= xmin) {
    x1 = xmin;
    istart = 0;
  } else {
    long n1, n2;
    T b1, b2;
    int i_ret = 0;
    i_ret = mesh.get_interval(x1, n1, b1, n2, b2);
    // Iprint2n(mcout, x1, i_ret);
    // Iprint4n(mcout, n1, b1, n2, b2);
    check_econd11(i_ret, != 1, mcerr);
    if (b2 - x1 > 0) {  // otherwise it could be only equal to 0
      if (x2 <= b2) {   // if x2 in the same interval
        if (xpower == 0) {
          s = (x2 - x1) * y[n1];
        } else {
          s = 0.5 * (x2 * x2 - x1 * x1) * y[n1];
        }
        return s;
      }
      if (xpower == 0) {
        s += (b2 - x1) * y[n1];
      } else {
        s += 0.5 * (b2 * b2 - x1 * x1) * y[n1];
      }
    }
    istart = n2;
  }
  if (x2 >= xmax) {
    x2 = xmax;
    iafterend = qi;
  } else {
    long n1, n2;
    T b1, b2;
    int i_ret = 0;
    i_ret = mesh.get_interval(x2, n1, b1, n2, b2);
    // Iprint2n(mcout, x2, i_ret);
    // Iprint4n(mcout, n1, b1, n2, b2);
    check_econd11(i_ret, != 1, mcerr);
    if (x2 - b1 > 0) {
      if (xpower == 0) {
        s += (x2 - b1) * y[n1];
      } else {
        s += 0.5 * (x2 * x2 - b1 * b1) * y[n1];
      }
    }
    iafterend = n1;
  }
  // Iprint2n(mcout, istart, iafterend);
  long i;
  double b;
  mesh.get_scoor(istart, b);
  if (xpower == 0) {
    for (i = istart; i < iafterend; i++) {
      double a = b;
      mesh.get_scoor(i + 1, b);
      s += (b - a) * y[i];
    }
  } else {
    for (i = istart; i < iafterend; i++) {
      double a = b;
      mesh.get_scoor(i + 1, b);
      s += 0.5 * (b * b - a * a) * y[i];
    }
  }
  return s;
}

/*
Generic integration.
fun can be modulating function - very convenient sometimes.
np is number of interval.
It can be used to obtain weight in a global array.
*/
template <class T, class D, class M>
T t_integ_generic_step_ar(const M& mesh,
                          const D& y,  // array of function values
                          T (*fun)(long np, T xp1, T xp2, T yp, T xmin, T xmax,
                                   T x1, T x2),
                          // This function should produce integral
                          // form x1 to x2.
                          T x1, T x2) {
  mfunname("double t_integ_step_ar(...)");

  check_econd12(x1, >, x2, mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0;
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  if (x2 <= xmin) return 0;
  if (x1 >= xmax) return 0;
  if (x1 == x2) return 0;
  long istart, iafterend;  // indexes to sum total intervals
  T s(0);
  if (x1 <= xmin) {
    x1 = xmin;
    istart = 0;
  } else {
    long n1, n2;
    T b1, b2;
    int i_ret = 0;
    i_ret = mesh.get_interval(x1, n1, b1, n2, b2);
    // Iprint2n(mcout, x1, i_ret);
    // Iprint4n(mcout, n1, b1, n2, b2);
    check_econd11(i_ret, != 1, mcerr);
    if (b2 - x1 > 0)  // otherwise it could be only equal to 0
    {
      if (x2 <= b2)  // if x2 in the same interval
      {
        s = fun(n1, b1, b2, y[n1], xmin, xmax, x1, x2);
        return s;
      }
      s = fun(n1, b1, b2, y[n1], xmin, xmax, x1, x2);
    }
    istart = n2;
  }
  if (x2 >= xmax) {
    x2 = xmax;
    iafterend = qi;
  } else {
    long n1, n2;
    T b1, b2;
    int i_ret = 0;
    i_ret = mesh.get_interval(x2, n1, b1, n2, b2);
    // Iprint2n(mcout, x2, i_ret);
    // Iprint4n(mcout, n1, b1, n2, b2);
    check_econd11(i_ret, != 1, mcerr);
    if (x2 - b1 > 0) {
      s += fun(n1, b1, b2, y[n1], xmin, xmax, b1, x2);
    }
    iafterend = n1;
  }
  // Iprint2n(mcout, istart, iafterend);
  long i;
  double b;
  mesh.get_scoor(istart, b);
  for (i = istart; i < iafterend; i++) {
    double a = b;
    mesh.get_scoor(i + 1, b);
    s += fun(i, a, b, y[i], xmin, xmax, a, b);
  }
  // Iprintn(mcout, s);

  // T t;
  return s;
}

// simplified version for total integration along all the mesh
// It is without "power" as function above.
// So it is sum of functions multiplied by bin widths.
/*
template <class T, class D, class M>
T t_total_integ_step_ar(const M& mesh, const D& y  // array of function values
                        ) {
  mfunname("double t_total_integ_step_ar(...)");

  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0;
  long istart, iafterend;  // indexes to sum total intervals
  T s(0);
  istart = 0;
  iafterend = qi;
  // Iprint2n(mcout, istart, iafterend);
  long i;
  double b;
  mesh.get_scoor(istart, b);
  for (i = istart; i < iafterend; i++) {
    double a = b;
    mesh.get_scoor(i + 1, b);
    s += (b - a) * y[i];
  }

  // T t;
  return s;
}

// total integration of two dimensional array in both dimensions

template <class T, class D, class M1, class M2>
T t_total_integ_step_ar(const M1& mesh1, const M2& mesh2,
                        const D& y  // array of function values
                        ) {
  mfunname("double t_total_integ_step_ar(...)");

  long qi1 = mesh1.get_qi();
  check_econd12(qi1, <, 1, mcerr);
  long qi2 = mesh2.get_qi();
  check_econd12(qi2, <, 1, mcerr);
  // if(x1 > x2) return 0;
  long istart1, iafterend1;  // indexes to sum total intervals
  T s1(0);
  istart1 = 0;
  iafterend1 = qi1;
  // Iprint2n(mcout, istart, iafterend);
  long i1;
  double b1;
  mesh1.get_scoor(istart1, b1);
  for (i1 = istart1; i1 < iafterend1; i1++) {
    double a1 = b1;
    mesh1.get_scoor(i1 + 1, b1);
    // time to obtain integral by the second dimension
    // if(x1 > x2) return 0;
    long istart2, iafterend2;  // indexes to sum total intervals
    T s2(0);
    istart2 = 0;
    iafterend2 = qi2;
    // Iprint2n(mcout, istart, iafterend);
    long i2;
    double b2;
    mesh2.get_scoor(istart2, b2);
    for (i2 = istart2; i2 < iafterend2; i2++) {
      double a2 = b2;
      mesh2.get_scoor(i2 + 1, b2);
      s2 += (b2 - a2) * y[i1][i2];
    }
    // OK, integral = s2
    s1 += (b1 - a1) * s2;
  }

  // T t;
  return s1;
}

// Faster version adapted for DynArr

template <class T, class M1, class M2>
T t_total_integ_step_ar(const M1& mesh1, const M2& mesh2,
                        const DynArr<T>& y  // array of function values
                        ) {
  mfunname("double t_total_integ_step_ar(...)");

  long qi1 = mesh1.get_qi();
  check_econd12(qi1, <, 1, mcerr);
  check_econd12(qi1, !=, y.get_qel()[0], mcerr);
  long qi2 = mesh2.get_qi();
  check_econd12(qi2, <, 1, mcerr);
  check_econd12(qi2, !=, y.get_qel()[1], mcerr);
  // if(x1 > x2) return 0;
  long istart1, iafterend1;  // indexes to sum total intervals
  T s1(0);
  istart1 = 0;
  iafterend1 = qi1;
  // Iprint2n(mcout, istart, iafterend);
  long i1;
  double b1;
  mesh1.get_scoor(istart1, b1);
  for (i1 = istart1; i1 < iafterend1; i1++) {
    double a1 = b1;
    mesh1.get_scoor(i1 + 1, b1);

    // time to obtain integral by the second dimension

    // if(x1 > x2) return 0.0;
    long istart2, iafterend2;  // indexes to sum total intervals
    T s2(0.0);
    istart2 = 0;
    iafterend2 = qi2;
    // Iprint2n(mcout, istart, iafterend);
    long i2;
    double b2;
    mesh2.get_scoor(istart2, b2);
    for (i2 = istart2; i2 < iafterend2; i2++) {
      double a2 = b2;
      mesh2.get_scoor(i2 + 1, b2);
      s2 += (b2 - a2) * y.acu(i1, i2);
    }

    // OK, integral = s2

    s1 += (b1 - a1) * s2;
  }

  // T t;
  return s1;
}
*/

/* Finds value x, such that the integral of y (rectangular bins)
is equal to integ.
*/
// This program is not fast enough for
// serial random number generation.
// For the last purpose it is more smart to integrate the array
// once. This program integrates it each time.
/*
template <class T, class D, class M>
T t_find_x_for_integ_step_ar(const M& mesh,
                             const D& y,           // array of function values
                             T integ, int* s_err)  // for power = 0 only
{
  mfunname("double t_integ_step_ar(...)");

  *s_err = 0;
  // check_econd11(xpower , != 0 , mcerr);
  check_econd11(integ, < 0.0, mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0.0;
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  if (integ == 0.0) return xmin;
  T s(0.0);
  long n = 0;
  T xp1(0.0);
  T xp2(0.0);
  mesh.get_scoor(n, xp2);
  for (n = 0; n < qi; n++) {
    xp1 = xp2;
    mesh.get_scoor(n + 1, xp2);
    T step = xp2 - xp1;
    T s1 = s + y[n] * step;
    // Iprint3n(mcout, n, s1, integ);
    if (s1 > integ) break;
    if (s1 == integ) return xp2;
    s = s1;
  }

  if (n == qi) {
    *s_err = 1;
    return xmax;
  }
  double x = xp1;
  // Iprint3n(mcout, n, s, x);
  x += (integ - s) / y[n];
  // Iprintn(mcout, x);
  return x;
}
*/
// The following program the same as previous, but
// considers the array already integrated.
// The algorithm is then much faster, since it uses binary search.
// It is used, in particular, for random numbers generation.

template <class T, class D, class M>
T t_find_x_for_already_integ_step_ar(const M& mesh,
                                     const D& y,  // array of function values
                                     T integ, int* s_err)  // for power = 0 only
{
  mfunname("double t_find_x_for_already_integ_step_ar(...)");

  *s_err = 0;
  // check_econd11(xpower , != 0 , mcerr);
  check_econd11(integ, < 0.0, mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0.0;
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  if (integ == 0.0) return xmin;
  if (integ > y[qi - 1]) {
    *s_err = 1;
    return xmax;
  }
  if (integ == y[qi - 1]) return xmax;
  if (integ < y[0]) {  // answer in the first bin
    T xp1(0.0);
    T xp2(0.0);
    mesh.get_scoor(0, xp1);
    mesh.get_scoor(1, xp2);
    return xp1 + (xp2 - xp1) * integ / y[0];
  }
  // binary search
  long nl = 0;
  long nr = qi - 1;
  long nc;
  while (nr - nl > 1) {
    nc = (nr + nl) / 2;
    if (integ < y[nc])
      nr = nc;
    else
      nl = nc;
  }
  // Iprint2n(mcout, nl, nr);
  T xl(0.0);
  T xr(0.0);
  mesh.get_scoor(nl + 1, xl);
  mesh.get_scoor(nr + 1, xr);
  // Note "+1" in the previous two expressions.
  // This arises from the fact that the nl'th element of
  // y-array contains integral of nl'th bin plus all previous bins.
  // So y[nl] is related to x of nl+1.
  T a = (xr - xl) / (y[nr] - y[nl]);
  T ret = xl + a * (integ - y[nl]);

  return ret;
}

// The same as previous, but return entire number, faster.
// Mesh should be entire too.
// In the contrary to the previous function
// y is interpreted here as y[i] is sum from 0 to y[i].
// Not to y[i+1], as for smooth case.
// But "+1" is persisting anyway.
// For example, if gamma is between the first (n=0) and second (n=1)
// bins by prob.density, then the solution is the second bin (n=1),
// not the first as one could think naively!

template <class T, class D, class M>
long t_find_entire_x_for_already_integ_step_ar(
    const M& mesh, const D& y,  // array of function values
    T integ, int* s_err)        // for power = 0 only
{
  mfunname("double t_find_entire_x_for_already_integ_step_ar(...)");
  // Iprintn(mcout, mesh);
  // Iprintn(mcout, integ);
  *s_err = 0;
  // check_econd11(xpower , != 0 , mcerr);
  check_econd11(integ, < 0.0, mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0.0;
  long xmin = mesh.get_xmin();
  long xmax = mesh.get_xmax();
  if (integ == 0) return xmin;
  if (integ > y[qi - 1]) {
    *s_err = 1;
    return xmax;
  }
  if (integ == y[qi - 1]) return xmax;
  if (integ < y[0]) {  // answer in the first bin
    long xp1(0);
    mesh.get_scoor(0, xp1);
    return xp1;
  }
  // binary search
  long nl = 0;
  long nr = qi - 1;
  long nc;
  while (nr - nl > 1) {
    nc = (nr + nl) / 2;
    if (integ < y[nc])
      nr = nc;
    else
      nl = nc;
  }
  // Iprint2n(mcout, nl, nr);
  // Iprint2n(mcout, y[nl], y[nr]);
  // Iprint2n(mcout, nl, nr);
  long x(0);
  mesh.get_scoor(nr, x);
  // Iprintn(mcout, x);

  return x;
}

// prepare histogram for generation of the random numbers
// return integral
// initialize probability density function
// y and integ_y can point to the same array

template <class T, class D, class M>
T t_hispre_step_ar(const M& mesh, const D& y,  // array of function values
                   D& integ_y                  // return integrated array
                   ) {
  mfunname("double t_hispre_step_ar(...)");

  // check_econd11(xpower , != 0 , mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);

  T s(0.0);
  long n = 0;
  T xp1(0.0);
  T xp2(0.0);
  mesh.get_scoor(n, xp2);
  for (n = 0; n < qi; n++) {
    xp1 = xp2;
    mesh.get_scoor(n + 1, xp2);
    T step = xp2 - xp1;
    check_econd11a(y[n], < 0.0,
                   "n=" << n << " xp1=" << xp1 << " xp2=" << xp2 << '\n',
                   mcerr);
    s = s + y[n] * step;
    integ_y[n] = s;
    // Iprint3n(mcout, n, s1, integ);
  }
  // TODO!! (HS)
  // check_econd11a(s, <= 0.0, "y=" << y << " integ_y=" << integ_y << '\n',
  // mcerr);
  for (n = 0; n < qi; n++) {
    integ_y[n] /= s;
  }
  return s;
}

// generate random number

template <class T, class D, class M>
T t_hisran_step_ar(const M& mesh, const D& integ_y, T rannum) {
  mfunname("double t_hisran_step_ar(...)");
  // check_econd11(xpower , != 0 , mcerr);
  long qi = mesh.get_qi();
  long s_same = apeq_mant(integ_y[qi - 1], 1.0, 1.0e-12);
  check_econd11a(s_same, != 1.0, "integ_y[qi-1]=" << integ_y[qi - 1] << '\n',
                 mcerr);

  // Iprintn(mcout, rannum);
  // check_econd11(integ_y[qi-1] , != 1.0 , mcerr);
  int s_err;

  T ret = t_find_x_for_already_integ_step_ar(mesh,     // dimension q
                                             integ_y,  // dimension q-1
                                             rannum, &s_err);
  // TODO (HS)!!
  // check_econd11a(s_err, != 0, "mesh=" << mesh << " integ_y=" << integ_y
  //                                     << " rannum=" << rannum << '\n',
  //                mcerr);
  return ret;
  // return  t_find_x_for_already_integ_step_ar
  // (mesh,               // dimension q
  //  integ_y,                  // dimension q-1
  //  rannum,
  //  &s_err);
}

// opposite to generate random number: find integral probability
// for a certain absciss

template <class T, class D, class M>
T t_opposite_hisran_step_ar(const M& mesh, const D& integ_y, T x) {
  mfunname("double t_opposite_hisran_step_ar(...)");

  // check_econd11(xpower , != 0 , mcerr);
  long qi = mesh.get_qi();
  long s_same = apeq_mant(integ_y[qi - 1], 1.0, 1.0e-12);
  check_econd11a(s_same, != 1.0, "integ_y[qi-1]=" << integ_y[qi - 1] << '\n',
                 mcerr);

  long n1;
  T b1;
  long n2;
  T b2;

  mesh.get_interval_extrap(x, n1, b1, n2, b2);
  T y1;
  T y2;
  if (n1 == 0) {
    y1 = 0.0;
    y2 = integ_y[0];
  } else {
    y1 = integ_y[n1 - 1];
    y2 = integ_y[n2 - 1];
  }
  T res = y1 + ((y2 - y1) / (b2 - b1)) * (x - b1);
  return res;
}

// generate entire random number

template <class T, class D, class M>
long t_entire_hisran_step_ar(const M& mesh, const D& integ_y, T rannum) {
  mfunname("double t_entire_hisran_step_ar(...)");

  // check_econd11(xpower , != 0 , mcerr);
  long qi = mesh.get_qi();
  long s_same = apeq_mant(integ_y[qi - 1], 1.0, 1.0e-12);
  check_econd11a(s_same, != 1.0, "integ_y[qi-1]=" << integ_y[qi - 1] << '\n',
                 mcerr);
  // check_econd11(integ_y[qi-1] , != 1.0 , mcerr);
  int s_err;

  long ret =
      t_find_entire_x_for_already_integ_step_ar(mesh,     // dimension q
                                                integ_y,  // dimension q-1
                                                rannum, &s_err);
  check_econd11a(s_err, != 0, "mesh=" << mesh << " integ_y=" << integ_y
                                      << " rannum=" << rannum << '\n',
                 mcerr);
  return ret;
  // return  t_find_entire_x_for_already_integ_step_ar
  //   (mesh,               // dimension q
  //   integ_y,                  // dimension q-1
  //   rannum,
  //   &s_err);
}

/*
This is mean of "step array".
*/
template <class T, class D, class M>
T t_mean_step_ar(const M& mesh, const D& y,  // array of function values
                 T x1, T x2, int& s_err) {
  mfunname("double t_mean_step_ar(...)");
  s_err = 0;
  T integ = t_integ_step_ar(mesh, y, x1, x2, 0);
  if (integ == 0) {
    s_err = 1;
    return 0.0;
  }
  T xinteg = t_integ_step_ar(mesh, y, x1, x2, 1);
  return xinteg / integ;
}

template <class T>
T t_value_straight_2point(T x1, T y1, T x2, T y2, T x, int s_ban_neg) {
  mfunname("double t_value_straight_2point(...)");
  check_econd12(x1, ==, x2, mcerr);

  T a = (y2 - y1) / (x2 - x1);
  // Less numerical precision
  // T b = y[n1];
  // T res = a * ( x - x1) + b;
  // More numerical precision (although it is not proved),
  // starting from what is closer to x
  T res;
  T dx1 = x - x1;
  T adx1 = (dx1 > 0) ? dx1 : -dx1;  // absolute value
  // if(dx1 > 0)
  //  adx1 = dx1;
  // else
  //  adx1 = -dx1;
  T dx2 = x - x2;
  T adx2 = (dx2 > 0) ? dx2 : -dx2;  // absolute value
  // if(dx2 > 0)
  //  adx2 = dx2;
  // else
  //  adx2 = -dx2;
  if (adx1 < adx2)  // x is closer to x1
  {
    res = a * dx1 + y1;
  } else {
    res = a * dx2 + y2;
  }
  if (s_ban_neg == 1 && res < 0.0) res = 0.0;
  return res;
}

template <class T>
T t_integ_straight_2point(T x1, T y1, T x2, T y2, T xl, T xr,
                          int xpower,  // currently 0 or 1
                          int s_ban_neg)
    // 0 - not include, 1 - include
{
  mfunname("double t_integ_straight_2point(...)");
  check_econd12(x1, ==, x2, mcerr);

  T a = (y2 - y1) / (x2 - x1);
  T b = y1;
  T yl = a * (xl - x1) + b;
  T yr = a * (xr - x1) + b;
  if (s_ban_neg == 1) {
    if (yl <= 0.0 && yr <= 0.0) return 0.0;
    if (yl < 0.0 || yr < 0.0) {
      T xz = x1 - b / a;
      if (yl < 0.0) {
        xl = xz;
        yl = 0.0;
      } else {
        xr = xz;
        yr = 0.0;
      }
    }
  }
  T res;
  if (xpower == 0)
    res = 0.5 * a * (xr * xr - xl * xl) + (b - a * x1) * (xr - xl);
  else
    res = a * (xr * xr * xr - xl * xl * xl) / 3.0 +
          0.5 * (b - a * x1) * (xr * xr - xl * xl);

  return res;
}

// Extract value defined by this array for abscissa x
// y have dimension q or qi+1.
template <class T, class D, class M>
T t_value_straight_point_ar(const M& mesh,
                            const D& y,  // array of function values
                            T x, int s_ban_neg, int s_extrap_left, T left_bond,
                            int s_extrap_right, T right_bond) {
  // 0 - not include, 1 - include
  mfunname("double t_value_straight_point_ar(...)");
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x < left_bond) return 0.0;
  if (x > right_bond) return 0.0;
  if (x < xmin && s_extrap_left == 0) return 0.0;
  if (x > xmax && s_extrap_right == 0) return 0.0;
  long n1, n2;
  T b1, b2;
  mesh.get_interval_extrap(x, n1, b1, n2, b2);
  T x1;
  mesh.get_scoor(n1, x1);
  T x2;
  mesh.get_scoor(n2, x2);
  return t_value_straight_2point(x1, y[n1], x2, y[n2], x, s_ban_neg);
}

// Extract value defined by this array for abscissa x
template <class T, class D, class M>
T t_value_generic_point_ar(
    const M& mesh, const D& y,  // array of function values
    T (*funval)(T xp1, T yp1, T xp2, T yp2, T xmin,
                T xmax,  // may be necessary for shape determination
                T x),
    T x, int s_extrap_left, T left_bond, int s_extrap_right, T right_bond) {
  // 0 - not include, 1 - include
  mfunname("double t_value_generic_point_ar(...)");
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  // Iprint3n(mcout, x, xmin, xmax);
  if (x < left_bond) return 0.0;
  if (x > right_bond) return 0.0;
  if (x < xmin && s_extrap_left == 0) return 0.0;
  if (x > xmax && s_extrap_right == 0) return 0.0;
  long n1, n2;
  T b1, b2;
  mesh.get_interval_extrap(x, n1, b1, n2, b2);
  T x1;
  mesh.get_scoor(n1, x1);
  T x2;
  mesh.get_scoor(n2, x2);
  return funval(x1, y[n1], x2, y[n2], left_bond, right_bond, x);
}

// power function x^pw

// not debugged
// No, perhaps already  checked in some application.
// If power function cannot be drawn, it exits.

template <class T>
T t_value_power_2point(T x1, T y1, T x2, T y2, T x) {
  mfunname("double t_value_power_2point(...)");

  check_econd11(y1, <= 0.0, mcerr);
  check_econd11(y2, <= 0.0, mcerr);
  check_econd12(y1, ==, y2, mcerr);
  check_econd12(x1, ==, x2, mcerr);
  T res = y1;
  if (x1 <= 0.0 && x2 >= 0.0) {
    mcerr << "T t_value_power_2point(...): \n";
    mcerr << "x's are of different sign, power cannot be drawn\n";
    spexit(mcerr);
  } else {
    T pw = log(y1 / y2) / log(x1 / x2);
    // check_econd11(pw , == -1.0 , mcerr);
    res = y1 * pow(x, pw) / pow(x1, pw);
  }
  return res;
}
/*
// in the case of zero of different signs of x it uses linear interpolation
template <class T>
T t_value_power_extended_2point(T x1, T y1, T x2, T y2, T x) {
  mfunname("double t_value_power_2point(...)");

  check_econd11(y1, <= 0.0, mcerr);
  check_econd11(y2, <= 0.0, mcerr);
  check_econd12(y1, ==, y2, mcerr);
  check_econd12(x1, ==, x2, mcerr);
  T res;
  if (x1 <= 0.0 && x2 >= 0.0) {
    res = y1 + (x - x1) * (y2 - y1) / (x2 - x1);
  } else {
    T pw = log(y1 / y2) / log(x1 / x2);
    // check_econd11(pw , == -1.0 , mcerr);
    res = y1 * pow(x, pw) / pow(x1, pw);
  }
  return res;
}
*/

template <class T>
T t_value_exp_2point(T x1, T y1, T x2, T y2, T x) {
  mfunname("double t_value_exp_2point(...)");

  check_econd11(y1, <= 0.0, mcerr);
  check_econd11(y2, <= 0.0, mcerr);
  check_econd12(y1, ==, y2, mcerr);
  check_econd12(x1, ==, x2, mcerr);
  T res;

  T a = log(y1 / y2) / (x1 - x2);
  T c = y1 / exp(a * x1);
  // check_econd11(pw , == -1.0 , mcerr);
  res = c * exp(a * x);
  ;
  return res;
}

template <class T>
T t_integ_power_2point(T x1, T y1, T x2, T y2, T xl, T xr)
    // 0 - not include, 1 - include
{
  mfunname("double t_integ_power_2point(...)");

  check_econd11(y1, <= 0.0, mcerr);
  check_econd11(y2, <= 0.0, mcerr);
  check_econd12(y1, ==, y2, mcerr);
  check_econd12(x1, ==, x2, mcerr);
  T pw = log(y1 / y2) / log(x1 / x2);
  check_econd11(pw, == -1.0, mcerr);
  T k = y1 * pow(x1, -pw);
  T t = k / (1 + pw) * (pow(xr, (pw + 1)) - pow(xl, (pw + 1)));
  return t;
}

template <class T, class D, class M>
T t_integ_straight_point_ar(const M& mesh,
                            const D& y,  // array of function values
                            T x1, T x2,  // intervals of integration
                            int xpower,  // currently 0 or 1
                            int s_ban_neg, int s_extrap_left, T left_bond,
                            int s_extrap_right, T right_bond) {
  mfunname("double t_integ_straight_point_ar(...)");

  // mcout<<"Strart t_integ_straight_point_ar\n";
  check_econd21(xpower, != 0 &&, != 1, mcerr);
  check_econd12(x1, >, x2, mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0.0;
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  if (x2 <= xmin && s_extrap_left == 0) return 0.0;
  if (x1 >= xmax && s_extrap_right == 0) return 0.0;
  if (x2 <= left_bond) return 0.0;
  if (x1 >= right_bond) return 0.0;
  T s(0.0);
  if (x1 < left_bond) x1 = left_bond;
  if (x2 > right_bond) x2 = right_bond;
  if (x1 <= xmin && s_extrap_left == 0) x1 = xmin;
  if (x2 > xmax && s_extrap_left == 0) x2 = xmax;
  long np1, np2;
  T bp1, bp2;
  int i_ret = 0;
  // restore the interval in which x1 reside
  i_ret = mesh.get_interval_extrap(x1, np1, bp1, np2, bp2);
  // restore the x-coordinates of given points
  T xp1;
  mesh.get_scoor(np1, xp1);
  T xp2;
  mesh.get_scoor(np2, xp2);
  T res;
  T yp1 = y[np1];
  T yp2 = y[np2];
  if (i_ret == 2 || x2 <= xp2)  // then all in one interval
  {
    res = t_integ_straight_2point<T>(xp1, yp1, xp2, yp2, x1, x2, xpower,
                                     s_ban_neg);
  } else {
    // integrate only till end of the current interval
    T x1i = x1;
    T x2i = xp2;
    res = t_integ_straight_2point<T>(xp1, yp1, xp2, yp2, x1i, x2i, xpower,
                                     s_ban_neg);
    // x2i = x1;  // prepere for loop
    int s_stop = 0;
    do {
      np1 = np2;
      np2++;
      xp1 = xp2;
      mesh.get_scoor(np2, xp2);
      x1i = x2i;
      if (xp2 >= x2) {
        x2i = x2;  // till end of integral
        s_stop = 1;
      } else {
        if (np2 == qi)  // end of the mesh, but x2 is farther
        {
          x2i = x2;  // till end of integral
          s_stop = 1;
        } else {
          x2i = xp2;  // till end of current interval
          s_stop = 0;
        }
      }
      yp1 = yp2;
      yp2 = y[np2];
      res += t_integ_straight_2point<T>(xp1, yp1, xp2, yp2, x1i, x2i, xpower,
                                        s_ban_neg);
      // Iprint2n(mcout, xp1, xp2);
      // Iprint2n(mcout, x1i, x2i);
      // Iprint2n(mcout, yp1, yp2);
      // Iprint2n(mcout, res, s_stop);

    } while (s_stop == 0);
  }
  return res;
}

template <class T, class D, class M>
T t_mean_straight_point_ar(const M& mesh,
                           const D& y,  // array of function values
                           T x1, T x2, int s_extrap_left, T left_bond,
                           int s_extrap_right, T right_bond, int& s_err) {
  mfunname("double t_mean_straight_point_ar(...)");
  s_err = 0;
  T integ = t_integ_straight_point_ar(mesh, y, x1, x2, 0, 1, s_extrap_left,
                                      left_bond, s_extrap_right, right_bond);
  if (integ == 0) {
    s_err = 1;
    return 0.0;
  }
  T xinteg = t_integ_straight_point_ar(mesh, y, x1, x2, 1, 1, s_extrap_left,
                                       left_bond, s_extrap_right, right_bond);
  return xinteg / integ;
}

// template<class T>
// typedef T(*GENERICFUN)(T xp1, T yp1, T xp2, T yp2,
//                        T xmin, T xmax, T x1, T x2);
// typedef T(*GENERICFUN)(T, T, T, T,
//                        T, T, T, T);

template <class T, class D, class M>
T t_integ_generic_point_ar(
    const M& mesh, const D& y,  // array of function values
    // GENERICFUN fun,
    T (*fun)(T xp1, T yp1, T xp2, T yp2, T xmin, T xmax, T x1, T x2), T x1,
    T x2, int s_extrap_left, T left_bond, int s_extrap_right, T right_bond) {
  mfunname("double t_integ_generic_point_ar(...)");

  // mcout<<"Strart t_integ_straight_point_ar\n";
  // check_econd21(xpower , != 0 &&  , != 1 , mcerr);
  check_econd12(x1, >, x2, mcerr);
  long qi = mesh.get_qi();
  check_econd12(qi, <, 1, mcerr);
  // if(x1 > x2) return 0.0;
  double xmin = mesh.get_xmin();
  double xmax = mesh.get_xmax();
  if (x2 <= xmin && s_extrap_left == 0) return 0.0;
  if (x1 >= xmax && s_extrap_right == 0) return 0.0;
  if (x2 <= left_bond) return 0.0;
  if (x1 >= right_bond) return 0.0;
  // long istart, iafterend; // indexes to sum total intervals
  if (x1 < left_bond) x1 = left_bond;
  if (x2 > right_bond) x2 = right_bond;
  if (x1 <= xmin && s_extrap_left == 0) x1 = xmin;
  if (x2 > xmax && s_extrap_left == 0) x2 = xmax;
  long np1, np2;
  T bp1, bp2;
  int i_ret = 0;
  // restore the interval in which x1 reside
  i_ret = mesh.get_interval_extrap(x1, np1, bp1, np2, bp2);
  // restore the x-coordinates of given points
  T xp1;
  mesh.get_scoor(np1, xp1);
  T xp2;
  mesh.get_scoor(np2, xp2);
  T res;
  T yp1 = y[np1];
  T yp2 = y[np2];
  if (i_ret == 2 || x2 <= xp2)  // then all in one interval
  {
    res = fun(xp1, yp1, xp2, yp2, xmin, xmax, x1, x2);
  } else {
    // integrate only till end of the current interval
    T x1i = x1;
    T x2i = xp2;
    res = fun(xp1, yp1, xp2, yp2, xmin, xmax, x1i, x2i);
    // x2i = x1;  // prepere for loop
    int s_stop = 0;
    do {
      np1 = np2;
      np2++;
      xp1 = xp2;
      mesh.get_scoor(np2, xp2);
      x1i = x2i;
      if (xp2 >= x2) {
        x2i = x2;  // till end of integral
        s_stop = 1;
      } else {
        if (np2 == qi)  // end of the mesh, but x2 is farther
        {
          x2i = x2;  // till end of integral
          s_stop = 1;
        } else {
          x2i = xp2;  // till end of current interval
          s_stop = 0;
        }
      }
      yp1 = yp2;
      yp2 = y[np2];
      res += fun(xp1, yp1, xp2, yp2, xmin, xmax, x1i, x2i);
      // Iprint2n(mcout, xp1, xp2);
      // Iprint2n(mcout, x1i, x2i);
      // Iprint2n(mcout, yp1, yp2);
      // Iprint2n(mcout, res, s_stop);

    } while (s_stop == 0);
  }
  return res;
}

// find width at half-height of a histogram
// doing straight line interpolation between centers of the bins
//(like straight_point_ar).
// But the mesh is understood as a range of the left points.
// if there are several maximal bin with the same height
// it will decline from the first one, which might be
// not accurate, although the result is anyway reasonable.
/*
template <class T, class D, class M>
T t_width_at_hheight_step_ar(const M& mesh, const D& y) {
  // 0 - not include, 1 - include
  mfunname("double t_width_at_hheight_step_ar(...)");
  // mcout<<"t_width_at_hheight_step_ar is started\n";
  long qi = mesh.get_qi();
  long n;
  T ymax = 0;
  long nmax;
  for (n = 0; n < qi; ++n) {
    if (y[n] > ymax) {
      check_econd11(y[n], < 0.0, mcerr);
      ymax = y[n];
      nmax = n;
    }
  }
  // Iprint2n(mcout, ymax, nmax);
  if (ymax == 0) return 0;
  T ylev = ymax / 2.0;
  T s2 = 0;
  long q = 0;
  for (n = nmax; n < qi; n++) {

    if (y[n] > ylev && y[n + 1] <= ylev) {
      T x1, x2;
      mesh.get_interval(n, x1, x2);
      T step1, step2;
      mesh.get_step(n, step1);
      mesh.get_step(n + 1, step2);
      step1 = step1 / 2.0;
      step2 = step2 / 2.0;
      s2 += t_value_straight_2point(y[n], x1 + step1, y[n + 1], x2 + step2,
                                    ylev, 0);
      // Iprint2n(mcout, x1, x2);
      // Iprint2n(mcout, x1+step1, x2+step2);
      // Iprint2n(mcout, y[n], y[n+1]);
      // Iprint2n(mcout, n, t_value_straight_2point(y[n], x1+step1, y[n+1],
      // x2+step2, ylev, 0));
      q++;
    }
  }
  check_econd11(q, <= 0, mcerr);
  s2 = s2 / q;
  T s1 = 0;
  q = 0;
  for (n = nmax; n >= 0; n--) {
    if (y[n] > ylev && y[n - 1] <= ylev) {
      T x1, x2;
      mesh.get_interval(n - 1, x1, x2);
      T step1, step2;
      mesh.get_step(n - 1, step1);
      mesh.get_step(n, step2);
      step1 = step1 / 2.0;
      step2 = step2 / 2.0;
      s1 += t_value_straight_2point(y[n - 1], x1 + step1, y[n], x2 + step2,
                                    ylev, 0);
      // Iprint2n(mcout, x1, x2);
      // Iprint2n(mcout, x1+step1, x2+step2);
      // Iprint2n(mcout, y[n-1], y[n]);
      // Iprint2n(mcout, n, t_value_straight_2point(y[n-1], x1+step1, y[n],
      // x2+step2, ylev, 0));
      q++;
    }
  }
  check_econd11(q, <= 0, mcerr);
  s1 = s1 / q;
  // Iprint3n(mcout, s1, s2, s2 - s1);
  return s2 - s1;
}
*/
}

#endif
