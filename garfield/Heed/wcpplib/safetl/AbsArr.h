#ifndef ABSARR_H
#define ABSARR_H
/*
Simple arrays with variable length.
DynLinArr - "Dynamic Linear Array" - it is what it is.
The name is choosen to distinguish this template class from every known
other libraries.
The information is kept in one block of memory and accessible with
indexes. The array is characterized by the only parameter - its physical
size. If you need a logicl size different from physical, keep the
logical size in a separated place.
Increasing of the array size leads to allocation of
another block of memory and copying all elements to it by
operator= (which can be correctly defined in the class definitions).
This is difference with STL, in which vector have logical size
which can be smaller than the volume of allocated memory.
Currently the array boundaries are checked at each access, this is
also difference with STL.
These checks can be switched out by undefining macro ALR_CHECK_BOUND.
The copying of array leads to copying all elements, destruction - to
destruction of elements.
The array DynLinArr can keep other array DynLinArr as elements
and constitute multy-dimensional array  of not "parallelogram" shape,
that is for each first index, the dimension corresponding to
the second index can be different from that of the other first indexes.
This is very powerful feature, but it is not always desirable.
If you need box-like array or "parallelogram" shape, use DynArr -
"Dynamic Array".

In DynArr the number of dimensions is arbitrary, but size of this
array along each dimension is fixed and independent on
the indexes of other dimensions. So this is rectangular array.
The access is performed by indexes,
but not through operators []. Since it is not trivial to provide
such access for this array, I preferred the simplest solutions
and arranged this access through little functions "ac":
  T& DynArr::ac(long i)  // for 1-dimensional array
  T& DynArr::ac(long i1, long i2)  // for 2-dimensional array
  T& DynArr::ac(const DynLinArr<long>& ind)
    // for arbitrary number of dimensions
    // but the number of them in array should be equal to size of ind.
    // ind is array of indexes. Its first element if the first index,
    // second is second, etc.
class DynArr is constructed with the help of DynLinArr.
DynArr can keep as elements another DynArr's or DynLinArr's.
DynLinArr can also keep DynArr as elements.

There are many various auxiliary utilites assosiated with these arrays.
Some of them look little bit antiquated, since they was created in
stone age and were not revised after innovation of electricity.:)
This happened because the author needed to port his programs on
very various computers, not all of them had very advanced C++-related
software. Today the situation with C++ is being changed rapidly
and some of these pearls may be arranged by more modern way.
But the main components of this file are actual and very convenient.

Copyright (c) 1999-2005 I. B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/
#include <iostream>
#include <iomanip>
#include <sstream>
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/stream/definp.h"
#include "wcpplib/util/String.h"
#include "wcpplib/safetl/AbsPtr.h"

// Here there is a good place to switch on the bound check in all programs
#ifndef ALR_CHECK_BOUND
#define ALR_CHECK_BOUND
#endif
#ifndef ALR_CHECK_EACH_BOUND
#define ALR_CHECK_EACH_BOUND
#endif

//#define DEBUG_DYNLINARR  // make some print
//#define DEBUG_DYNARR  // make some print and in addition
// functions from DynArr make some formally unnecessary checks, which are
// however useful for debug.

namespace Heed {
extern long max_qel_DynLinArr;  // increase it if need
// Helps to detect access to not inited DynLinArr,
// what may happen at initializetion of class members
// and use of not inited member for initialization of the other.

template <class T>
class DynArr;

class ArgInterp_Arr {};
class ArgInterp_SingleAdr {}; // for put_qel()
class ArgInterp_Val {};

template <class T>
class DynLinArr : public RegPassivePtr {
 public:
  // Constructors
  DynLinArr(void) : qel(0), el(NULL) { ; }
  explicit DynLinArr(long fqel) : qel(fqel), el(NULL) {
    if (qel > max_qel_DynLinArr) {
      mcerr << "ERROR in DynLinArr(long fqel):\n";
      mcerr << "qel > max_qel_DynLinArr:\n";
      Iprint2n(mcout, qel, max_qel_DynLinArr);
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
    if (qel < 0) {
      mcerr << "ERROR in DynLinArr(long fqel):\n";
      mcerr << "qel < 0:\n";
      Iprintn(mcout, qel);
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
    el = (fqel > 0) ? (new T[fqel]) : (T*)NULL;
  }

  DynLinArr(long fqel, const T& val) : qel(fqel), el(NULL) {
    if (qel > max_qel_DynLinArr) {
      mcerr << "ERROR in DynLinArr(long fqel, const T& val):\n";
      mcerr << "qel > max_qel_DynLinArr:\n";
      Iprint2n(mcout, qel, max_qel_DynLinArr);
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
    if (qel < 0) {
      mcerr << "ERROR in DynLinArr(long fqel, const T& val):\n";
      mcerr << "qel < 0:\n";
      Iprintn(mcout, qel);
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
    el = (fqel > 0) ? (new T[fqel]) : (T*)NULL;
    assignAll(val);
  }
  DynLinArr(long fqel, const T* ar, ArgInterp_Arr /*t*/) : qel(fqel), el(NULL) {
    if (qel > max_qel_DynLinArr) {
      mcerr << "ERROR in DynLinArr(long fqel, const T* ar, ArgInterp_Arr):\n";
      mcerr << "qel > max_qel_DynLinArr:\n";
      Iprint2n(mcout, qel, max_qel_DynLinArr);
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
    if (qel < 0) {
      mcerr << "ERROR in DynLinArr(long fqel, const T* ar, ArgInterp_Arr):\n";
      mcerr << "qel < 0:\n";
      Iprintn(mcout, qel);
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
    el = (fqel > 0) ? (new T[fqel]) : (T*)NULL;
    for (long n = 0; n < qel; n++) el[n] = ar[n];
  }
  // const T* ar is array here (of course).
  // ArgInterp_Arr serves to distinguish this
  // from previous constructor when second argument
  // does not coincide with T but can be converted to it.
  // Typical example is when T is double and second argument is 0.
  // It is assumed that  0 should be converted to 0.0 and previous
  // constructor should be called. But in practice (Red Hat Linux 6.0)
  // the compiler says
  // call of overloaded `DynLinArr(int &, int)' is ambiguous.
  // I don't know whether this is error of particular compiler or
  // general problem. But the third dummy argument is anyway convenient
  // to distringuish these cases.

  DynLinArr<T>& operator=(const DynLinArr<T>& f);
  template <class D>
  DynLinArr<T>& operator=(const DynLinArr<D>& f);

  void pass(long fqel, T* fel) {
    // Do not call directly! Is to be used only
    // from assignment operator above
    clear();
    qel = fqel;
    el = fel;
  }

  inline DynLinArr(const DynLinArr<T>& f);
  DynLinArr(const DynLinArr<T>& f, Pilfer) : qel(f.qel), el(f.el) {
#ifdef DEBUG_DYNLINARR
    mcout << "DynLinArr( DynLinArr<T>& f, Pilfer) is working\n";
#endif
    f.qel = 0;
    f.el = 0;
  }
  DynLinArr(const DynArr<T>& f);  // works only if f has one dimension
                                  // otherwise calls spexit.

  DynLinArr(const DynArr<T>& f, int n_of_dim,
            // 0 - first dim) 1 - second dim)
            long roc_number);
  // takes only mentioned raw or column.

  DynLinArr& assignAll(const T& f) {
    check();
    for (long n = 0; n < qel; n++) el[n] = f;
    return *this;
  }
  template <class X>
  DynLinArr<T>& assignAll1(const X& f) {
    // assumes that element is object
    // which also accepts assignAll, which is called for it.
    check();
    for (long n = 0; n < qel; n++) el[n].assignAll(f);
    return *this;
  }
  // DynLinArr& operator=(const T& f) { int n; for( n=0; n<qel; n++) el[n]=f; }
  inline T& operator[](long n) {
#ifdef ALR_CHECK_BOUND
    if (n >= 0 && n < qel) return el[n];
    mcerr << "ERROR in const T& DynLinArr::operator[](long n) const: "
          << "n is out of bounds, n=" << n << " qel=" << qel << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else
    return el[n];
#endif
  }
  inline const T& operator[](long n) const {
#ifdef ALR_CHECK_BOUND
    if (n >= 0 && n < qel) return el[n];
    mcerr << "ERROR in const T& DynLinArr::operator[](long n) const: "
          << "n is out of bounds, n=" << n << " qel=" << qel << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else
    return el[n];
#endif
  }
  inline T& acu(long n) {
    // unchecked access
    return el[n];
  }
  inline const T& acu(long n) const {
    // unchecked access
    return el[n];
  }
  inline T& last_el(void) {
#ifdef ALR_CHECK_BOUND
    if (qel > 0) return el[qel - 1];
    mcerr << "ERROR in const T& DynLinArr::last_el(void) const: qel <=0:"
          << " qel" << qel << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else
    return el[qel];
#endif
  }

  inline const T& last_el(void) const {
#ifdef ALR_CHECK_BOUND
    if (qel > 0) return el[qel - 1];
    mcerr << "ERROR in const T& DynLinArr::last_el(void) const: qel <=0:"
          << " qel" << qel << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else
    return el[qel];
#endif
  }

  long get_qel(void) const { return qel; }
  void put_qel(long fqel);
  void put_qel(long fqel, const T* val, ArgInterp_SingleAdr t);
  // creates array with size fqel
  // If old array existed, then
  //   If it was less than fqel, it all is copied to new array
  //       and the other elements either assigned *val or
  //       remains not inited.
  //   else its fqel part is copyed to new array.
  // ArgInterp_SingleAdr serves to distinguish this
  // from next function when second argument
  // does not coincide with T but can be converted to it
  // (for example NULL to int and it is not clear which function
  // should be called.
  // Attention!, val is an element, this is assimetry with contructor,
  // in which ar is an array.

  void put_qel(long fqel, const T& val) {
    put_qel(fqel, &val, ArgInterp_SingleAdr());
  }
  void increment(const T* val = NULL) {
    check();
    long q = qel + 1;
    put_qel(q, *val);
  }
  void increment(const T& val) {
    check();
    long q = qel + 1;
    put_qel(q, val);
  }
  void clear(void) { put_qel(0); }  // Not only clears the content,
                                    // but makes zero dimension.
  void pilfer(const DynLinArr<T>& f) {
#ifdef DEBUG_DYNLINARR
    mcout << "DynLinArr::pilfer is called\n";
#endif
    if (this != &f) {
      if (qel != 0) {
        if (f.qel != 0) {
          mcerr << "ERROR in DynLinArr::pilfer(...):\n";
          mcerr << "Both the destination and source arrays are not empty\n";
          // For explanations why it is dengerous, see similar function
          // of ActivePtr.
          spexit(mcerr);
        } else {
          delete[] el;
          qel = 0;
        }
      }
      el = f.el;
      qel = f.qel;
      f.el = NULL;
      f.qel = 0;
    }
  }

  void check(void) const;

  /*
  void print(std::ostream& file, long qpr) const
    {
      Ifile<<"DynLinArr<T>: qel="<<get_qel()<<" qpr="<<qpr<<'\n';
      long n;
      indn.n+=2;
      for( n=0; n<qpr; n++)
      {
        Ifile<<"n="<<n<<" el[n]="<<this->DynLinArr<T>::operator[](n)<<'\n';
      }
      indn.n-=2;
    }
  */
  DynArr<T> top(void);  // transpose the vector, rotate it
  // from vertical colunm to horisontal line for the purpose
  // of linear algebra calculations.

  // friend void DLA_sort<T>(DynLinArr<T>& f);
  // Apply any function of one argument (of type T) to each element.
  template <class P>
  friend void apply1(DynLinArr<P>& ar, void (*fun)(P& f));
  // template<class P> friend void apply1m(DynLinArr<P>& ar,
  //					void (*P::fun)(void));
  // Apply any function of two arguments
  // (first of which is of type T and the second is of type of address
  // of another function (possibly apply1) to each element.
  template <class P, class X>
  friend void apply2(DynLinArr<P>& ar, void (*fun1)(P& f, void (*fun21)(X& f)),
                     void (*fun2)(X& f));

  // template<class P, class X> friend void apply2m(DynLinArr<P>& ar,
  //			void (*fun1)(P& f, void (*fun21)(X& f) ),
  //			void (*X::fun2)(void) );
  // void apply(void (*fun)(T& f))
  //{ long n; for(n=0; n<qel; n++) (*fun)(el[n]); }

  // Attention: the both sorts below at large N are much less efficient
  // then sort for standard vectors.
  // At N of the order of 50000 thay take several seconds,
  // whereas sort for vectors takes several tenths of second.

  void sort(long q_to_sort = 0);  // sorts first q_to_sort to increasing order.
  // If q_to_sort = 0, it sorts all get_qel() elements.

  void sort(DynLinArr<long>& sort_ind, long q_to_sort = 0) const;
  // to increasing order
  // Does not change array, but creates index array which gives access
  // in sorted order. For example:
  // DynLinArr< long > sort_ind;
  // collection.sort(sort_ind);
  // for(n=0; n<q; n++)
  // {
  //   collection_ordered[n] = collection[sort_ind[n]];
  // }
  // Thus content of each n-th element of sort_ind gives
  // position of n-th (by size) element of array.

  // finds and sorts minimal q_to_sort elements from all.
  void sort_select_increasing(DynLinArr<long>& sort_ind,
                              long q_to_sort = 0) const;
  void sort_select_decreasing(DynLinArr<long>& sort_ind,
                              long q_to_sort = 0) const;

  virtual DynLinArr* copy() const;

  virtual ~DynLinArr() {
    check();
    if (el) delete[] el;
  }

 private:
  mutable long qel;  // number of elements, mutable only for pilfer
  mutable T* el;     // array of qel elements, mutable only for pilfer
  //(regarding mutable and pilfer see ActivePtr for more comments).
};
template <class T>
DynLinArr<T>* DynLinArr<T>::copy() const {
  return new DynLinArr<T>(*this);  
}

template <class T>
void apply1(DynLinArr<T>& ar, void (*fun)(T& f)) {
  for (long n = 0; n < ar.qel; n++) (*fun)(ar.el[n]);
}

template <class T, class X>
void apply2(DynLinArr<T>& ar, void (*fun1)(T& f, void (*fun21)(X& f)),
            void (*fun2)(X& f)) {
  for (long n = 0; n < ar.qel; n++) (*fun1)(ar.el[n], fun2);
}

template <class T>
void DynLinArr<T>::check(void) const {
  if (qel < 0) {
    mcerr << "ERROR in template<class T> void DynLinArr<T>::check(void):\n";
    mcerr << "qel < 0, qel=" << qel << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  if (qel == 0 && el != NULL) {
    mcerr << "ERROR in template<class T> void DynLinArr<T>::check(void):\n";
    mcerr << "qel == 0 && el != NULL: el=" << el << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  if (qel > 0) {
    if (el == NULL) {
      mcerr << "ERROR in template<class T> void DynLinArr<T>::check(void):\n";
      mcerr << "qel > 0 && el == NULL: qel=" << qel << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
    if (qel > max_qel_DynLinArr) {
      mcerr << "ERROR in template<class T> void DynLinArr<T>::check(void):\n";
      mcerr << "qel > max_qel_DynLinArr: \n";
      Iprint2n(mcout, qel, max_qel_DynLinArr);
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
  }
}

template <class T>
DynLinArr<T>& DynLinArr<T>::operator=(const DynLinArr<T>& f) {
#ifdef DEBUG_DYNLINARR
  mcout << "DynLinArr<T>& DynLinArr<T>::operator=(const DynLinArr<T>& f) is "
           "working\n";
#endif
  if (this != &f) {
    // mcout<<"DynLinArr<T>& operator=(const DynLinArr<T>& f): long(el)="
    //<<long(el)<<'\n';
    check();
    f.check();
    // First of all we allocate memory and copy to there
    // all elements from f, because f can be part of this array,
    // for example, one of its elements.
    long q = f.get_qel();
    T* temp_el = (T*)NULL;
    if (q > 0) {
      temp_el = new T[q];
      for (long n = 0; n < q; n++) temp_el[n] = f.el[n];
    }
    pass(q, temp_el);
  }
  return *this;
}

template <class T>
template <class D>
DynLinArr<T>& DynLinArr<T>::operator=(const DynLinArr<D>& f) {
#ifdef DEBUG_DYNLINARR
  mcout << "DynLinArr<T>& DynLinArr<T>::operator=(const DynLinArr<D>& f) is "
           "working\n";
#endif
  // if(this != &f)
  //{
  // mcout<<"DynLinArr<T>& operator=(const DynLinArr<T>& f): long(el)="
  //<<long(el)<<'\n';
  check();
  f.check();
  // First of all we allocate memory and copy to there
  // all elements from f, because f can be part of this array,
  // for example, one of its elements.
  long q = f.get_qel();
  T* temp_el = (T*)NULL;
  if (q > 0) {
    temp_el = new T[q];
    for (long n = 0; n < q; n++) temp_el[n] = f[n];
  }
  pass(q, temp_el);
  return *this;
}

template <class T>
inline DynLinArr<T>::DynLinArr(const DynLinArr<T>& f)
    : RegPassivePtr(),
      qel(0),
      el(NULL) {
#ifdef DEBUG_DYNLINARR
  mcout << "DynLinArr(const DynLinArr<T>& f) is working\n";
#endif
  *this = f;
}

template <class T>
void DynLinArr<T>::put_qel(long fqel) {
  //
  // creates array with size fqel
  // If old array existed, then
  //   If it was less than fqel, it all is copied to new array
  //       and the other elements either assigned *val or
  //       remains not inited.
  //   else its fqel part is copyed to new array.
  // mcout<<"put_qel: *this="<<(*this);
  if (fqel < 0) {
    mcerr << "ERROR in template<class T> void DynLinArr<T>::put_qel(long "
             "fqel):\n";
    mcerr << "fqel < 0, fqel=" << fqel << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  check();
  if (el == NULL) {
    qel = fqel;
    if (qel > 0) el = new T[fqel];
  } else {
    if (qel != fqel) {
      if (fqel <= 0) {
        qel = 0;
        delete[] el;
        el = NULL;
      } else {
        T* elh;
        elh = new T[fqel];  
        for (long n = 0; n < fqel; ++n) {
          if (n < qel) elh[n] = el[n];
        }
        delete[] el;
        el = elh;
        qel = fqel;
      }
    }
  }
}

template <class T>
void DynLinArr<T>::put_qel(long fqel, const T* val, ArgInterp_SingleAdr t) {
  // By default val == NULL
  // creates array with size fqel
  // If old array existed, then
  //   If it was less than fqel, it all is copied to new array
  //       and the other elements either assigned *val or
  //       remains not inited.
  //   else its fqel part is copyed to new array.
  // mcout<<"put_qel: *this="<<(*this);
  if (fqel < 0) {
    mcerr << "ERROR in template<class T> void DynLinArr<T>::put_qel(long fqel, "
             "const T* val, ArgInterp_SingleAdr):\n";
    mcerr << "fqel < 0, fqel=" << fqel << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    // Avoid compiler warning because of unused variable t (HS).
    mcerr << sizeof(t) << "\n";
  }
  check();
  if (el == NULL) {
    qel = fqel;
    if (qel > 0) el = new T[fqel];
    if (val != NULL)
      for (long n = 0; n < qel; ++n) el[n] = *val;
  } else {
    if (qel != fqel) {
      if (fqel <= 0) {
        qel = 0;
        delete[] el;
        el = NULL;
      } else {
        T* elh;
        elh = new T[fqel];
        for (long n = 0; n < fqel; ++n) {
          if (n < qel)
            elh[n] = el[n];
          else if (val != NULL)
            elh[n] = *val;
        }
        delete[] el;
        el = elh;
        qel = fqel;
      }
    }
  }
}

// Simple sorting routine optimized for DynLiArr. )ptimized means
// not doing checks at each indexing, but it is not the best for the large
// number of N.
// Note that it can be made better if at choosing n_possible_next
// to arrange going backward. But this is much more complicated so currently
// I am not going to do this.

template <class T>
void DynLinArr<T>::sort(long q_to_sort) {
  mfunnamep("void DynLinArr<T>::sort(long q_to_sort = 0)");

  check_econd12(q_to_sort, >, qel, mcerr);
  if (q_to_sort <= 0) q_to_sort = qel;
  if (q_to_sort <= 1) return;

  long n_possible_next = 1;
  long q_comp = 0;
  long n, m;
  for (n = 0; n < q_to_sort - 1; n++) {
    // Iprint2n(mcout, n, n_possible_next);
    // first it finds the minimum along the rest and replaces if it is less
    // long nmin = n+1;
    long nmin = n_possible_next;
    T el_min = el[nmin];
    int s_change_possible_next = 0;

    // for(m=n+2; m<q_to_sort; m++)
    for (m = n_possible_next + 1; m < q_to_sort; m++) {
      q_comp++;
      // if(el[nmin] > el[m])
      if (el_min > el[m]) {
        n_possible_next = nmin;
        s_change_possible_next = 1;
        nmin = m;
        el_min = el[nmin];
      }
    }
    // Iprint4n(mcout, n_possible_next, s_change_possible_next, nmin, el_min);
    if (s_change_possible_next == 0 || n_possible_next < n + 2) {
      n_possible_next = n + 2;
    }
    // if(el[n] > el[nmin])
    //{
    //  T t = el[nmin];
    //  el[nmin] = el[n];
    //  el[n] = t;
    //}
    // Iprintn(mcout, n_possible_next);
    // Iprint2n(mcout, el[n], el_min);
    if (el[n] > el_min) {
      if (s_change_possible_next == 1) {
        // if(n_possible_next < q_to_sort)
        if (n_possible_next < q_to_sort && el[n] < el[n_possible_next]) {
          n_possible_next = nmin;
        }
      }
      // mcout<<"replacing el[n] and el_min\n";
      T t = el_min;
      el[nmin] = el[n];
      el[n] = t;
    }
    // Iprintn(mcout, (*this));
  }
  // Iprintn(mcout, q_comp);
}

// New variant, should be faster, the old is below.

template <class T>
void DynLinArr<T>::sort(DynLinArr<long>& sort_ind, long q_to_sort) const {
  mfunnamep(
      "void DynLinArr<T>::sort(DynLinArr< long >& sort_ind, long "
      "q_to_sort = 0) const");

  check_econd12(q_to_sort, >, qel, mcerr);
  if (q_to_sort <= 0) q_to_sort = qel;
  // if(q_to_sort <= 1) return;

  sort_ind.clear();
  sort_ind.pilfer(DynLinArr<long>(q_to_sort));
  long n, m;
  for (n = 0; n < q_to_sort; n++) {
    sort_ind.acu(n) = n;
  }
  if (q_to_sort <= 1) return;

  long n_possible_next = 1;

  for (n = 0; n < q_to_sort - 1; n++) {
    // first it finds the minimum along the rest and replaces if it is less
    long nmin = n_possible_next;
    long ind_nmin = sort_ind.acu(nmin);
    int s_change_possible_next = 0;

    // for(m=n+2; m<q_to_sort; m++)
    for (m = n_possible_next + 1; m < q_to_sort; m++) {
      if (el[ind_nmin] > el[sort_ind.acu(m)])
          // if(el[sort_ind.acu(nmin)] > el[sort_ind.acu(m)])
      {
        n_possible_next = nmin;
        s_change_possible_next = 1;
        nmin = m;
        ind_nmin = sort_ind.acu(nmin);
      }
    }
    if (s_change_possible_next == 0 || n_possible_next < n + 2) {
      n_possible_next = n + 2;
    }
    if (el[sort_ind.acu(n)] > el[ind_nmin])
        // if(el[sort_ind.acu(n)] > el[sort_ind.acu(nmin)])
    {
      if (s_change_possible_next == 1) {
        if (n_possible_next < q_to_sort &&
            el[sort_ind.acu(n)] < el[sort_ind.acu(n_possible_next)]) {
          n_possible_next = nmin;
        }
      }
      // long t = sort_ind.acu(nmin);
      sort_ind.acu(nmin) = sort_ind.acu(n);
      sort_ind.acu(n) = ind_nmin;
    }
  }
}

template <class T>
void DynLinArr<T>::sort_select_increasing(DynLinArr<long>& sort_ind,
                                          long q_to_sort) const {
  mfunnamep(
      "void DynLinArr<T>::sort_select_increasing(DynLinArr< long >& "
      "sort_ind, long q_to_sort = 0) const");

  check_econd12(q_to_sort, >, qel, mcerr);
  long s_last_noninc = 0;
  if (q_to_sort <= 0) {
    q_to_sort = qel;
    s_last_noninc = 1;
  } else if (q_to_sort == qel) {
    s_last_noninc = 1;
  }

  // if(qel <= 1) return;
  sort_ind.clear();
  sort_ind.pilfer(DynLinArr<long>(qel));
  long n, m;
  for (n = 0; n < qel; n++) {
    sort_ind[n] = n;
  }
  if (q_to_sort <= 1) return;

  long n_possible_next = 1;

  for (n = 0; n < q_to_sort - s_last_noninc; n++) {
    // first it finds the minimum along the rest and replaces if it is less
    long nmin = n_possible_next;
    long ind_nmin = sort_ind[nmin];
    int s_change_possible_next = 0;

    // for(m=n+2; m<q_to_sort; m++)
    for (m = n_possible_next + 1; m < qel; m++) {
      if (el[ind_nmin] > el[sort_ind[m]])
          // if(el[sort_ind.acu(nmin)] > el[sort_ind.acu(m)])
      {
        n_possible_next = nmin;
        s_change_possible_next = 1;
        nmin = m;
        ind_nmin = sort_ind[nmin];
      }
    }
    if (s_change_possible_next == 0 || n_possible_next < n + 2) {
      n_possible_next = n + 2;
    }
    if (el[sort_ind[n]] > el[ind_nmin])
        // if(el[sort_ind.acu(n)] > el[sort_ind.acu(nmin)])
    {
      if (s_change_possible_next == 1) {
        if (n_possible_next < q_to_sort &&
            el[sort_ind[n]] < el[sort_ind[n_possible_next]]) {
          n_possible_next = nmin;
        }
      }
      // long t = sort_ind.acu(nmin);
      sort_ind[nmin] = sort_ind[n];
      sort_ind[n] = ind_nmin;
    }
  }
  sort_ind.put_qel(qel);
}

template <class T>
void DynLinArr<T>::sort_select_decreasing(DynLinArr<long>& sort_ind,
                                          long q_to_sort) const {
  mfunnamep(
      "void DynLinArr<T>::sort_select_decreasing(DynLinArr< long >& "
      "sort_ind, long q_to_sort = 0) const");

  check_econd12(q_to_sort, >, qel, mcerr);
  long s_last_noninc = 0;
  if (q_to_sort <= 0) {
    q_to_sort = qel;
    s_last_noninc = 1;
  } else if (q_to_sort == qel) {
    s_last_noninc = 1;
  }
  // Iprintn(mcout, q_to_sort);

  // if(qel <= 1) return;
  sort_ind.clear();
  sort_ind.pilfer(DynLinArr<long>(qel));
  long n, m;
  for (n = 0; n < qel; n++) {
    sort_ind[n] = n;
  }
  if (q_to_sort <= 1) return;

  long n_possible_next = 1;

  for (n = 0; n < q_to_sort - s_last_noninc; n++) {
    // Iprintn(mcout, n);
    // first it finds the minimum along the rest and replaces if it is less
    long nmax = n_possible_next;
    // Iprintn(mcout, nmax);
    long ind_nmax = sort_ind[nmax];
    int s_change_possible_next = 0;

    // for(m=n+2; m<q_to_sort; m++)
    for (m = n_possible_next + 1; m < qel; m++) {
      // Iprint3n(mcout, ind_nmax, m, sort_ind[m]);
      if (el[ind_nmax] < el[sort_ind[m]]) {
        n_possible_next = nmax;
        s_change_possible_next = 1;
        nmax = m;
        // Iprintn(mcout, nmax);
        ind_nmax = sort_ind[nmax];
      }
    }
    if (s_change_possible_next == 0 || n_possible_next < n + 2) {
      n_possible_next = n + 2;
    }
    // Iprint4n(mcout, n, sort_ind[n], ind_nmax, el[ind_nmax]);
    if (el[sort_ind[n]] < el[ind_nmax]) {
      if (s_change_possible_next == 1) {
        if (n_possible_next < q_to_sort &&
            el[sort_ind[n]] > el[sort_ind[n_possible_next]]) {
          n_possible_next = nmax;
        }
      }
      // long t = sort_ind.acu(nmin);
      sort_ind[nmax] = sort_ind[n];
      sort_ind[n] = ind_nmax;
    }
  }
  sort_ind.put_qel(qel);
}

/*
template<class T>
void DynLinArr<T>::sort(DynLinArr< long >& sort_ind, long q_to_sort) const
{
  mfunnamep("void DynLinArr<T>::sort(DynLinArr< long >& sort_ind, long q_to_sort
= 0) const");

  check_econd12(q_to_sort , > , qel , mcerr);
  if(q_to_sort <= 0) q_to_sort = qel;
  if(q_to_sort <= 1) return;

  //if(qel <= 1) return;
  sort_ind.clear();
  sort_ind.pilfer(DynLinArr< long >(q_to_sort));
  long n,m;
  for(n=0; n<q_to_sort; n++)
  {
    sort_ind.acu(n) = n;
  }

  for(n=0; n<q_to_sort-1; n++)
  {
    // first it finds the minimum along the rest and replaces if it is less
    long nmin = n+1;
    long ind_nmin = sort_ind.acu(nmin);
    for(m=n+2; m<q_to_sort; m++)
    {
      if(el[ind_nmin] > el[sort_ind.acu(m)])
      //if(el[sort_ind.acu(nmin)] > el[sort_ind.acu(m)])
      {
        nmin = m;
        ind_nmin = sort_ind.acu(nmin);
      }
    }
    if(el[sort_ind.acu(n)] > el[ind_nmin])
      //if(el[sort_ind.acu(n)] > el[sort_ind.acu(nmin)])
    {
      //long t = sort_ind.acu(nmin);
      sort_ind.acu(nmin) = sort_ind.acu(n);
      sort_ind.acu(n) = ind_nmin;
    }
  }
}
*/

/*
template<T> void DLA_sort(DynLinArr<T>& f)
{
  mfunnamep("template<T> void DLA_sort(DynLinArr<T>& f)");

  long q = f.get_qel();
  if(q <= 1) return;
  long n,m;
  T* a = &(f[0]);
  for(n=0; n<q-1; n++)
  {
    for(m=n+1; m<q; m++)
    {
      if(a[n] > a[m])
      {
        T t = a[m];
        a[m] = a[n];
        a[n] = t;
      }
    }
  }
}
*/
/*
//Somewhy this doen not work prolerly if T is template class itself
//AbsCont_ts.c:167: type unification failed for function template
//`template <class T> long int append_TreeNode(TreeNode<...> &,
//TreeNode<...> &, long int &, T * = 0, long int = 0)'
*/
/*
This is not compiled in Solaris:
Error: Function templates may not have default parameters.
*/
#ifndef BAN_DEFAULT_PAR_FUN_TEMPL
template <class T>
long append(const T& t,         // value to assign
            DynLinArr<T>& dla,  // array to whose elements this value is to be
            // assigned
            long& qael,  // input: index of element to which it will be assigned
            // if qael == get_qel(), the qel will be increased
            // to either new_qel or by 3 times.
            // But if qael > get_qel(), it is considered as error.
            // output: the next index
            T* tempt = NULL,  // data filled to extra elements
            long new_qel = 0  // recommended new size
            ) {
  if (dla.get_qel() < qael) {
    mcerr << "ERROR in long append(class DynLinArr& dla, ...): dla.get_qel() < "
             "qael\n"
          << "dla.get_qel()=" << dla.get_qel() << " qael=" << qael << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  if (dla.get_qel() == qael) {
    if (new_qel <= qael) new_qel = std::max(3 * qael, long(3));
    dla.put_qel(new_qel, tempt, ArgInterp_SingleAdr());
  }
  dla[qael++] = t;
  return qael;
}

#else

#define append_to_DynLinArr(t, dla, qael, tempt, new_qel)                     \
  {                                                                           \
    if (dla.get_qel() < qael) {                                               \
      mcerr << "append(class DynLinArr& dla, ...): dla.get_qel() < qael, "    \
            << "dla.get_qel()=" << dla.get_qel() << " qael=" << qael << '\n'; \
      spexit(mcerr);                                                          \
    }                                                                         \
    int nn = new_qel;                                                         \
    if (dla.get_qel() == qael) {                                              \
      if (new_qel <= qael) nn = std::max(3 * qael, long(3));                  \
      dla.put_qel(nn, tempt);                                                 \
    }                                                                         \
    dla[qael++] = t;                                                          \
  }
#define append_to_DynLinArr_short(t, dla, qael)                               \
  {                                                                           \
    if (dla.get_qel() < qael) {                                               \
      mcerr << "append(class DynLinArr& dla, ...): dla.get_qel() < qael, "    \
            << "dla.get_qel()=" << dla.get_qel() << " qael=" << qael << '\n'; \
      spexit(mcerr);                                                          \
    }                                                                         \
    int nn = 0;                                                               \
    if (dla.get_qel() == qael) {                                              \
      nn = std::max(3 * qael, long(3));                                       \
      dla.put_qel(nn, NULL);                                                  \
    }                                                                         \
    dla[qael++] = t;                                                          \
  }
#endif

template <class T>
std::ostream& operator<<(std::ostream& file, const DynLinArr<T>& f) {
  // mfunnamep("template<class T> std::ostream& operator<<(std::ostream& file,
  // const DynLinArr<T>& f)");
  // mcout<<"operator<<(std::ostream& file, const DynLinArr<T>& f) is
  // started\n";
  Ifile << "DynLinArr<T>: qel=" << f.get_qel() << '\n';
  f.check();
  long n;
  indn.n += 2;
  for (n = 0; n < f.get_qel(); n++) {
    // Ifile<<"n="<<n<<" el[n]="<<noindent<<f[n]<<yesindent<<'\n';
    if (s_short_output == 0) {
      Ifile << "n=" << n << " el[n]=";
    }
    std::ostringstream ost;
    ost << indn << noindent << f[n] << yesindent;
    put_one_n(ost);
    file << ost.str();
  }
  // file<<yesindent;
  indn.n -= 2;
  return file;
}



template <class T>
std::istream& operator>>(std::istream& file, DynLinArr<T>& f) {
  mfunnamep(
      "template<class T> istream& operator>>(istream& file, DynLinArr<T>& f)");
  // mcout<<"operator<<(std::ostream& file, const DynLinArr<T>& f) is
  // started\n";
  definp_endpar dep(&file, 0, 1, 0, s_short_output);
  long qel = 0;
  dep.s_short = 0;
  DEFINPAP(qel);
  dep.s_short = s_short_output;
  check_econd11(qel, < 0, mcerr);
  f.clear();
  f.put_qel(qel);
  long fn;
  for (fn = 0; fn < qel; fn++) {
    if (s_short_output == 0) {
      long n = 0;
      DEFINPAP(n);
      check_econd12(fn, !=, n, mcerr);
    }
    // set_position("el[n]=", *dep.istrm, dep.s_rewind, dep.s_req_sep);
    // file >> f[n];
    definp_any_par(f[fn], "el[n]=", dep);
  }
  return file;
}

// Commented out unused function (hschindl)
/*
template<class T>
void output_DynLinArr(std::ostream& file, const DynLinArr<T>& f, int l, long q)
{
  //mfunnamep("template<class T> void output_DynLinArr(std::ostream& file, const
DynLinArr<T>& f, int l, long q)");
  Ifile<<"DynLinArr<T>: qel="<<f.get_qel()<<" q to print is "<<q<<'\n';
  f.check();
  if(q>f.get_qel())
  {
    mcerr<<"output_DynLinArr(...): q>f.get_qel(), q="<<q
         <<" f.get_qel()="<<f.get_qel()<<'\n';
    mcerr<<"Type of T is (in internal notations) "<<typeid(T).name()<<'\n';
    spexit(mcerr);
  }
  long n;
  indn.n+=2;
  for( n=0; n<q; n++)
  {
    //Ifile<<"n="<<n<<" el[n]="<<noindent<<f[n]<<yesindent<<'\n';
    Ifile<<"n="<<n<<" el[n]="<<noindent;
    std::ostringstream ost;
    ost<<f[n]<<yesindent;
    put_one_n(ost);
    file<<ost.str();
  }
  //file<<yesindent;
  indn.n-=2;
}
*/

template <class T>
void print_DynLinArr(std::ostream& file, const DynLinArr<T>& f, int l) {
  // mfunnamep("template<class T> void print_DynLinArr(std::ostream& file, const
  // DynLinArr<T>& f, int l)");
  Ifile << "DynLinArr<T>: qel=" << f.get_qel() << '\n';
  f.check();
  long n;
  indn.n += 2;
  for (n = 0; n < f.get_qel(); n++) {
    // Ifile<<"n="<<n<<" el[n]="<<noindent; f[n].print(file, l);
    // file<<yesindent;
    Ifile << "n=" << n << " el[n]=" << noindent;
    std::ostringstream ost;
    f[n].print(ost, l);
    ost << yesindent;
    put_one_n(ost);
    file << ost.str();
  }
  indn.n -= 2;
}

template <class T>
void print_DynLinArr(std::ostream& file, const DynLinArr<T>& f, int l, long q) {
  // mfunnamep("template<class T> void print_DynLinArr(std::ostream& file, const
  // DynLinArr<T>& f, int l, long q)");
  Ifile << "DynLinArr<T>: qel=" << f.get_qel() << " q to print is " << q
        << '\n';
  f.check();
  if (q > f.get_qel()) {
    mcerr << "print_DynLinArr(...): q>f.get_qel(), q=" << q
          << " f.get_qel()=" << f.get_qel() << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  long n;
  indn.n += 2;
  for (n = 0; n < q; n++) {
    // Ifile<<"n="<<n<<" el[n]="<<noindent; f[n].print(file, l);
    // file<<yesindent;
    Ifile << "n=" << n << " el[n]=" << noindent;
    std::ostringstream ost;
    f[n].print(ost, l);
    ost << yesindent;
    put_one_n(ost);
    file << ost.str();
  }
  indn.n -= 2;
}

template <class T>
void print_adr_DynLinArr(std::ostream& file, const DynLinArr<T>& f, int l,
                         long q) {
  // mfunnamep("template<class T> void print_adr_DynLinArr(std::ostream& file,
  // const DynLinArr<T>& f, int l, long q)");
  Ifile << "DynLinArr<T>: qel=" << f.get_qel() << " q to print is " << q
        << '\n';
  f.check();
  if (q > f.get_qel()) {
    mcerr << "print_adr_DynLinArr(...): q>f.get_qel(), q=" << q
          << " f.get_qel()=" << f.get_qel() << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  long n;
  indn.n += 2;
  for (n = 0; n < q; n++) {
    // Ifile<<"n="<<n<<" el[n]="<<noindent; f[n]->print(file, l);
    // file<<yesindent;
    Ifile << "n=" << n << " el[n]=" << noindent;
    std::ostringstream ost;
    f[n]->print(ost, l);
    ost << yesindent;
    put_one_n(ost);
    file << ost.str();
  }
  indn.n -= 2;
}

const int pq_arrelem_in_line = 5;

void print_DynLinArr_int(std::ostream& file, const DynLinArr<int>& f);
void print_DynLinArr_long(std::ostream& file, const DynLinArr<long>& f);
void print_DynLinArr_float(std::ostream& file, const DynLinArr<float>& f);
void print_DynLinArr_double(std::ostream& file, const DynLinArr<double>& f);
// See AbsArrD for similar function with DoubleAc

void print_DynLinArr_double2(std::ostream& file, const DynLinArr<double>& f1,
                             const DynLinArr<double>& f2);
// Print two arrays in two colums side-by-side.
// Good for arrays of equal dimensions.

void print_DynLinArr_int_double(std::ostream& file, const DynLinArr<int>& iar,
                                const DynLinArr<double>& dar);

void print_DynLinArr_int_double3(std::ostream& file, const DynLinArr<int>& iar,
                                 const DynLinArr<double>& dar1,
                                 const DynLinArr<double>& dar2,
                                 const DynLinArr<double>& dar3);
// Print 4 arrays in two colums side-by-side.
// Good for arrays of equal dimensions.

#define Iprintdla_int(file, name)           \
  file << indn << #name << "=" << noindent; \
  print_DynLinArr_int(file, name);
#define Iprintdla_long(file, name)          \
  file << indn << #name << "=" << noindent; \
  print_DynLinArr_long(file, name);
#define Iprintdla_float(file, name)         \
  file << indn << #name << "=" << noindent; \
  print_DynLinArr_float(file, name);
#define Iprintdla_double(file, name)        \
  file << indn << #name << "=" << noindent; \
  print_DynLinArr_double(file, name);
// See AbsArrD for similar function with DoubleAc

template <class T, class X>
void copy_DynLinArr(const T& s, X& d) {
  mfunnamep("template<class T, class X> void copy_DynLinArr(const T& s, X& d)");
  s.check();
  d.check();
  long q = s.get_qel();
  d.put_qel(q);
  long n;
  for (n = 0; n < q; n++) {
    d[n] = s[n];
  }
}

// Covert to another compatible type
template <class T, class X>
void convert_DynLinArr(const T& s, X& d) {
  long q = s.get_qel();
  d.put_qel(q);
  long n;
  for (n = 0; n < q; n++) {
    d[n] = X(s[n]);
  }
}

template <class T>
void put_qel_1(DynLinArr<T>& f, long fq)  // change dimensions of arrays
    // which are elements of the main one
{
  long q = f.get_qel();
  long n;
  for (n = 0; n < q; n++) f[n].put_qel(fq);
}

template <class T, class T1>
void assignAll_1(DynLinArr<T>& f, const T1& ft)  // assign ft to all elements
    // of arrays which are elements of the main one
{
  long q = f.get_qel();
  long n;
  for (n = 0; n < q; n++) f[n].assignAll(ft);
}

template <class T>
class IterDynLinArr {
 public:
  IterDynLinArr(DynLinArr<T>* fdar) : dar(fdar), ncur(-1) { ; }
  T* more(void) {
    if (ncur < dar->get_qel() - 1)
      return &((*dar)[++ncur]);
    else
      ncur = dar->get_qel();
    return NULL;
  }
  T* current(void) {
    if (ncur >= 0 || ncur < dar->get_qel())
      return &((*dar)[ncur]);
    else
      return NULL;
  }
  T* less(void)  // switch current to previous
  {
    if (ncur >= 1)
      return &((*dar)[--ncur]);
    else
      ncur = -1;
    return NULL;
  }
  long get_ncur(void) { return ncur; }

 private:
  long ncur;
  DynLinArr<T>* dar;
};

int gconfirm_ind(const DynLinArr<long>& qel, const DynLinArr<long>& ind);
int gconfirm_ind_ext(const DynLinArr<long>& qel, const DynLinArr<long>& ind);

// The following function checks whether the arrays or their first qfirst
// elements are equal.
// In the first case this means that their dimensions and all elements
// should be equal to each other.
// In the second case if either of the dimensions is less than qfirst,
// the result is negative (0), so the arrays are considered different.
// If the dimensions are equal or more than qfirst,
// the function compares that number of first elements.
// If to put here just equal, the program may not be compiled since
// instead of this function the program will insert
// template <class InputIterator1, class InputIterator2>
// inline bool equal(InputIterator1 first1, InputIterator1 last1,
//		  InputIterator2 first2) {
// Therefore I substituted it to ifequal.
template <class T>
int ifequal(const DynLinArr<T>& fd1, const DynLinArr<T>& fd2,
            long qfirst = -1) {
  long n;
  if (qfirst == -1) {
    if ((qfirst = fd1.get_qel()) != fd2.get_qel()) return 0;
    // qfirst=fd1.get_qel();
  } else {
    if (qfirst > fd1.get_qel() || qfirst > fd2.get_qel()) return 0;
  }
  // Iprintn(mcout, qfirst);
  for (n = 0; n < qfirst; n++) {
    // Iprint3n(mcout, n, fd1[n], fd2[n]);
    if (!(fd1[n] == fd2[n])) return 0;
  }
  return 1;
}

// The same as above, but compares DynLinArr with ordinary array.
template <class T>
int ifequal(const DynLinArr<T>& fd1, const T* fd2, long qfirst = -1) {
  long n;
  if (qfirst == -1) {
    qfirst = fd1.get_qel();
  } else {
    if (qfirst > fd1.get_qel()) return 0;
  }
  for (n = 0; n < qfirst; n++)
    if (!(fd1[n] == fd2[n])) return 0;
  return 1;
}

template <class T>
int ifequal(T* fd1, T* fd2, long qfirst) {
  long n;
  for (n = 0; n < qfirst; n++)
    if (!(fd1[n] == fd2[n])) return 0;
  return 1;
}

template <class T>
DynLinArr<T> merge(const DynLinArr<T>& fd1, long qfd1, const DynLinArr<T>& fd2,
                   long qfd2) {
  long n;
  if (qfd1 < 0) {
    qfd1 = fd1.get_qel();
  }
  if (qfd2 < 0) {
    qfd2 = fd2.get_qel();
  }
  DynLinArr<T> ret(qfd1 + qfd2);
  if (qfd1 + qfd2 == 0) return ret;
  for (n = 0; n < qfd1; n++) {
    ret[n] = fd1[n];
  }
  for (n = 0; n < qfd2; n++) {
    ret[qfd1 + n] = fd2[n];
  }
  return ret;
}

template <class T>
class DynArr : public RegPassivePtr {
 public:
  // Constructors
  DynArr(void) { ; }
  // For one-dimensional array:
  explicit DynArr(long fqel, T* val = NULL)
      : qel(DynLinArr<long>(1)),
        cum_qel(DynLinArr<long>(1)),
        el(DynLinArr<T>(fqel)) {
#ifdef DEBUG_DYNARR
    mcout << "explicit DynArr(long fqel, T* val=NULL) is called\n";
#endif
    qel[0] = fqel;
    cum_qel[0] = 1;
    if (val != NULL) assignAll(*val);
  }
  DynArr(long fqel, T val, ArgInterp_Val /*t*/)
      : qel(DynLinArr<long>(1)),
        cum_qel(DynLinArr<long>(1)),
        el(DynLinArr<T>(fqel)) {
#ifdef DEBUG_DYNARR
    mcout << "explicit DynArr(long fqel, T* val=NULL) is called\n";
#endif
    qel[0] = fqel;
    cum_qel[0] = 1;
    assignAll(val);
  }
  DynArr(long fqel, const T* ar, ArgInterp_Arr /*t*/)
      : qel(DynLinArr<long>(1)), cum_qel(DynLinArr<long>(1)), el(fqel) {
    qel[0] = fqel;
    cum_qel[0] = 1;
    long n;
    for (n = 0; n < fqel; n++) el.acu(n) = ar[n];
  }
  // Another variant for one-dimensional array
  // Attention: if the T is long, this might be mixed with array of dimensions.
  // To avoid this the latter should be accompanied by address, see below.
  DynArr(const DynLinArr<T>& f)
      : qel(DynLinArr<long>(1)), cum_qel(DynLinArr<long>(1)), el(f.get_qel()) {
    qel[0] = f.get_qel();
    cum_qel[0] = 1;
    for (long n = 0; n < qel[0]; n++) ac(n) = f[n];
  }

  // For two-dimensional array:
  DynArr(long fqel1, long fqel2, T* val = NULL)
      : qel(DynLinArr<long>(2)),
        cum_qel(DynLinArr<long>(2)),
        el(fqel1 * fqel2) {
#ifdef DEBUG_DYNARR
    mcout << "DynArr(long fqel1, long fqel2, T* val=NULL) is called\n";
#endif
    qel[0] = fqel1;
    qel[1] = fqel2;
    cum_qel[0] = fqel2;
    cum_qel[1] = 1;
    if (val != NULL) assignAll(*val);
  }

  DynArr(long fqel1, long fqel2, T val, ArgInterp_Val /*t*/)
      : qel(DynLinArr<long>(2)),
        cum_qel(DynLinArr<long>(2)),
        el(fqel1 * fqel2) {
#ifdef DEBUG_DYNARR
    mcout
        << "DynArr(long fqel1, long fqel2, T val, ArgInterp_Val t) is called\n";
#endif
    qel[0] = fqel1;
    qel[1] = fqel2;
    cum_qel[0] = fqel2;
    cum_qel[1] = 1;
    assignAll(val);
  }

  // For three-dimensional array:
  DynArr(long fqel1, long fqel2, long fqel3, T* val = NULL)
      : qel(DynLinArr<long>(3)),
        cum_qel(DynLinArr<long>(3)),
        el(fqel1 * fqel2 * fqel3) {
    qel[0] = fqel1;
    qel[1] = fqel2;
    qel[2] = fqel3;
    cum_qel[0] = fqel2 * fqel3;
    cum_qel[1] = fqel3;
    cum_qel[2] = 1;
    if (val != NULL) assignAll(*val);
  }

  // For four-dimensional array:
  DynArr(long fqel1, long fqel2, long fqel3, long fqel4, T* val = NULL)
      : qel(DynLinArr<long>(4)),
        cum_qel(DynLinArr<long>(4)),
        el(fqel1 * fqel2 * fqel3 * fqel4) {
    qel[0] = fqel1;
    qel[1] = fqel2;
    qel[2] = fqel3;
    qel[3] = fqel4;
    cum_qel[0] = fqel2 * fqel3 * fqel4;
    cum_qel[1] = fqel3 * fqel4;
    cum_qel[2] = fqel4;
    cum_qel[3] = 1;
    if (val != NULL) assignAll(*val);
  }

  // Default value is removed in order to avoid confusions with copy
  // constructor from DynLinArr at T = long.
  // If initialization of values is not necessary, just put NULL as argument.
  // It creates array with structure determined by fqel.

  DynArr(const DynLinArr<long>& fqel, T val, ArgInterp_Val /*t*/) : qel(fqel) {
    long qdim = qel.get_qel();
    if (qdim <= 0) return;
    cum_qel.put_qel(qdim);
    long ndim;
    long size = qel[0];
    for (ndim = 1; ndim < qdim; ndim++) size *= qel[ndim];
    el.put_qel(size);
    cum_qel[qdim - 1] = 1;
    for (ndim = qdim - 2; ndim >= 0; ndim--)
      cum_qel[ndim] = qel[ndim + 1] * cum_qel[ndim + 1];
    assignAll(val);
  }

  explicit DynArr(const DynLinArr<long>& fqel, T* val) : qel(fqel) {
    // fqel: array of dimensions
    // val: address of value to fill, may be NULL
    long qdim = qel.get_qel();
    if (qdim <= 0) return;
    cum_qel.put_qel(qdim);
    long ndim;
    long size = qel[0];
    for (ndim = 1; ndim < qdim; ndim++) size *= qel[ndim];
    el.put_qel(size);
    cum_qel[qdim - 1] = 1;
    for (ndim = qdim - 2; ndim >= 0; ndim--)
      cum_qel[ndim] = qel[ndim + 1] * cum_qel[ndim + 1];
    if (val != NULL) assignAll(*val);
  }

  DynArr(const DynArr<T>& f) : RegPassivePtr() {
#ifdef DEBUG_DYNARR
    mcout << "DynArr(const DynArr<T>& f) is working\n";
#endif
    *this = f;
  }
  DynArr(const DynArr<T>& f, Pilfer)
      : qel(f.qel, steal), cum_qel(f.cum_qel, steal), el(f.el, steal) {
#ifdef DEBUG_DYNARR
    mcout << "DynArr( DynArr<T>& f, Pilfer) is working\n";
#endif
  }

  void pilfer(const DynArr<T>& f) {
#ifdef DEBUG_DYNARR
    mcout << "DynArr::pilfer is called\n";
#endif
    if (this != &f) {
      if (qel.get_qel() != 0) {
        if (f.qel.get_qel() != 0) {
          mcerr << "ERROR in DynArr::pilfer(...):\n";
          mcerr << "Both the destination and source arrays are not empty\n";
          // For explanations why it is dangerous, see similar function
          // of ActivePtr.
          spexit(mcerr);
        } else {
          qel.clear();
          cum_qel.clear();
          el.clear();
        }
      }
      qel.pilfer(f.qel);
      cum_qel.pilfer(f.cum_qel);
      el.pilfer(f.el);
      f.qel.clear();
      f.cum_qel.clear();
      f.el.clear();
    }
  }

  DynArr<T>& operator=(const DynArr<T>& f);
  template <class D>
  DynArr<T>& operator=(const DynArr<D>& f);

  void pass(long q, DynLinArr<long> fqel, DynLinArr<long> fcum_qel, T* fel)
      // Do not call directly! Is to be used only
      // from assignment operator above
  {
    clear();
    qel = fqel;
    cum_qel = fcum_qel;
    el.pass(q, fel);
  }

  // Auxiliary class that provides indexing through ordinary way
  // It is perhaps quite slow.
  template <class D>
  class IndexingProvider {
   public:
    DynArr<D>& arr;
    mutable long q_deref_ind;
    mutable long current_pos;
    IndexingProvider(DynArr<D>& farr, long fq_deref_ind, long fcurrent_pos)
        : arr(farr), q_deref_ind(fq_deref_ind), current_pos(fcurrent_pos) {}
    operator D&() const {
      if (q_deref_ind != arr.qel.get_qel()) {
        mcerr << "ERROR in IndexingProvider::operator D& (): q_deref_ind != "
                 "qel.get_qel()\n";
        Iprint2n(mcerr, q_deref_ind, arr.qel.get_qel());
        mcerr << "Type of T is (in internal notations) " << typeid(D).name()
              << '\n';
        spexit(mcerr);
      }
#ifdef ALR_CHECK_BOUND
      return arr.el[current_pos];
#else
      return arr.el.acu(current_pos);
#endif
    }
    D& operator=(const D& f) {
      if (q_deref_ind != arr.get_qel().get_qel()) {
        mcerr << "ERROR in T& IndexingProvider::operator=(T& f): q_deref_ind "
                 "!= arr.get_qel().get_qel()\n";
        Iprint2n(mcerr, q_deref_ind, arr.get_qel().get_qel());
        mcerr << "Type of T is (in internal notations) " << typeid(D).name()
              << '\n';
        spexit(mcerr);
      }
#ifdef ALR_CHECK_BOUND
      arr.ac_lin(current_pos) = f;
#else
      arr.acu_lin(current_pos) = f;
#endif
      // return arr.el[current_pos];
    }

    inline IndexingProvider<D>& operator[](long n) {
      if (q_deref_ind >= arr.get_qel().get_qel()) {
        mcerr << "ERROR in DynArr::IndexingProvider& DynArr::operator[](long "
                 "n): q_deref_ind >= arr.get_qel().get_qel()\n";
        Iprint2n(mcerr, q_deref_ind, arr.get_qel().get_qel());
        mcerr << "Type of T is (in internal notations) " << typeid(D).name()
              << '\n';
        spexit(mcerr);
      }
#ifdef ALR_CHECK_EACH_BOUND
      if (n >= 0 && n < arr.qel.acu(q_deref_ind)) {
#endif
        current_pos += n * arr.get_cum_qel().acu(q_deref_ind);
        q_deref_ind++;
        return *this;
#ifdef ALR_CHECK_EACH_BOUND
      } else {
        mcerr << "Error in IndexingProvider<D>& "
                 "IndexingProvider::operator[](long n): n < 0 || n >= "
                 "qel.acu(q_deref_ind)\n";
        Iprint2n(mcout, n, arr.qel.acu(q_deref_ind));
        mcerr << "Type of T is (in internal notations) " << typeid(T).name()
              << '\n';
        spexit(mcerr);
      }
#endif
    }

    inline const IndexingProvider<D>& operator[](long n) const {
      if (q_deref_ind >= arr.get_qel().get_qel()) {
        mcerr << "ERROR in DynArr::IndexingProvider& DynArr::operator[](long "
                 "n): q_deref_ind >= arr.get_qel().get_qel()\n";
        Iprint2n(mcerr, q_deref_ind, arr.get_qel().get_qel());
        mcerr << "Type of T is (in internal notations) " << typeid(D).name()
              << '\n';
        spexit(mcerr);
      }
#ifdef ALR_CHECK_EACH_BOUND
      if (n >= 0 && n < arr.qel.acu(q_deref_ind)) {
#endif
        current_pos += n * arr.get_cum_qel().acu(q_deref_ind);
        q_deref_ind++;
        return *this;
#ifdef ALR_CHECK_EACH_BOUND
      } else {
        mcerr << "Error in IndexingProvider<D>& "
                 "IndexingProvider::operator[](long n): n < 0 || n >= "
                 "qel.acu(q_deref_ind)\n";
        Iprint2n(mcout, n, arr.qel.acu(q_deref_ind));
        mcerr << "Type of T is (in internal notations) " << typeid(T).name()
              << '\n';
        spexit(mcerr);
      }
#endif
    }
  };

  // This operator can be used to provide ordinary indexing [][][]... .
  // It is perhaps quite slow compared with ac() functions.
  // It is provided only for the use in contexts in which [][][]...
  // is needed.
  inline IndexingProvider<T> operator[](long n) {
    if (qel.get_qel() < 1) {
      mcerr << "ERROR in DynArr::IndexingProvider DynArr::operator[](long n): "
               "qel.get_qel()< 1, qel.get_qel()=" << qel.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#ifdef ALR_CHECK_EACH_BOUND
    if (n >= 0 && n < qel.acu(0)) {
#endif
      return IndexingProvider<T>(*this, 1, n * cum_qel.acu(0));
#ifdef ALR_CHECK_EACH_BOUND
    } else {
      mcerr << "Error in IndexingProvider<T> DynArr::operator[](long n): n < 0 "
               "|| n >= qel.acu(0)\n";
      Iprint2n(mcout, n, qel.acu(0));
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#endif
  }

  inline const IndexingProvider<T> operator[](long n) const {
    if (qel.get_qel() < 1) {
      mcerr << "ERROR in DynArr::IndexingProvider DynArr::operator[](long n): "
               "qel.get_qel()< 1, qel.get_qel()=" << qel.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#ifdef ALR_CHECK_EACH_BOUND
    if (n >= 0 && n < qel.acu(0)) {
#endif
      // DynArr<T>* temp = static_cast<DynArr<T>* >(this);
      DynArr<T>* temp = const_cast<DynArr<T>*>(this);
      //	static_cast<const IndexingProvider<T> >(*this);
      return IndexingProvider<T>(*temp, 1, n * cum_qel.acu(0));
#ifdef ALR_CHECK_EACH_BOUND
    } else {
      mcerr << "Error in IndexingProvider<T> DynArr::operator[](long n): n < 0 "
               "|| n >= qel.acu(0)\n";
      Iprint2n(mcout, n, qel.acu(0));
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#endif
  }

  T& ac(long i) {
    // for 1-dimensional array
    if (qel.get_qel() == 1) return el[i];
    mcerr << "ERROR in DynArr::ac(long i): qel.get_qel()!= 1, qel.get_qel()="
          << qel.get_qel() << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
  }
  const T& ac(long i) const {
    // for 1-dimensional array
    if (qel.get_qel() == 1) return el[i];
    mcerr << "ERROR in DynArr::ac(long i): qel.get_qel()!= 1, qel.get_qel()="
          << qel.get_qel() << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
  }
  inline T& acu(long i1)  // for 1-dimensional array, completely unchecked
  {
    return el.acu(i1);
  }
  inline const T& acu(long i1) const  // for 1-dimensional array, completely
                                      // unchecked
  {
    return el.acu(i1);
  }

  T& ac(const DynLinArr<long>& ind)  // for arbitrary number of dimensions
      // but the number of them in array should be equal to size of ind.
      // ind is array of indexes. Its first element if the first index,
      // second is second, etc.
  {
    long q;
    if ((q = qel.get_qel()) != ind.get_qel()) {
      mcerr << "ERROR in DynArr::ac(const DynLinArr<long>& ind): "
            << "qel.get_qel()!= ind.get_qel()\n"
            << "qel.get_qel()=" << qel.get_qel()
            << " ind.get_qel()=" << ind.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#ifdef ALR_CHECK_EACH_BOUND
#ifdef DEBUG_DYNARR
    if (q == 1)  // faster for this case
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el.acu(calc_lin_ind(ind));
#endif
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#endif
  }
  const T& ac(const DynLinArr<long>& ind) const  // the same as above
  {
    long q;
    if ((q = qel.get_qel()) != ind.get_qel()) {
      mcerr << "ERROR in DynArr::ac(const DynLinArr<long>& ind): "
            << "qel.get_qel()!= ind.get_qel()\n"
            << "qel.get_qel()=" << qel.get_qel()
            << " ind.get_qel()=" << ind.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
/*
#ifdef ALR_CHECK_EACH_BOUND
#ifdef DEBUG_DYNARR
      return el[calc_lin_ind(ind)];
#else
      return el.acu(calc_lin_ind(ind));
#endif
#else
      return el[calc_lin_ind(ind)];
#endif
      //return el[calc_lin_ind(ind)];
      */

#ifdef ALR_CHECK_EACH_BOUND
#ifdef DEBUG_DYNARR
    if (q == 1)  // faster for this case
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el.acu(calc_lin_ind(ind));
#endif
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#endif
  }

  T& acp(const DynLinArr<long>& ind)  // the same as above, but
      // the size of ind can be more than the number of indexes
      // (the rest is unused)
  {
    long q;
    if ((q = qel.get_qel()) > ind.get_qel()) {
      mcerr << "ERROR in DynArr::acp(const DynLinArr<long>& ind): "
            << "qel.get_qel()!= ind.get_qel()\n"
            << "qel.get_qel()=" << qel.get_qel()
            << " ind.get_qel()=" << ind.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }

#ifdef ALR_CHECK_EACH_BOUND
#ifdef DEBUG_DYNARR
    if (q == 1)  // faster for this case
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el.acu(calc_lin_ind(ind));
#endif
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#endif
  }

  const T& acp(const DynLinArr<long>& ind) const {
    long q;
    if ((q = qel.get_qel()) > ind.get_qel()) {
      mcerr << "ERROR in DynArr::acp(const DynLinArr<long>& ind): "
            << "qel.get_qel()!= ind.get_qel()\n"
            << "qel.get_qel()=" << qel.get_qel()
            << " ind.get_qel()=" << ind.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#ifdef ALR_CHECK_EACH_BOUND
#ifdef DEBUG_DYNARR
    if (q == 1)  // faster for this case
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el.acu(calc_lin_ind(ind));
#endif
#else
    if (q == 1)
      return el[ind.acu(0)];
    else
      return el[calc_lin_ind(ind)];
#endif
  }

  T& acu(const DynLinArr<long>& ind)  // unchecked
  {
    if (qel.get_qel() == 1)
      return el.acu(ind.acu(0));
    else
      return el.acu(calc_lin_ind(ind));
  }
  const T& acu(const DynLinArr<long>& ind) const  // unchecked
  {
    if (qel.get_qel() == 1)
      return el.acu(ind.acu(0));
    else
      return el.acu(calc_lin_ind(ind));
  }

  T& ac(long i1, long i2)  // for 2-dimensional array
  {
    if (qel.get_qel() == 2) {
#ifdef ALR_CHECK_EACH_BOUND
      if (i1 >= 0 && i1 < qel.acu(0)) {
        if (i2 >= 0 && i2 < qel.acu(1)) {
#ifdef DEBUG_DYNARR
          return el[i1 * cum_qel.acu(0) + i2];
#else
          return el.acu(i1 * cum_qel.acu(0) + i2);
#endif
        } else {
          mcerr << "Error in DynArr::ac(long i1, long i2): i2 < 0 || i2 >= "
                   "qel.acu(1)\n";
          Iprint2n(mcout, i2, qel[1]);
        }
      } else {
        mcerr << "Error in DynArr::ac(long i1, long i2): i1 < 0 || i1 >= "
                 "qel.acu(0)\n";
        Iprint2n(mcout, i1, qel[0]);
      }
    } else {
      mcerr << "ERROR in DynArr::ac(long i1, long i2): qel.get_qel()!= 2,"
            << " qel.get_qel()=" << qel.get_qel() << '\n';
    }
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else  // for ifdef ALR_CHECK_EACH_BOUND
      return el[i1 * cum_qel.acu(0) + i2];

#endif
  }

  /*
  {
    if(qel.get_qel() != 2)
    { mcerr<<"ERROR in DynArr::ac(long i1, long i2): qel.get_qel()!= 2,"
           <<" qel.get_qel()=" <<qel.get_qel()<<'\n';
    mcerr<<"Type of T is (in internal notations) "<<typeid(T).name()<<'\n';
    spexit(mcerr); }
#ifdef ALR_CHECK_EACH_BOUND
    if(i1 < 0 || i1 >= qel.acu(0))
    {
      mcerr<<"Error in DynArr::ac(long i1, long i2): i1 < 0 || i1 >=
qel.acu(0)\n";
      Iprint2n(mcout, i1, qel[0]);
      spexit(mcerr);
    }
    if(i2 < 0 || i2 >= qel.acu(1))
    {
      mcerr<<"Error in DynArr::ac(long i1, long i2): i2 < 0 || i2 >=
qel.acu(1)\n";
      Iprint2n(mcout, i2, qel[1]);
      spexit(mcerr);
    }
#ifdef DEBUG_DYNARR
    return el[i1*cum_qel.acu(0) + i2];
#else
    return el.acu(i1*cum_qel.acu(0) + i2);
#endif
#else
    return el[i1*cum_qel.acu(0) + i2];
#endif
  }
  */
  const T& ac(long i1, long i2) const  // for 2-dimensional array
  {
    if (qel.get_qel() == 2) {
#ifdef ALR_CHECK_EACH_BOUND
      if (i1 >= 0 && i1 < qel.acu(0)) {
        if (i2 >= 0 && i2 < qel.acu(1)) {
#ifdef DEBUG_DYNARR
          return el[i1 * cum_qel.acu(0) + i2];
#else
          return el.acu(i1 * cum_qel.acu(0) + i2);
#endif
        } else {
          mcerr << "Error in DynArr::ac(long i1, long i2): i2 < 0 || i2 >= "
                   "qel.acu(1)\n";
          Iprint2n(mcout, i2, qel[1]);
        }
      } else {
        mcerr << "Error in DynArr::ac(long i1, long i2): i1 < 0 || i1 >= "
                 "qel.acu(0)\n";
        Iprint2n(mcout, i1, qel[0]);
      }
    } else {
      mcerr << "ERROR in DynArr::ac(long i1, long i2): qel.get_qel()!= 2,"
            << " qel.get_qel()=" << qel.get_qel() << '\n';
    }
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else  // for ifdef ALR_CHECK_EACH_BOUND
        return el[i1 * cum_qel.acu(0) + i2];

#endif
  }
  /*
  {
    if(qel.get_qel() != 2)
    { mcerr<<"ERROR in DynArr::ac(long i1, long i2): qel.get_qel()!= 2,"
           <<" qel.get_qel()=" <<qel.get_qel()<<'\n';
    mcerr<<"Type of T is (in internal notations) "<<typeid(T).name()<<'\n';
    spexit(mcerr); }
#ifdef ALR_CHECK_EACH_BOUND
    if(i1 < 0 || i1 >= qel.acu(0))
    {
      mcerr<<"Error in DynArr::ac(long i1, long i2): i1 < 0 || i1 >=
qel.acu(0)\n";
      Iprint2n(mcout, i1, qel[0]);
      spexit(mcerr);
    }
    if(i2 < 0 || i2 >= qel.acu(1))
    {
      mcerr<<"Error in DynArr::ac(long i1, long i2): i2 < 0 || i2 >=
qel.acu(1)\n";
      Iprint2n(mcout, i2, qel[1]);
      spexit(mcerr);
    }
#ifdef DEBUG_DYNARR
    return el.acu(i1*cum_qel.acu(0) + i2);
#else
    return el[i1*cum_qel.acu(0) + i2];
#endif
#else
    return el[i1*cum_qel.acu(0) + i2];
#endif
    //return el[i1*cum_qel[0] + i2];
  }
    */
  inline T& acu(long i1,
                long i2)  // for 2-dimensional array, completely unchecked
  {
    return el.acu(i1 * cum_qel.acu(0) + i2);
  }
  inline const T& acu(long i1, long i2) const  // for 2-dimensional array
  {
    return el.acu(i1 * cum_qel.acu(0) + i2);
  }

  T& ac(long i1, long i2, long i3)  // for 3-dimensional array
  {
    if (qel.get_qel() != 3) {
      mcerr << "ERROR in DynArr::ac(long i1, long i2, long i3): "
               "qel.get_qel()!= 3,"
            << " qel.get_qel()=" << qel.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#ifdef ALR_CHECK_EACH_BOUND
    if (i1 < 0 || i1 >= qel.acu(0)) {
      mcerr << "Error in DynArr::ac(long i1, long i2, long i3): i1 < 0 || i1 "
               ">= qel.acu(0)\n";
      Iprint2n(mcout, i1, qel[0]);
      spexit(mcerr);
    }
    if (i2 < 0 || i2 >= qel.acu(1)) {
      mcerr << "Error in DynArr::ac(long i1, long i2, long i3): i2 < 0 || i2 "
               ">= qel.acu(1)\n";
      Iprint2n(mcout, i2, qel[1]);
      spexit(mcerr);
    }
    if (i3 < 0 || i3 >= qel.acu(2)) {
      mcerr << "Error in DynArr::ac(long i1, long i2, long i3): i3 < 0 || i3 "
               ">= qel.acu(2)\n";
      Iprint2n(mcout, i3, qel[2]);
      spexit(mcerr);
    }
#ifdef DEBUG_DYNARR
    return el.acu(i1 * cum_qel.acu(0) + i2 * cum_qel.acu(1) + i3);
#else
    return el[i1 * cum_qel.acu(0) + i2 * cum_qel[1] + i3];
#endif
#else
        return el[i1 * cum_qel.acu(0) + i2 * cum_qel[1] + i3];
#endif
    // return el[i1*cum_qel[0] + i2*cum_qel[1] + i3];
  }

  const T& ac(long i1, long i2, long i3) const  // for 3-dimensional array
  {
    if (qel.get_qel() != 3) {
      mcerr << "ERROR in DynArr::ac(long i1, long i2, long i3): "
               "qel.get_qel()!= 3,"
            << " qel.get_qel()=" << qel.get_qel() << '\n';
      mcerr << "Type of T is (in internal notations) " << typeid(T).name()
            << '\n';
      spexit(mcerr);
    }
#ifdef ALR_CHECK_EACH_BOUND
    if (i1 < 0 || i1 >= qel.acu(0)) {
      mcerr << "Error in DynArr::ac(long i1, long i2, long i3): i1 < 0 || i1 "
               ">= qel.acu(0)\n";
      Iprint2n(mcout, i1, qel[0]);
      spexit(mcerr);
    }
    if (i2 < 0 || i2 >= qel.acu(1)) {
      mcerr << "Error in DynArr::ac(long i1, long i2, long i3): i2 < 0 || i2 "
               ">= qel.acu(1)\n";
      Iprint2n(mcout, i2, qel[1]);
      spexit(mcerr);
    }
    if (i3 < 0 || i3 >= qel.acu(2)) {
      mcerr << "Error in DynArr::ac(long i1, long i2, long i3): i3 < 0 || i3 "
               ">= qel.acu(2)\n";
      Iprint2n(mcout, i3, qel[2]);
      spexit(mcerr);
    }
#ifdef DEBUG_DYNARR
    return el.acu(i1 * cum_qel.acu(0) + i2 * cum_qel.acu(1) + i3);
#else
    return el[i1 * cum_qel.acu(0) + i2 * cum_qel[1] + i3];
#endif
#else
        return el[i1 * cum_qel.acu(0) + i2 * cum_qel[1] + i3];
#endif
    // return el[i1*cum_qel[0] + i2*cum_qel[1] + i3];
  }

  long get_qel_lin(void) const { return el.get_qel(); }

  // access to array as linear array
  inline T& ac_lin(long n) {
    long qelln = el.get_qel();
#ifdef ALR_CHECK_BOUND
    if (n >= 0 && n < qelln) return el[n];
    mcerr << "ERROR in T& DynArr::ac_lin(long n): "
          << "n is out of bounds, n=" << n << " qelln=" << qelln << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else
        return el[n];
#endif
  }
  inline const T& ac_lin(long n) const {
    long qelln = el.get_qel();
#ifdef ALR_CHECK_BOUND
    if (n >= 0 && n < qelln) return el[n];
    mcerr << "ERROR in T& DynArr::ac_lin(long n): "
          << "n is out of bounds, n=" << n << " qelln=" << qelln << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
    return el[0];
#else
        return el[n];
#endif
  }
  // access to array as linear array always without check
  inline T& acu_lin(long n) { return el[n]; }
  inline const T& acu_lin(long n) const { return el[n]; }

  void assignAll(const T& val);

  long get_qdim(void) const { return qel.get_qel(); }
  const DynLinArr<long>& get_qel(void) const { return qel; }
  const DynLinArr<T>& get_el(void) const { return el; }

  // The following is mainly for debug (diagnostic print):
  const DynLinArr<long>& get_cum_qel(void) const { return cum_qel; }

  // void put_qel(const DynLinArr<long>& fqel, T* val=NULL);
  void put_qel(T* val = NULL);
  // 25.10.2006: Today I do not understand these following  comments.
  // They looks like simple copy from these in DynLinArr.
  // creates array with size fqel
  // If old array existed, then
  //   If it was less than fqel, it all is copied to new array
  //       and the other elements are either remains not inited
  //       or assignned *val.
  //   else its fqel part is copyed to new array.

  void clear(void) {
    qel.clear();
    cum_qel.clear();
    el.clear();
  }

  int confirm_ind(const DynLinArr<long>& ind) { return gconfirm_ind(qel, ind); }
  int confirm_ind_ext(const DynLinArr<long>& ind) {
    return gconfirm_ind_ext(qel, ind);
  }
  DynArr<T> top(void);

  // Apply any function of one argument (of type T) to each element.
  template <class P>
  friend void apply1(DynArr<P>& ar, void (*fun)(P& f));

  // Apply any function of two arguments
  // (first of which is of type T and the second is of type of address
  // of another function (possibly apply1) to each element.
  template <class P, class X>
  friend void apply2(DynArr<P>& ar, void (*fun1)(P& f, void (*fun21)(X& f)),
                     void (*fun2)(X& f));

  void check(void) const {
    qel.check();
    cum_qel.check();
    el.check();
  }
  int get_s_non_emplty(void) const {
    long q = qel.get_qel();
    if (q == 0) return 0;
    long n;
    for (n = 0; n < q; n++) {
      if (qel[n] <= 0) return 0;
    }
    return 1;
  }
  virtual DynArr* copy() const { return new DynArr(*this); }
  virtual ~DynArr() {}

 private:
  mutable DynLinArr<long> qel;
  // Linear array with number of elements by each dimension
  mutable DynLinArr<long> cum_qel;
  // "cumulative qel": each element contains product
  // of next elements of qel.
  // The last element is always 1.
  // Used for fast search of proper element in el.

  mutable DynLinArr<T> el;
  // Contains all elements.
  // The last index varies faster.

  long calc_lin_ind(const DynLinArr<long>& ind) const {
    long i = 0;
    long n;
    long qdim1 = qel.get_qel() - 1;
    /*
      // This check is not necessary by two reasons
      // 1. The checks of this condition are made in calling functions.
      // 2. The correct condition is not != but >.
      if(qdim1 != ind.get_qel() - 1)
      {
        mcerr<<"ERROR in long DynArr::calc_lin_ind(const DynLinArr<long>& ind)
const\n";
        mcerr<<"qdim1 != ind.get_qel() - 1\n";
        Iprint2n(mcerr, qdim1, (ind.get_qel() - 1));
        spexit(mcerr);
      }
      */
    for (n = 0; n < qdim1; n++) {
#ifdef ALR_CHECK_EACH_BOUND
#ifdef DEBUG_DYNARR
      if (ind[n] < 0 || ind[n] >= qel[n]) {
        mcerr << "ERROR in long DynArr::calc_lin_ind(const DynLinArr<long>& "
                 "ind) const\n";
        mcerr << "ind[n] < 0 || ind[n] >= qel[n]\n";
        Iprint3n(mcout, n, ind[n], qel[n]);
        Iprintn(mcout, qel);
        spexit(mcerr);
      }
#else
      if (ind.acu(n) < 0 || ind.acu(n) >= qel.acu(n)) {
        mcerr << "ERROR in long DynArr::calc_lin_ind(const DynLinArr<long>& "
                 "ind) const\n";
        mcerr << "ind.acu(n) < 0 || ind.acu(n) >= qel.acu(n)\n";
        Iprint3n(mcout, n, ind[n], qel[n]);
        Iprintn(mcout, qel);
        spexit(mcerr);
      }
#endif
#endif
      i += ind[n] * cum_qel[n];
    }
#ifdef ALR_CHECK_EACH_BOUND
#ifdef DEBUG_DYNARR
    if (ind[qdim1] < 0 || ind[qdim1] >= qel[qdim1]) {
      mcerr << "ERROR in long DynArr::calc_lin_ind(const DynLinArr<long>& ind) "
               "const\n";
      mcerr << "ind[qdim1] < 0 || ind[qdim1] >= qel[qdim1]\n";
      Iprint3n(mcout, n, ind[qdim1], qel[qdim1]);
      Iprintn(mcout, qel);
      spexit(mcerr);
    }
#else
    if (ind.acu(qdim1) < 0 || ind.acu(qdim1) >= qel.acu(qdim1)) {
      mcerr << "ERROR in long DynArr::calc_lin_ind(const DynLinArr<long>& ind) "
               "const\n";
      mcerr << "ind.acu(qdim1) < 0 || ind.acu(qdim1) >= qel.acu(qdim1)\n";
      Iprint3n(mcout, n, ind[qdim1], qel[qdim1]);
      Iprintn(mcout, qel);
      spexit(mcerr);
    }
#endif
#endif
    i += ind[qdim1];  // the last index, for which multiplication by cum_qel
                      // is not necessary.
    return i;
  }
};

template <class T>
DynArr<T>& DynArr<T>::operator=(const DynArr<T>& f) {
#ifdef DEBUG_DYNARR
  mcout << "DynArr<T>& DynArr<T>::operator=(const DynArr<T>& f)\n";
#endif
  if (this != &f) {
    // mcout<<"DynLinArr<T>& operator=(const DynLinArr<T>& f): long(el)="
    //<<long(el)<<'\n';
    check();
    f.check();
    qel = f.qel;
    cum_qel = f.cum_qel;
    el = f.el;
  }
  return *this;
}

template <class T>
template <class D>
DynArr<T>& DynArr<T>::operator=(const DynArr<D>& f) {
#ifdef DEBUG_DYNLINARR
  mcout << "DynArr<T>& DynArr<T>::operator=(const DynArr<D>& f)\n";
#endif
  check();
  f.check();
  DynLinArr<long> fqel = f.get_qel();
  DynLinArr<long> fcum_qel = f.get_cum_qel();
  // for example, one of its elements.
  const long q = f.get_qel_lin();
  T* temp_el = (q > 0) ? (new T[q]) : (T*)NULL;
  for (long n = 0; n < q; n++) temp_el[n] = f.acu_lin(n);
  pass(q, fqel, fcum_qel, temp_el);
  return *this;
}

template <class T>
void apply1(DynArr<T>& ar, void (*fun)(T& f)) {
  const long q = ar.el.get_qel();
  for (long n = 0; n < q; n++) (*fun)(ar.el[n]);
}
template <class T, class X>
void apply2(DynArr<T>& ar, void (*fun1)(T& f, void (*fun21)(X& f)),
            void (*fun2)(X& f)) {
  const long q = ar.el.get_qel();
  for (long n = 0; n < q; n++) (*fun1)(ar.el[n], fun2);
}

int find_next_comb(const DynLinArr<long>& qel, DynLinArr<long>& f);
int find_prev_comb(const DynLinArr<long>& qel, DynLinArr<long>& f);
int find_next_comb_not_less(const DynLinArr<long>& qel, DynLinArr<long>& f);

template <class T>
class IterDynArr {
 public:
  IterDynArr(const DynArr<T>* fdar)
      : ncur(fdar->get_qdim()), dar((DynArr<T>*)fdar) {
    long n;
    long qdim1 = ncur.get_qel() - 1;
    if (qdim1 >= 0) {
      for (n = 0; n < qdim1; n++) ncur[n] = 0;
      ncur[qdim1] = -1;
    }
  }
  T* more(void)  // Just next element. Why not "next", do not remember.
  {
    if (find_next_comb(dar->get_qel(), ncur))
      return &(dar->ac(ncur));
    else
      return NULL;
  }

  T* current(void)  // Element currently pointed by ncut.
  {
    long n;
    long qdim = ncur.get_qel();
    if (qdim <= 0) return NULL;
    for (n = 0; n < qdim; n++) {
      if (ncur[n] < 0 || ncur[n] >= dar->get_qel()[n]) return NULL;
    }
    return &(dar->ac(ncur));
  }

  T* less(void)  // Switch current to previous.
      // Again do not remember why not" prev"
  {
    if (find_prev_comb(dar->get_qel(), ncur))
      return &(dar->ac(ncur));
    else
      return NULL;
  }

  const DynLinArr<long>& get_ncur(void) const { return ncur; }

 private:
  DynLinArr<long> ncur;
  DynArr<T>* dar;
};

extern DynLinArr<long> qel_communicat;

template <class T>
void DynArr<T>::put_qel(T* val)
    //  by default           val=NULL
{
  check();
  if (qel.get_qel() == 0) {
    *this = DynArr<T>(qel_communicat, val);
    return;
  }

  DynArr<T> datemp(qel_communicat, val);  // all init to val
  IterDynArr<T> iter(&datemp);
  T* at;
  while ((at = iter.more()) != NULL) {
    if (confirm_ind_ext(iter.get_ncur()))
      *at = acp(iter.get_ncur());  // change to old values where they were
  }
  *this = datemp;
}

template <class T>
int operator==(const DynLinArr<T>& f1, const DynLinArr<T>& f2) {
  if (f1.get_qel() != f2.get_qel()) return 0;
  long q = f1.get_qel();
  long n;
  for (n = 0; n < q; n++) {
    if (!(f1.acu(n) == f2.acu(n))) return 0;
  }
  return 1;
}

template <class T, class P>
int apeq_mant(const DynLinArr<T>& f1, const DynLinArr<T>& f2, P prec) {
  if (f1.get_qel() != f2.get_qel()) return 0;
  long q = f1.get_qel();
  long n;
  for (n = 0; n < q; n++) {
    if (!apeq_mant(f1.acu(n), f2.acu(n), prec)) return 0;
  }
  return 1;
}

template <class T>
int operator!=(const DynLinArr<T>& f1, const DynLinArr<T>& f2) {
  if (f1 == f2)
    return 0;
  else
    return 1;
}

template <class T>
int operator==(const DynArr<T>& f1, const DynArr<T>& f2) {
  if (f1.get_qel() != f2.get_qel()) return 0;
  if (f1.get_el() != f2.get_el()) return 0;
  return 1;
}

template <class T, class P>
int apeq_mant(const DynArr<T>& f1, const DynArr<T>& f2, P prec) {
  if (f1.get_qel() != f2.get_qel()) return 0;
  if (!apeq_mant(f1.get_el(), f2.get_el(), prec)) return 0;
  return 1;
}

template <class T>
int operator!=(const DynArr<T>& f1, const DynArr<T>& f2) {
  if (f1.get_qel() == f2.get_qel()) return 0;
  if (f1.get_el() == f2.get_el()) return 0;
  return 1;
}

template <class T>
void DynArr<T>::assignAll(const T& val) {
  check();
  // try faster and simpler way (30.10.2006):
  el.assignAll(val);
  /*
  IterDynArr<T> iter(this);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    *at=val;
  }
  */
}

template <class T, class X>
void copy_DynArr(const DynArr<T>& s, DynArr<X>& d) {
  mfunnamep(
      "template<class T, class X> void copy_DynArr(const DynArr<T>& s, "
      "DynArr<X>& d)");
  s.check();
  d.check();
  d = DynArr<X>(s.get_qel(), NULL);
  IterDynArr<T> iter(&s);
  T* at;
  while ((at = iter.more()) != NULL) {
    const DynLinArr<long>& ncur = iter.get_ncur();
    d.ac(ncur) = *at;
  }
}

// Convert types of elements from T to X.
template <class T, class X>
void convert_DynArr(const DynArr<T>& s, DynArr<X>& d) {
  mfunnamep(
      "template<class T, class X> void convert_DynArr(const DynArr<T>& "
      "s, DynArr<X>& d)");
  s.check();
  d.check();
  d = DynArr<X>(s.get_qel(), NULL);
  IterDynArr<T> iter(&s);
  T* at;
  while ((at = iter.more()) != NULL) {
    const DynLinArr<long>& ncur = iter.get_ncur();
    d.ac(ncur) = X(*at);
  }
}

template <class T>
DynArr<T> DynLinArr<T>::top(void) {
  check();
  DynArr<T> r(1, qel);
  long n;
  for (n = 0; n < qel; n++) {
    r.ac(0, n) = el[n];
  }
  return r;
}
template <class T>
DynArr<T> DynArr<T>::top(void) {
  mfunnamep("template<class T> DynArr<T> DynArr<T>::top(void)");
  check();
  long qdim = get_qdim();
  check_econd11(qdim, != 2, mcerr);
  long n1, n2;
  DynArr<T> r(qel[1], qel[0]);
  for (n1 = 0; n1 < qel[0]; n1++) {
    for (n2 = 0; n2 < qel[1]; n2++) {
      r.ac(n2, n1) = ac(n1, n2);
    }
  }
  return r;
}

template <class T>
DynLinArr<T>::DynLinArr(const DynArr<T>& f) {
  // mcout<<"template<class T> DynLinArr<T>::DynLinArr(const DynArr<T>& f):\n";
  f.check();
  long qdim = f.get_qdim();
  if (qdim != 1) {
    mcerr << "ERROR in DynLinArr<T>::DynLinArr(const DynArr<T>& f):\n"
          << "f.get_qdim() != 1, f.get_qdim()=" << f.get_qdim() << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  const DynLinArr<long>& qelem = f.get_qel();
  qel = qelem[0];
  // mcout<<"qel="<<qel<<'\n';
  if (qel > 0) {
    el = new T[qel];
    for (long n = 0; n < qel; n++) el[n] = f.ac(n);
  } else
    el = NULL;
}

template <class T>
DynLinArr<T>::DynLinArr(const DynArr<T>& f, int n_of_dim,
                        // 0 - first dim) 1 - second dim)
                        long roc_number)
    // takes only mentioned raw or column.
{
  f.check();
  long qdim = f.get_qdim();
  if (qdim != 2) {
    mcerr << "ERROR in DynLinArr<T>::DynLinArr(const DynArr<T>& f, int "
             "n_of_dim,long roc_number):\n"
          << "f.get_qdim() != 2, f.get_qdim()=" << f.get_qdim() << '\n';
    mcerr << "Type of T is (in internal notations) " << typeid(T).name()
          << '\n';
    spexit(mcerr);
  }
  const DynLinArr<long>& qelem = f.get_qel();
  if (n_of_dim == 0) {
    qel = qelem[1];
  } else {
    qel = qelem[0];
  }
  // mcout<<"qel="<<qel<<'\n';
  if (qel > 0) {
    el = new T[qel];
    long n;
    if (n_of_dim == 0) {
      for (n = 0; n < qel; n++) el[n] = f.ac(roc_number, n);
    } else {
      for (n = 0; n < qel; n++) el[n] = f.ac(n, roc_number);
    }
  }
}

template <class T>
std::ostream& operator<<(std::ostream& file, const DynArr<T>& f) {
  // mfunnamep("template<class T> std::ostream& operator<<(std::ostream& file,
  // const DynArr<T>& f)");
  f.check();
  Ifile << "DynArr<T>: qdim=" << f.get_qdim() << '\n';
  indn.n += 2;
  if (s_short_output > 0) {
    Ifile << noindent << f.get_qel() << yesindent;
  } else {
    Ifile << "qel=" << noindent << f.get_qel() << yesindent;
    Ifile << "cum_qel=" << noindent << f.get_cum_qel() << yesindent;
  }
  if (f.get_s_non_emplty() == 1) {
    if (s_short_output == 0) {
      Ifile << "Content element by element:\n";
      Ifile << "(The first number is sequencial number, then there are "
               "indexes, the last is the element)\n";
      // DynArr<T>& ff(f);
    }
    long nseq = 0;
    IterDynArr<T> iter_f(&((DynArr<T>&)f));
    T* at;
    while ((at = iter_f.more()) != NULL) {
      std::ostringstream ost;
      if (s_short_output == 0) {
        // Ifile<<"ncur="<<noindent<<iter_f.get_ncur()<<yesindent;
        Ifile << "nseq=" << std::setw(5) << nseq << " ncur=";
        long n;
        for (n = 0; n < iter_f.get_ncur().get_qel(); n++) {
          file << ' ' << std::setw(5) << iter_f.get_ncur()[n];
        }
        ost << indn << " element=" << noindent << (*at) << yesindent;
      } else {
        ost << indn << noindent << (*at) << yesindent;
      }
      put_one_n(ost);
      file << ost.str();
      nseq++;
    }
    file << yesindent;
  } else {
    if (s_short_output == 0) {
      Ifile << "Content is empty.\n";
    }
  }
  indn.n -= 2;
  return file;
}
/*
template<class T>
void DybArr<T>::short_output(std::ostream& file)
{
  mfunnamep("template<class T> void DybArr<T>::short_output(std::ostream&
file))");
  f.check();
  Ifile<<"DynArr<T>: qdim="<<f.get_qdim()<<'\n';
  indn.n+=2;
  qel.short_output(file);
  if(f.get_s_non_emplty() == 1)
  {
    long nseq=0;
    IterDynArr<T> iter_f( &((DynArr<T>&) f));
    T* at;
    while( (at=iter_f.more()) != NULL )
    {
      std::ostringstream ost;
      ost<<indn<<noindent<<(*at)<<yesindent;
      put_one_n(ost);
      file<<ost.str();
      nseq++;
    }
    file<<yesindent;
  }
  indn.n-=2;
  return file;
}
*/
template <class T>
std::istream& operator>>(std::istream& file, DynArr<T>& f) {
  mfunnamep(
      "template<class T> istream& operator>>(istream& file, DynArr<T>& f)");
  definp_endpar dep(&file, 0, 1, 0, s_short_output);
  long qdim = 0;
  dep.s_short = 0;
  DEFINPAP(qdim);
  dep.s_short = s_short_output;
  check_econd11(qdim, < 0, mcerr);
  if (s_short_output == 0) {
    set_position("qel=DynLinArr<T>:", *dep.istrm, dep.s_rewind, dep.s_req_sep);
  } else {
    set_position("DynLinArr<T>:", *dep.istrm, dep.s_rewind, dep.s_req_sep);
  }
  DynLinArr<long> qel_loc;
  // mcout<<"now will read qel_loc\n";
  file >> qel_loc;
  // mcout<<"qel_loc is read\n";
  if (s_short_output == 0) {
    set_position("cum_qel=DynLinArr<T>:", *dep.istrm, dep.s_rewind,
                 dep.s_req_sep);
    DynLinArr<long> cum_qel_loc;
    file >> cum_qel_loc;  // this is in general unnecessary
  }
  if (qel_loc.get_qel() > 0) {
    f.pilfer(DynArr<T>(qel_loc, NULL));
    long nseq;
    long n;
    long qseq = qel_loc[0];
    for (n = 1; n < qel_loc.get_qel(); n++) {
      qseq *= qel_loc[n];
    }
    for (n = 0; n < qseq; n++) {
      if (s_short_output == 0) {
        DEFINPAP(nseq);
        check_econd12(nseq, !=, n, mcerr);
        DynLinArr<long> ncur(qel_loc.get_qel());
        set_position("ncur=", *dep.istrm, dep.s_rewind, dep.s_req_sep);
        long m;
        for (m = 0; m < qel_loc.get_qel(); m++) {
          file >> ncur[m];
        }
        // T element;
        // DEFINPAP(element);
        // f.ac(ncur) = element;
        set_position("element=", *dep.istrm, dep.s_rewind, dep.s_req_sep);
      }
      file >> f.ac_lin(n);
    }
  } else {
    if (s_short_output == 0) {
      // just pass to end
      set_position("Content is empty.", *dep.istrm, dep.s_rewind,
                   dep.s_req_sep);
    }
    f.pilfer(DynArr<T>());
  }
  return file;
}

/*
template<class T>
void DynArr<T>::short_read(istream& file)
{
  mfunnamep("template<class T> void DynArr<T>::short_read(istream& file)");
  definp_endpar dep(&file, 0, 1, 0);
  long qdim=0;
  DEFINPAP(qdim);
  check_econd11(qdim, < 0 , mcerr);
  DynLinArr<long> qel_loc;
  qel_loc.short_read(file);
  // generate cum
  if(qel_loc.get_qel() > 0 )
  {
    f.pilfer(DynArr<T>(qel_loc, NULL));
    long nseq;
    long n;
    long qseq=qel[0];
    for(n=1; n<el.get_qel(); n++)
    {
      qseq*=el[n];
    }
    for(n=0; n<qseq; n++)
    {
      file>>f.ac(ncur);
    }
  }
  else
  {
    // just pass to end
    set_position("Content is empty.",
                 *dep.istrm, dep.s_rewind, dep.s_req_sep);
  }
  return file;
}
*/

template <class T>
void print_DynArr(std::ostream& file, const DynArr<T>& f, int l) {
  // mfunnamep("template<class T> oid print_DynArr(std::ostream& file, const
  // DynArr<T>& f, int l)");
  f.check();
  // Ifile<<"DynArr<T>: qdim="<<f.get_qdim()
  //     <<" qel="<<noindent<<f.get_qel()<<yesindent<<'\n';
  Ifile << "DynArr<T>: qdim=" << f.get_qdim() << '\n';
  indn.n += 2;
  Ifile << "qel=" << noindent << f.get_qel() << yesindent;
  Ifile << "cum_qel=" << noindent << f.get_cum_qel() << yesindent;
  Ifile << "Content element by element:\n";
  Ifile << "(The first number is sequencial number, then there are indexes, "
           "the last is the element)\n";
  // DynArr<T>& ff(f);
  long nseq = 0;
  IterDynArr<T> iter_f(&((DynArr<T>&)f));
  T* at;
  while ((at = iter_f.more()) != NULL) {
    // Ifile<<"ncur="<<noindent<<iter_f.get_ncur()<<yesindent;
    Ifile << "nseq=" << std::setw(5) << nseq << " ncur=";
    long n;
    for (n = 0; n < iter_f.get_ncur().get_qel(); n++) {
      file << ' ' << std::setw(5) << iter_f.get_ncur()[n];
    }
    // file<<'\n';
    // Ifile<<"element="<<noindent; at->print(file, l);
    // file<<yesindent<<'\n';
    std::ostringstream ost;
    ost << indn << " element=" << noindent;
    at->print(ost, l);
    ost << yesindent;
    put_one_n(ost);
    file << ost.str();
  }
  file << yesindent;
  indn.n -= 2;
}

// New experimental approach.
// give the width of field, which is put in setw().
// Whether the array will be printed in single lines
// or by columns, is determined by whether 80 symbols are enough
void print_DynArr_int_w(std::ostream& file, const DynArr<int>& f, int w);

void print_DynArr_float(std::ostream& file, const DynArr<float>& f);
void print_DynArr_double(std::ostream& file, const DynArr<double>& f);
// ^Identical functions
// See AbsArrD for similar function with DoubleAc

#define Iprintda_double(file, name)         \
  file << indn << #name << "=" << noindent; \
  print_DynArr_double(file, name);
// See AbsArrD for similar function with DoubleAc

}

#endif
