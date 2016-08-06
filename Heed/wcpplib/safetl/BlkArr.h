#ifndef BLKARR_H
#define BLKARR_H
/*
Blocked array, the compromise between efficiency of access by indexing
and efficiency of size increasing. In addition it is sometimes
important that the size increasing is  performed without copying of elements.
The information is allocated by several blocks, as many as needed.
So this is some intermediate case between the ordinary array and the list.
In the array the information is kept in one block, whereas in the list each
element is saved individually. Here, in the blocked array, the elements are
grouped, but there is no requirement to keep them in a single block.
Consequently, when the reserved space is exhausted, the next block is created.
It is convenient to establish its size as no less then the total size of
already allocated blocks. Then the number of blocks will grouth much slower
than the array size, and the efficient indexing will be preserved.
To avoid memory limits, the total " default" increment should be limited,
here by parameter
const long max_size_of_extra_increment = 1000000;  // in bytes

That means that the simple model used in DynLinArr and consisted in
characterising the array by only one parameter, its physical size,
occurs inappropriate for the blocked array. The physical size of blocked array
may be more that its logical size. Not only the part of block can be unused,
but the total block can be unused as well. This may happen as result of
array decreasing and intention to preserve something like histeresis.
At decreasing of such array it is reasonable to preserve the "next" allocated
block. That means that if before decrease the array was kept in, for example,
5 blocks, and  after decrease the array is kept in 3 blocks, the 4 block
is kept allocated to assure quick increase in future.

If the order of value of required memory is known, it is reasonable to
allocate the first block with corresponding size, so as to optimise further
operations. If the array is empty logically as well as physically,
for example at or right after the initialization with zero size,
this can be done with ordinary call put_qel(number of elements).
But the initializes both the physical and the logical size,
whereas the latter is not always necessary.
In addition, if this call is made for the array that is not physically empty,
the allocation of this array as a single block is not guaranteed due to
mentioned histeresis. Then the only choice to allocate the physical size of
the first block only (not the logucal size) is the use of call
void BlkArr::allocate_block(long fqel).
Note: if array has contained something, this call clears it.
So actually it always allocates the first block and makes it empty.
This call inialize only the physical structure, and lefts the logical size
zero. If it need to allocate the logical size as well,
use double calls:
void BlkArr::allocate_block(long fqel).
void BlkArr::put_qel(long fqel).

This is the sence.



Copyright (c) 2003 I. B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/safetl/AbsList.h"

//#define DEBUG_BLKARR  // make some print

const long max_size_of_extra_increment = 1000000;  // in bytes

#ifndef DONT_USE_ABSPTR
template <class T>
class BlkArr : public RegPassivePtr {
#else
template <class T>
class BlkArr {
#endif
 public:
  BlkArr(void) : qel(0), la(), size_of_element(sizeof(T)) { ; }
  explicit BlkArr(long fqel)
      : qel(fqel), la(DynLinArr<T>(0)), size_of_element(sizeof(T)) {
    if (qel > 0) la.get_first_node()->el.put_qel(qel);
  }
  BlkArr(long fqel, const T& val)
      : qel(fqel), la(DynLinArr<T>(0)), size_of_element(sizeof(T)) {
    if (qel > 0) la.get_first_node()->el.put_qel(qel, val);
  }
  BlkArr(long fqel, const T* ar, ArgInterp_Arr t)
      : qel(fqel), la(DynLinArr<T>(0)), size_of_element(sizeof(T)) {
    if (qel > 0)
      // la.get_first_node()->el.put_qel(qel, ar, interp_as_arr); error
      la.get_first_node()->el.pilfer(DynLinArr<T>(qel, ar, t));
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
  BlkArr<T>& operator=(const BlkArr<T>& f) {
#ifdef DEBUG_BLKARR
    mcout << "BlkArr<T>& operator=(const BlkArr<T>& f) is called\n";
#endif
    if (this != &f) {
      qel = f.qel;
      size_of_element = f.size_of_element;
      la = f.la;
    }
    return *this;
  }
  //BlkArr(      BlkArr<T>& f):qel(0), la() {*this=f;}
  BlkArr(const BlkArr<T>& f)
      :
#ifndef DONT_USE_ABSPTR
        RegPassivePtr(),
#endif
        qel(0),
        la() {
#ifdef DEBUG_BLKARR
    mcout << "BlkArr(const BlkArr<T>& f) is working\n";
#endif
    *this = f;
  }
  BlkArr(PILF_CONST BlkArr<T>& f, Pilfer)
      : qel(f.qel), size_of_element(f.size_of_element), la(f.la, steal) {
#ifdef DEBUG_BLKARR
    mcout << "BlkArr( BlkArr<T>& f, Pilfer) is working\n";
#endif
    f.qel = 0;
    f.size_of_element = 0;
  }
  void pilfer(PILF_CONST BlkArr<T>& f) {
#ifdef DEBUG_BLKARR
    mcout << "BlkArr::pilfer is called\n";
#endif
    if (this != &f) {
      if (qel != 0) {
        if (f.qel != 0) {
          mcerr << "ERROR in BlkArr::pilfer(...):\n";
          mcerr << "Both the destination and source arrays are not empty\n";
          // For explanations why it is dengerous, see similar function
          // of ActivePtr.
          spexit(mcerr);
        } else {
          la.clear();
          size_of_element = 0;
          qel = 0;
        }
      }
      qel = f.qel;
      size_of_element = f.size_of_element;
      la.pilfer(f.la);
      f.qel = 0;
      f.size_of_element = 0;
    }
  }
  BlkArr& assignAll(const T& f) {
    long n;
    for (n = 0; n < qel; n++)
      operator[](n) = f;
    return *this;
  }
  T& operator[](long n);
  const T& operator[](long n) const;
  T& last_el(void);
  const T& last_el(void) const;

  long get_qel(void) const { return qel; }
  void put_qel(long fqel);
  //void put_qel(long fqel, const T* val, int s);
  // creates array with size fqel
  // If old array existed, then
  //   If it was less than fqel, it all is copied to new array
  //       and the other elements either assigned *val or
  //       remains not inited.
  //   else its fqel part is copyed to new array.
  // s serves to distinguish this
  // from previous constructor when second argument
  // does not coincide with T but can be converted to it.
  // Attention!, val is an element, this is assimetry with contructor,
  // in which ar is an array.

  //void put_qel(long fqel, const T& val){put_qel(fqel, &val, 1);}  later
  void increment(void) {
    long q = qel + 1;
    put_qel(q);
  }
  void decrement(void) {
    long q = qel - 1;
    put_qel(q);
  }
  void append(const T& val) {
    increment();
    operator[](qel - 1) = val;
  }
  //void increment(const T* val=NULL)
  //  { check(); long q=qel+1; put_qel(q, *val); }
  //void increment(const T& val)
  //  { check(); long q=qel+1; put_qel(q, *val); }
  //void check(void) const;

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

  //~BlkArr() { if(el != NULL) delete[] el; }
  void print_struct(std::ostream& file) const;
  void allocate_block(long fqel)  // allocates first blocks but qel = 0
      {
    la.clear();
    qel = 0;
    put_qel(fqel);
    qel = 0;
  }
  void clear(void) {
    la.clear();
    qel = 0;
  }
#ifndef DONT_USE_ABSPTR
  macro_copy_total(BlkArr);
#endif
  virtual ~BlkArr(void) {}

 private:
  PILF_MUTABLE long qel;  // number of elements currently activated
  PILF_MUTABLE AbsList<DynLinArr<T> > la;  // list of arrays
  PILF_MUTABLE long size_of_element;
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

template <class T> T& BlkArr<T>::operator[](long n) {
  if (n < 0 || n >= qel) {
    mcerr << "T& BlkArr::operator[](long n): n is out of bounds, n=" << n
          << " qel=" << qel << '\n';
    spexit(mcerr);
  }
  long q;
  AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
  do {
    if ((q = aln->el.get_qel()) > n)
      return aln->el.acu(n);
    else
      n -= q;
    aln = aln->get_next_node();
  } while (aln != NULL);
  mcerr << "T& BlkArr::operator[](long n): should never happen, n=" << n
        << " qel=" << qel << '\n';
  spexit(mcerr);
#ifdef INS_CRETURN    // Insert calming return
  return aln->el[0];  // to quiet Microsoft Visial Studio compiler against
                      // "not all control paths return a value"
#endif
}

template <class T> const T& BlkArr<T>::operator[](long n) const {
  if (n < 0 || n >= qel) {
    mcerr << "const T& BlkArr::operator[](long n): n is out of bounds, n=" << n
          << " qel=" << qel << '\n';
    spexit(mcerr);
  }
  long q;
  AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
  do {
    if ((q = aln->el.get_qel()) > n)
      return aln->el.acu(n);
    else
      n -= q;
    aln = aln->get_next_node();
  } while (aln != NULL);
  mcerr << "T& BlkArr::operator[](long n): should never happen, n=" << n
        << " qel=" << qel << '\n';
  spexit(mcerr);
#ifdef INS_CRETURN    // Insert calming return
  return aln->el[0];  // to quiet Microsoft Visial Studio compiler against
                      // "not all control paths return a value"
#endif
}

template <class T> T& BlkArr<T>::last_el(void) {
  if (qel <= 0) {
    mcerr << "T& BlkArr::last_el(): no elements in array\n"
          << " qel=" << qel << '\n';
    spexit(mcerr);
  }
  long q;
  AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
  long n = qel - 1;
  do {
    if ((q = aln->el.get_qel()) > n)
      return aln->el.acu(n);
    else
      n -= q;
    aln = aln->get_next_node();
  } while (aln != NULL);
  mcerr << "T& BlkArr::::last_el(): should never happen, n=" << n
        << " qel=" << qel << '\n';
  spexit(mcerr);
#ifdef INS_CRETURN    // Insert calming return
  return aln->el[0];  // to quiet Microsoft Visial Studio compiler against
                      // "not all control paths return a value"
#endif
}

template <class T> const T& BlkArr<T>::last_el(void) const {
  if (qel <= 0) {
    mcerr << "const T& BlkArr::last_el(): no elements in array\n"
          << " qel=" << qel << '\n';
    spexit(mcerr);
  }
  long q;
  AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
  long n = qel - 1;
  do {
    if ((q = aln->el.get_qel()) > n)
      return aln->el.acu(n);
    else
      n -= q;
    aln = aln->get_next_node();
  } while (aln != NULL);
  mcerr << "T& BlkArr::::last_el(): should never happen, n=" << n
        << " qel=" << qel << '\n';
  spexit(mcerr);
#ifdef INS_CRETURN    // Insert calming return
  return aln->el[0];  // to quiet Microsoft Visial Studio compiler against
                      // "not all control paths return a value"
#endif
}

template <class T> void BlkArr<T>::put_qel(long fqel) {
  //mcout<<"BlkArr<T>::put_qel: fqel="<<fqel<<'\n';
  if (fqel == qel) return;
  /*
  if(fqel == 0)
  {
    qel = 0;
    la.clear();
  }
  else
  {
  */
  //long n;
  AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
  if (aln == NULL) {
    la.insert_after(NULL, DynLinArr<T>(0));
    la.get_first_node()->el.put_qel(fqel);
    qel = fqel;
    return;
  } else {
    if (fqel > qel)  // the case of increase
        {
      long fqel_res = fqel;  // space to reserve
                             // fqel_res should be more than fqel
      // But remember, both are the total number of elements, not the increase
      if (fqel_res < 2 * qel)  // then consider more space to reserve
          {
        long ex = 2 * qel * size_of_element;   // compute the memory expense
        if (ex < max_size_of_extra_increment)  // check whether it is not large
          fqel_res = 2 * qel;                  // OK
        else {                                 // No, too large
          long q = max_size_of_extra_increment / size_of_element;  //less number
          if (q <= 0) q = 1;                                       // 1 at least
          if (fqel_res - qel < q) fqel_res = qel + q;              // increase
        }
      }
      long q;
      long left_old_qel = qel;
      long left_new_qel = fqel;
      long left_new_qel_res = fqel_res;
      //Iprintn(mcout, left_new_qel);
      //Iprintn(mcout, left_new_qel_res);
      AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
      do {
        q = aln->el.get_qel();
        //Iprintn(mcout, q);
        //Iprintn(mcout, left_old_qel);
        //Iprintn(mcout, left_new_qel);
        if (q >= left_old_qel)  // this node, there is space
            {
          if (q >= left_new_qel)  // that's enough, nothing to do
              {                   // and no reservation additional space
            qel = fqel;
            return;
          } else  // No, the new requested space is larger then the available
              {  // in the current node. But there may be next ones inited
                 // already
            left_new_qel -= q;
            left_new_qel_res -= q;
            AbsListNode<DynLinArr<T> >* aln1;  // pointer to block
            while (
                (aln1 = aln->get_next_node()) !=
                NULL) {  // scanning next blocks, which may be present already
              aln = aln1;
              q = aln->el.get_qel();
              if (q >= left_new_qel)  // Good, existing space is enogh
                  {
                qel = fqel;
                return;
              }
              left_new_qel -= q;
              left_new_qel_res -= q;
            }  // No, is not enough
               // then make new node with reservation
            la.insert_after(aln, DynLinArr<T>(0));
            la.get_last_node()->el.put_qel(left_new_qel_res);
            qel = fqel;
            //Iprintn(mcout, qel);
            return;
          }
        }
        left_old_qel -= q;
        left_new_qel -= q;
        aln = aln->get_next_node();
      } while (aln != NULL);
      mcout << "BlkArr<T>::put_qel: should never happen\n";
    } else  // the case of decrease
        {
      long q;
      long left_old_qel = qel;
      long left_new_qel = fqel;
      AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
      do {
        q = aln->el.get_qel();
        if (q >= left_new_qel)  // this node, there is space
            {
          // initialize by dummy constructor so as to get rid of garbage,
          // that is of elements at the end of this block
          long n;
          for (n = left_new_qel; n < q; n++) {
            aln->el[n] = T();
          }
          left_old_qel -= q;
          //left_new_qel -= q;
          AbsListNode<DynLinArr<T> >* aln1 = aln->get_next_node();
          if (aln1 != NULL)  // so preserving next node as histeresis
              {
            if (left_new_qel != 0)  // possible if fqel=0
                {  // if fqel=0, all the next nodes will be deleted in total
                   // later
              aln = aln1;
              q = aln->el.get_qel();
              if (left_old_qel > 0)  // then getting rid of garbage
                  {
                long l = left_old_qel;
                if (q < l) l = q;
                for (n = 0; n < l; n++) {
                  aln->el[n] = T();
                }
              }
            }
            while ((aln1 = aln->get_next_node()) != NULL)  // deleting all next
                // nodes
                {
              la.erase(aln1);
            }
          }
          qel = fqel;
          return;
        }
        left_old_qel -= q;
        left_new_qel -= q;
        aln = aln->get_next_node();
      } while (aln != NULL);
      qel = fqel;
    }
  }
}

template <class T> void BlkArr<T>::print_struct(std::ostream& file) const {
  Ifile << "structure: qel=" << qel << " size_of_element=" << size_of_element
        << "\n";
  indn.n += 2;
  long total_q = 0;
  long n = 0;
  AbsListNode<DynLinArr<T> >* aln(la.get_first_node());
  if (aln != NULL) {
    do {
      Ifile << "n=" << n << " qel=" << aln->el.get_qel() << '\n';
      total_q += aln->el.get_qel();
      aln = aln->get_next_node();
    } while (aln != NULL);
  }
  Ifile << "total_q=" << total_q << '\n';
  Ifile << "end of structure.\n";
  indn.n -= 2;
}

template <class T>
    std::ostream& operator<<(std::ostream& file, const BlkArr<T>& f) {
  //mfunnamep("template<class T> std::ostream& operator<<(std::ostream& file,
  //const BlkArr<T>& f)");
  Ifile << "BlkArr<T>: qel=" << f.get_qel() << '\n';
  f.print_struct(file);
  indn.n += 2;
  for (long n = 0; n < f.get_qel(); n++) {
    Ifile << "n=" << n << " el[n]=" << noindent << f[n] << yesindent << '\n';
  }
  file << yesindent;
  indn.n -= 2;
  return file;
}

template <class T>
void print_BlkArr(std::ostream& file, const DynLinArr<T>& f, int l) {
  Ifile << "BlkArr<T>: qel=" << f.get_qel() << '\n';
  f.print_struct(file);
  indn.n += 2;
  for (long n = 0; n < f.get_qel(); n++) {
    Ifile << "n=" << n << " el[n]=" << noindent;
    f[n].print(file, l);
  }
  file << yesindent;
  indn.n -= 2;
}

template <class T> int operator==(const BlkArr<T>& f1, const BlkArr<T>& f2) {
  if (f1.get_qel() != f2.get_qel()) return 0;
  const long q = f1.get_qel();
  for (long n = 0; n < q; n++) {
    if (!(f1[n] == f2[n])) return 0;
  }
  return 1;
}

template <class T, class P>
int apeq_mant(const BlkArr<T>& f1, const BlkArr<T>& f2, P prec) {
  if (f1.get_qel() != f2.get_qel()) return 0;
  const long q = f1.get_qel();
  for (long n = 0; n < q; n++) {
    if (!apeq_mant(f1[n], f2[n], prec)) return 0;
  }
  return 1;
}

template <class T> int operator!=(const BlkArr<T>& f1, const BlkArr<T>& f2) {
  return f1 == f2 ? 0 : 1;
}

template <class T>
DynLinArr<T> convert(const BlkArr<T>& f) {
  // may be useful if one needs single chunk
  const long q = f.get_qel();
  DynLinArr<T> temp(q);
  for (long n = 0; n < q; ++n) {
    temp[n] = f[n];
  }
  return DynLinArr<T>(temp, steal);
}
#endif
