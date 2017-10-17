/***************************************************************************
 *
 * $Id: StMatrix.hh,v 1.21 2016/01/22 17:10:50 smirnovd Exp $
 *
 * Author: Original code from CLHEP by Mike Smyth
 *         Modified April 17, 1998 Brian Lasiuk (templated version)
 *
 ***************************************************************************
 *
 * Description:
 * StMatrix class, does basic matrix operations
 *
 * Remarks:   Since not all compilers support member templates
 *            we have to specialize the templated member on these
 *            platforms. If member templates are not supported the
 *            ST_NO_MEMBER_TEMPLATES flag has to be set. tu.
 *
 ***************************************************************************
 *
 * $Log: StMatrix.hh,v $
 * Revision 1.21  2016/01/22 17:10:50  smirnovd
 * StarClassLibrary: Removed deprecated storage class specifier 'register'
 *
 * This keyword is deprecated since C++11 and serves no purpose
 *
 * "
 * The register specifier is only allowed for objects declared at block scope and
 * in function parameter lists. It indicates automatic storage duration, which is
 * the default for these kinds of declarations. Additionally, the presence of this
 * keyword may be used as a hint for the optimizer to store the value of this
 * variable in a CPU register.
 * "
 *
 * Revision 1.20  2012/06/11 15:29:26  fisyak
 * std namespace
 *
 * Revision 1.19  2006/04/07 22:02:34  ullrich
 * Fixed bug in dfinv and dfact that were affecting
 * determinant(), invert(), and inverse().
 *
 * Revision 1.18  2006/01/09 23:47:27  fisyak
 * Add missing methods (found by Zhangbu) to Cint dictionary
 *
 * Revision 1.17  2005/09/22 20:09:20  fisyak
 * Make StLorentzVector persistent
 *
 * Revision 1.16  2005/07/06 18:49:56  fisyak
 * Replace StHelixD, StLorentzVectorD,StLorentzVectorF,StMatrixD,StMatrixF,StPhysicalHelixD,StThreeVectorD,StThreeVectorF by templated version
 *
 * Revision 1.15  2004/01/14 22:37:27  fisyak
 *  unsigned int => size_t to make alpha happy
 *
 * Revision 1.14  2003/09/02 17:59:35  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.13  2002/07/02 22:20:53  ullrich
 * Add dummy return statements to get rid of warnings under Linux.
 *
 * Revision 1.12  2001/10/31 15:11:36  ullrich
 * Rewrote swap() to work as non-friend to StMatrix.
 *
 * Revision 1.11  2001/10/31 00:33:34  ullrich
 * Remove macro ifdef for GCC which is not needed anymore.
 *
 * Revision 1.10  2000/02/02 18:31:05  lasiuk
 * restore files
 *
 * Revision 1.9  2000/02/01 16:02:59  lasiuk
 * namespace std is different on SUN CC5 and KCC.  Redefine macros!
 *
 * Revision 1.8  2000/01/31 20:53:45  lasiuk
 * using std::swap
 *
 * Revision 1.7  1999/12/21 15:14:13  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.6  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.5  1999/04/14 23:12:07  fisyak
 * Add __CINT__ to handle references
 *
 * Revision 1.4  1999/03/04 18:12:24  ullrich
 * Added namespace 'std'.
 *
 * Revision 1.3  1999/02/17 11:38:54  ullrich
 * Removed specialization for 'long double'.
 *
 * Revision 1.2  1999/02/14 23:11:43  fisyak
 * Fixes for Rootcint
 *
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
// This code was modified from the CLHEP files:
// GenMatrixD.h/.icc/.cc MatrixD.h/.icc/.cc
// Comments that were kept from the original code are preceding
// by ///.

///"This code has been written by Mike Smyth, and the algorithms used are
///"described in the thesis "A Tracking Library for a Silicon Vertex Detector"
///"(Mike Smyth, Cornell University, June 1993).
///"Copyright (C) Cornell University 1993. Permission is granted to copy and 
///"distribute this code, provided this copyright is not changed or deleted.
///"You may modify your copy, providing that you cause the modified file to
///"carry prominent notices stating that you changed the files, and the date
///"of any change. This code may not be sold, nor may it be contained in
///"programs that are to be sold without the written permission of the author.
///"You may, however, charge a fee for the physical act of transferring a
///"copy of this code. The code is offered "as is" without warranty of any 
///"kind, either expressed or implied.  By copying, distributing, or 
///"modifying this code you indicate your acceptance of this license to
///"do so, and all its terms and conditions.
///"This is file contains C++ stuff for doing things with Matrices.
///"To turn on bound checking, define MATRIX_BOUND_CHECK before including
///"this file.
///
/// Usage:
//  The Matrix class provides the tools to do matrix arithmetic.
//  The Matrices are templates with the default type being "double"
//  A Matrix is declared by:
//
//   StMatrix<double> m1(n,m);    // n x m matrix
//   StMatrix<> m2(n,m);          // default type is double
//
//  A Matrix may be initialized by giving it a third integer
//  argument, either 0 or 1:
//  - Zero (0) means initialize all elements to 0.
//  - One (1) means initialize to the identity matrix if the matrix is
//    square (NxN); otherwise an exception is thrown.
//  - The default behavior is to initialize all elements to zero (0)
//
//   StMatrix<double> m2(n, m, 0);  // all zeros
//   StMatrix<double> m1(n, n, 1);  // diagnol elements are 1 
//   StMatrix<double> m1(n, m, 1);  // error if m!=n
//
//  Copying and Assignment:
//
//   StMatrix<double> m1(m2);      // m2 copied to m1
//   StMatrix<double> m1 = m2;     // m1 assigned value of m1
//
//  Element Access
//  Single elements of the matrix can be accessed in two differernt ways:
//  - C array style operators []
//
//  For a pXq matrix:
//  --> m1[i][j]:
//       0 <= i <= (p-1)
//       0 <= j <= (q-1)
//
//  - Fortran array style operators ()
//  --> m1(i,j):
//       1 <= i <= p
//       1 <= j <= q
//
///  Assignments and Reads:
///
///  m(row, col) = 6;        // Assign element (row, col)
///  m[row][col] = 6.;       // Assign element (row-1, col-1)
///  double a = m(row, col); // Assignment
///  double a = m[row][col]; // "
//
///  If bound checking is desired, include the line:
//   #define MATRIX_BOUND_CHECK
//   before including StMatrix.hh
/// 
//  Access Functions:
//  Properties of the matrices can be extracted via the following
//  access functions:
//
//   m1.num_row();   // number of rows
//   m1.numRow();    // "
//   m1.num_col();   // number of columns
//   m1.numCol();    // "
//
/// Operations:
//  - Scaler operations:
//    A matrix may be multiplied or divided by a scaler
//    via the /= and *= operators:
//    m1 /= 5;      // divides all elements in matrix m1 by 5
//
//  - Matrix operations:
//    Two matrices can be added, subtracted, and multiplied with the
//    operators +=, -=, *=
//
//    m1 += m2;   // same as m1 = m1+m2;
//
/// Other functions
///
///  cout << m1;             // print a formatted matrix out
///  mt = m.T();             // transpose of StMatrix m
//   mt = m.transpose() ;    // "
//
//   unsigned int ierr;      // unsigned int == unsigned int
///  minv = m.inverse(ierr); // inverts a matrix
//                           // ierr will be non-zero if the matrix is singular.
///  m.invert(ierr);         // overwrites the matrix m with its inverse!
//                           // faster (avoids copy operator)
///
///  ms = m2.sub(row_min, row_max, col_min, col_max); //  returns the subMatrix.
//                                                    //  m2(row_min:row_max, col_min:col_max)
///  m2.sub(row, col, m1);   //  m2(row:row+m1.num_row()-1, col:col+m1.num_col()-1)
//                           //  is replaced with m1.
///
///  m = m1.apply(HEP_MATRIX_ELEMENT (*f)(HEP_MATRIX_ELEMENT, int r, int c));
//                //  applies f to every element of m1 and places it in m.
//
///  md = dsum(m1,m2);       // returns the direct sum of the two matrices.
///
//
//  There also exists functionality to multiply a
//  1) StThreeVector
//  2) StLorentzVector
//  3) Std C++ Library vector<>
//  by a matrix.
//
//  StThreeVector<> a(1,1,1);
//  StMatrix<> M1(3,3,1);
//  StThreeVector<> b = a*M1;   // multiplication by identity matrix
//
//  This can be used for simple rotations.  The same syntax is also used
//  for the LorentzVector and Std C++ vector.  In order to use the
//  StThreeVector and StLorentzVector capablilties simply define:
//
//  #define WITH_ST_THREEVECTOR     // for use with StThreeVector
//  #define WITH_ST_LORENTZVECTOR   // for use with StLorentzVector
///
/// ./" The program that this class was orginally used with lots of small
/// ./" Matrices.  It was very costly to malloc and free array space every
/// ./" time a Matrix is needed.  So, a number of previously freed arrays are
/// ./" kept around, and when needed again one of these array is used.  Right
/// ./" now, a set of piles of arrays with row <= row_max and col <= col_max
/// ./" are kept around.  The piles of kept Matrices can be easily changed.
/// 

#ifndef ST_MATRIX_HH
#define ST_MATRIX_HH

#include <Stiostream.h>
#include <math.h>
#include <vector>
#include <float.h>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::out_of_range;
        using std::domain_error;
#   endif
#endif

#include "StThreeVector.hh"
#include "StLorentzVector.hh"

// in types.h assumed:
// typedef   unsigned int  size_t;

template<class DataType> class StMatrix {
public:
    StMatrix();
    StMatrix(size_t p, size_t q, size_t init=0);

    // Copy constructor.
#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
    template<class X>
    StMatrix(const StMatrix<X>&);
    StMatrix(const StMatrix<DataType>&);
#else
    StMatrix(const StMatrix<float>&);
    StMatrix(const StMatrix<double>&);
#endif
    
    // Assignment operators.
#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
    template<class X>
    StMatrix<DataType>& operator=(const StMatrix<X>&);
    StMatrix<DataType>& operator=(const StMatrix<DataType>&);
#else
    StMatrix<DataType>& operator=(const StMatrix<float>&);
    StMatrix<DataType>& operator=(const StMatrix<double>&);
#endif
    
    // Destructor. --
    //Problem with LINUX (virtual table error when exception thrown)
    virtual ~StMatrix();

    //
    // access functions
    //
    unsigned int numRow()  const;
    unsigned int numCol()  const;
    unsigned int numSize() const;
    unsigned int num_row()  const;
    unsigned int num_col()  const;
    unsigned int num_size() const;
    //
    // Element access:
    // ** Note that the indexing starts from:
    // For (,)   --> (1,1). **
    // For [][] --> [0][0]
    const DataType& operator()(size_t row, size_t col) const;
    DataType& operator()(size_t row, size_t col);

    // classes for implementing m[i][j]
    class StMatrixRow {
    public:
	StMatrixRow(StMatrix<DataType>&, size_t);
	DataType& operator[](size_t);
    private:
#ifndef __CINT__
	StMatrix<DataType>& _a;
#else
	StMatrix<DataType>* _a;
#endif
	size_t _r;
    };
    class StMatrixRowConst {
    public:
	StMatrixRowConst (const StMatrix<DataType>&, size_t);
	const DataType & operator[](size_t) const;
    private:
#ifndef __CINT__
	const StMatrix<DataType>& _a;
#else
	const StMatrix<DataType>* _a;
#endif
	size_t _r;
    };

    StMatrixRow operator[] (size_t r);
    const StMatrixRowConst operator[] (size_t) const;
    
    // Scaler Operations
    StMatrix<DataType>& operator*=(double t);
    StMatrix<DataType>& operator/=(double t);

    // Matrix Operations
#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
    template<class X> StMatrix<DataType>& operator+=(const StMatrix<X>&);
    template<class X> StMatrix<DataType>& operator-=(const StMatrix<X>&);
    template<class X> StMatrix<DataType>  dot(const StMatrix<X>&);
#else
    StMatrix<DataType>& operator+=(const StMatrix<float>&);
    StMatrix<DataType>& operator+=(const StMatrix<double>&);
    
    StMatrix<DataType>& operator-=(const StMatrix<float>&);
    StMatrix<DataType>& operator-=(const StMatrix<double>&);

    StMatrix<DataType>  dot(const StMatrix<float>&);
    StMatrix<DataType>  dot(const StMatrix<double>&);
#endif
    
    // unary Operations
    StMatrix<DataType> operator+ () const;
    StMatrix<DataType> operator- () const;

    // Equality
    bool operator==(const StMatrix<DataType>&) const;
    bool operator!=(const StMatrix<DataType>&) const;
    
    // Apply a function to all elements of the matrix.
    StMatrix<DataType> apply(DataType (*f)(DataType, size_t, size_t)) const;

    StMatrix<DataType> T() const;
    StMatrix<DataType> transpose() const;

    StMatrix<DataType> sub(size_t min_row, size_t max_row, size_t min_col, size_t max_col) const;
    
    void sub(size_t row, size_t col, const StMatrix<DataType> &m1);

    StMatrix<DataType> inverse(size_t& ierr) const;
    void invert(size_t& ierr);

    DataType determinant() const;
    
    // Must be of same type to swap
    static void   swap(unsigned int&, unsigned int&);
    static void   swap(DataType *&, DataType *&);
    
private:
    // Friend classes.
    friend class  StMatrixRow;
    friend class  StMatrixRowConst;

    // If successful, the return code is 0.
    // On return, det is the determinant and ir[] is row-unsigned interchange
    // matrix. See CERNLIB's DFACT routine.
    int dfact(DataType &det, int *ir); // factorize the matrix. 

    // invert the matrix. See CERNLIB DFINV.
    int dfinv(int *ir);

protected:
    DataType *mElement;
    unsigned int mRow, mCol;
    unsigned int mSize;
#ifdef __ROOT__
  ClassDef(StMatrix,3)
#endif /* __ROOT__ */
};

#ifndef __CINT__
// Constructors. 

template<class DataType>
inline StMatrix<DataType>::StMatrix()
    : mElement(0), mRow(0), mCol(0), mSize(0) {}

template<class DataType>
StMatrix<DataType>::StMatrix(size_t p,size_t q, size_t init)
    : mRow(p), mCol(q)
{
    mSize    = mRow*mCol;
    mElement = new DataType[mSize];

    DataType *a = mElement;
    DataType *b = mElement + mSize;
    for( ; a<b; a++)
	*a = 0;
    
    if (mSize > 0) {
	switch(init)
	    {
	    case 0:
		break;
	    case 1:
		if ( mCol == mRow ) {
		    a = mElement;
		    b = mElement + mSize;
		    for( ; a<b; a+=(mCol+1)) *a = 1.0;
		}
		else {
#ifndef ST_NO_EXCEPTIONS
		    throw domain_error("StMatrix<T>::StMatrix(): Matrix must be NxN");
#else
		    cerr << "StMatrix<T>::StMatrix(): Matrix must be NxN" << endl;
#endif
		}
		break;
	    default:
#ifndef ST_NO_EXCEPTIONS
		throw domain_error("StMatrix<T>::StMatrix(p,q,init): init must be 0 or 1");
#else
		cerr << "StMatrix<T>::StMatrix(p,q,init): init must be 0 or 1" << endl;
#endif
		break;
	    }
    }
}

#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
template<class DataType>
template<class X>
StMatrix<DataType>::StMatrix(const StMatrix<X>& m1)
    : mRow(m1.numRow()), mCol(m1.numCol()), mSize(m1.numSize())
{
    mElement = new DataType[mSize];

    for(unsigned int ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
}

template<class DataType>
StMatrix<DataType>::StMatrix(const StMatrix<DataType>& m1)
    : mRow(m1.numRow()), mCol(m1.numCol()), mSize(m1.numSize())
{
    mElement = new DataType[mSize];

    for(unsigned int ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
}
#else
template<class DataType>
StMatrix<DataType>::StMatrix(const StMatrix<float>& m1)
    : mRow(m1.numRow()), mCol(m1.numCol()), mSize(m1.numSize())
{
    mElement = new DataType[mSize];

    for(unsigned int ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
}

template<class DataType>
StMatrix<DataType>::StMatrix(const StMatrix<double>& m1)
    : mRow(m1.numRow()), mCol(m1.numCol()), mSize(m1.numSize())
{
    mElement = new DataType[mSize];

    for(unsigned int ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
}

#endif

#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
template<class DataType>
template<class X>
StMatrix<DataType>& StMatrix<DataType>::operator=(const StMatrix<X>& m1)
{   
    if ((void *)&m1 == (void *)this) {
	return *this;
    }
    else {
	delete [] mElement;
	mSize    = m1.numRow()*m1.numCol();
	mElement = new DataType[mSize];

	mRow = m1.numRow();
	mCol = m1.numCol();
	
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		*(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
	    }
	return (*this);
    }
}

template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator=(const StMatrix<DataType>& m1)
{   
    if ((void *)&m1 == (void *)this) {
	return *this;
    }
    else {
	delete [] mElement;
	mSize    = m1.numRow()*m1.numCol();
	mElement = new DataType[mSize];

	mRow = m1.numRow();
	mCol = m1.numCol();
	
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		*(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
	    }
	return (*this);
    }
}
#else // ST_NO_MEMBER_TEMPLATES
template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator=(const StMatrix<float>& m1)
{   
    if ((void *)&m1 == (void *)this) {
	return *this;
    }
    else {
	delete [] mElement;
	mSize    = m1.numRow()*m1.numCol();
	mElement = new DataType[mSize];

	mRow = m1.numRow();
	mCol = m1.numCol();
	
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		*(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
	    }
	return (*this);
    }
}
template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator=(const StMatrix<double>& m1)
{   
    if ((void *)&m1 == (void *)this) {
	return *this;
    }
    else {
	delete [] mElement;
	mSize    = m1.numRow()*m1.numCol();
	mElement = new DataType[mSize];

	mRow = m1.numRow();
	mCol = m1.numCol();
	
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		*(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
	    }
	return (*this);
    }
}

#endif

// Destructor
template<class DataType>
StMatrix<DataType>::~StMatrix() {
    delete [] mElement;
}

//
// access functions
//
template<class DataType>
unsigned int StMatrix<DataType>::numRow() const { return mRow;}

template<class DataType>
unsigned int StMatrix<DataType>::numCol() const  { return mCol;}

template<class DataType>
unsigned int StMatrix<DataType>::numSize() const { return mSize;}

// backward compatibility
template<class DataType>
unsigned int StMatrix<DataType>::num_row() const { return mRow;}

template<class DataType>
unsigned int StMatrix<DataType>::num_col() const  { return mCol;}

template<class DataType>
unsigned int StMatrix<DataType>::num_size() const { return mSize;}

//
// Element Access
//
template<class DataType>
const DataType& StMatrix<DataType>::operator()(size_t row, size_t col) const 
{
#ifdef MATRIX_BOUND_CHECK
    if(row<1 || row>numRow() || col<1 || col>numCol()) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<DataType>::operator(): const Bad Index");
#else
	cerr << "StMatrix<DataType>::operator(): const Bad Index" << endl;
#endif
    }
#endif
    return *(mElement+(row-1)*mCol+col-1);
}

template<class DataType>
DataType& StMatrix<DataType>::operator()(size_t row, size_t col)
{
#ifdef MATRIX_BOUND_CHECK
    if(row<1 || row>mRow || col<1 || col>mCol) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<DataType>::operator(): Bad Index");
#else
	cerr << "StMatrix<DataType>::operator(): Bad Index" << endl;
#endif
    }
#endif
    return *(mElement+(row-1)*mCol+col-1);
}

/* -----------------------------------------------------------------------
   This section contains the assignment and inplace operators =,+=,-=,*=,/=.
   ----------------------------------------------------------------------- */

template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator*=(double fact)
{
    for(unsigned int ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) *= fact;
    
    return (*this);
}

template<class DataType>
StMatrix<DataType> & StMatrix<DataType>::operator/=(double fact)
{
    if(fact == 0) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<T>::operator/=(): Cannot divide by zero!");
#else
	cerr << "StMatrix<T>::operator/=(): Cannot divide by zero!" << endl;
#endif
    }
    for(unsigned int ii=0; ii<mCol; ii++)
 	for(unsigned int jj=0; jj<mRow; jj++)
 	    *(mElement+(ii)*mCol+jj) /= fact;
    
    return (*this);
}

#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
template<class DataType>
template<class X>
StMatrix<DataType>& StMatrix<DataType>::operator+=(const StMatrix<X>& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) += m2(ii+1,jj+1);
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<T>::operator+=(): Matrices are not same size!");
#else
	cerr << "StMatrix<T>::operator+=(): Matrices are not same size!" << endl;
#endif
    }	
    return (*this);
}

template<class DataType>
template<class X>
StMatrix<DataType>& StMatrix<DataType>::operator-=(const StMatrix<X>& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) -= m2(ii+1,jj+1);
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<T>::operator-=(): Matrices are not same size!");
#else
	cerr << "StMatrix<T>::operator-=(): Matrices are not same size!" << endl;
#endif
    }
    return (*this);
}

template<class DataType>
template<class X>
StMatrix<DataType> StMatrix<DataType>::dot(const StMatrix<X>& m2)
{
    if(mCol == m2.numRow() ) {
	StMatrix<DataType> mret(mRow, m2.numCol(), 0);	
	for(unsigned int i=0; i<mRow; i++)
	    for(unsigned int j=0; j<m2.numCol(); j++) {
		for(unsigned int kk=0; kk<mCol; kk++)
		    mret(i+1, j+1) += (*(mElement+(i)*mCol+kk))*m2(kk+1,j+1);
	    }
	return mret;
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<T>::dot(): incompatible matrix sizes");
#else
	cerr << "StMatrix<T>::dot(): incompatible matrix sizes" << endl;
#endif
	return StMatrix();
    }
}
#else  // ST_NO_MEMBER_TEMPLATES
// operator+=
template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator+=(const StMatrix<float>& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) += m2(ii+1,jj+1);
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<float>::operator+=(): Matrices are not same size!");
#else
	cerr << "StMatrix<float>::operator+=(): Matrices are not same size!" << endl;
#endif
    }	
    return (*this);
}

template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator+=(const StMatrix<double>& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) += m2(ii+1,jj+1);
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<double>::operator+=(): Matrices are not same size!");
#else
	cerr << "StMatrix<double>::operator+= Matrices are not same size!" << endl;
#endif
    }	
    return (*this);
}

// operator -=
template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator-=(const StMatrix<float>& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) -= m2(ii+1,jj+1);
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<float>::operator-=(): Matrices are not same size!");
#else
	cerr << "StMatrix<float>::operator-=(): Matrices are not same size!" << endl;
#endif
    }
    return (*this);
}

template<class DataType>
StMatrix<DataType>& StMatrix<DataType>::operator-=(const StMatrix<double>& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) -= m2(ii+1,jj+1);
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<double>::operator-=(): Matrices are not same size!");
#else
	cerr << "StMatrix<double>::operator-=(): Matrices are not same size!" << endl;
#endif
    }
    return (*this);
}

// operator::dot
template<class DataType>
StMatrix<DataType> StMatrix<DataType>::dot(const StMatrix<float>& m2)
{
    if(mCol == m2.numRow() ) {
	StMatrix<DataType> mret(mRow, m2.numCol(), 0);
	
	for(unsigned int i=0; i<mRow; i++)
	    for(unsigned int j=0; j<m2.numCol(); j++) {
		for(unsigned int kk=0; kk<mCol; kk++)
		    mret(i+1, j+1) += (*(mElement+(i)*mCol+kk))*m2(kk+1,j+1);
	    }
	return mret;
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<float>::dot(): Incompatible matrix sizes");
#else
	cerr << "StMatrix<float>::dot(): Incompatible matrix sizes" << endl;
#endif
	return StMatrix();
    }
}

template<class DataType>
StMatrix<DataType> StMatrix<DataType>::dot(const StMatrix<double>& m2)
{
    if(mCol == m2.numRow() ) {
	StMatrix<DataType> mret(mRow, m2.numCol(), 0);
	
	for(unsigned int i=0; i<mRow; i++)
	    for(unsigned int j=0; j<m2.numCol(); j++) {
		for(unsigned int kk=0; kk<mCol; kk++)
		    mret(i+1, j+1) += (*(mElement+(i)*mCol+kk))*m2(kk+1,j+1);
	    }
	return mret;
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<double>::dot(): Incompatible matrix sizes");
#else
	cerr << "StMatrix<double>::dot(): Incompatible matrix sizes" << endl;
#endif
	return StMatrix();
    }
}

///////////////////////////////////////////////////////////////////////
#endif // ST_NO_MEMBER_TEMPLATES

template<class DataType>
StMatrix<DataType> StMatrix<DataType>::operator+ () const 
{
    return *this;
}

template<class DataType>
StMatrix<DataType> StMatrix<DataType>::operator- () const 
{
    // this can be streamlined
    StMatrix<DataType> mret(*this);
    return mret*=-1;
}

template<class DataType>
bool StMatrix<DataType>::operator== (const StMatrix<DataType>& m1) const
{
    if (mCol == m1.numCol() && mRow == m1.numRow()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		if(*(mElement+(ii)*mCol+jj) != m1(ii+1,jj+1))
		    return false;
	    }
	return true;
    }
    else
	return false;  // not even right size
}

template<class DataType>
bool StMatrix<DataType>::operator!= (const StMatrix<DataType>& m1) const
{
    return !(*this == m1);
}

//Apply a function to all elements
template<class DataType>
StMatrix<DataType> StMatrix<DataType>::apply(DataType (*f)(DataType, size_t, size_t)) const
{
    StMatrix<DataType> mret(mRow, mCol);
    DataType *a = mElement;
    for(unsigned int ir=1; ir<=mRow; ir++) {
	for(unsigned int ic=1; ic<=mCol; ic++) {
	    mret(ir,ic) = (*f)(*(a++), ir, ic);
	}
    }
    return mret;
}

template<class DataType>
StMatrix<DataType> StMatrix<DataType>::T() const
{
    StMatrix<DataType> mret(mCol,mRow);
    DataType *pl = mElement + mSize;
    DataType *pme = mElement;
    DataType *pt = mret.mElement;
    DataType *ptl = mret.mElement + mSize;
    for (; pme < pl; pme++, pt+=mRow)
	{
	    if (pt >= ptl)
		pt -= (mSize-1);
	    (*pt) = (*pme);
	}
    return mret;
}
template<class DataType>
StMatrix<DataType> StMatrix<DataType>::transpose() const
{
    return this->T();
}

//
// Sub matrix
//
template<class DataType>
StMatrix<DataType> StMatrix<DataType>::sub(size_t min_row, size_t max_row,
						 size_t min_col,size_t max_col) const
{
    StMatrix<DataType> mret(max_row-min_row+1,max_col-min_col+1);
    if(max_row > mRow || max_col > mCol) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<DataType>::sub(): Index out of range");
#else
	cerr << "StMatrix<DataType>::sub(): Index out of range" << endl;
#endif
    }
    int nrows = mret.numRow();
    int ncols = mret.numCol();
    for(int ii=0; ii<nrows; ii++)
	for(int jj=0; jj<ncols; jj++) {
	    mret(ii+1, jj+1) =
		*(mElement+(min_row+ii-1)*mCol+(min_col+jj-1));
	}
    return mret;
}

template<class DataType>
void StMatrix<DataType>::sub(size_t row,size_t col,const StMatrix<DataType> &m1)
{
    if((row <1)                   ||
       (row+m1.numRow()-1 > mRow) ||
       (col <1)                   ||
       (col+m1.numCol()-1 > mCol)) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<DataType>::sub(): Index out of range");
#else
	cerr << "StMatrix<DataType>::sub(): Index out of range" << endl;
#endif
    }
    DataType *a = m1.mElement;
    unsigned int nc = numCol();
    DataType *b1 = mElement + (row - 1) * nc + col - 1;
    
    for(unsigned int irow=1; irow<=m1.numRow(); irow++) {
	DataType *brc = b1;
	for(unsigned int icol=1; icol<=m1.numCol(); icol++) {
	    *(brc++) = *(a++);
	}
	b1 += nc;
    }
}

// Contents of the matrix are replaced by the inverse--less overhead then inverse().
template<class DataType>
StMatrix<DataType> StMatrix<DataType>::inverse(size_t &ierr) const
{
    StMatrix<DataType> tmp(*this);
    tmp.invert(ierr);
    return tmp;
}

template<class DataType>
void StMatrix<DataType>::invert(size_t &ierr) {
    if(mCol != mRow) {
#ifndef ST_NO_EXCEPTIONS
	throw domain_error("StMatrix<DataType>::invert(): not a NxN matrix");
#else
	cerr << "StMatrix<DataType>::invert(): not a NxN matrix" << endl;
#endif
    }
    static unsigned int max_array = 20;
    static int *ir = new int [max_array+1];
    
    if (mCol > max_array) {
	delete [] ir;
	max_array = mRow;
	ir = new int [max_array+1];
    }
    DataType t1, t2, t3;
    DataType det, temp, s;
    int ifail;
    switch(mRow) {
    case 3:
	{
	    DataType c11,c12,c13,c21,c22,c23,c31,c32,c33;
	    ifail = 0;
	    c11 = (*(mElement+4)) * (*(mElement+8)) - (*(mElement+5)) * (*(mElement+7));
	    c12 = (*(mElement+5)) * (*(mElement+6)) - (*(mElement+3)) * (*(mElement+8));
	    c13 = (*(mElement+3)) * (*(mElement+7)) - (*(mElement+4)) * (*(mElement+6));
	    c21 = (*(mElement+7)) * (*(mElement+2)) - (*(mElement+8)) * (*(mElement+1));
	    c22 = (*(mElement+8)) * (*mElement)     - (*(mElement+6)) * (*(mElement+2));
	    c23 = (*(mElement+6)) * (*(mElement+1)) - (*(mElement+7)) * (*mElement);
	    c31 = (*(mElement+1)) * (*(mElement+5)) - (*(mElement+2)) * (*(mElement+4));
	    c32 = (*(mElement+2)) * (*(mElement+3)) - (*mElement) * (*(mElement+5));
	    c33 = (*mElement)     * (*(mElement+4)) - (*(mElement+1)) * (*(mElement+3));
	    t1  = fabs(*mElement);
	    t2  = fabs(*(mElement+3));
	    t3  = fabs(*(mElement+6));
	    if (t1 >= t2) {
		if (t3 >= t1) {
		    temp = *(mElement+6);
		    det = c23*c12-c22*c13;
		} else {
		    temp = *mElement;
		    det = c22*c33-c23*c32;
		}
	    } else if (t3 >= t2) {
		temp = *(mElement+6);
		det = c23*c12-c22*c13;
	    } else {
		temp = *(mElement+3);
		det = c13*c32-c12*c33;
	    }
	    if (det==0) {
		ierr = 1;
		return;
	    }
	    {
		s = temp/det;
		DataType *tmp = mElement;
		*(tmp++) = s*c11;
		*(tmp++) = s*c21;
		*(tmp++) = s*c31;
		*(tmp++) = s*c12;
		*(tmp++) = s*c22;
		*(tmp++) = s*c32;
		*(tmp++) = s*c13;
		*(tmp++) = s*c23;
		*(tmp) = s*c33;
	    }
	}
	break;
    case 2:
	ifail = 0;
	det = (*mElement)*(*(mElement+3)) - (*(mElement+1))*(*(mElement+2));
	if (det==0) {
	    ierr = 1;
	    return;
	}
	s = 1.0/det;
	temp = s*(*(mElement+3));
	*(mElement+1) *= -s;
	*(mElement+2) *= -s;
	*(mElement+3) = s*(*mElement);
	*mElement = temp;
	break;
    case 1:
	ifail = 0;
	if ((*mElement)==0) {
	    ierr = 0;
	    return;
	}
	*mElement = 1.0/(*mElement);
	break;
    default:
	ifail = dfact(det, ir);
	if(ifail) {
	    ierr = 1;
	    return;
	}
	dfinv(ir);
	break;
    }
    ierr = 0;
    return;
}

template<class DataType>
DataType StMatrix<DataType>::determinant() const {
    static unsigned int max_array = 20;
    static int *ir = new int [max_array+1];
    if(mCol != mRow) {
#ifndef ST_NO_EXCEPTIONS
		throw out_of_range("StMatrix<DataType>::determinant(): not a NxN matrix");
#else
		cerr << "StMatrix<DataType>::determinant(): not a NxN matrix" << endl;
#endif
    }
    if (mCol > max_array) {
	delete [] ir;
	max_array = mRow;
	ir = new int [max_array+1];
    }
    DataType det;
    StMatrix<DataType> mt(*this);
    int i = mt.dfact(det, ir);
    if(i==0) return det;
    return 0;
}

//
// Access operators
//
template<class DataType>
StMatrix<DataType>::StMatrixRow::StMatrixRow(StMatrix<DataType>& a, size_t r) 
    : _a(a) {
	_r = r;
}

template<class DataType>
DataType& StMatrix<DataType>::StMatrixRow::operator[](size_t c) {
#ifdef MATRIX_BOUND_CHECK
    if (_r<0 || _r>=_a.numRow() || c<0 || c>=_a.numCol()) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<DataType>::operator[]: index out of range");
#else
	cerr << "StMatrix<DataType>::operator[]: index out of range" << endl;
#endif
    }
#endif
    return *(_a.mElement+_r*_a.mCol+c);
}

template<class DataType>
StMatrix<DataType>::StMatrixRowConst::StMatrixRowConst(const StMatrix<DataType>& a, size_t r) 
    : _a(a) {
    _r = r;
}

template<class DataType>
const DataType& StMatrix<DataType>::StMatrixRowConst::operator[](size_t c) const
{
#ifdef MATRIX_BOUND_CHECK
    if (_r<0 || _r>=_a.numRow() || c<0 || c>=_a.numCol()) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("StMatrix<DataType>::operator[]: const index out of range");
#else
	cerr << "StMatrix<DataType>::operator[]: const index out of range" << endl;
#endif
    }
#endif
    return *(_a.mElement+_r*_a.mCol+c);
}

template<class DataType> 
typename StMatrix<DataType>::StMatrixRow StMatrix<DataType>::operator[] (size_t r)
{
    StMatrixRow b(*this,r);
    return b;
}

template<class DataType>
const typename StMatrix<DataType>::StMatrixRowConst StMatrix<DataType>::operator[] (size_t r) const
{
    StMatrixRowConst b(*this,r);
    return b;
}

template<class DataType>
void StMatrix<DataType>::swap(unsigned int &i, unsigned int &j)
{
    unsigned int tmp=i;
    i=j;
    j=tmp;
}

template<class DataType>
void StMatrix<DataType>::swap(DataType *&i, DataType *&j)
{
    DataType *tmp=i;
    i=j;
    j=tmp;
}

// This function swaps two Matrices doing a full copy
template<class DataType>
void swap(StMatrix<DataType>& m1, StMatrix<DataType>& m2) {
    StMatrix<DataType> tmp(m1);
    m1 = m2;
    m2 = tmp;
}

// This function swaps two Matrices without doing a full copy.
// The version below had some problems with the new GCC compiler
// (friendship with StMatrix caused trouble) and was replaced
// by the version above. tu
// template<class DataType>
// void swap(StMatrix<DataType>& m1, StMatrix<DataType>& m2) {
//     StMatrix<DataType>::swap(m1.mElement, m2.mElement);
//     StMatrix<DataType>::swap(m1.mRow,     m2.mRow);
//     StMatrix<DataType>::swap(m1.mCol,     m2.mCol);
//     StMatrix<DataType>::swap(m1.mSize,    m2.mSize);
// }


template<class DataType>
int StMatrix<DataType>::dfact(DataType &det, int *ir)
{
    if (mCol!=mRow) {
#ifndef ST_NO_EXCEPTIONS
	throw domain_error("StMatrix<DataType>::dfact(): Matrix not NxN");
#else
	cerr << "StMatrix<DataType>::dfact(): Matrix not NxN" << endl;
#endif
    }
 
    int ifail, jfail;
    int n = mCol;
 
    DataType tf;
    DataType g1 = 1.0e-19;
    DataType g2 = 1.0e19;
 
    DataType p, q, t;
    DataType s11, s12;

    
    DataType epsilon;
    if (sizeof(DataType) == sizeof(double))
        epsilon = 8*DBL_EPSILON;
    else
        epsilon = 8*FLT_EPSILON;
    // could be set to zero (like it was before)
    // but then the algorithm often doesn't detect
    // that a matrix is singular
    
    int normal = 0, imposs = -1;
    int jrange = 0, jover = 1, junder = -1;
    ifail = normal;
    jfail = jrange;
    int nxch = 0;
    det = 1.0;
    DataType *mj = mElement;
    DataType *mjj = mj;
    for (int j=1;j<=n;j++) {
        int k = j;
        p = (fabs(*mjj));
        if (j!=n) {
            DataType *mij = mj + n + j - 1;
            for (int i=j+1;i<=n;i++) {
                q = (fabs(*(mij)));
                if (q > p) {
                    k = i;
                    p = q;
                }
                mij += n;
            }
            if (k==j) {
                if (p <= epsilon) {
                    det = 0;
                    ifail = imposs;
                    jfail = jrange;
                    return ifail;
                }
                det = -det; // in this case the sign of the determinant
                // must not change. So I change it twice.
            }
            DataType *mjl = mj;
            DataType *mkl = mElement + (k-1)*n;
            for (int l=1;l<=n;l++) {
                tf = *mjl;
                *(mjl++) = *mkl;
                *(mkl++) = tf;
            }
            nxch = nxch + 1;  // this makes the determinant change its sign
            ir[nxch] = (((j)<<12)+(k));
        } else {
            if (p <= epsilon) {
                det = 0.0;
                ifail = imposs;
                jfail = jrange;
                return ifail;
            }
        }
        det *= *mjj;
        *mjj = 1.0 / *mjj;
        t = (fabs(det));
        if (t < g1) {
            det = 0.0;
            if (jfail == jrange) jfail = junder;
        }
        else if (t > g2) {
            det = 1.0;
            if (jfail==jrange) jfail = jover;
        }
        if (j!=n) {
            DataType *mk = mj + n;
            DataType *mkjp = mk + j;
            DataType *mjk = mj + j;
            for (k=j+1;k<=n;k++) {
                s11 = - (*mjk);
                s12 = - (*mkjp);
                if (j!=1) {
                    DataType *mik = mElement + k - 1;
                    DataType *mijp = mElement + j;
                    DataType *mki = mk;
                    DataType *mji = mj;
                    for (int i=1;i<j;i++) {
                        s11 += (*mik) * (*(mji++));
                        s12 += (*mijp) * (*(mki++));
                        mik += n;
                        mijp += n;
                    }
                }
                *(mjk++) = -s11 * (*mjj);
                *(mkjp) = -(((*(mjj+1)))*((*(mkjp-1)))+(s12));
                mk += n;
                mkjp += n;
            }
        }
        mj += n;
        mjj += (n+1);
    }
    if (nxch%2==1) det = -det;
    if (jfail !=jrange) det = 0.0;
    ir[n] = nxch;
    return 0;
}

template<class DataType>
int StMatrix<DataType>::dfinv(int *ir)
{
    if (mCol != mRow) {
#ifndef ST_NO_EXCEPTIONS
	throw domain_error("StMatrix<DataType>::dfinv(): Matrix not NxN");
#else
	cerr << "StMatrix<DataType>::dfinv(): Matrix not NxN" << endl;
#endif
    }
    
    int n = mCol;
    if (n==1) return 0;
    
    DataType s31, s32;
    DataType s33, s34;
    
    DataType *m11 = mElement;
    DataType *m12 = m11 + 1;
    DataType *m21 = m11 + n;
    DataType *m22 = m12 + n;
    *m21 = -(*m22) * (*m11) * (*m21);
    *m12 = -(*m12);
    if (n>2) {
        DataType *mi = mElement + 2 * n;
        DataType *mii= mElement + 2 * n + 2;
        DataType *mimim = mElement + n + 1;
        for (int i=3;i<=n;i++) {
            int im2 = i - 2;
            DataType *mj = mElement;
            DataType *mji = mj + i - 1;
            DataType *mij = mi;
            for (int j=1;j<=im2;j++) {
                s31 = 0.0;
                s32 = *mji;
                DataType *mkj = mj + j - 1;
                DataType *mik = mi + j - 1;
                DataType *mjkp = mj + j;
                DataType *mkpi = mj + n + i - 1;
                for (int k=j;k<=im2;k++) {
                    s31 += (*mkj) * (*(mik++));
                    s32 += (*(mjkp++)) * (*mkpi);
                    mkj += n;
                    mkpi += n;
                }
                *mij = -(*mii) * (((*(mij-n)))*( (*(mii-1)))+(s31));
                *mji = -s32;
                mj += n;
                mji += n;
                mij++;
            }
            *(mii-1) = -(*mii) * (*mimim) * (*(mii-1));
            *(mimim+1) = -(*(mimim+1));
            mi += n;
            mimim += (n+1);
            mii += (n+1);
        }
    }
    DataType *mi = mElement;
    DataType *mii = mElement;
    for (int i=1;i<n;i++) {
        int ni = n - i;
        DataType *mij = mi;
        int j;
        for (j=1; j<=i;j++) {
            s33 = *mij;
            DataType *mikj = mi + n + j - 1;
            DataType *miik = mii + 1;
            DataType *min_end = mi + n;
            for (;miik<min_end;) {
                s33 += (*mikj) * (*(miik++));
                mikj += n;
            }
            *(mij++) = s33;
        }
        for (j=1;j<=ni;j++) {
            s34 = 0.0;
            DataType *miik = mii + j;
            DataType *mikij = mii + j * n + j;
            for (int k=j;k<=ni;k++) {
                s34 += *mikij * (*(miik++));
                mikij += n;
            }
            *(mii+j) = s34;
        }
        mi += n;
        mii += (n+1);
    }
    int nxch = ir[n];
    if (nxch==0) return 0;
    for (int mm=1;mm<=nxch;mm++) {
        int k = nxch - mm + 1;
        int ij = ir[k];
        int i = ij >> 12;
        int j = ij%4096;
        DataType *mki = mElement + i - 1;
        DataType *mkj =mElement + j - 1;
        for (k=1; k<=n;k++) {
            DataType ti = *mki;
            *mki = *mkj;
            *mkj = ti;
            mki += n;
            mkj += n;
        }
    }
    return 0;
}
 


template<class DataType>
StMatrix<DataType> operator/(const StMatrix<DataType>& m1, double fact)
{
    StMatrix<DataType> mret(m1);
    mret /= fact;
    return mret;
}

template<class DataType>
StMatrix<DataType> operator*(const StMatrix<DataType>& m1, double fact)
{
    StMatrix<DataType> mret(m1);
    mret *= fact;
    return mret;
}

template<class DataType>
StMatrix<DataType> operator*(double fact,const StMatrix<DataType>& m1)
{
    StMatrix<DataType> mret(m1);
    mret *= fact;
    return mret;
}

// Private Members
// Direct sum of two matricies
template<class DataType>
StMatrix<DataType> dsum(const StMatrix<DataType> &m1, const StMatrix<DataType> &m2)
{
    StMatrix<DataType> mret(m1.numRow() + m2.numRow(), m1.numCol() + m2.numCol());
    mret.sub(1,1,m1);
    mret.sub(m1.numRow()+1, m1.numCol()+1, m2);
    return mret;
}

#endif /* ! __CINT__ */
#ifdef __CINT__
template<> StMatrix<double> operator*(const StMatrix<double>& m1,const StMatrix<double>& m2);
template<> StMatrix<double> operator*(const StMatrix<double>& m1,const StMatrix<float>&  m2);
template<> StMatrix<double> operator*(const StMatrix<float>&  m1,const StMatrix<double>& m2);
template<> StMatrix<float>  operator*(const StMatrix<float>&  m1,const StMatrix<float>&  m2);

template<> vector<double> operator*(const StMatrix<double>& m1, const vector<double>& v);
template<> vector<double> operator*(const vector<double>&    v, const StMatrix<double>& m1);
template<> vector<double> operator*(const StMatrix<double>& m1, const vector<float>& v);
template<> vector<double> operator*(const vector<float>&     v, const StMatrix<double>& m1);
template<> vector<double> operator*(const StMatrix<float>&  m1, const vector<double>& v);
template<> vector<double> operator*(const vector<double>&    v, const StMatrix<float>& m1);
template<> vector<float>  operator*(const StMatrix<float>&  m1, const vector<float>& v);
template<> vector<float>  operator*(const vector<float>&     v, const StMatrix<float>& m1);

template<> StThreeVector<double> operator*(const StMatrix<double>& m1, const StThreeVector<double>& v);
template<> StThreeVector<double> operator*(const StThreeVector<double>&    v, const StMatrix<double>& m1);
template<> StThreeVector<double> operator*(const StMatrix<double>& m1, const StThreeVector<float>& v);
template<> StThreeVector<double> operator*(const StThreeVector<float>&     v, const StMatrix<double>& m1);
template<> StThreeVector<double> operator*(const StMatrix<float>&  m1, const StThreeVector<double>& v);
template<> StThreeVector<double> operator*(const StThreeVector<double>&    v, const StMatrix<float>& m1);
template<> StThreeVector<float>  operator*(const StMatrix<float>&  m1, const StThreeVector<float>& v);
template<> StThreeVector<float>  operator*(const StThreeVector<float>&     v, const StMatrix<float>& m1);

template<> StLorentzVector<double> operator*(const StMatrix<double>& m1, const StLorentzVector<double>& v);
template<> StLorentzVector<double> operator*(const StLorentzVector<double>&    v, const StMatrix<double>& m1);
template<> StLorentzVector<double> operator*(const StMatrix<double>& m1, const StLorentzVector<float>& v);
template<> StLorentzVector<double> operator*(const StLorentzVector<float>&     v, const StMatrix<double>& m1);
template<> StLorentzVector<double> operator*(const StMatrix<float>&  m1, const StLorentzVector<double>& v);
template<> StLorentzVector<double> operator*(const StLorentzVector<double>&    v, const StMatrix<float>& m1);
template<> StLorentzVector<float>  operator*(const StMatrix<float>&  m1, const StLorentzVector<float>& v);
template<> StLorentzVector<float>  operator*(const StLorentzVector<float>&     v, const StMatrix<float>& m1);

template<> StMatrix<double> operator+(const StMatrix<double>& m1,const StMatrix<double>& m2);
template<> StMatrix<double> operator+(const StMatrix<float>&  m1,const StMatrix<double>& m2);
template<> StMatrix<double> operator+(const StMatrix<double>& m1,const StMatrix<float>& m2);
template<> StMatrix<float>  operator+(const StMatrix<float>&  m1,const StMatrix<float>& m2);
template<> StMatrix<double> operator-(const StMatrix<double>& m1,const StMatrix<double>& m2);
template<> StMatrix<double> operator-(const StMatrix<float>&  m1,const StMatrix<double>& m2);
template<> StMatrix<double> operator-(const StMatrix<double>& m1,const StMatrix<float>& m2);
template<> StMatrix<float>  operator-(const StMatrix<float>&  m1,const StMatrix<float>& m2);

template<> ostream& operator<<(ostream& s, const StMatrix<double>& q);
template<> ostream& operator<<(ostream& s, const StMatrix<float>& q);

template<> double norm_infinity(const StMatrix<double>& m1);
template<> double normInfinity(const StMatrix<double>& m1);
template<> double norm1(const StMatrix<double>& m1);
template<> float norm_infinity(const StMatrix<float>& m1);
template<> float normInfinity(const StMatrix<float>& m1);
template<> float norm1(const StMatrix<float>& m1);
#else /* ! __CINT__ */
// Non-Member
template<class DataType, class X>
StMatrix<DataType> operator*(const StMatrix<DataType>& m1,const StMatrix<X>& m2)
{
    return StMatrix<DataType>(m1).dot(m2);
}

// STL vector
template <class DataType, class X>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
vector<DataType, allocator<DataType> >
operator*(const StMatrix<X>& m1, const vector<DataType, allocator<DataType> >& v)
#else
vector<DataType> operator*(const StMatrix<X>& m1, const vector<DataType>& v)
#endif
{
    vector<DataType> mret(m1.numRow());
    if(m1.numCol() == v.size() ) {
	for (unsigned int i=0; i<v.size(); i++) {
	    DataType temp = 0;
	    for(unsigned int j=0; j<m1.numCol(); j++)
		temp += m1(i+1,j+1)*v[j];
	    mret[i]=temp;
	}
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator*(): StMatrix * STL vector Sizes are incompatible");
#else
	cerr << "operator*(): StMatrix * STL vector Sizes are incompatible" << endl;
#endif
    }
    return mret;
}

template <class DataType, class X>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
vector<DataType, allocator<DataType> >
operator*(const vector<DataType, allocator<DataType> >& v, const StMatrix<X>& m1)
#else
vector<DataType> operator*(const vector<DataType>& v, const StMatrix<X>& m1)
#endif
{

    if(v.size() == m1.numRow()) {
	vector<DataType> mret(m1.numCol());
	for (unsigned int i=0; i<m1.numCol(); i++) {
	    DataType temp = 0;
	    for(unsigned int j=0; j<m1.numRow(); j++)
		temp+=v[j]*m1(j+1,i+1);
	    mret[i]=temp;
	}
	return mret;
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator*(): STL vector * StMatrix : Matrix Sizes are incompatible");
#else
	cerr << "operator*(): STL vector * StMatrix : Matrix Sizes are incompatible" << endl;
#endif
	return vector<DataType>();
    }
}

template <class DataType, class X>
StThreeVector<DataType> operator*(const StMatrix<X>& m1, const StThreeVector<DataType>& v3)
{
    if(m1.numRow() == 3 && m1.numCol() == 3) {
	return StThreeVector<DataType>(m1[0][0]*v3.x()+m1[0][1]*v3.y()+m1[0][2]*v3.z(),
				       m1[1][0]*v3.x()+m1[1][1]*v3.y()+m1[1][2]*v3.z(),
				       m1[2][0]*v3.x()+m1[2][1]*v3.y()+m1[2][2]*v3.z());
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator*(): StMatrix<> * StThreeVector<> : Matrix Must be 3x3.");
	
#else
	cerr << "StMatrix<> * StThreeVector<>: Matrix Must be 3x3" << endl;
#endif
	return StThreeVector<DataType>();
    }
}


template <class DataType, class X>
StThreeVector<DataType> operator*(const StThreeVector<DataType>& v3, const StMatrix<X>& m1)
{
    if(m1.numRow() == 3 && m1.numCol() == 3) {
	return StThreeVector<DataType>(m1[0][0]*v3.x()+m1[1][0]*v3.y()+m1[2][0]*v3.z(),
				       m1[0][1]*v3.x()+m1[1][1]*v3.y()+m1[2][1]*v3.z(),
				       m1[0][2]*v3.x()+m1[1][2]*v3.y()+m1[2][2]*v3.z());
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator*(): StThreeVector<> * StMatrix<>: Matrix Must be 3x3.");
	
#else
	cerr << "operator*(): StThreeVector<> * StMatrix<>: Matrix Must be 3x3" << endl;
#endif
	return StThreeVector<DataType>();
    }
}

template <class DataType, class X>
StLorentzVector<DataType> operator*(const StMatrix<X>& m1, const StLorentzVector<DataType>& v4)
{
    if (m1.numRow() == 4 && m1.numCol() == 4) {
	return StLorentzVector<DataType>(m1[0][0]*v4.x()+m1[0][1]*v4.y()+m1[0][2]*v4.z()+m1[0][3]*v4.t(),
					 m1[1][0]*v4.x()+m1[1][1]*v4.y()+m1[1][2]*v4.z()+m1[1][3]*v4.t(),
					 m1[2][0]*v4.x()+m1[2][1]*v4.y()+m1[2][2]*v4.z()+m1[2][3]*v4.t(),
					 m1[3][0]*v4.x()+m1[3][1]*v4.y()+m1[3][2]*v4.z()+m1[3][3]*v4.t());
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator*(): StMatrix<> * StLorentzVector<> : Matrix Must be 4x4.");
	
#else
	cerr << "operator*(): StMatrix<> * StLorentzVector<>: Matrix Must be 4x4" << endl;
#endif
	return StLorentzVector<DataType>();
    }	
}

template <class DataType, class X>
StLorentzVector<DataType> operator*(const StLorentzVector<DataType>& v4, const StMatrix<X>& m1)
{
    if (m1.numRow() == 4 && m1.numCol() == 4) {
	return StLorentzVector<DataType>(m1[0][0]*v4.x()+m1[1][0]*v4.y()+m1[2][0]*v4.z()+m1[0][3]*v4.t(),
					 m1[0][1]*v4.x()+m1[1][1]*v4.y()+m1[2][1]*v4.z()+m1[1][3]*v4.t(),
					 m1[0][2]*v4.x()+m1[1][2]*v4.y()+m1[2][2]*v4.z()+m1[2][3]*v4.t(),
					 m1[0][3]*v4.x()+m1[1][3]*v4.y()+m1[2][3]*v4.z()+m1[2][3]*v4.t());
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator*(): StLorentzVector<> * StMatrix<>: Matrix Must be 3x3.");
	
#else
	cerr << "StLorentzVector<> * StMatrix<>: Matrix Must be 3x3" << endl;
#endif
	return StLorentzVector<DataType>();
    }
}

template<class DataType, class X>
StMatrix<DataType> operator+(const StMatrix<DataType>& m1,const StMatrix<X>& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	StMatrix<DataType> mret(m1);
	mret +=m2;
	return mret;
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator+(): Matrix Sizes must be the same.");
	
#else
	cerr << "operator+(): Matrix Sizes must be the same." << endl;
#endif
	return StMatrix<DataType>();
    }	
}

template<class DataType, class X>
StMatrix<DataType> operator-(const StMatrix<DataType>& m1,const StMatrix<X>& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	return StMatrix<DataType>(m1) -= m2;
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("operator-(): Matrix Sizes must be the same.");
	
#else
	cerr << "operator-(): Matrix Sizes must be the same." << endl;
#endif
	return StMatrix<DataType>();
    }
}

// Print the Matrix.
template<class DataType>
ostream& operator<<(ostream& s, const StMatrix<DataType>& q)
{
    s << "\n";
    // Fixed format needs 3 extra characters for field
    // Scientific format needs 7
    unsigned int width;
    if(s.flags()&std::ios::fixed)
	width = s.precision()+3;
    else
	width = s.precision()+7;
    for(unsigned int irow = 1; irow<= q.numRow(); irow++)
	{
	    for(unsigned int icol=1; icol<=q.numCol(); icol++)
		{
		    s.width(width);
		    s << q(irow,icol) << " ";
		}
	    s<< endl;
	}
    return s;
}

template<class DataType>
DataType norm_infinity(const StMatrix<DataType>& m1)
{
    return normInfinity(m1);
}

template<class DataType>
DataType normInfinity(const StMatrix<DataType>& m1)
{
    DataType max=0,sum;
    for(unsigned int r=1; r<=m1.numRow(); r++) {
	sum=0;
	for(unsigned int c=1; c<=m1.numCol(); c++) {
	    sum+=fabs(m1(r,c));
	}
	if(sum>max) max=sum;
    }
    return max;
}

template<class DataType>
DataType norm1(const StMatrix<DataType>& m1)
{
    DataType max=0,sum;
    for(unsigned int c=1; c<=m1.numCol(); c++) {
	sum=0;
	for(unsigned int r=1; r<=m1.num_row(); r++)
	    sum+=fabs(m1(r,c));
	if(sum>max) max=sum;
    }
    return max;
}
#endif /* __CINT__ */
#endif /* ST_MATRIX_HH */
