/***************************************************************************
 *
 * $Id: StMatrixD.hh,v 1.3 2000/01/31 20:53:45 lasiuk Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StMatrix<T>
 *            for T=double. This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StMatrixD.hh,v $
 * Revision 1.3  2000/01/31 20:53:45  lasiuk
 * using std::swap
 *
 * Revision 1.3  2000/01/31 20:53:45  lasiuk
 * using std::swap
 *
 * Revision 1.2  1999/02/14 23:11:44  fisyak
 * Fixes for Rootcint
 *
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:58  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_MATRIX_D_HH
#define ST_MATRIX_D_HH

#include <iostream.h>
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"
#ifndef ST_NO_NAMESPACES
using std::swap;
#endif

#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"

class StMatrixF;

class StMatrixD 
#ifdef __ROOT__
 : public TObject 
#endif
{
public:
    StMatrixD();
    StMatrixD(size_t p, size_t q, size_t init=0);

    // Copy constructor.
    StMatrixD(const StMatrixD&);
    StMatrixD(const StMatrixF&);
    
    // Assignment operators.
    StMatrixD& operator=(const StMatrixD&);
    StMatrixD& operator=(const StMatrixF&);
    
    // Destructor. --
    //Problem with LINUX (virtual table error when exception thrown)
    ~StMatrixD();

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
    const double& operator()(size_t row, size_t col) const;
    double& operator()(size_t row, size_t col);

    // classes for implementing m[i][j]
    class StMatrixRowD {
    public:
	StMatrixRowD(StMatrixD&, size_t);
	double& operator[](size_t);
    private:
#ifndef __CINT__
	StMatrixD& _a;
#else
	StMatrixD* _a;
#endif
	size_t _r;
    };
    class StMatrixRowConstD {
    public:
	StMatrixRowConstD (const StMatrixD&, size_t);
	const double & operator[](size_t) const;
    private:
#ifndef __CINT__
	const StMatrixD& _a;
#else
        const StMatrixD* _a;
#endif

	size_t _r;
    };

    StMatrixRowD operator[] (size_t r);
    const StMatrixRowConstD operator[] (size_t) const;
    
    // Scaler Operations
    StMatrixD& operator*=(double t);
    StMatrixD& operator/=(double t);

    // Matrix Operations
    StMatrixD& operator+=(const StMatrixD&);
    StMatrixD& operator+=(const StMatrixF&);
    
    StMatrixD& operator-=(const StMatrixD&);
    StMatrixD& operator-=(const StMatrixF&);

    StMatrixD  dot(const StMatrixD&);
    StMatrixD  dot(const StMatrixF&);
    
    // unary Operations
    StMatrixD operator+ () const;
    StMatrixD operator- () const;

    // Equality
    int operator==(const StMatrixD&) const;
    int operator!=(const StMatrixD&) const;
    
    // Apply a function to all elements of the matrix.
    StMatrixD apply(double (*f)(double, size_t, size_t)) const;

    StMatrixD T() const;
    StMatrixD transpose() const;

    StMatrixD sub(size_t min_row, size_t max_row, size_t min_col, size_t max_col) const;
    
    void sub(size_t row, size_t col, const StMatrixD &m1);

    StMatrixD inverse(size_t& ierr) const;
    void invert(size_t& ierr);

    double determinant() const;
    
    // Must be of same type to swap
    static void   swap(unsigned int&, unsigned int&);
    static void   swap(double *&, double *&);
    friend void   swap(StMatrixD&, StMatrixD&);
    
private:
    // Friend classes.
    friend class  StMatrixRowD;
    friend class  StMatrixRowConstD;

    // If successful, the return code is 0.
    // On return, det is the determinant and ir[] is row-unsigned interchange
    // matrix. See CERNLIB's DFACT routine.
    unsigned int dfact(double &det, size_t *ir); // factorize the matrix. 

    // invert the matrix. See CERNLIB DFINV.
    unsigned int dfinv(size_t *ir);

protected:
    double *mElement;
    unsigned int mRow, mCol;
    unsigned int mSize;
#ifdef __ROOT__
    ClassDef(StMatrixD,1)
#endif
};

//
//   Non-member
//
StMatrixD        operator*(const StMatrixD& m1,const StMatrixD& m2);
StMatrixD        operator*(const StMatrixD& m1,const StMatrixF& m2);
StMatrixD        operator*(const StMatrixF& m1,const StMatrixD& m2);
StThreeVectorD   operator*(const StMatrixD& m1, const StThreeVectorF& v3);
StThreeVectorD   operator*(const StMatrixD& m1, const StThreeVectorD& v3);
StThreeVectorD   operator*(const StThreeVectorF& v3, const StMatrixD& m1);
StThreeVectorD   operator*(const StThreeVectorD& v3, const StMatrixD& m1);
StLorentzVectorD operator*(const StMatrixD& m1, const StLorentzVectorF& v4);
StLorentzVectorD operator*(const StMatrixD& m1, const StLorentzVectorD& v4);
StLorentzVectorD operator*(const StLorentzVectorF& v4, const StMatrixD& m1);
StLorentzVectorD operator*(const StLorentzVectorD& v4, const StMatrixD& m1);
StMatrixD        operator+(const StMatrixD& m1,const StMatrixD& m2);
StMatrixD        operator+(const StMatrixF& m1,const StMatrixD& m2);
StMatrixD        operator+(const StMatrixD& m1,const StMatrixF& m2);
StMatrixD        operator-(const StMatrixD& m1,const StMatrixD& m2);
StMatrixD        operator-(const StMatrixF& m1,const StMatrixD& m2);
StMatrixD        operator-(const StMatrixD& m1,const StMatrixF& m2);
ostream&         operator<<(ostream& s, const StMatrixD& q);
double           norm_infinity(const StMatrixD& m1);
double           normInfinity(const StMatrixD& m1);
double           norm1(const StMatrixD& m1);                                      

#endif
