/***************************************************************************
 *
 * $Id: StMatrixF.hh,v 1.5 2000/02/02 18:31:06 lasiuk Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StMatrix<T>
 *            for T=float. This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StMatrixF.hh,v $
 * Revision 1.5  2000/02/02 18:31:06  lasiuk
 * restore files
 *
 * Revision 1.5  2000/02/02 18:31:06  lasiuk
 * restore files
 *
 * Revision 1.4  2000/02/01 16:03:02  lasiuk
 * namespace std is different on SUN CC5 and KCC.  Redefine macros!
 *
 * Revision 1.3  2000/01/31 20:53:46  lasiuk
 * using std::swap
 *
 * Revision 1.2  1999/02/14 23:11:47  fisyak
 * Fixes for Rootcint
 *
 * Revision 1.1  1999/01/30 03:59:04  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:59  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_MATRIX_F_HH
#define ST_MATRIX_F_HH

#include <iostream.h>
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"

class StMatrixD;

class StMatrixF
#ifdef __ROOT__
 : public TObject 
#endif
{
public:
    StMatrixF();
    StMatrixF(size_t p, size_t q, size_t init=0);

    // Copy constructor.
    StMatrixF(const StMatrixF&);
    StMatrixF(const StMatrixD&);
    
    // Assignment operators.
    StMatrixF& operator=(const StMatrixF&);
    StMatrixF& operator=(const StMatrixD&);
    
    // Destructor. --
    //Problem with LINUX (virtual table error when exception thrown)
    ~StMatrixF();

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
    const float& operator()(size_t row, size_t col) const;
    float& operator()(size_t row, size_t col);

    // classes for implementing m[i][j]
    class StMatrixRowF {
    public:
	StMatrixRowF(StMatrixF&, size_t);
	float& operator[](size_t);
    private:
#ifndef __CINT__
	StMatrixF& _a;
#else
	StMatrixF* _a;
#endif
	size_t _r;
    };
    class StMatrixRowConstF {
    public:
	StMatrixRowConstF (const StMatrixF&, size_t);
	const float & operator[](size_t) const;
    private:
#ifndef __CINT__
	const StMatrixF& _a;
#else
	const StMatrixF* _a;
#endif
	size_t _r;
    };

    StMatrixRowF operator[] (size_t r);
    const StMatrixRowConstF operator[] (size_t) const;
    
    // Scaler Operations
    StMatrixF& operator*=(double t);
    StMatrixF& operator/=(double t);

    // Matrix Operations
    StMatrixF& operator+=(const StMatrixF&);
    StMatrixF& operator+=(const StMatrixD&);
    
    StMatrixF& operator-=(const StMatrixF&);
    StMatrixF& operator-=(const StMatrixD&);

    StMatrixF  dot(const StMatrixF&);
    StMatrixF  dot(const StMatrixD&);
    
    // unary Operations
    StMatrixF operator+ () const;
    StMatrixF operator- () const;

    // Equality
    int operator==(const StMatrixF&) const;
    int operator!=(const StMatrixF&) const;
    
    // Apply a function to all elements of the matrix.
    StMatrixF apply(float (*f)(float, size_t, size_t)) const;

    StMatrixF T() const;
    StMatrixF transpose() const;

    StMatrixF sub(size_t min_row, size_t max_row, size_t min_col, size_t max_col) const;
    
    void sub(size_t row, size_t col, const StMatrixF &m1);

    StMatrixF inverse(size_t& ierr) const;
    void invert(size_t& ierr);

    float determinant() const;
    
    // Must be of same type to swap
    static void   swap(unsigned int&, unsigned int&);
    static void   swap(float *&, float *&);
    friend void   swap(StMatrixF&, StMatrixF&);
    
private:
    // Friend classes.
    friend class  StMatrixRowF;
    friend class  StMatrixRowConstF;

    // If successful, the return code is 0.
    // On return, det is the determinant and ir[] is row-unsigned interchange
    // matrix. See CERNLIB's DFACT routine.
    unsigned int dfact(float &det, size_t *ir); // factorize the matrix. 

    // invert the matrix. See CERNLIB DFINV.
    unsigned int dfinv(size_t *ir);

protected:
    float *mElement;
    unsigned int mRow, mCol;
    unsigned int mSize;
#ifdef __ROOT__
    ClassDef(StMatrixF,1)
#endif
};

//
//   Non-member
//
StMatrixF        operator*(const StMatrixF& m1,const StMatrixF& m2);
StThreeVectorF   operator*(const StMatrixF& m1, const StThreeVectorF& v3);
StThreeVectorD   operator*(const StMatrixF& m1, const StThreeVectorD& v3);
StThreeVectorF   operator*(const StThreeVectorF& v3, const StMatrixF& m1);
StThreeVectorD   operator*(const StThreeVectorD& v3, const StMatrixF& m1);
StLorentzVectorF operator*(const StMatrixF& m1, const StLorentzVectorF& v4);
StLorentzVectorD operator*(const StMatrixF& m1, const StLorentzVectorD& v4);
StLorentzVectorF operator*(const StLorentzVectorF& v4, const StMatrixF& m1);
StLorentzVectorD operator*(const StLorentzVectorD& v4, const StMatrixF& m1);
StMatrixF        operator+(const StMatrixF& m1,const StMatrixF& m2);
StMatrixF        operator-(const StMatrixF& m1,const StMatrixF& m2);
ostream&         operator<<(ostream& s, const StMatrixF& q);
float            norm_infinity(const StMatrixF& m1);
float            normInfinity(const StMatrixF& m1);
float            norm1(const StMatrixF& m1);					

#endif
