/***************************************************************************
 *
 * $Id: StMatrixD.cc,v 1.3 1999/12/07 23:43:04 ullrich Exp $
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
 * $Log: StMatrixD.cc,v $
 * Revision 1.3  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.3  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.2  1999/03/07 15:02:19  wenaus
 * fix scope problems
 *
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:19  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StMatrixF.hh"
#include "StMatrixD.hh"
#include <math.h>

#ifdef __ROOT__
ClassImp(StMatrixD)
#endif

// Constructors. 

StMatrixD::StMatrixD()
    : mElement(0), mRow(0), mCol(0), mSize(0) {}

StMatrixD::StMatrixD(size_t p,size_t q, size_t init)
    : mRow(p), mCol(q)
{
    mSize    = mRow*mCol;
    mElement = new double[mSize];

    double *a = mElement;
    double *b = mElement + mSize;
    for( ; a<b; a++)
	*a = 0;
    
    if (mSize > 0) {
	switch(init)
	    {
	    case 0:
		break;
	    case 1:
		if ( mCol == mRow ) {
		    double *a = mElement;
		    double *b = mElement + mSize;
		    for( ; a<b; a+=(mCol+1)) *a = 1.0;
		}
		else {
		    cerr << "StMatrixD: Matrix must be NxN" << endl;
		}
		break;
	    default:
		cerr << "StMatrixD::StMatrixD(p,q,init): init must be 0 or 1" << endl;
		break;
	    }
    }
}

StMatrixD::StMatrixD(const StMatrixD& m1)
    : mRow(m1.numRow()), mCol(m1.numCol()), mSize(m1.numSize())
{
    mElement = new double[mSize];

    for(unsigned int ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
}

StMatrixD::StMatrixD(const StMatrixF& m1)
    : mRow(m1.numRow()), mCol(m1.numCol()), mSize(m1.numSize())
{
    mElement = new double[mSize];

    for(int unsigned ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
}

StMatrixD& StMatrixD::operator=(const StMatrixD& m1)
{   
    if ((void *)&m1 == (void *)this) {
	return *this;
    }
    else {
	delete [] mElement;
	mSize    = m1.numRow()*m1.numCol();
	mElement = new double[mSize];

	mRow = m1.numRow();
	mCol = m1.numCol();
	
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		*(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
	    }
    }
    return (*this);

}

StMatrixD& StMatrixD::operator=(const StMatrixF& m1)
{   
    if ((void *)&m1 == (void *)this) {
	return *this;
    }
    else {
	delete [] mElement;
	mSize    = m1.numRow()*m1.numCol();
	mElement = new double[mSize];

	mRow = m1.numRow();
	mCol = m1.numCol();
	
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		*(mElement+(ii)*mCol+jj) = m1(ii+1,jj+1);
	    }
    }
    return (*this);

}

// Destructor
StMatrixD::~StMatrixD() {
    delete [] mElement;
}

//
// access functions
//
unsigned int StMatrixD::numRow() const { return mRow;}

unsigned int StMatrixD::numCol() const  { return mCol;}

unsigned int StMatrixD::numSize() const { return mSize;}

// backward compatibility
unsigned int StMatrixD::num_row() const { return mRow;}

unsigned int StMatrixD::num_col() const  { return mCol;}

unsigned int StMatrixD::num_size() const { return mSize;}

//
// Element Access
//
const double& StMatrixD::operator()(size_t row, size_t col) const 
{
#ifdef MATRIX_BOUND_CHECK
    if(row<1 || row>numRow() || col<1 || col>numCol()) {
	cerr << "StMatrixD::operator(): const Bad Index" << endl;
    }
#endif
    return *(mElement+(row-1)*mCol+col-1);
}

double& StMatrixD::operator()(size_t row, size_t col)
{
#ifdef MATRIX_BOUND_CHECK
    if(row<1 || row>mRow || col<1 || col>mCol) {
	cerr << "StMatrixD::operator(): Bad Index" << endl;
    }
#endif
    return *(mElement+(row-1)*mCol+col-1);
}

/* -----------------------------------------------------------------------
   This section contains the assignment and inplace operators =,+=,-=,*=,/=.
   ----------------------------------------------------------------------- */

StMatrixD& StMatrixD::operator*=(double fact)
{
    for(unsigned int ii=0; ii<mRow; ii++)
 	for(unsigned int jj=0; jj<mCol; jj++)
 	    *(mElement+(ii)*mCol+jj) *= fact;
    
    return (*this);
}

StMatrixD & StMatrixD::operator/=(double fact)
{
    if(fact == 0) {
	cerr << "StMatrixD::operator/=(): Cannot divide by zero!" << endl;
    }
    for(unsigned int ii=0; ii<mCol; ii++)
 	for(unsigned int jj=0; jj<mRow; jj++)
 	    *(mElement+(ii)*mCol+jj) /= fact;
    
    return (*this);
}

// operator+=
StMatrixD& StMatrixD::operator+=(const StMatrixD& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) += m2(ii+1,jj+1);

	return (*this);
    }
    else {
	cerr << "StMatrixD::operator+=(): Matrices are not same size!" << endl;
	return (*this);
    }	
}

StMatrixD& StMatrixD::operator+=(const StMatrixF& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(int unsigned ii=0; ii<mRow; ii++)
	    for(int unsigned jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) += m2(ii+1,jj+1);

	return (*this);
    }
    else {
	cerr << "StMatrixF::operator+= Matrices are not same size!" << endl;
	return (*this);
    }	
}

// operator -=
StMatrixD& StMatrixD::operator-=(const StMatrixD& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) -= m2(ii+1,jj+1);

	return (*this);
    }
    else {
	cerr << "StMatrixD::operator-=(): Matrices are not same size!" << endl;
	return (*this);
    }
}

StMatrixD& StMatrixD::operator-=(const StMatrixF& m2)
{
    if(mRow == m2.numRow() && mCol == m2.numCol()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++)
		*(mElement+(ii)*mCol+jj) -= m2(ii+1,jj+1);

	return (*this);
    }
    else {
	cerr << "StMatrixF::operator-=(): Matrices are not same size!" << endl;
	return (*this);
    }
}

// operator::dot
StMatrixD StMatrixD::dot(const StMatrixD& m2)
{
    if(mCol == m2.numRow() ) {
	StMatrixD mret(mRow, m2.numCol(), 0);
	
	for(unsigned int i=0; i<mRow; i++)
	    for(unsigned int j=0; j<m2.numCol(); j++) {
		for(unsigned int kk=0; kk<mCol; kk++)
		    mret(i+1, j+1) += (*(mElement+(i)*mCol+kk))*m2(kk+1,j+1);
	    }
	return mret;
    }
    else {
	cerr << "StMatrixD::dot(): Incompatible matrix sizes" << endl;
	return StMatrixD();
    }
}

StMatrixD StMatrixD::dot(const StMatrixF& m2)
{
    if(mCol == m2.numRow() ) {
	StMatrixD mret(mRow, m2.numCol(), 0);
	
	for(unsigned int i=0; i<mRow; i++)
	    for(unsigned int j=0; j<m2.numCol(); j++) {
		for(unsigned int kk=0; kk<mCol; kk++)
		    mret(i+1, j+1) += (*(mElement+(i)*mCol+kk))*m2(kk+1,j+1);
	    }
	return mret;
    }
    else {
	cerr << "StMatrixF::dot(): Incompatible matrix sizes" << endl;
	return StMatrixD();
    }
}

StMatrixD StMatrixD::operator+ () const 
{
    return *this;
}

StMatrixD StMatrixD::operator- () const 
{
    // this can be streamlined
    StMatrixD mret(*this);
    return mret*=-1;
}

int StMatrixD::operator== (const StMatrixD& m1) const
{
    if (mCol == m1.numCol() && mRow == m1.numRow()) {
	for(unsigned int ii=0; ii<mRow; ii++)
	    for(unsigned int jj=0; jj<mCol; jj++) {
		if(*(mElement+(ii)*mCol+jj) != m1(ii+1,jj+1))
		    return 0;
	    }
	return 1;
    }
    else
	return 0;  // not even right size
}

int StMatrixD::operator!= (const StMatrixD& m1) const
{
    return !(*this == m1);
}

//Apply a function to all elements
StMatrixD StMatrixD::apply(double (*f)(double, size_t, size_t)) const
{
    StMatrixD mret(mRow, mCol);
    double *a = mElement;
    for(unsigned int ir=1; ir<=mRow; ir++) {
	for(unsigned int ic=1; ic<=mCol; ic++) {
	    mret(ir,ic) = (*f)(*(a++), ir, ic);
	}
    }
    return mret;
}

StMatrixD StMatrixD::T() const
{
    StMatrixD mret(mCol,mRow);
    register double *pl = mElement + mSize;
    register double *pme = mElement;
    register double *pt = mret.mElement;
    register double *ptl = mret.mElement + mSize;
    for (; pme < pl; pme++, pt+=mRow)
	{
	    if (pt >= ptl)
		pt -= (mSize-1);
	    (*pt) = (*pme);
	}
    return mret;
}

StMatrixD StMatrixD::transpose() const
{
    return this->T();
}

//
// Sub matrix
//
StMatrixD StMatrixD::sub(size_t min_row, size_t max_row,
						 size_t min_col,size_t max_col) const
{
    StMatrixD mret(max_row-min_row+1,max_col-min_col+1);
    if(max_row > mRow || max_col > mCol) {
	cerr << "StMatrixD::sub(): Index out of range" << endl;
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

void StMatrixD::sub(size_t row,size_t col,const StMatrixD &m1)
{
    if((row <1)                   ||
       (row+m1.numRow()-1 > mRow) ||
       (col <1)                   ||
       (col+m1.numCol()-1 > mCol)) {
	cerr << "StMatrixD::sub(): Index out of range" << endl;
    }
    double *a = m1.mElement;
    unsigned int nc = numCol();
    double *b1 = mElement + (row - 1) * nc + col - 1;
    
    for(unsigned int irow=1; irow<=m1.numRow(); irow++) {
	double *brc = b1;
	for(unsigned int icol=1; icol<=m1.numCol(); icol++) {
	    *(brc++) = *(a++);
	}
	b1 += nc;
    }
}

// Contents of the matrix are replaced by the inverse--less overhead then inverse().
StMatrixD StMatrixD::inverse(size_t &ierr) const
{
    StMatrixD tmp(*this);
    tmp.invert(ierr);
    return tmp;
}

void StMatrixD::invert(size_t &ierr) {
    if(mCol != mRow) {
	cerr << "StMatrixD::invert(): not a NxN matrix" << endl;
    }
    static unsigned int max_array = 20;
    static unsigned int *ir = new unsigned int [max_array+1];
    
    if (mCol > max_array) {
	delete [] ir;
	max_array = mRow;
	ir = new unsigned int [max_array+1];
    }
    double t1, t2, t3;
    double det, temp, s;
    unsigned int ifail;
    switch(mRow) {
    case 3:
	{
	    double c11,c12,c13,c21,c22,c23,c31,c32,c33;
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
		double s = temp/det;
		double *tmp = mElement;
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

double StMatrixD::determinant() const {
    static unsigned int max_array = 20;
    static unsigned int *ir = new unsigned int [max_array+1];
    if(mCol != mRow) {
		cerr << "StMatrixD::determinant(): not a NxN matrix" << endl;
    }
    if (mCol > max_array) {
	delete [] ir;
	max_array = mRow;
	ir = new unsigned int [max_array+1];
    }
    double det;
    StMatrixD mt(*this);
    unsigned int i = mt.dfact(det, ir);
    if(i==0) return det;
    return 0;
}

//
// Access operators
//
StMatrixD::StMatrixRowD::StMatrixRowD(StMatrixD& a, size_t r) 
    : _a(a) {
	_r = r;
}

double& StMatrixD::StMatrixRowD::operator[](size_t c) {
#ifdef MATRIX_BOUND_CHECK
    if (_r<0 || _r>=_a.numRow() || c<0 || c>=_a.numCol()) {
	cerr << "StMatrixD::operator[]: index out of range" << endl;
    }
#endif
    return *(_a.mElement+_r*_a.mCol+c);
}

StMatrixD::StMatrixRowConstD::StMatrixRowConstD(const StMatrixD& a, size_t r) 
    : _a(a) {
    _r = r;
}

const double& StMatrixD::StMatrixRowConstD::operator[](size_t c) const
{
#ifdef MATRIX_BOUND_CHECK
    if (_r<0 || _r>=_a.numRow() || c<0 || c>=_a.numCol()) {
	cerr << "StMatrixD::operator[]: const index out of range" << endl;
    }
#endif
    return *(_a.mElement+_r*_a.mCol+c);
}

StMatrixD::StMatrixRowD StMatrixD::operator[] (size_t r)
{
    StMatrixRowD b(*this,r);
    return b;
}

const StMatrixD::StMatrixRowConstD StMatrixD::operator[] (size_t r) const
{
    StMatrixRowConstD b(*this,r);
    return b;
}

void StMatrixD::swap(unsigned int &i, unsigned int &j)
{
    unsigned int tmp=i;
    i=j;
    j=tmp;
}

void StMatrixD::swap(double *&i, double *&j)
{
    double *tmp=i;
    i=j;
    j=tmp;
}

// This function swaps two Matrices without doing a full copy.
void swap(StMatrixD& m1, StMatrixD& m2) {
    StMatrixD::swap(m1.mElement, m2.mElement);
    StMatrixD::swap(m1.mRow,     m2.mRow);
    StMatrixD::swap(m1.mCol,     m2.mCol);
    StMatrixD::swap(m1.mSize,    m2.mSize);
}

unsigned int StMatrixD::dfact(double& det, size_t *ir) {
    if (mCol!=mRow) {
	cerr << "StMatrixD::dfact(): Matrix not NxN" << endl;
    }
    unsigned int ifail, jfail;
    register unsigned int n = mCol;
    
    double tf;
    double g1 = 1.0e-19, g2 = 1.0e19;
    
    double p, q, t;
    double s11, s12;
    
    unsigned int normal = 0, imposs = (unsigned int)-1;
    unsigned int jrange = 0, jover = 1, junder = (unsigned int)-1;
    ifail = normal;
    jfail = jrange;
    unsigned int nxch = 0;
    det = 1.0;
    double *mj = mElement;
    double *mjj = mj;
    for (unsigned int j=1; j<=n; j++) {
	unsigned int k = j;
	p = (fabs(*mjj));
	if (j!=n) {
	    double *mij = mj + n + j - 1; 
	    for (unsigned int i=j+1; i<=n; i++) {
		q = (fabs(*(mij)));
		if (q > p) {
		    k = i;
		    p = q;
		}
		mij += n;
	    }
	    if (k==j) {
		if (p <=0) {
		    det = 0;
		    ifail = imposs;
		    jfail = jrange;
		    return ifail;
		}
	    }
	    double *mjl = mj;
	    double *mkl = mElement + (k-1)*n;
	    for (unsigned int l=1; l<=n; l++) {
		tf = *mjl;
		*(mjl++) = *mkl;
		*(mkl++) = tf;
	    }
	    nxch = nxch + 1;
	    ir[nxch] = (((j)<<12)+(k));
	} else {
	    if (p <=0) {
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
	} else if (t > g2) {
	    det = 1.0;
	    if (jfail==jrange) jfail = jover;
	}
	if (j!=n) {
	    double *mk = mj + n;
	    double *mkjp = mk + j;
	    double *mjk = mj + j;
	    for (unsigned int k=j+1; k<=n; k++) {
		s11 = - (*mjk);
		s12 = - (*mkjp);
		if (j!=1) {
		    double *mik = mElement + k - 1;
		    double *mijp = mElement + j;
		    double *mki = mk;
		    double *mji = mj;
		    for (unsigned int i=1; i<j; i++) {
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

unsigned int StMatrixD::dfinv(size_t *ir) {
    if (mCol != mRow) {
	cerr << "StMatrixD::dfinv(): Matrix not NxN" << endl;
    }
    register unsigned int n = mCol;
    if (n==1) return 0;
    
    double s31, s32;
    register double s33, s34;
    
    double *m11 = mElement;
    double *m12 = m11 + 1;
    double *m21 = m11 + n;
    double *m22 = m12 + n;
    *m21 = -(*m22) * (*m11) * (*m21);
    *m12 = -(*m12);
    if (n>2) {
	double *mi = mElement + 2 * n;
	double *mii= mElement + 2 * n + 2;
	double *mimim = mElement + n + 1;
	for (unsigned int i=3; i<=n; i++) {
	    unsigned int im2 = i - 2;
	    double *mj = mElement;
	    double *mji = mj + i - 1;
	    double *mij = mi;
	    for (unsigned int j=1; j<=im2; j++) { 
		s31 = 0.0;
		s32 = *mji;
		double *mkj = mj + j - 1;
		double *mik = mi + j - 1;
		double *mjkp = mj + j;
		double *mkpi = mj + n + i - 1;
		for (unsigned int k=j; k<=im2; k++) {
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
    double *mi = mElement;
    double *mii = mElement;
    for (unsigned int i=1; i<n; i++) {
	unsigned int ni = n - i;
	double *mij = mi;
    unsigned int j;
	for (j=1; j<=i; j++) {
	    s33 = *mij;
	    register double *mikj = mi + n + j - 1;
	    register double *miik = mii + 1;
	    double *min_end = mi + n;
	    for ( ; miik<min_end; ) {
		s33 += (*mikj) * (*(miik++));
		mikj += n;
	    }
	    *(mij++) = s33;
	}
	for (j=1; j<=ni; j++) {
	    s34 = 0.0;
	    double *miik = mii + j;
	    double *mikij = mii + j * n + j;
	    for (unsigned int k=j; k<=ni; k++) {
		s34 += *mikij * (*(miik++));
		mikij += n;
	    }
	    *(mii+j) = s34;
	}
	mi += n;
	mii += (n+1);
    }
    unsigned int nxch = ir[n];
    if (nxch==0) return 0;
    for (unsigned int cnt=1; cnt<=nxch; cnt++) {
	unsigned int k = nxch - cnt + 1;
	unsigned int ij = ir[k];
	unsigned int i = ij >> 12;
	unsigned int j = ij%4096;
	double *mki = mElement + i - 1;
	double *mkj = mElement + j - 1;
	for (k=1; k<=n; k++) {
	    register double ti = *mki;
	    *mki = *mkj;
	    *mkj = ti;
	    mki += n;
	    mkj += n;
	}
    }
    return 0;
}

StMatrixD operator/(const StMatrixD& m1, double fact)
{
    StMatrixD mret(m1);
    mret /= fact;
    return mret;
}

StMatrixD operator*(const StMatrixD& m1, double fact)
{
    StMatrixD mret(m1);
    mret *= fact;
    return mret;
}

StMatrixD operator*(double fact,const StMatrixD& m1)
{
    StMatrixD mret(m1);
    mret *= fact;
    return mret;
}

// Private Members
// Direct sum of two matricies
StMatrixD dsum(const StMatrixD &m1, const StMatrixD &m2)
{
    StMatrixD mret(m1.numRow() + m2.numRow(), m1.numCol() + m2.numCol());
    mret.sub(1,1,m1);
    mret.sub(m1.numRow()+1, m1.numCol()+1, m2);
    return mret;
}


// Non-Member
StMatrixD operator*(const StMatrixD& m1,const StMatrixD& m2)
{
    return StMatrixD(m1).dot(m2);
}

StMatrixD operator*(const StMatrixD& m1,const StMatrixF& m2)
{
    return StMatrixD(m1).dot(m2);
}

StMatrixD operator*(const StMatrixF& m1,const StMatrixD& m2)
{
    return StMatrixD(m1).dot(m2);
}

StThreeVectorD operator*(const StMatrixD& m1, const StThreeVectorF& v3)
{
    if(m1.numRow() == 3 && m1.numCol() == 3) {
	return StThreeVectorD(m1[0][0]*v3.x()+m1[0][1]*v3.y()+m1[0][2]*v3.z(),
			      m1[1][0]*v3.x()+m1[1][1]*v3.y()+m1[1][2]*v3.z(),
			      m1[2][0]*v3.x()+m1[2][1]*v3.y()+m1[2][2]*v3.z());
    }
    else {
	cerr << "StMatrixD * StThreeVectorF: Matrix Must be 3x3" << endl;
	return StThreeVectorD();
    }
}

StThreeVectorD operator*(const StMatrixD& m1, const StThreeVectorD& v3)
{
    if(m1.numRow() == 3 && m1.numCol() == 3) {
	return StThreeVectorD(m1[0][0]*v3.x()+m1[0][1]*v3.y()+m1[0][2]*v3.z(),
			      m1[1][0]*v3.x()+m1[1][1]*v3.y()+m1[1][2]*v3.z(),
			      m1[2][0]*v3.x()+m1[2][1]*v3.y()+m1[2][2]*v3.z());
    }
    else {
	cerr << "StMatrixD * StThreeVectorD: Matrix Must be 3x3" << endl;
	return StThreeVectorD();
    }
}

StThreeVectorD operator*(const StThreeVectorF& v3, const StMatrixD& m1)
{
    if(m1.numRow() == 3 && m1.numCol() == 3) {
	return StThreeVectorD(m1[0][0]*v3.x()+m1[1][0]*v3.y()+m1[2][0]*v3.z(),
			      m1[0][1]*v3.x()+m1[1][1]*v3.y()+m1[2][1]*v3.z(),
			      m1[0][2]*v3.x()+m1[1][2]*v3.y()+m1[2][2]*v3.z());
    }
    else {
	cerr << "operator*(): StThreeVectorF * StMatrixD: Matrix Must be 3x3" << endl;
	return StThreeVectorD();
    }
}

StThreeVectorD operator*(const StThreeVectorD& v3, const StMatrixD& m1)
{
    if(m1.numRow() == 3 && m1.numCol() == 3) {
	return StThreeVectorD(m1[0][0]*v3.x()+m1[1][0]*v3.y()+m1[2][0]*v3.z(),
				       m1[0][1]*v3.x()+m1[1][1]*v3.y()+m1[2][1]*v3.z(),
				       m1[0][2]*v3.x()+m1[1][2]*v3.y()+m1[2][2]*v3.z());
    }
    else {
	cerr << "operator*(): StThreeVectorD * StMatrixD: Matrix Must be 3x3" << endl;
	return StThreeVectorD();
    }
}

StLorentzVectorD operator*(const StMatrixD& m1, const StLorentzVectorF& v4)
{
    if (m1.numRow() == 4 && m1.numCol() == 4) {
	return StLorentzVectorD(m1[0][0]*v4.x()+m1[0][1]*v4.y()+m1[0][2]*v4.z()+m1[0][3]*v4.t(),
					 m1[1][0]*v4.x()+m1[1][1]*v4.y()+m1[1][2]*v4.z()+m1[1][3]*v4.t(),
					 m1[2][0]*v4.x()+m1[2][1]*v4.y()+m1[2][2]*v4.z()+m1[2][3]*v4.t(),
					 m1[3][0]*v4.x()+m1[3][1]*v4.y()+m1[3][2]*v4.z()+m1[3][3]*v4.t());
    }
    else {
	cerr << "operator*(): StMatrixD * StLorentzVectorF: Matrix Must be 4x4" << endl;
	return StLorentzVectorD();
    }	
}

StLorentzVectorD operator*(const StMatrixD& m1, const StLorentzVectorD& v4)
{
    if (m1.numRow() == 4 && m1.numCol() == 4) {
	return StLorentzVectorD(m1[0][0]*v4.x()+m1[0][1]*v4.y()+m1[0][2]*v4.z()+m1[0][3]*v4.t(),
					 m1[1][0]*v4.x()+m1[1][1]*v4.y()+m1[1][2]*v4.z()+m1[1][3]*v4.t(),
					 m1[2][0]*v4.x()+m1[2][1]*v4.y()+m1[2][2]*v4.z()+m1[2][3]*v4.t(),
					 m1[3][0]*v4.x()+m1[3][1]*v4.y()+m1[3][2]*v4.z()+m1[3][3]*v4.t());
    }
    else {
	cerr << "operator*(): StMatrixD * StLorentzVectorD: Matrix Must be 4x4" << endl;
	return StLorentzVectorD();
    }	
}

StLorentzVectorD operator*(const StLorentzVectorF& v4, const StMatrixD& m1)
{
    if (m1.numRow() == 4 && m1.numCol() == 4) {
	return StLorentzVectorD(m1[0][0]*v4.x()+m1[1][0]*v4.y()+m1[2][0]*v4.z()+m1[0][3]*v4.t(),
					 m1[0][1]*v4.x()+m1[1][1]*v4.y()+m1[2][1]*v4.z()+m1[1][3]*v4.t(),
					 m1[0][2]*v4.x()+m1[1][2]*v4.y()+m1[2][2]*v4.z()+m1[2][3]*v4.t(),
					 m1[0][3]*v4.x()+m1[1][3]*v4.y()+m1[2][3]*v4.z()+m1[2][3]*v4.t());
    }
    else {
	cerr << "StLorentzVectorF * StMatrixD: Matrix Must be 3x3" << endl;
	return StLorentzVectorD();
    }
}

StLorentzVectorD operator*(const StLorentzVectorD& v4, const StMatrixD& m1)
{
    if (m1.numRow() == 4 && m1.numCol() == 4) {
	return StLorentzVectorD(m1[0][0]*v4.x()+m1[1][0]*v4.y()+m1[2][0]*v4.z()+m1[0][3]*v4.t(),
					 m1[0][1]*v4.x()+m1[1][1]*v4.y()+m1[2][1]*v4.z()+m1[1][3]*v4.t(),
					 m1[0][2]*v4.x()+m1[1][2]*v4.y()+m1[2][2]*v4.z()+m1[2][3]*v4.t(),
					 m1[0][3]*v4.x()+m1[1][3]*v4.y()+m1[2][3]*v4.z()+m1[2][3]*v4.t());
    }
    else {
	cerr << "StLorentzVectorD * StMatrixD: Matrix Must be 3x3" << endl;
	return StLorentzVectorD();
    }
}

StMatrixD operator+(const StMatrixD& m1,const StMatrixD& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	StMatrixD mret(m1);
	mret +=m2;
	return mret;
    }
    else {
	cerr << "operator+(): Matrix Sizes must be the same." << endl;
	return StMatrixD();
    }	
}

StMatrixD operator+(const StMatrixF& m1,const StMatrixD& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	StMatrixD mret(m1);
	mret +=m2;
	return mret;
    }
    else {
	cerr << "operator+(): Matrix Sizes must be the same." << endl;
	return StMatrixD();
    }	
}

StMatrixD operator+(const StMatrixD& m1,const StMatrixF& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	StMatrixD mret(m1);
	mret +=m2;
	return mret;
    }
    else {
	cerr << "operator+(): Matrix Sizes must be the same." << endl;
	return StMatrixD();
    }	
}

StMatrixD operator-(const StMatrixD& m1,const StMatrixD& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	return StMatrixD(m1) -= m2;
    }
    else {
	cerr << "operator-(): Matrix Sizes must be the same." << endl;
	return StMatrixD();
    }
}

StMatrixD operator-(const StMatrixF& m1,const StMatrixD& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	return StMatrixD(m1) -= m2;
    }
    else {
	cerr << "operator-(): Matrix Sizes must be the same." << endl;
	return StMatrixD();
    }
}

StMatrixD operator-(const StMatrixD& m1,const StMatrixF& m2)
{
    if(m1.numRow() == m2.numRow() && m1.numCol() == m2.numCol()) {
	return StMatrixD(m1) -= m2;
    }
    else {
	cerr << "operator-(): Matrix Sizes must be the same." << endl;
	return StMatrixD();
    }
}

// Print the Matrix.
ostream& operator<<(ostream& s, const StMatrixD& q)
{
    s << "\n";
    // Fixed format needs 3 extra characters for field
    // Scientific format needs 7
    unsigned int width;
    if(s.flags()&ios::fixed)
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

double norm_infinity(const StMatrixD& m1)
{
    return normInfinity(m1);
}

double normInfinity(const StMatrixD& m1)
{
    double max=0,sum;
    for(unsigned int r=1; r<=m1.numRow(); r++) {
	sum=0;
	for(unsigned int c=1; c<=m1.numCol(); c++) {
	    sum+=fabs(m1(r,c));
	}
	if(sum>max) max=sum;
    }
    return max;
}

double norm1(const StMatrixD& m1)
{
    double max=0,sum;
    for(unsigned int c=1; c<=m1.numCol(); c++) {
	sum=0;
	for(unsigned int r=1; r<=m1.num_row(); r++)
	    sum+=fabs(m1(r,c));
	if(sum>max) max=sum;
    }
    return max;
}

#ifdef __ROOT__
void StMatrixD::Streamer(TBuffer& R__b)
{
   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      TObject::Streamer(R__b);
      if (mElement) delete [] mElement;
      mElement = 0;
      R__b.ReadArray(mElement);  // allocates memory if mElement = 0
      R__b >> mRow;
      R__b >> mCol;
      R__b >> mSize;
   } else {
      R__b.WriteVersion(StMatrixD::IsA());
      TObject::Streamer(R__b);
      R__b.WriteArray(mElement, mSize);
      R__b << mRow;
      R__b << mCol;
      R__b << mSize;
   }
}
#endif
