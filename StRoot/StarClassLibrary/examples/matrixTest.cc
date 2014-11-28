/***************************************************************************
 *
 * $Id: matrixTest.cc,v 1.2 1999/12/21 15:14:53 ullrich Exp $
 *
 * Author: Brian Lasiuk, April 1998
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: matrixTest.cc,v $
 * Revision 1.2  1999/12/21 15:14:53  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/02/17 12:44:00  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StGlobals.hh"

#define MATRIX_BOUND_CHECK
#define WITH_ST_THREEVECTOR
#include "StMatrix.hh"

#define SYMMETRIC
#ifndef SYMMETRIC
#	define ASYMMETRIC
#endif

#if !defined(ST_NO_NAMESPACES) && !defined(ST_NO_EXCEPTIONS)
using std::logic_error;
#endif

template <class X>
X hold1(X a, unsigned int r, unsigned int q)
{
    return a*a;
}

//         --------------------- MAIN --------------------------       //
int main()
{
#ifdef SYMMETRIC
    StMatrix<StDouble> A(2,2,1);
    
    A(1,1) = 2;
    A(1,2) = 1;
    A(2,1) = 0;
    A(2,2) = 5;

    cout << "A=" << A << endl;

    StMatrix<StFloat> B(2,2);
    
    B(1,1) = 0;
    B(1,2) = 1;
    B(2,1) = 1;
    B(2,2) = 0;
    
    cout << "-B=" << -B;

    StMatrix<StFloat> C(A);
    
    cout << "C=" << C;
    cout << "A=" << C;
    
    C+=C;
    cout << "C+=C =>" << C << endl;

    cout << "C " << C;

    C = B+A;
    cout << "C=B+A:" << C << endl;

    unsigned int ierr;
    cout << "B.inverse(ierr)" << B.inverse(ierr) << endl;

    StMatrix<StDouble> IDENTITY(2,2,1);

    C = B*B.inverse(ierr);

    if(C == IDENTITY)
	cout << "C=B*B.inverse(ierr) == IDENTITY" << endl;
    else
	cout << "oops.." << endl;

    cout << "1st row of C" << endl;
    cout << C.sub(1,1,1,2) << endl;
    
    StMatrix<StDouble> K(3,3,1);
    K(1,3) = 2;
    K(2,1) = -2;
    K(1,3) = -1;

    cout << "K=" << K << endl;

    StMatrix<double> X(3,3);
    for(int ii=1; ii<=3; ii++)
	for(int jj=1; jj<=3; jj++)
	    X(ii,jj) = 1;

    StMatrix<float> Y(X);
    
    cout << "X" << X << endl;
    
    cout << "Y+X" << (Y+X) << endl;
    cout << "Y*X" << (Y*X) << endl;
    
#ifdef ST_NO_TEMPLATE_DEF_ARGS
    StMatrix<double> TT;
#else
    StMatrix<> TT;
#endif
    TT = K.apply(hold1);
    cout << "square each element of K:=" << TT << endl;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
    StThreeVector<double> a(1,1,1);
#else
    StThreeVector<> a(1,1,1);
#endif
    cout << "StThreeVector a= " << a << endl;
    cout << "a*K=" << a*K << endl;
    cout << "K*a=" << K*a << endl;

    cout << "Testing exception handling." << endl;
    cout << "Note that depending on your platform" << endl;
    cout << "the resulting actions might differ." << endl;
    StMatrix<StDouble> Z(3,3,1);
#ifndef ST_NO_EXCEPTIONS
    try {
        Z = K*A;
    }
    catch(logic_error &e){
	cout << "Caught exception: " << e.what() << endl;
    }
#else
    Z = K*A;
#endif    
    
#endif // SYMMETRIC
    
#ifdef ASYMMETRIC
    cout << "TESTING ANTI-SYMMETRIC MATRICES" << endl;
    StMatrix<StDouble> A(2,3);
    
    A(1,1) = 2;
    A(1,2) = 1;
    A(2,1) = 0;
    A(2,2) = 5;
    
    cout << "A=" << A << endl;
    
    StMatrix<StFloat> B(3,2);
    
    B(1,1) = 0;
    B(1,2) = 1;
    B(2,1) = 1;
    B(2,2) = 0;
    
    cout << "-B=" << -B;
    
    StMatrix<StFloat> C(A);
    
    cout << "C=" << C;
    
    C+=C;
    cout << "C+=C =>" << C << endl;
    
    A*B;
    cout << "A*B:" << (A*B) << endl;

    cout << "B*A:" << (B*A) << endl;

    unsigned int ierr;
    cout << "B.inverse(ierr)" << B.inverse(ierr) << endl;

    cout << "C=" << C << endl;
    cout << "1st row of C" << endl;
    cout << C.sub(1,1,1,3) << endl;

    
#ifdef ST_NO_TEMPLATE_DEF_ARGS
    StMatrix<double> TT;
#else
    StMatrix<> TT;
#endif
    TT = A.apply(hold1);
    cout << "square all elements in B:" << B << endl;
#endif
    return(0);
}


