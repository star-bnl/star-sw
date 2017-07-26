// Author: Victor Perev   06/06/10


#ifndef ROOT_TRandomVector
#define ROOT_TRandomVector


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TRandomVector                                                        //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TNamed.h"
#include "TString.h"
#include "TMatrixD.h"
#include "TMatrixDSym.h"
#include "TVectorD.h"
#include "TRandom.h"

class TRandomVector: public TObject
{
public:
	TRandomVector();
	TRandomVector(const TMatrixDSym& errMtx,UInt_t seed = 1946);
	TRandomVector(const TVectorD&    diaMtx,UInt_t seed = 1946);
	TRandomVector(int nSide,const double *G,UInt_t seed = 1946);
       ~TRandomVector(){;}
   int  Set(const TMatrixDSym& errMtx,UInt_t  seed = 65539);
static void   RandRotate(TMatrixDSym& errMtx);
static double Sign(const TMatrixDSym& errMtx);

const TMatrixDSym &GetMtx() const 	{return fErrMtx;}
const TMatrixD    &GetEig() const 	{return fEigMtx;}
const TVectorD    &GetLam() const 	{return fEigVal;}
const TVectorD& Gaus();
static void Test(int nevt = 10000);
static void TestXi2();

//		Data members
private:
int 		fDim;
TRandom      	fRandom;
TMatrixDSym 	fErrMtx;
TMatrixD    	fEigMtx;
TVectorD    	fEigVal;
TVectorD    	fResult;
ClassDef(TRandomVector,0)

};
#endif //ROOT_TRandomVector
