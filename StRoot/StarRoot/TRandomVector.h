// Author: Victor Perev   06/06/10


#ifndef ROOT_TRandomVector
#define ROOT_TRandomVector


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TRandomVector                                                          //
//                                                                     //
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
	TRandomVector(const TMatrixDSym& errMtx,UInt_t seed = 65539);
	TRandomVector(const TVectorD& diaMtx,UInt_t seed = 65539);
       ~TRandomVector(){;}
   int  Set(const TMatrixDSym& errMtx,UInt_t  seed = 65539);
const TMatrixDSym &GetMtx() const 	{return fErrMtx;}
const TVectorD& Gaus();
static void Test(int nevt = 10000);

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
