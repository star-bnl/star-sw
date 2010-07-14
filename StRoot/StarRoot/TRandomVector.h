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
#include "TMatrixT.h"
#include "TMatrixTSym.h"
#include "TVectorT.h"
#include "TRandom.h"

class TRandomVector: public TObject
{
public:
	TRandomVector();
	TRandomVector(const TMatrixTSym<double>& errMtx,UInt_t seed = 65539);
       ~TRandomVector(){;}
   int  Set(const TMatrixTSym<double>& errMtx,UInt_t  seed = 65539);
const TVectorT<double>& Gaus();
static void Test(int nevt = 10000);

//		Data members
private:
                int fDim;
TRandom             fRandom;
TMatrixTSym<double> fErrMtx;
TMatrixT<double>    fEigMtx;
TVectorT<double>    fEigVal;
TVectorT<double>    fResult;
ClassDef(TRandomVector,0)

};
#endif //ROOT_TRandomVector
