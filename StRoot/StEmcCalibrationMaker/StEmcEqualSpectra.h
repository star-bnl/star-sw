/**********************************************************************
* StEmcEqualSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/

/*!\class StEmcEqualSpectra
\author Alexandre A. P. Suaide

This class makes EMC equalization. It is derived from StEmcSpectra.
*/

#ifndef STAR_StEmcEqualSpectra
#define STAR_StEmcEqualSpectra
#include "StEmcSpectra.h"
#include "TH1.h"
#include "TH2.h"
class StEvent;

class StEmcEqualSpectra : public StEmcSpectra 
{
  private:
					 Int_t    mEqualMode;
					 Float_t  mEqualMin;
					 Float_t  mEqualMax;
					                                                                
  protected:   
  public: 
           
                    StEmcEqualSpectra(const char*, Int_t = 500, Float_t = 0, Float_t = 500); ///< Default constructor 
  virtual           ~StEmcEqualSpectra(); ///< Default destructor
           void     DrawEqualConst(); ///< Draw Equalization constants
           Bool_t   Equalize(Int_t,Int_t,Int_t); ///< Equalize two bins
					 Bool_t   Fill(TH1F*,StEvent*);
					 Bool_t   Equalize();
					 
					 void     SetEqualMode(Int_t a)  		{ mEqualMode = a; }
					 void     SetEqualMin(Float_t a) 		{ mEqualMin = a; }
					 void     SetEqualMax(Float_t a) 		{ mEqualMax = a; }
					 
					 Int_t    GetEqualMode()  					{ return mEqualMode; }
					 Float_t  GetEqualMin() 						{ return mEqualMin; }
					 Float_t  GetEqualMax() 						{ return mEqualMax; }
           

  ClassDef(StEmcEqualSpectra,1)
};
#endif
