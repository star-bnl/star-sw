/**********************************************************************
* StEmcPedSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/

/*!\class StEmcPedSpectra
\author Alexandre A. P. Suaide

This class makes EMC equalization. It is derived from StEmcSpectra.
*/

#ifndef STAR_StEmcPedSpectra
#define STAR_StEmcPedSpectra
#include "StEmcSpectra.h"
#include "TH1.h"
#include "TH2.h"
class StEvent;

class StEmcPedSpectra : public StEmcSpectra 
{
  private:
					 Int_t    mPedMode;
           TH2F*    mPed;
					                                                                
  protected:   
  public: 
           
                    StEmcPedSpectra(const char*, Int_t = 150, Float_t = 1, Float_t = 151); ///< Default constructor 
  virtual           ~StEmcPedSpectra(); ///< Default destructor
					 Bool_t   Fill(TH1F*,StEvent*);	
           Bool_t   CalculatePedestals();
           TH2F*    GetPedestals()            { return mPed; }				 
					 void     SetPedMode(Int_t a)  		  { mPedMode = a; }					 
					 Int_t    GetPedMode()  					  { return mPedMode; }
           

  ClassDef(StEmcPedSpectra,1)
};
#endif
