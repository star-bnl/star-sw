/**********************************************************************
* StEmcMipSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for MIP Calibration
***********************************************************************/


#ifndef STAR_StEmcMipSpectra
#define STAR_StEmcMipSpectra
#include "StEmcSpectra.h"
#include "TArrayF.h"

class StEmcMipSpectra : public StEmcSpectra 
{
  private:
                                                                     
  protected:   
  public: 

                   StEmcMipSpectra(const char*);  
  virtual          ~StEmcMipSpectra();
           void    DrawEtaBin(Int_t);
           Bool_t  CalibrateEtaBin(Int_t,Int_t);
           Bool_t  CalibrateBin(Int_t,Int_t);
           Bool_t  CalibrateByMip(Int_t,TArrayF,Int_t,Int_t); 
           TArrayF GetEtaBinSpectra(Int_t);

  ClassDef(StEmcMipSpectra,1)
};
#endif
