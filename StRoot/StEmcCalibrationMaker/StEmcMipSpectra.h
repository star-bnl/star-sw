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

/*!\class StEmcMipSpectra
\author Alexandre A. P. Suaide

This class makes EMC MIP calibration. It is derived from StEmcSpectra.
*/
class StEmcMipSpectra : public StEmcSpectra 
{
  private:
                                                                     
  protected:   
  public: 

                   StEmcMipSpectra(const char*); //!< Default constructor 
  virtual          ~StEmcMipSpectra();//!< Default destructor 
           void    DrawEtaBin(Int_t);//!< Draw eta bin spectrum  
           Bool_t  CalibrateEtaBin(Int_t,Int_t);//!< Calibrate eta bin 
           Bool_t  CalibrateBin(Int_t,Int_t);//!< Calibrate individual bin 
           Bool_t  CalibrateByMip(Int_t,TArrayF,Int_t,Int_t); //!< Does spectrum fit 
           TArrayF GetEtaBinSpectra(Int_t);//!< Return Eta bin spectrum 

  ClassDef(StEmcMipSpectra,1)
};
#endif
