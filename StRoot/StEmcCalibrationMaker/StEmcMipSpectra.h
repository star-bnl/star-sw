/**********************************************************************
* StEmcMipSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for MIP Calibration
***********************************************************************/

#ifndef STAR_StEmcMipSpectra
#define STAR_StEmcMipSpectra
#include "StEmcSpectra.h"
#include "TH1.h"
#define MAXBIN 40
#include "TF1.h"

/*!\class StEmcMipSpectra
\author Alexandre A. P. Suaide

This class makes EMC MIP calibration. It is derived from StEmcSpectra.
*/
class StEvent;

class StEmcMipSpectra : public StEmcSpectra 
{
  private:
          Bool_t     doUseGuess[MAXBIN];
          Float_t    guess[MAXBIN][6];  
          Int_t      fixed[MAXBIN][6];  
          Int_t      FIRST[MAXBIN];
          Int_t      LAST[MAXBIN];
          Int_t      NPars;
          TF1*       peak;
          TF1*       back;
          
          TH2D*      mMipFit;          
                                                                     
  protected:   
  public: 

                   StEmcMipSpectra(const char*,Int_t =300,Float_t =0, Float_t =300 );       ///< Default constructor 
  virtual          ~StEmcMipSpectra();                           ///< Default destructor 
           TH1D*   DrawEtaBin(Int_t);                            ///< Draw eta bin spectrum  
           Bool_t  CalibrateEtaBin(Int_t,Int_t);                 ///< Calibrate eta bin 
           Bool_t  CalibrateBin(Int_t,Int_t);                    ///< Calibrate individual bin 
           Bool_t  CalibrateByMip(Int_t,TH1D*,Int_t,Int_t);      ///< Does spectrum fit 
           void    SetInitialGuess(Int_t,Float_t*,Int_t*,Int_t,Int_t);  ///< Set Initial Guess for EtaBin fit
           Bool_t  GetMipPosition(Int_t,Float_t*,Float_t*);      ///< Return Mip peak position for a given channel

           Bool_t  Fill(TH1F*,StEvent*);         ///< Fills MIP spectra
           Bool_t  MipCalib();         ///< Finds mip peak
  ClassDef(StEmcMipSpectra,1)
};
#endif
