/**********************************************************************
* StEmcEffPedSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/
#include "StEmcEffPedSpectra.h"
#include <iostream.h>
#include <math.h>
#include "emc_def.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TGraphErrors.h"

ClassImp(StEmcEffPedSpectra);
//_____________________________________________________________________________
StEmcEffPedSpectra::StEmcEffPedSpectra(const char* cdet):StEmcSpectra(cdet)
{  
}
//_____________________________________________________________________________
StEmcEffPedSpectra::~StEmcEffPedSpectra()
{ }
//_____________________________________________________________________________
Bool_t StEmcEffPedSpectra::CalcPedestal(Int_t position)
{ 
  if (GetStatus(position)!=1) return kFALSE;
  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  Int_t mode=Settings_st[0].EffPedMethod;
  
  Bool_t ok=kFALSE;
  Float_t avg,rms;
  
  if(mode==0)
  {
    if(!GetMeanAndRms(position,&avg,&rms)) return kFALSE;
    cout <<"Eff pedestal for id = "<<position<<"  avg = "<<avg<<"  rms = "<<rms<<endl;    
  }
  
  if(ok)
  {
    emcCalibration_st* Calib_st=CalibTable->GetTable();
    Calib_st[position-1].AdcPedestal=avg;
    Calib_st[position-1].AdcPedestalRms=rms;
  }
  
  return ok;
}
