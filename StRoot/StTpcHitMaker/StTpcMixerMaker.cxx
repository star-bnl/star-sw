/***************************************************************************
 *
 * $Id: StTpcMixerMaker.cxx,v 1.5 2018/10/17 20:45:27 fisyak Exp $
 * $Log: StTpcMixerMaker.cxx,v $
 * Revision 1.5  2018/10/17 20:45:27  fisyak
 * Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
 *
 * Revision 1.4  2014/06/26 21:31:42  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.3  2012/05/02 22:21:59  fisyak
 * Remove requirement to have 2 and only 2 inputs
 *
 * Revision 1.2  2012/03/20 15:43:30  fisyak
 * Ignore  Input1/2 setting, find Event data sets from chain in order which they have been instantiated, bug 2299
 *
 * Revision 1.1  2008/07/31 20:45:27  fisyak
 * Add TpcMixer
 *
 **************************************************************************/
#include "StTpcMixerMaker.h"
#include "StMessMgr.h" 
#include "StTpcRawData.h"
#include "TSystem.h"
ClassImp(StTpcMixerMaker)
//______________________________________________________________________________
Int_t StTpcMixerMaker::Make() {
  StTpcRawData *inputs[2] = {0, 0};
  TDataSetIter next(StMaker::GetTopChain(),9999);
  TDataSet *set = 0;
  Int_t nfound = 0;
  while (( set = next()) && nfound < 2) {//loop over makers/data sets 
    TString Path(gSystem->BaseName(set->Path()));
    if (Path != "Event") continue;
    TObjectSet *event = dynamic_cast<TObjectSet *>(set);
    if (! event) continue;
    inputs[nfound] = dynamic_cast<StTpcRawData *>(event->GetObject());
    if (! inputs[nfound]) continue;
    LOG_INFO << "Found \t" << Path << " in " << set->Path() << "\tas Input" << nfound+1 << endm;
    nfound++;
  }
  if (nfound == 2) {
    *inputs[0] += *inputs[1];
  } else {
    LOG_ERROR << "Found " << nfound << " Makers with StTpcRawData Event" << endm;
    if (! nfound) return kStERR;
  }
  return kStOK;
} 


