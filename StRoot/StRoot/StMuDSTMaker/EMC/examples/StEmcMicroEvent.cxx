//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StEmcMicroEvent.h"
ClassImp(StEmcMicroEvent)

StEmcMicroEvent::StEmcMicroEvent()
{
  fpTracks  = new TObjArray();
  fgTracks  = new TObjArray();
  fV0 = new TObjArray();
  mVersion=3;
}
StEmcMicroEvent::~StEmcMicroEvent()
{
  clear();
  if(fpTracks) delete fpTracks;
  if(fgTracks) delete fgTracks;
  if(fV0) delete fV0;
  if(fEmc) { delete fEmc; fEmc=NULL;}
  if(fFpd) { delete fFpd; fFpd = NULL; }  
}
void StEmcMicroEvent::clear(Option_t *option)
{
  Int_t n =getNPrimaryTrack();
  for(Int_t i=0;i<n;i++) 
  {
    StEmcMicroTrack* t = getPrimaryTrack(i);
    if(t) delete t;
  } 
  n =getNGlobalTrack();
  for(Int_t i=0;i<n;i++) 
  {
    StEmcMicroTrack* t = getGlobalTrack(i);
    if(t) delete t;
  } 
  n=getNV0();
  for(Int_t i=0;i<n;i++)
  {
    StEmcMicroV0* v = getV0(i);
    if(v) delete v;
  }
  fpTracks->Clear();
  fgTracks->Clear();
  fV0->Clear();
  if(fEmc) fEmc->clear();
  return;
}
