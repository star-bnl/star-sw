#include "LaserDriftTimeHistogramGroup.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif
#include <iostream>
#include "TMapFile.h"
#include "EvpUtil.h"
#include "StReadLaserEvent.h"

#include <rtsLog.h>

#define NO_RTS_LOG

ClassImp(LaserDriftTimeHistogramGroup) ;

LaserDriftTimeHistogramGroup::LaserDriftTimeHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector) : HistogramGroup(group,subGroup,trigger,detector), mLaser(0), hDriftTime(0) {
  hDriftTime = new TH1D( pre("hDriftTime"), "TPC Drift Velocity (cm/us)",400,5.4,5.8);
  rtsLogLevel(NOTE);
  rtsLogAddDest("172.16.0.1",8002);
  rtsLogOutput(RTS_LOG_NET);
}

LaserDriftTimeHistogramGroup::~LaserDriftTimeHistogramGroup() {
  //cout << __PRETTY_FUNCTION__ << endl;
  delete hDriftTime;
  delete mLaser;
  hDriftTime = 0;
  mLaser =0;
}

void LaserDriftTimeHistogramGroup::reset() {
  hDriftTime->Reset();
  delete mLaser;
  mLaser = new StReadLaserEvent();
}


void LaserDriftTimeHistogramGroup::draw(TCanvas* cc) {
  //cout << __PRETTY_FUNCTION__ << endl;
  cc->cd();
  cc->Clear();
  hDriftTime->Draw();
  cc->Update();
} 

#include <stdlib.h>
bool LaserDriftTimeHistogramGroup::fill(evpReader* evp, char* datap) {  
  //disable not compatible yet
  float vDrift = 0.0;//mLaser->Make(evp->run, evp->event_number, &tpc, datap);
  //if(mDebugLevel)printf("drift velocity: %2.4f\n", vDrift);
  //    if(mDebugLevel)
  //LOG(INFO,"run %d  event %d : Tpc drift time = %f ",evp->run, evp->event_number,vDrift);
  //printf("EventLopp::vDrift = %10.3f\n  run=%d", vDrift,evp->run);
  if (vDrift > 1.){ 
    hDriftTime->Fill(vDrift); 
  }
  return true;
}



 

