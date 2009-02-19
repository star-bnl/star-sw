#include "LaserTpcPadChargeHistogramGroup.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_TPC/tpcReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif
#include <iostream>
#include "TMapFile.h"
#include "EvpUtil.h"
#include "StReadLaserEvent.h"



ClassImp(LaserTpcPadChargeHistogramGroup) ;

unsigned int LaserTpcPadChargeHistogramGroup::mSec[6] = { 1, 6, 11, 13, 19, 23};   // sector indices start at 0

LaserTpcPadChargeHistogramGroup::LaserTpcPadChargeHistogramGroup() {
  // For ROOT I/O
  memset( hTpcSec,0,sizeof(hTpcSec));
}

LaserTpcPadChargeHistogramGroup::LaserTpcPadChargeHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector) : HistogramGroup(group,subGroup,trigger,detector) {  
  char name[1024];
  char title[1024];
  for ( int i = 0; i<6; i++) { 
    sprintf(name,"hTpcSecPadChargeLaser%02d",mSec[i]);
    sprintf(title,"Sec. %2d charge per pad LASER",mSec[i]+1);
    hTpcSec[i] = new TH2D(pre(name),title,182,0.,182.,45,0.,45.);
  }
}

LaserTpcPadChargeHistogramGroup::~LaserTpcPadChargeHistogramGroup() {
  for ( int i = 0; i<6; i++) { 
    delete hTpcSec[i];
  }
}

void LaserTpcPadChargeHistogramGroup::reset() {
  for ( int i = 0; i<6; i++) { 
    hTpcSec[i]->Reset();
  }
}


void LaserTpcPadChargeHistogramGroup::draw(TCanvas* cc) {
  //cout << __PRETTY_FUNCTION__ << endl;
  cc->cd();
  cc->Clear();
  cc->Divide(2,3);
  for ( int i = 0; i<6; i++) { 
    cc->cd(i+1);
    hTpcSec[i]->Draw("colz");
  }
  cc->Update();
} 

#include <stdlib.h>
bool LaserTpcPadChargeHistogramGroup::fill(evpReader* evp, char* datap) {  
  for(unsigned int i=0; i<6; i++) {
    unsigned int sec = mSec[i];
    int ret = tpcReader(datap,sec) ;
    // 0 is what I found EVP_NO_DET to be defined as, but maybe this should be hard-coded
    if ( ret == 0 ) return false; // no TPC data present
    //if ( ret == EVP_NO_DET ) return false; // no TPC data present
    if ( ret < 0 ) continue;        // some error in this sector
    
    double adc_sector = 0. ; // adc sum for the sector
    if(tpc.mode==0) {	// normal event
      for(unsigned int r=0;r<45;r++) { // padrow
	for(unsigned int p=0;p<182;p++) { // pad
	  double pad_adc =0; // adc sum per pad
	  for(unsigned int t=0;t<tpc.counts[r][p];t++) {
	    unsigned int val = tpc.adc[r][p][t] ;
	    pad_adc += val;
	  }//end pad time sequence
	  adc_sector += pad_adc;// adc sum per sector
	  hTpcSec[i]->Fill( (double)p, (double)r, pad_adc);
	} // end of pad loop
      } // end padrow
    } // mode
  }
  return true;
}



 

