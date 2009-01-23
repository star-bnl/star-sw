#include "SVTAnodeSumHistogramGroup.h"
#include "SVTAnodeHybridHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"
#include "TStyle.h"
#include <iomanip>

ClassImp(SVTAnodeSumHistogramGroup) ;


SVTAnodeSumHistogramGroup::SVTAnodeSumHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
  mRunNumber = 0;
  mEvents = 0;
  hSVT[0] = new TH1D( pre("SVTAnodeOCC"), "SVTAnodeOCC", 432,-0.5,431.5);
  hSVT[1] = new TH1D( pre("SVTAnodeOCCCount"), "SVTAnodeOCCCount", 432,-0.5,431.5);
  hSVT[0]->SetYTitle("Anode Occupancy");
  hSVT[0]->SetXTitle("Hybrid");
}


SVTAnodeSumHistogramGroup::~SVTAnodeSumHistogramGroup() {
  for ( unsigned int i=0; i<2; i++) {
    delete hSVT[i];
  }
}


void SVTAnodeSumHistogramGroup::reset() {
  mRunNumber = 0;
  mEvents = 0;
  for ( unsigned int i=0; i<2; i++) {
    hSVT[i]->Reset();
  }
}


void SVTAnodeSumHistogramGroup::draw(TCanvas* cc) {
  cc->cd();
  cc->Clear();
  gStyle->SetPalette(1,0);
  cc->cd(1); hSVT[0]->Draw("");
  cc->Update();
} 

void SVTAnodeSumHistogramGroup::beginRun(evpReader* evp, char* datap) {
  mRunNumber = evp->run;
}

void SVTAnodeSumHistogramGroup::endRun() {  
  char name[1024];
  sprintf(name,"/a/histos/laue/run%d.svt.anodeOcc.txt",mRunNumber);
  ofstream os(name);
  for ( int i=1; i<=hSVT[0]->GetNbinsX(); i++) {
    os <<  (int)hSVT[0]->GetBinCenter(i) << " " << hSVT[0]->GetBinContent(i) << endl;
  }
  os.close();
}


bool SVTAnodeSumHistogramGroup::fill(evpReader* evp, char* datap) { 
#ifndef NEW_DAQ_READER
int ret = svtReader(datap) ;
if(ret <= 0) return false;
  mEvents++;
  int s,r,p,a,t;
  int b,l,w,h;
  unsigned int adc;
  unsigned char val, tb;
  int index = -1;
  int barrel[3] = {8,12,16};
  int ladder[3] = {4,6,7};
  // for(s=0;s<24;s++) { // receiver
  //   for(r=0;r<3;r++) { // mezzanine
  //     for(p=0;p<6;p++) { // asic
   for(b=1;b<=3;b++) { // barrel
     for(l=1;l<=barrel[b-1];l++) { // ladder
       for(w=1;w<=ladder[b-1];w++) { // wafer
	 for(h=1; h<=2; h++){
	   SVTAnodeHybridHistogramGroup::blwh2rma(b, l, w, h, s, r, p);
	   index++;
	   adc = 0;
	   for(a=0;a<240;a++) { // anode
	     for(t=0;t<svt.counts[s][r][p][a];t++) {
	       val = svt.adc[s][r][p][a][t] ;
	       tb = svt.timebin[s][r][p][a][t];
	       adc += val;
	     } 
	   } // anode 
	   //cout << endl << dec << index << "  " << a << " " << adc << endl;
	   if ( adc>0 ) {
	     hSVT[0]->Fill( (float)index, 1. );
	     hSVT[1]->Fill( (float)index, 1. );
	   }
	 } // asic
       } // wafer
     }// ladder
   }  //barrel
  hSVT[0]->Add(hSVT[1],hSVT[0],1,0);
  hSVT[0]->Scale(1./mEvents);
  return true;
#else
  return false;
#endif
}
