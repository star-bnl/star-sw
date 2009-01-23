#include "SVTAnodeHybridHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>
#include "svtMap.h"

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

ClassImp(SVTAnodeHybridHistogramGroup) ;

unsigned int SVTAnodeHybridHistogramGroup::mFirstHybrid[__MAXBOARDS__+1] = { 0, 32, 64, 100, 136, 172, 208, 236, 264, 292, 320, 348, 376, 404, 432};
const char* SVTAnodeHybridHistogramGroup::mNames[4] = { "AdcSum", "AdcSumCount", "Hit", "HitCount" };

SVTAnodeHybridHistogramGroup::SVTAnodeHybridHistogramGroup(unsigned int board, const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector), mBoard(board) {
  if ( board >= __MAXBOARDS__ ) {
    cerr << __PRETTY_FUNCTION__ << "  board " << board << " does not exists " << endl; 
    exit(-1);
  }
  mEvents = 0;
  char name[1024];
  for ( unsigned int i=0; i<4; i++) {
    sprintf(name,"%s%02d",mNames[i],mBoard);
    hSVT[i] = new TH2D( pre(name), name, mFirstHybrid[mBoard+1]-mFirstHybrid[mBoard],mFirstHybrid[mBoard]-0.5,mFirstHybrid[mBoard+1]-0.5, 240,-0.5, 239.5);
    hSVT[i]->SetXTitle("Hybrid");
    hSVT[i]->SetYTitle("Anode");
  }
}


SVTAnodeHybridHistogramGroup::~SVTAnodeHybridHistogramGroup() {
  for ( unsigned int i=0; i<4; i++) {
    delete hSVT[i];
  }
}


void SVTAnodeHybridHistogramGroup::reset() {
  mEvents = 0;
  for ( unsigned int i=0; i<4; i++) {
    hSVT[i]->Reset();
  }
}


void SVTAnodeHybridHistogramGroup::draw(TCanvas* cc) {
  cc->cd();
  cc->Clear();
  cc->Divide(1,2);
  gStyle->SetPalette(1,0);
  cc->cd(1); hSVT[0]->Draw("COLZ");
  cc->cd(2); hSVT[2]->Draw("COLZ");
  cc->Update();
} 


bool SVTAnodeHybridHistogramGroup::fill(evpReader* evp, char* datap) { 
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
	   blwh2rma(b, l, w, h, s, r, p);
	   index++;
	   if ( (index >= mFirstHybrid[mBoard]) && (index < mFirstHybrid[mBoard+1]) ) { 
	     for(a=0;a<240;a++) { // anode
	       adc = 0;
	       for(t=0;t<svt.counts[s][r][p][a];t++) {
		 val = svt.adc[s][r][p][a][t] ;
		 tb = svt.timebin[s][r][p][a][t];
		 adc += val;
		 
		 if ( val ) hSVT[3]->Fill( (float)index, (float)a);
	       }
	       //cout   << dec << "index " << index << "  " << a << " " << adc << endl;
	       hSVT[1]->Fill( (float)index, (float)a, adc );

	     } // anode 
	   }  // board
	   
	 }// hybrid 
	}  // wafer
     } // ladder
   } // barrel
	   //      } // asic
	   //    } // mezzanine
	   //  }// receiver

  hSVT[0]->Add(hSVT[1],hSVT[0],1,0);
  hSVT[0]->Scale(1./mEvents);
  hSVT[2]->Add(hSVT[3],hSVT[2],1,0);
  hSVT[2]->Scale(1./mEvents);
  return true;
#else
  return false;
#endif
}

void SVTAnodeHybridHistogramGroup:: blwh2rma(int barrel, int ladder, 
				   int wafer, int hybrid,
				   int& recBoard, int& mezz, int& mz_hyb)
{
  switch (barrel) {
    case 1:
      recBoard = key_barrel1[ladder-1][wafer-1][hybrid-1][0];
      mezz = key_barrel1[ladder-1][wafer-1][hybrid-1][1];
      mz_hyb = key_barrel1[ladder-1][wafer-1][hybrid-1][2];
      break;
    case 2:
      recBoard = key_barrel2[ladder-1][wafer-1][hybrid-1][0];
      mezz = key_barrel2[ladder-1][wafer-1][hybrid-1][1];
      mz_hyb = key_barrel2[ladder-1][wafer-1][hybrid-1][2];
      break;
    case 3:
      recBoard = key_barrel3[ladder-1][wafer-1][hybrid-1][0];
      mezz = key_barrel3[ladder-1][wafer-1][hybrid-1][1];
      mz_hyb = key_barrel3[ladder-1][wafer-1][hybrid-1][2];
      break;
  }
}
