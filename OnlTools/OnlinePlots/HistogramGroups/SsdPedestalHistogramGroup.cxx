#include "SsdPedestalHistogramGroup.h"
#include "SVTAnodeHybridHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"
#include "TBox.h"

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

ClassImp(SsdPedestalHistogramGroup) ;


extern int  SsdDaqToRealLadder[40];  // declared in HistoHandler.cxx



/*
int   SsdDaqToRealLadder[40] = {11, 13, 15, 12, 14,
				16, 18, 20, 17, 19,
				10,  8,  6,  9,  7,
				 5,  3,  1,  4,  2,
				30, 28, 26, 29, 27,
				25, 23, 21, 24, 22, 
				31, 33, 35, 32, 34,
				36, 38, 40, 37, 39};
*/

///////////////////////////////////////
// Structures 
///////////////////////////////////////
struct SsdCalibPedStruct {           // for Pedestal and Noise
  int evt    ;
  int ladder ;
  int det    ;
  int strip  ;
  int ped    ;
  int noise  ;
};


void SsdPedestalHistogramGroup::SsdTH1Setup(TH1 *rHisto,const char *rTitleX, const char *rTitleY,int rMax,int rFillColor, int rLineColor) {
  rHisto->SetXTitle(rTitleX);
  rHisto->SetYTitle(rTitleY);
  if(rMax) rHisto->SetMaximum(rMax); // don't set max if 0 specified
  if(rFillColor) rHisto->SetFillColor(rFillColor);
  if(rLineColor) rHisto->SetLineColor(rLineColor);
  rHisto->SetLabelSize(0.025,"xy");
  rHisto->SetTitleSize(0.03,"xy");
}


SsdPedestalHistogramGroup::SsdPedestalHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
  //  mRunNumber = 0;
  //mEvents = 0;
  h1SsdMeanPedestal = new TH1F( pre("h1SsdMeanPedestal"), "Mean Pedestal per ladder",40,1,41);
  SsdTH1Setup(h1SsdMeanPedestal,"Real Ladder #","Pedestal (ADC Counts)",0,15);
  h1SsdMeanNoise= new TH1F( pre("h1SsdMeanNoise"), "Mean Noise per ladder",40,1,41);
  SsdTH1Setup(h1SsdMeanNoise,"Real Ladder #","Noise (ADC Counts)",0,15);
}


SsdPedestalHistogramGroup::~SsdPedestalHistogramGroup() {
  delete h1SsdMeanPedestal;
  delete h1SsdMeanNoise;
}


void SsdPedestalHistogramGroup::reset() {
  // mRunNumber = 0;
  // mEvents = 0;
  good_ssd=0;
  h1SsdMeanPedestal->Reset();
  h1SsdMeanNoise->Reset();
}


void SsdPedestalHistogramGroup::draw(TCanvas* cc) {
  cc->cd();
  cc->Clear();
  TBox *box = new TBox(0,35,20,220);
  box->SetFillStyle(3001);
  box->SetFillColor(70);
  box->Draw();
  TBox *box2 = new TBox(0,25,20,140);
  box2->SetFillStyle(3001);
  box2->SetFillColor(40);
  box2->Draw();
  cc->Divide(1,2);
  gStyle->SetPalette(1,0);
  cc->cd(1);
  h1SsdMeanPedestal->Draw("same");
  cc->cd(2);
  h1SsdMeanNoise->Draw("same");
  cc->Update();
} 

void SsdPedestalHistogramGroup::beginRun(evpReader* evp, char* datap) {
  // mRunNumber = evp->run;
}

void SsdPedestalHistogramGroup::endRun() {  
  /*
  char name[1024];
  sprintf(name,"/a/histos/laue/run%d.svt.anodeOcc.txt",mRunNumber);
  ofstream os(name);
  for ( int i=1; i<=hSVT[0]->GetNbinsX(); i++) {
    os <<  (int)hSVT[0]->GetBinCenter(i) << " " << hSVT[0]->GetBinContent(i) << endl;
  }
  os.close();
  */
}


bool SsdPedestalHistogramGroup::fill(evpReader* evp, char* datap) { 
  cout << __PRETTY_FUNCTION__ << endl;
#ifndef NEW_DAQ_READER
  int ret = ssdReader(datap) ;
  if(ret <= 0) return false;
  cout << __PRETTY_FUNCTION__ << endl;


  
  // ******************************************
  // If pedestal data (pedestals and noises)
  //
  // In these data the ssd bank structure is :
  // adc   = uncompress pedestal value as computed by DAQ
  // strip = uncompress noise value as computed by DAQ
  //
  // The corresponding mapping is made in
  // 40 ladders for each
  //    192 (virtual) time bins for each
  //        64 (virtual) pads
  // actually 192x64=1288 strips in ONE ladder
  // the ped and noise values are stored in DAQ STRIP order
  // so the module and strip # are reordered !
  printf ("SSD Pedestal mode \n");
  good_ssd++ ;
  SsdCalibPedStruct ssdCalibPedStruct ;
  int pos=0, nonEmptyStrip=0, ladderStrip=0;
  int pedSum=0, ped2Sum=0, noiseSum=0, noise2Sum=0;
  float meanPed=0, rmsPed=0, meanNoise=0, rmsNoise=0;
  for(int ladder=0; ladder<40; ladder++) {
    pos=0;
    nonEmptyStrip=0;
    pedSum=ped2Sum=noiseSum=noise2Sum=0;
    for (int time =0 ; time<192 ; time++) {
      for (int pad=0 ; pad<64 ; pad++) {
	ssdCalibPedStruct.evt = good_ssd;
	//  ssdCalibPedStruct.ladder = SsdDaqToRealLadder[ladder]+(ladder>19?20:0); // correct physical ladder #
	ssdCalibPedStruct.ladder = SsdDaqToRealLadder[ladder];
	ssdCalibPedStruct.det = pos/768; // pseudo module #
	ssdCalibPedStruct.strip = pos%768; // pseudo strip # in the module (from 1-n_strip)
	ssdCalibPedStruct.ped = ssd.adc[ladder][pad][time]; // pedestal
	ssdCalibPedStruct.noise = ssd.strip[ladder][pad][time]; // noise
	// prepare computation of mean and rms
	// only for strips with non 0 noise
	if( ssdCalibPedStruct.noise>0 ) { 
	  nonEmptyStrip++;
	  pedSum += ssdCalibPedStruct.ped;
	  ped2Sum += ssdCalibPedStruct.ped*ssdCalibPedStruct.ped;
	  noiseSum += ssdCalibPedStruct.noise;
	  noise2Sum += ssdCalibPedStruct.noise*ssdCalibPedStruct.noise;
	}
	
	ladderStrip = ssdCalibPedStruct.det*768+ssdCalibPedStruct.strip;
	pos++ ;
      } // end pad loop
    } // end time loop
    // compute mean ped and noise end fill histos
    if( nonEmptyStrip>0 ) {
      meanPed = (float)pedSum/nonEmptyStrip;
      rmsPed = sqrt( (float)(ped2Sum-nonEmptyStrip*meanPed*meanPed)/(nonEmptyStrip-1) );
      meanNoise = (Float_t)noiseSum/nonEmptyStrip;
      rmsNoise = sqrt( (Float_t)(noise2Sum-nonEmptyStrip*meanNoise*meanNoise)/(nonEmptyStrip-1) );
      h1SsdMeanPedestal->SetBinContent( ssdCalibPedStruct.ladder, meanPed );
      h1SsdMeanPedestal->SetBinError( ssdCalibPedStruct.ladder, rmsPed );
      h1SsdMeanNoise->SetBinContent( ssdCalibPedStruct.ladder, meanNoise );
      h1SsdMeanNoise->SetBinError( ssdCalibPedStruct.ladder, rmsNoise );
      //printf(" ladder %2d: ped = %3d+-%3d, noise = %3d+-%3d\n",ssdCalibPedStruct.ladder, (int)meanPed, (int)rmsPed, (int)meanNoise, (int)rmsNoise);
    }
    //printf("Loop Ladder %2d, Real Ladder %2d, (nonEmptyStrip=%5d) ped = %.0f+-%.0f, noise = %.0f+-%.0f\n",ladder,ssdCalibPedStruct.ladder, nonEmptyStrip, meanPed, rmsPed, meanNoise, rmsNoise);
  } // end ladder loop




  return true;
#else
  return false;
#endif
}
