#include "TOFL1multHistogramGroup.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <stdlib.h>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"
#include "TStyle.h"
#include <TEnv.h>

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
//#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_TOF/tofReader.h"
#  include "DAQ_READER/cfgutil.h"
#  include "StEvent/StTriggerData.h"
//#  include "DAQ_L3/l3Reader.h"
#  include "TriggerData.h"
#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"

using namespace std;


ClassImp(TOFL1multHistogramGroup) ;

TOFL1multHistogramGroup::TOFL1multHistogramGroup() {
  // For ROOT I/O

  TOF_L1mult_vs_ZDCadcsum=0;

}

TOFL1multHistogramGroup::TOFL1multHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  //
  TOF_L1mult_vs_ZDCadcsum=new TH2F("TOF_L1mult_vs_ZDCadcsum","TOF_L1mult_vs_ZDCadcsum",288,0.5,2880.5,200,0,3000);
  TOF_L1mult_vs_ZDCadcsum->SetXTitle("TOF L1 Mult");
  TOF_L1mult_vs_ZDCadcsum->SetYTitle("ZDC hardware adc Sum");
 

}


TOFL1multHistogramGroup::~TOFL1multHistogramGroup() {

  //
  delete TOF_L1mult_vs_ZDCadcsum;
}

void TOFL1multHistogramGroup::reset() {

  TOF_L1mult_vs_ZDCadcsum->Reset();
}


void TOFL1multHistogramGroup::draw(TCanvas* cc) {

  TLatex label;
  //label.SetTextAlign(23);  // center, top
  label.SetTextSize(0.056);
  label.SetTextColor(45);

  TLine  line;
  line.SetLineColor(4);
  line.SetLineWidth(2);
  //
  gStyle->SetPalette(1);
  gStyle->SetLabelSize(0.1,"y");
  gStyle->SetLabelSize(0.1,"x");

  gStyle->SetOptTitle(1);
  gStyle->SetTitleX(0.1); gStyle->SetTitleY(1.);
  gStyle->SetTitleW(0.8); gStyle->SetTitleH(0.088);
  //gStyle->SetTitleSize(0.06);
 
  gStyle->SetOptStat(1);
  gStyle->SetStatX(0.9); gStyle->SetStatY(0.9);
  gStyle->SetStatW(0.21); gStyle->SetStatH(0.15);
  //gStyle->SetStatFontSize(0.14);

  //char tmpchr[200];
  cc->cd(); cc->SetFillColor(0);
  cc->Clear();
  //cc->Divide(1, 2);
  gPad->SetGridx(1);
  gPad->SetGridy(0);

  TOF_L1mult_vs_ZDCadcsum->Draw("colz");

  cc->Update();

} 


bool TOFL1multHistogramGroup::fill(evpReader* evp, char* datap) { 
  
  int ret=tofReader(datap);
  if(ret <= 0)   {
    fprintf(stderr,"TOF: problems in data (%d) - continuing...",ret);
    return false;
  }

  // information from trigger .
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd) return false;  
  float ZDCadcsum= float(trgd->zdcHardwareSum());
  float TOF_L1mult=float(trgd->tofMultiplicity(0));
  TOF_L1mult_vs_ZDCadcsum->Fill(TOF_L1mult,ZDCadcsum);

  //cout<<"TOF L1 ::: "<<ZDCadcsum<<" "<<TOF_L1mult<<endl;

  return true;

}

