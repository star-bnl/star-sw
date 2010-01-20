#include "TOFL0HistogramGroup.h"

#include <iostream>
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
#  include "DAQ_TOF/tofReader.h"
#  include "DAQ_READER/cfgutil.h"
#  include "StEvent/StTriggerData.h"
#  include "TriggerData.h"
#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"

using namespace std;
char*  TOFL0HistogramGroup::mTrayList = EvpUtil::cat(gEnv->GetValue("Online.tofConfigPath","."),"TOF_TrayNotInRun.txt");
char*  TOFL0HistogramGroup::mTraymaskoutList = EvpUtil::cat(gEnv->GetValue("Online.tofConfigPath","."),"TOF_TrayMaskout.txt");

ClassImp(TOFL0HistogramGroup) ;

TOFL0HistogramGroup::TOFL0HistogramGroup() {
  // For ROOT I/O
  memset( TOF_L0_hit, 0, sizeof(TOF_L0_hit));
  memset( TOF_L0_trg, 0, sizeof(TOF_L0_trg));
}

TOFL0HistogramGroup::TOFL0HistogramGroup(unsigned int ipart,const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector),mPart(ipart),mNtray(MAXTRAYS) {
 
  for(int i=0;i<MAXTRAYS;i++){
    actualTrayNum[i]=i+mPart*mNtray+1;
  }

  char tmpchr[200];
  char tmpchr1[200];
  for(int itray=0;itray<mNtray;itray++){
    sprintf(tmpchr,"TOF_Tray%03d_hit_L0",actualTrayNum[itray]);
    sprintf(tmpchr1,"Tray %03d L0 (hit)",actualTrayNum[itray]);
    TOF_L0_hit[itray]=new TH1F(tmpchr,tmpchr1,32,-0.5,31.5);

    sprintf(tmpchr,"TOF_Tray%03d_trg_L0",actualTrayNum[itray]);
    sprintf(tmpchr1,"Tray %03d L0 (trg)",actualTrayNum[itray]);
    TOF_L0_trg[itray]=new TH1F(tmpchr,tmpchr1,32,-0.5,31.5);

  }
  ReadTrayList(); 
  ReadTraymaskoutList();

}


TOFL0HistogramGroup::~TOFL0HistogramGroup() {
  for (int i = 0; i < mNtray; ++i)delete TOF_L0_hit[i];
  for (int i = 0; i < mNtray; ++i)delete TOF_L0_trg[i];
}


void TOFL0HistogramGroup::reset() {
  for (int i = 0; i < mNtray; ++i)TOF_L0_hit[i]->Reset();
  for (int i = 0; i < mNtray; ++i)TOF_L0_trg[i]->Reset();
  ReadTrayList();
  ReadTraymaskoutList();

}


void TOFL0HistogramGroup::draw(TCanvas* cc) {

  TLatex label;
  label.SetTextAlign(23);  // center, top
  label.SetTextSize(0.06);
  //label.SetTextColor(4);

  TLine  line;
  line.SetLineColor(4);
  line.SetLineWidth(2);
  //
  gStyle->SetPalette(1);
  gStyle->SetLabelSize(0.39,"y");
  gStyle->SetLabelSize(0.09,"x");
  gStyle->SetLabelSize(0.06,"xyz");
  gStyle->SetLabelSize(0.06,"y");
  gStyle->SetLabelSize(0.08,"x");
  gStyle->SetLabelOffset(0.01,"x");
  gStyle->SetLabelOffset(0.01,"y");

  gStyle->SetNdivisions(705,"xy");

  gStyle->SetOptTitle(0);
  gStyle->SetTitleX(0.1); gStyle->SetTitleY(1.);
  gStyle->SetTitleW(0.8); gStyle->SetTitleH(0.086);
  //gStyle->SetTitleSize(0.06);
 
  gStyle->SetOptStat(0);
  gStyle->SetStatX(0.95); gStyle->SetStatY(0.92);
  gStyle->SetStatW(0.42); gStyle->SetStatH(0.20);

 //gStyle->SetStatFontSize(0.14);
  //gStyle->SetOptStat(1);
  char tmpchr[200];
  cc->cd(); cc->SetFillColor(0);
  cc->Clear();
  cc->Divide(5,6,0.00005,0.00005);
  //cc->Divide(5,6);


  for(int i=0;i<mNtray;i++){
    cc->cd(i+1);
    gPad->SetGridx(0);
    gPad->SetGridy(0);
    gPad->SetLogy(1);

    TOF_L0_trg[i]->GetYaxis()->SetLabelSize(0.06);
    TOF_L0_trg[i]->GetXaxis()->SetLabelSize(0.07);

    TOF_L0_trg[i]->SetXTitle("L0 value");
    //TOF_L0_trg[i]->SetYTitle("Counts");

    int fillcolor=0;
   
    if(TOF_L0_trg[i]->Integral(26,32) > 0) fillcolor=2;     // any L0 >= 25 (bin=26)?
    TOF_L0_trg[i]->SetFillColor(fillcolor);
    TOF_L0_trg[i]->SetLineColor(4);
    TOF_L0_trg[i]->Draw();
    //TOF_L0_hit[i]->SetLineColor(4);
    //TOF_L0_hit[i]->Draw("same");

    label.SetTextSize(0.11);

    label.SetTextColor(45);
    float hmax=TOF_L0_trg[i]->GetMaximum();
    if(hmax<1) hmax=1;
    sprintf(tmpchr,"%d",actualTrayNum[i]);
    label.DrawLatex(  3., 0.95*hmax, tmpchr);

    if(NotActiveTray[actualTrayNum[i]]) { 
      sprintf(tmpchr,"Not Active");
      label.SetTextColor(2);
      label.SetTextSize(0.09);
      label.DrawLatex(15., 0.72*hmax, tmpchr);
    } 
    if(MaskoutTray[actualTrayNum[i]]) { 
      sprintf(tmpchr,"Mask out");
      label.SetTextColor(4);
      label.SetTextSize(0.1);
      label.DrawLatex(15, 0.12*hmax, tmpchr);
    }

  }

  cc->Update();

} 


bool TOFL0HistogramGroup::fill(evpReader* evp, char* datap) { 
  
  int ret=tofReader(datap);
  if(ret <= 0)   {
    fprintf(stderr,"TOF: problems in data (%d) - continuing...",ret);
    return false;
  }
  // 


  int lowtraynum= actualTrayNum[0];
  int hightraynum= actualTrayNum[MAXTRAYS-1];

  StTriggerData* trgd = TriggerData::Instance(datap);  
  if(!trgd) return -9;

  int npre1=trgd->numberOfPreXing();
  int npost1=trgd->numberOfPostXing();

  for( int ipost=-npre1; ipost<npost1+1; ipost++) {
    int prepost=ipost;
    if(prepost != 0) continue;    // only look at prepost =0 data.
    for(int itray=lowtraynum;itray<hightraynum+1;itray++){
      int atrayid=itray;
      int trigger_mult=trgd->tofTrayMultiplicity(atrayid,prepost);
      if(trigger_mult > 31) trigger_mult=31;
      int histogramnum = (atrayid-1)%MAXTRAYS;
      TOF_L0_trg[histogramnum]->Fill(trigger_mult);
    }
  }
  return true;

}

void TOFL0HistogramGroup::ReadTrayList(){
  
  //cout<<"TOFL0HistogramGroup::TrayList config file:"<<mTrayList<<endl;

  TString buffer;

  ifstream filein(mTrayList);
  for(int i=0;i<128;i++){NotActiveTray[i]=false;}
  if(filein){ 
    while(!filein.eof()) {
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      int trayid = atoi(buffer.Data());
      if(trayid<1 || trayid>127) continue;
      NotActiveTray[trayid]=true;
    }   
  } else {cout<<"TOFL0HistogramGroup::Can not open file:"<<mTrayList<<endl;}
  filein.close();

}

void TOFL0HistogramGroup::ReadTraymaskoutList(){
  
  TString buffer;

  ifstream filein(mTrayList);
  for(int i=0;i<128;i++){MaskoutTray[i]=false;}
  if(filein){ 
    while(!filein.eof()) {
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      int trayid = atoi(buffer.Data());
      if(trayid<1 || trayid>127) continue;
      MaskoutTray[trayid]=true;
    }   
  } else {cout<<"TOFL0HistogramGroup::Can not open file:"<<mTrayList<<endl;}
  filein.close();

}
