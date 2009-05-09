#include "MTDtriggerinfoHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"
#include "TStyle.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
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

ClassImp(MTDtriggerinfoHistogramGroup) ;

MTDtriggerinfoHistogramGroup::MTDtriggerinfoHistogramGroup() {
  // For ROOT I/O
  memset( MTD_adc, 0, sizeof(MTD_adc));
  memset( MTD_tac, 0, sizeof(MTD_tac));
  MTD_eastTac_vs_westTac = 0;
  MTD_aveTac_vs_vpd_aveTac = 0;
}

MTDtriggerinfoHistogramGroup::MTDtriggerinfoHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  char tmpchr[200];
  sprintf(tmpchr,"MTD_east_adc");
  MTD_adc[0]=new TH1F(tmpchr,"MTD ADC (east)",100,0,2048);
  MTD_adc[0]->SetXTitle("ADC");
  sprintf(tmpchr,"MTD_west_adc");
  MTD_adc[1]=new TH1F(tmpchr,"MTD ADC (west)",100,0,2048);
  MTD_adc[1]->SetXTitle("ADC");

  sprintf(tmpchr,"MTD_east_tac");
  MTD_tac[0]=new TH1F(tmpchr,"MTD TAC (east)",150,0,3000);
  MTD_tac[0]->SetXTitle("TAC");
  sprintf(tmpchr,"MTD_west_tac");
  MTD_tac[1]=new TH1F(tmpchr,"MTD TAC (west)",150,0,3000);
  MTD_tac[1]->SetXTitle("TAC");

  sprintf(tmpchr,"MTD_eastTac_vs_westTac");
  MTD_eastTac_vs_westTac=new TH2F(tmpchr,"MTD eastTac vs westTac",150,0,3000,150,0,3000);
  MTD_eastTac_vs_westTac->SetXTitle("east TAC");
  //MTD_eastTac_vs_westTac->SetYTitle("west TAC");

  sprintf(tmpchr,"MTD_aveTac_vs_vpd_aveTac");
  MTD_aveTac_vs_vpd_aveTac=new TH2F(tmpchr,"MTD aveTac vs vpd aveTac",150,0,3000,100,500,2048);
  MTD_aveTac_vs_vpd_aveTac->SetXTitle("MTD aveTAC");

}


MTDtriggerinfoHistogramGroup::~MTDtriggerinfoHistogramGroup() {

  for (int i = 0; i < 2; ++i)delete MTD_adc[i];
  for (int i = 0; i < 2; ++i)delete MTD_tac[i];
  delete MTD_eastTac_vs_westTac;
  delete MTD_aveTac_vs_vpd_aveTac;
}


void MTDtriggerinfoHistogramGroup::reset() {

  for (int i = 0; i < 2; ++i)MTD_adc[i]->Reset();
  for (int i = 0; i < 2; ++i)MTD_tac[i]->Reset();
  MTD_eastTac_vs_westTac->Reset();
  MTD_aveTac_vs_vpd_aveTac->Reset();
}


void MTDtriggerinfoHistogramGroup::draw(TCanvas* cc) {

  TLatex label;
  label.SetTextAlign(23);  // center, top
  label.SetTextSize(0.055);
  label.SetTextColor(45);
  TLatex labely;
  //labely.SetTextAlign(23);  // center, top
  labely.SetTextSize(0.04);
  labely.SetTextColor(1);
  labely.SetTextAngle(90);

  TLine  line;
  line.SetLineColor(4);
  line.SetLineWidth(1);
  //
  gROOT->SetStyle("Plain");
  gStyle->SetPaperSize(TStyle::kUSLetter);

  gStyle->SetPalette(1);
  gStyle->SetLabelSize(0.09,"y");
  gStyle->SetLabelSize(0.09,"x");
  gStyle->SetLabelSize(0.06,"xyz");
  gStyle->SetLabelSize(0.06,"y");
  gStyle->SetLabelSize(0.08,"x");
  gStyle->SetLabelOffset(0.01,"x");
  gStyle->SetLabelOffset(0.01,"y");

  gStyle->SetOptTitle(11);
  gStyle->SetTitleX(0.1); gStyle->SetTitleY(1.);
  gStyle->SetTitleW(0.8); gStyle->SetTitleH(0.086);
  //gStyle->SetTitleSize(0.06);
 
  gStyle->SetOptStat(110110);
  gStyle->SetStatX(0.99); gStyle->SetStatY(0.91);
  gStyle->SetStatW(0.20); gStyle->SetStatH(0.16);

  gStyle->SetNdivisions(505,"xyz");


  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);

  cc->cd(); cc->SetFillColor(0);
  cc->Clear();
  cc->Divide(2, 3);
  cc->cd(1);

  MTD_adc[0]->SetFillColor(19);
  MTD_adc[0]->GetYaxis()->SetLabelSize(0.07);
  MTD_adc[0]->GetXaxis()->SetLabelSize(0.055);
  if(MTD_adc[0]->GetEntries()>0)gPad->SetLogy(1);
  MTD_adc[0]->Draw();
  float hmax=MTD_adc[0]->GetMaximum();
  labely.DrawLatex(-200, 0.6*hmax, "Counts");
  
  cc->cd(2);
  MTD_adc[1]->SetFillColor(19);
  MTD_adc[1]->GetYaxis()->SetLabelSize(0.07);
  MTD_adc[1]->GetXaxis()->SetLabelSize(0.055);
  if(MTD_adc[1]->GetEntries()>0)gPad->SetLogy(1);
  MTD_adc[1]->Draw();
  hmax=MTD_adc[1]->GetMaximum();
  labely.DrawLatex(-200, 0.6*hmax, "Counts");

  cc->cd(3);
  MTD_tac[0]->SetFillColor(19);
  MTD_tac[0]->GetYaxis()->SetLabelSize(0.07);
  MTD_tac[0]->GetXaxis()->SetLabelSize(0.055);
  MTD_tac[0]->Draw();
  hmax=MTD_tac[0]->GetMaximum();
  labely.DrawLatex(-300, 0.6*hmax, "Counts");

  cc->cd(4);
  MTD_tac[1]->SetFillColor(19);
  MTD_tac[1]->GetYaxis()->SetLabelSize(0.07);
  MTD_tac[1]->GetXaxis()->SetLabelSize(0.055);
  MTD_tac[1]->Draw();
  hmax=MTD_tac[1]->GetMaximum();
  labely.DrawLatex(-300, 0.6*hmax, "Counts");

  cc->cd(5);
  MTD_eastTac_vs_westTac->GetYaxis()->SetLabelSize(0.07);
  MTD_eastTac_vs_westTac->GetXaxis()->SetLabelSize(0.055);
  MTD_eastTac_vs_westTac->Draw("col");
  labely.DrawLatex(-300, 0.6*2048, "west TAC");

  cc->cd(6);
  MTD_aveTac_vs_vpd_aveTac->GetYaxis()->SetLabelSize(0.07);
  MTD_aveTac_vs_vpd_aveTac->GetXaxis()->SetLabelSize(0.055);
  MTD_aveTac_vs_vpd_aveTac->Draw("col");
  labely.DrawLatex(-300, 600+0.6*1500, "VPD aveTac");

  cc->Update();

} 


bool MTDtriggerinfoHistogramGroup::fill(evpReader* evp, char* datap) { 
  
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd) return false;  

  float east_adc=trgd->mtdAdc(east,0);
  float west_adc=trgd->mtdAdc(west,0);
  float east_tac=trgd->mtdTdc(east,0);
  float west_tac=trgd->mtdTdc(west,0);

  if(east_adc>0)MTD_adc[0]->Fill(east_adc);
  if(west_adc>0)MTD_adc[1]->Fill(west_adc);
  if(east_tac>0)MTD_tac[0]->Fill(east_tac);
  if(west_tac>0)MTD_tac[1]->Fill(west_tac);

  if((east_tac+west_tac)>0) MTD_eastTac_vs_westTac->Fill(east_tac,west_tac);

  int vpd_maxTacEast = trgd->vpdEarliestTDC((StBeamDirection)0);
  int vpd_maxTacWest = trgd->vpdEarliestTDC((StBeamDirection)1);

  //cout<<" eastadc="<<east_adc<<" westadc="<<west_adc<<" easttac="<<east_tac<<" westtac="<<west_tac<<" vpdeast="<<vpd_maxTacEast<<" vpdwest="<<vpd_maxTacWest<<endl;
  if((east_tac+west_tac)>0)MTD_aveTac_vs_vpd_aveTac->Fill( (east_tac+west_tac)/2.,(vpd_maxTacEast+vpd_maxTacWest)/2.);

  return true;

}
