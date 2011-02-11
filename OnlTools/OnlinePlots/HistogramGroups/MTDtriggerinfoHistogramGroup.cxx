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
}

MTDtriggerinfoHistogramGroup::MTDtriggerinfoHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  char tmpchr[200];
	  for (int itray=0; itray<=2; itray++){
  sprintf(tmpchr,"MTD%d_east_adc",itray);
  MTD_adc[itray][0]=new TH1F(tmpchr,tmpchr,100,0,2048);
  MTD_adc[itray][0]->SetXTitle("ADC");
  sprintf(tmpchr,"MTD%d_west_adc",itray);
  MTD_adc[itray][1]=new TH1F(tmpchr,tmpchr,100,0,2048);
  MTD_adc[itray][1]->SetXTitle("ADC");

  sprintf(tmpchr,"MTD%d_east_tac",itray);
  MTD_tac[itray][0]=new TH1F(tmpchr,tmpchr,150,0,3000);
  MTD_tac[itray][0]->SetXTitle("TAC");
  sprintf(tmpchr,"MTD%d_west_tac",itray);
  MTD_tac[itray][1]=new TH1F(tmpchr,tmpchr,150,0,3000);
  MTD_tac[itray][1]->SetXTitle("TAC");
	  }//tray loop over
	  

}


MTDtriggerinfoHistogramGroup::~MTDtriggerinfoHistogramGroup() {

	for (int itray=0;itray<=2;itray++){
  for (int i = 0; i < 2; ++i)delete MTD_adc[itray][i];
  for (int i = 0; i < 2; ++i)delete MTD_tac[itray][i];
	}
}


void MTDtriggerinfoHistogramGroup::reset() {
	for (int itray=0;itray<=2;itray++){
  for (int i = 0; i < 2; ++i)MTD_adc[itray][i]->Reset();
  for (int i = 0; i < 2; ++i)MTD_tac[itray][i]->Reset();
	}
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
  cc->Divide(2, 6);
	for(int itray=0;itray<=2;itray++){
  cc->cd(itray*4+1);

  MTD_adc[itray][0]->SetFillColor(19);
  MTD_adc[itray][0]->GetYaxis()->SetLabelSize(0.07);
  MTD_adc[itray][0]->GetXaxis()->SetLabelSize(0.055);
  gPad->SetLogy(MTD_adc[itray][0]->GetEntries() ? 1 : 0);
  MTD_adc[itray][0]->Draw();
  float hmax=MTD_adc[itray][0]->GetMaximum();
  labely.DrawLatex(-200, 0.6*hmax, "Counts");
  
  cc->cd(itray*4+2);
  MTD_adc[itray][1]->SetFillColor(19);
  MTD_adc[itray][1]->GetYaxis()->SetLabelSize(0.07);
  MTD_adc[itray][1]->GetXaxis()->SetLabelSize(0.055);
  gPad->SetLogy(MTD_adc[itray][1]->GetEntries() ? 1 : 0);
  MTD_adc[itray][1]->Draw();
  hmax=MTD_adc[itray][1]->GetMaximum();
  labely.DrawLatex(-200, 0.6*hmax, "Counts");

  cc->cd(itray*4+3);
  MTD_tac[itray][0]->SetFillColor(19);
  MTD_tac[itray][0]->GetYaxis()->SetLabelSize(0.07);
  MTD_tac[itray][0]->GetXaxis()->SetLabelSize(0.055);
  MTD_tac[itray][0]->Draw();
  hmax=MTD_tac[itray][0]->GetMaximum();
  labely.DrawLatex(-300, 0.6*hmax, "Counts");

  cc->cd(itray*4+4);
  MTD_tac[itray][1]->SetFillColor(19);
  MTD_tac[itray][1]->GetYaxis()->SetLabelSize(0.07);
  MTD_tac[itray][1]->GetXaxis()->SetLabelSize(0.055);
  MTD_tac[itray][1]->Draw();
  hmax=MTD_tac[itray][1]->GetMaximum();
  labely.DrawLatex(-300, 0.6*hmax, "Counts");
	}


  cc->Update();

} 


bool MTDtriggerinfoHistogramGroup::fill(evpReader* evp, char* datap) { 
  
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd){ 
	  cout<<"NO TRIGGER DATA"<<endl;
	  return false;  
  }

  //MTD for run 11
  //0 for MTD-1; 1 for MTD-26, the center tray; 2 for MTD-26, the left and right trays. 
	float west_adc[3];
	float west_tac[3];
	float east_adc[3];
	float east_tac[3];
	for(int itray=0;itray<=2;itray++){ 
		west_adc[itray]=-9999.;
		west_tac[itray]=-9999.;
		east_adc[itray]=-9999.;
		east_tac[itray]=-9999.;
	}
	
	 west_adc[0]= trgd->mtdAtAddress(8, 0);
   west_tac[0]= trgd->mtdAtAddress(12, 0);
   east_adc[0]= trgd->mtdAtAddress(9, 0);
   east_tac[0]= trgd->mtdAtAddress(13, 0);

   west_adc[1]= trgd->mtdAtAddress(16, 0);
   west_tac[1]= trgd->mtdAtAddress(20, 0);
   east_adc[1]= trgd->mtdAtAddress(17, 0);
   east_tac[1]= trgd->mtdAtAddress(21, 0);

   west_adc[2]= trgd->mtdAtAddress(24, 0);
   west_tac[2]= trgd->mtdAtAddress(28, 0);
   east_adc[2]= trgd->mtdAtAddress(25, 0);
   east_tac[2]= trgd->mtdAtAddress(29, 0);

for (int itray=0;itray<=2; itray++){
  if(east_adc[itray]>0)MTD_adc[itray][0]->Fill(east_adc[itray]);
  if(west_adc[itray]>0)MTD_adc[itray][1]->Fill(west_adc[itray]);
  if(east_tac[itray]>0)MTD_tac[itray][0]->Fill(east_tac[itray]);
  if(west_tac[itray]>0)MTD_tac[itray][1]->Fill(west_tac[itray]);

	}

 // for (int itray=0;itray<=2; itray++){cout <<"itray="<<itray<<endl;cout<<"eastadc="<<east_adc[itray]<<" westadc[itray]="<<west_adc[itray]<<" easttac="<<east_tac[itray]<<" westtac0="<<west_tac[itray]<<endl;}

	//if (west_adc[1]>0) cout<<"west_adc[1]="<<west_adc[1]<<"!!!!!!!!!!!<"endl;

  return true;

}
