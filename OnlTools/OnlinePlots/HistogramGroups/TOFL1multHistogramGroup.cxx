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
  TOF_L1mult_vs_sumL0=0;

}

TOFL1multHistogramGroup::TOFL1multHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  //
  TOF_L1mult_vs_ZDCadcsum=new TH2F("TOF_L1_vs_ZDCadcsum","TOF_L1_vs_ZDCadcsum",144,0.5,2880.5,150,0,3000);
  TOF_L1mult_vs_ZDCadcsum->SetXTitle("TOF L1 Mult");
  TOF_L1mult_vs_ZDCadcsum->SetYTitle("ZDC hardware adc Sum");

  TOF_L1mult_vs_sumL0=new TH2F("TOF_L1_vs_sumL0","TOF_L1_vs_sumL0",144,0.5,2880.5,144,0.5,2880.5);
  TOF_L1mult_vs_sumL0->SetXTitle("TOF L1 Mult");
  TOF_L1mult_vs_sumL0->SetYTitle("Sum L0 of hits");
 

}


TOFL1multHistogramGroup::~TOFL1multHistogramGroup() {

  //
  delete TOF_L1mult_vs_ZDCadcsum;
  delete TOF_L1mult_vs_sumL0;
}

void TOFL1multHistogramGroup::reset() {

  TOF_L1mult_vs_ZDCadcsum->Reset();
  TOF_L1mult_vs_sumL0->Reset();
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
  cc->Divide(1, 2);

  gPad->SetGridx(1);
  gPad->SetGridy(0);
  cc->cd(1);
  TOF_L1mult_vs_ZDCadcsum->Draw("colz");

  cc->cd(2);
  TOF_L1mult_vs_sumL0->Draw("colz");
  
  cc->Update();

} 


bool TOFL1multHistogramGroup::fill(evpReader* evp, char* datap) { 
  
  int ret=tofReader(datap);
  if(ret <= 0)   {
    fprintf(stderr,"TOF: problems in data (%d) - continuing...",ret);
    return false;
  }

  // get mult. from hits.
  int halftrayid=-1;
  int trayid=-1;
  int bunchid=0;
  int tinohit[120][24];
  for(int itray=0;itray<120;itray++)for(int i=0;i<24;i++)tinohit[itray][i]=0;

  // TOF trigger window per THUB, subject to change anytime.
  //int trigwindowLow[4]={2830,2840,2910,2910};
  //int trigwindowHigh[4]={2910,2920,2990,2990};
  //trigger window per tray AuAu200
  int trigwindowLowpertray[120]={
  2809,2809,2806,2808,2809, 2809,2809,2809,2809,2810, 2809,2809,2809,2809,2809,
  2809,2808,2809,2809,2808, 2809,2809,2809,2809,2806, 2809,2818,2818,2818,2818,
  2818,2824,2826,2822,2828, 2826,2827,2825,2827,2819, 2821,2821,2820,2818,2809,
  2809,2809,2809,2809,2809, 2784,2784,2784,2795,2796, 2797,2797,2797,2809,2809,
  2884,2884,2871,2873,2872, 2883,2877,2884,2878,2883, 2884,2884,2884,2884,2885,
  2884,2892,2894,2895,2893, 2894,2893,2892,2894,2884, 2884,2883,2883,2884,2875,
  2876,2877,2875,2876,2878, 2884,2884,2890,2884,2897, 2897,2897,2897,2896,2897,
  2896,2896,2894,2896,2897, 2896,2884,2891,2884,2884, 2884,2885,2883,2884,2884
  };
  int trigwindowHighpertray[120];
  for(int iii=0;iii<120;iii++)
    trigwindowHighpertray[iii]=trigwindowLowpertray[iii]+80;

  for(int ifib=0;ifib<4;ifib++){
    int ndataword = tof.ddl_words[ifib];    // 
    if(ndataword<=0) continue;
    for(int iword=0;iword<ndataword;iword++){
      int dataword=tof.ddl[ifib][iword];
      if(ndataword<=0) continue;
      if( (dataword&0xF0000000)>>28 == 0xD) continue;  
      if( (dataword&0xF0000000)>>28 == 0xE) continue;  
      if( (dataword&0xF0000000)>>28 == 0xA) {  // header trigger data flag
        // do nothing at this moment.
        continue;
      }
      // geographical data words for tray number.
      if( (dataword&0xF0000000)>>28 == 0xC) {
         halftrayid = dataword&0x01;    
         trayid     = (dataword&0x0FE)>>1;
         continue;
      }

      if(trayid < 1 || trayid > 120) continue;  
      if( (dataword&0xF0000000)>>28 == 0x6) {continue;}
      if( (dataword&0xF0000000)>>28 == 0x2) {
        bunchid=dataword&0xFFF;
        continue;  
      }
      //
      int edgeid =int( (dataword & 0xf0000000)>>28 );
      if(edgeid !=4) continue;    // leading edge data is enough

      int timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
      int time = timeinbin * 25./1024;   // time in ns 

      float trgTime = 25.*bunchid;
      float timeDiff = time- trgTime;
      while(timeDiff<0) timeDiff += 51200;

//      int thub=Get_TOFTHUB(trayid);
//      if(timeDiff<trigwindowLow[thub] || timeDiff>trigwindowHigh[thub]) continue;   // trigger window cut
      if(timeDiff<trigwindowLowpertray[trayid-1] || timeDiff>trigwindowHighpertray[trayid-1]) continue;   // trigger window per tray

      int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
      int tdigboardid=tdcid/4;   // 0-3 for half tray.
      int  tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
      int globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray
      //
      int atdig = globaltdcchan/24;   // 0,1,....7
      int atdcid  = globaltdcchan/8;    // 0,1,....24    
      int ahptdcid = atdcid%3;
      int atdcchan = globaltdcchan%8;
      int tinoid=TDIGChan2TINOChan(ahptdcid,atdcchan);
      int tinoidx = atdig*3 + tinoid;
      //
      tinohit[trayid-1][tinoidx]++; 
    }  // end loop nword
  }  // end loop fiber

  //
  int sum_L0_hit=0;
  for(int itray=0;itray<120;itray++){
    //if(Tray_NotInRun(itray+1)) continue;
    // mult. from hits
    int hit_mult=0;
    for(int i=0;i<24;i++){if(tinohit[itray][i]>0) hit_mult++;}
    sum_L0_hit = hit_mult+sum_L0_hit;
  }



  // information from trigger .
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd) return false;  
  float ZDCadcsum= float(trgd->zdcHardwareSum());
  float TOF_L1mult=float(trgd->tofMultiplicity(0));
  TOF_L1mult_vs_ZDCadcsum->Fill(TOF_L1mult,ZDCadcsum);
  TOF_L1mult_vs_sumL0->Fill(TOF_L1mult,sum_L0_hit);

  return true;

}

int TOFL1multHistogramGroup::Get_TOFTHUB(int trayid){

  int trayinTHUB1[30]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,51,52,53,54,55,56,57,58,59,60};
  int trayinTHUB2[30]={21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
  int trayinTHUB3[30]={61,62,63,64,65,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120};
  int trayinTHUB4[30]={66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95};

  if(trayid<1 || trayid>120) return -1;

  int nthub=-1;

  for(int itray=0;itray<30;itray++){
    if(trayid == trayinTHUB1[itray]) nthub=0;
    if(trayid == trayinTHUB2[itray]) nthub=1;
    if(trayid == trayinTHUB3[itray]) nthub=2;
    if(trayid == trayinTHUB4[itray]) nthub=3;
  }

  return nthub;  

}
int TOFL1multHistogramGroup::TDIGChan2TINOChan(int tdc,int chan)
{
  int tdcid[24]  ={0,1,0,1,0,1,2,0,2,0,1,0,1,2,0,2,0,2,2,1,2,1,2,1};
  int tdcchan[24]={7,7,0,2,5,6,7,4,4,2,3,6,0,2,3,3,1,6,0,5,1,4,5,1};
  int tinoid[24] ={2,2,2,2,2,2,1,1,1,1,2,2,0,0,1,1,1,1,0,0,0,0,0,0};
  int index=-1;
  for(int i=0;i<24;i++){
    if(tdcid[i]==tdc && tdcchan[i]==chan) {index=i;break;}
  }
  if(index<0) cout<<" tdc="<<tdc<<" chan="<<chan<<" not valid!!!"<<endl;
  return tinoid[index];
}
