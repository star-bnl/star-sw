#include "TOFcheckHistogramGroup.h"

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

ClassImp(TOFcheckHistogramGroup) ;

TOFcheckHistogramGroup::TOFcheckHistogramGroup() {
  // For ROOT I/O
  TOF_Error1=0;
  TOF_Error2=0;
  TOF_Tray_hits1=0;
  TOF_Tray_hits2=0;
}

TOFcheckHistogramGroup::TOFcheckHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  mNevents=0;
  TOF_Error1=new TH1F("TOF_Error1","TOF packetID error ",122,0.5,122.5);
  TOF_Error2=new TH1F("TOF_Error2","TOF bunchid shift error",12,0,6.);
  TOF_Tray_hits1=new TH1F("TOF_Tray_hits1","Hits in Trays",240,-0.5,119.5);
  TOF_Tray_hits2=new TH1F("TOF_Tray_hits2","Hits in Trays",240,-0.5,119.5);

  TOF_Error1->SetXTitle("Tray #");
  TOF_Error2->SetXTitle("THUB+Tray121 122 #");
 
}


TOFcheckHistogramGroup::~TOFcheckHistogramGroup() {

  delete TOF_Error1;
  delete TOF_Error2;
  delete TOF_Tray_hits1;
  delete TOF_Tray_hits2;

}

void TOFcheckHistogramGroup::reset() {

  TOF_Error1->Reset();
  TOF_Error2->Reset();
  TOF_Tray_hits1->Reset();
  TOF_Tray_hits2->Reset();
}


void TOFcheckHistogramGroup::draw(TCanvas* cc) {

  TLatex label;
  //label.SetTextAlign(23);  // center, top
  label.SetTextSize(0.10);
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
 
  gStyle->SetOptStat(0);
  gStyle->SetStatX(0.95); gStyle->SetStatY(0.92);
  gStyle->SetStatW(0.42); gStyle->SetStatH(0.20);

  gStyle->SetPadGridX(0);
  gStyle->SetPadGridX(1);
  //gStyle->SetStatFontSize(0.14);
  //gStyle->SetOptStat(1);
  char tmpchr[200];
  cc->cd(); cc->SetFillColor(0);
  cc->Clear();
  cc->Divide(1, 2);
  cc->cd(1);

  TOF_Error1->GetYaxis()->SetLabelSize(0.08);
  TOF_Error1->GetXaxis()->SetLabelSize(0.06);
  TOF_Error1->SetFillColor(45);
  TOF_Error1->Draw();
  double entry = TOF_Error1->GetEntries();
  if(entry>0) {
     float hmax=TOF_Error1->GetMaximum();
     label.SetTextColor(2);
     label.DrawLatex(30, 0.8*hmax, "Electronics Error Found!!");
  } else {
    float hmax=0.9 * gPad->GetUymax();
    label.SetTextColor(3);
    sprintf(tmpchr,"No electronics Error in %d events!",mNevents);
    label.DrawLatex( 30, hmax, tmpchr);
  }

  cc->cd(2);
  TOF_Error2->GetYaxis()->SetLabelSize(0.08);
  TOF_Error2->GetXaxis()->SetLabelSize(0.06);
  TOF_Error2->SetFillColor(45);
  TOF_Error2->Draw();
  entry = TOF_Error2->GetEntries();
  if(entry>0) {
     float hmax=TOF_Error2->GetMaximum();
     label.SetTextColor(2);
     label.DrawLatex( 1.5, 0.8*hmax, "Invalid Bunchid Shift Found!!");
  } else {
    float hmax=0.9 * gPad->GetUymax();
    label.SetTextColor(3);
    sprintf(tmpchr,"No invalid bunchid shift in %d events!",mNevents);
    label.DrawLatex( 1.5, hmax, tmpchr);
  }

  /*
  cc->cd(3);
  TOF_Tray_hits1->SetFillColor(2);
  TOF_Tray_hits2->SetFillColor(4);
  TOF_Tray_hits1->Draw();
  TOF_Tray_hits2->Draw("same");
  */
  cc->Update();

} 


bool TOFcheckHistogramGroup::fill(evpReader* evp, char* datap) { 
  
  int ret=tofReader(datap);
  if(ret <= 0)   {
    fprintf(stderr,"TOF: problems in data (%d) - continuing...",ret);
    return false;
  }

  mNevents++;

  // 
  int halftrayid=-1;
  int trayid=-1;
  int allbunchid[2][122];
  for(int i=0;i<2;i++)for(int j=0;j<122;j++)allbunchid[i][j]=0;

  for(int ifib=0;ifib<4;ifib++){
    int ndataword = tof.ddl_words[ifib];    // 
    if(ndataword<=0) continue;
    int thebunchid =0; 
    for(int iword=0;iword<ndataword;iword++){
      int dataword=tof.ddl[ifib][iword];
      //cout<<"TOF:: ifib="<<ifib<<" dataword=0x"<<hex<<dataword<<dec<<endl;
      int packetid = (dataword&0xF0000000)>>28 ;
      if(packetid == 0xD) continue;  
      if(packetid == 0xE) continue;  
      if(packetid == 0xA) {  // header trigger data flag
	// do nothing at this moment.
	continue;
      }
      //
      // geographical data words for tray number.
      if(packetid== 0xC) {
         halftrayid = dataword&0x01;    
         trayid     = (dataword&0x0FE)>>1;
         continue;
      }
      if(!ValidDataword(packetid)) TOF_Error1->Fill(trayid);
      // bunch id 
      if(packetid == 0x2) {
        thebunchid = dataword&0xFFF;
        //cout<<"tray="<<trayid<<" halftray="<<halftrayid<<" bunchid="<<thebunchid<<endl;
        allbunchid[halftrayid][trayid-1]=thebunchid;         
       continue;  
      }

      if(trayid <1 || trayid >122) continue;


      if( (dataword&0xF0000000)>>28 == 0x6) {continue;}
      //
      int edgeid =packetid;
      if((edgeid !=4) && (edgeid!=5)) continue;
      if(trayid<121) {
        if(halftrayid==0) TOF_Tray_hits1->Fill(trayid-1);
        if(halftrayid==1) TOF_Tray_hits2->Fill(trayid-0.5);
      }

    }  // end loop nword
  }  // end loop fiber

  // check bunch id shift
  int bunchidref1 =   allbunchid[0][0];   // bunchid from tray 1 as reference.
  int bunchidref2 =   allbunchid[1][0];   // bunchid from tray 1 as reference.
  if(bunchidref1 != bunchidref2) {TOF_Error2->Fill(0);}

  for(int itray=0;itray<122;itray++){
    int traynum=itray+1;
    if(Tray_NotInRun(traynum)) continue;
    for(int ihalf=0;ihalf<2;ihalf++){
      int bunchid=allbunchid[ihalf][itray];
      int ret=ValidBunchid(traynum,ihalf,bunchid,bunchidref1);
      if(ret>=0) TOF_Error2->Fill(ret+0.5*ihalf);
    }
  }

  return true;

}

bool TOFcheckHistogramGroup::ValidDataword(int packetid)
{
  if(packetid == 0x2) return true;
  if(packetid == 0xD) return true;
  if(packetid == 0xE) return true;
  if(packetid == 0xA) return true;
  if(packetid == 0xC) return true;
  if(packetid == 0x4) return true;
  if(packetid == 0x5) return true;

  return false;

}
bool TOFcheckHistogramGroup::Tray_NotInRun(int trayid)
{
  // the following 34 trays is not in run for run9!!
  int notinrunlist[34]={13,14,42,43,73,74,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,
102,103,38,39,52,75,47,68,112,118};

  for(int i=0;i<34;i++) {
    if(trayid == notinrunlist[i]) {return true;}
  }

  return false;

}
int TOFcheckHistogramGroup::ValidBunchid(int trayid,int halftrayid,int bunchid,int refbunchid)
{
  if(trayid<1 || trayid>122) return -1;

  int trayinTHUB1[30]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,51,52,53,54,55,56,57,58,59,60};
  int trayinTHUB2[30]={21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
  int trayinTHUB3[30]={61,62,63,64,65,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120};
  int trayinTHUB4[30]={66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95};
  int trayvalidshift[2][4]={{0,4,-7,-5},{0,5,-6,-4}};
  //=
  //tray 121 half0: 4 5    half1: 5 6
  //tray 122 half0: -5 -4  half1: -4 -3
  int tray121shift[2][2]={{ 4, 5},{ 5, 6}};
  int tray122shift[2][2]={{-5,-4},{-4,-3}};

  int nthub=-1;
  int ret=-1;
  for(int itray=0;itray<30;itray++){
    if(trayid == trayinTHUB1[itray]) nthub=0;
    if(trayid == trayinTHUB2[itray]) nthub=1;
    if(trayid == trayinTHUB3[itray]) nthub=2;
    if(trayid == trayinTHUB4[itray]) nthub=3;
  }
  if(trayid == 121) nthub =4;
  if(trayid == 122) nthub =5;
  int diff=bunchid-refbunchid;
  if(diff>2048)   {diff =diff-4096;} 
  else if(diff<-2048) {diff =diff+4096;}

  //cout<<"tray="<<trayid<<" halftrayid="<<halftrayid<<" bunchid="<<bunchid<<" refbunchid="<<refbunchid<<" diff="<<diff<<" nthub="<<nthub<<endl;
  if(trayid>1 && trayid<121){
    if( (diff != trayvalidshift[0][nthub])  && (diff != trayvalidshift[1][nthub]) ) ret=nthub;
  } else if(trayid==121){
    if(diff !=tray121shift[halftrayid][0] && diff != tray121shift[halftrayid][1]) ret=nthub;
  } else if(trayid==122){
    if(diff !=tray122shift[halftrayid][0] && diff != tray122shift[halftrayid][1]) ret=nthub;
  }

  //if(ret>=0)cout<<"ERROR!! tray="<<trayid<<" halftrayid="<<halftrayid<<" bunchid="<<bunchid<<" refbunchid="<<refbunchid<<" diff="<<diff<<" nthub="<<ret<<endl;

  return ret;
}
