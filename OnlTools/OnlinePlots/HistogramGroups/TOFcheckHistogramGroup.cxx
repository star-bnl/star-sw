#include "TOFcheckHistogramGroup.h"

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

char*  TOFcheckHistogramGroup::mTrayList = EvpUtil::cat(gEnv->GetValue("Online.tofConfigPath","."),"TOF_TrayNotInRun.txt");
char*  TOFcheckHistogramGroup::mBunchShiftList = EvpUtil::cat(gEnv->GetValue("Online.tofConfigPath","."),"TOF_ValidBunchidPhase.txt");

ClassImp(TOFcheckHistogramGroup) ;

TOFcheckHistogramGroup::TOFcheckHistogramGroup() {
  // For ROOT I/O
  TOF_Error1=0;
  TOF_Error2=0;
  TOF_Error3=0;
  TOF_EventCount=0;
  TOF_Tray_hits1=0;
  TOF_Tray_hits2=0;
}

TOFcheckHistogramGroup::TOFcheckHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  TOF_Error1=new TH1F("TOF_Error1","TOF electronics errors",244,0.5,122.5);
  TOF_Error2=new TH1F("TOF_Error2","TOF incorrect bunchid errors",122,0.5,122.5);
  TOF_Error3=new TH1F("TOF_Error3","TOF trays not read out",122,0.5,122.5); //defined as if bunchid not found

  TOF_EventCount=new TH1F("TOF_EventCount","TOF_EventCount",2,0,2);

  TOF_Tray_hits1=new TH1F("TOF_Tray_hits1","TOF Hits by TrayHalf",244,0.5,122.5);
  TOF_Tray_hits2=new TH1F("TOF_Tray_hits2","TOF Hits by TrayHalf",244,0.5,122.5);

  ReadTrayList(); 
  ReadValidBunchidPhase();
}


TOFcheckHistogramGroup::~TOFcheckHistogramGroup() {

  delete TOF_Error1;
  delete TOF_Error2;
  delete TOF_Error3;
  delete TOF_EventCount;
  delete TOF_Tray_hits1;
  delete TOF_Tray_hits2;

}

void TOFcheckHistogramGroup::reset() {

  TOF_Error1->Reset();
  TOF_Error2->Reset();
  TOF_Error3->Reset();
  TOF_EventCount->Reset();
  ReadTrayList();
  ReadValidBunchidPhase();
  TOF_Tray_hits1->Reset();
  TOF_Tray_hits2->Reset();
}


void TOFcheckHistogramGroup::draw(TCanvas* cc) {

  TLatex label;
  //label.SetTextAlign(23);  // center, top
  label.SetTextSize(0.08);
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
  gStyle->SetStatFontSize(0.14);

  char tmpchr[200];
  cc->cd(); cc->SetFillColor(0);
  cc->Clear();
  cc->Divide(1, 4, 0.00002,0.00002);
  cc->cd(1);
  gPad->SetGridx(1);
  gPad->SetGridy(0);

  int mNevents=int(TOF_EventCount->GetEntries());

  TOF_Error1->GetYaxis()->SetLabelSize(0.07);
  TOF_Error1->GetXaxis()->SetLabelSize(0.055);
  TOF_Error1->SetFillColor(45);
  TOF_Error1->Draw();
  int nerror1 = int(TOF_Error1->GetEntries());
  if(nerror1>0) {
     label.SetTextColor(2);
     sprintf(tmpchr,"#scale[1.2]{%d electronics errors in %d events!}",nerror1,mNevents);
		 label.SetNDC();
		 label.DrawLatex(0.15, 0.8, tmpchr);
  } else {
    label.SetTextColor(3);
    sprintf(tmpchr,"#scale[1.2]{No electronics error in %d events}",mNevents);
		label.SetNDC();
    label.DrawLatex( 0.15, 0.8, tmpchr);
  }

  cc->cd(2);
  gPad->SetGridx(1);
  gPad->SetGridy(0);
  TOF_Error2->GetYaxis()->SetLabelSize(0.07);
  TOF_Error2->GetXaxis()->SetLabelSize(0.055);
  TOF_Error2->SetFillColor(45);
  TOF_Error2->Draw();
  int nerror2 = int(TOF_Error2->GetEntries());
  if(nerror2>0) {
     label.SetTextColor(2);
     sprintf(tmpchr,"#scale[1.2]{%d incorrect bunchid errors in %d events!}",nerror2,mNevents);
    label.DrawLatex( 0.15, 0.8, tmpchr);
  } else {
    label.SetTextColor(3);
    sprintf(tmpchr,"#scale[1.2]{No incorrect bunchid values in %d events!}",mNevents);
    label.DrawLatex( 0.15, 0.8, tmpchr);
  }

  cc->cd(3);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  gPad->SetLogy(1);
  TOF_Tray_hits1->GetYaxis()->SetLabelSize(0.07);
  TOF_Tray_hits1->GetXaxis()->SetLabelSize(0.055);
  TOF_Tray_hits1->SetFillColor(5);
  TOF_Tray_hits2->SetFillColor(7);
  TOF_Tray_hits1->SetMinimum(0.5);
  TOF_Tray_hits1->Draw();
  TOF_Tray_hits2->Draw("same");
	int nerror3 = int(TOF_Error3->GetEntries());
	float hmax=0;
	if(nerror3>0){
		hmax=TOF_Error3->GetMaximum();
		int nbadtrays=0;
		for(int itray=0;itray<122;itray++){
		Int_t nError = TOF_Error3->GetBinContent(itray+1);
			if (nError>0) {
				nbadtrays=nbadtrays+1;
			  sprintf(tmpchr,"#scale[0.8]{%d}",itray+1);
				label.SetNDC(0);
			  label.DrawLatex(itray+0.5, 0.9*hmax,tmpchr);
			}
		}
		if(nbadtrays==1)sprintf(tmpchr,"#scale[1.2]{%d tray not read out properly }",nbadtrays);
		if(nbadtrays>1) sprintf(tmpchr,"#scale[1.2]{%d trays not read out properly}",nbadtrays);
		label.SetTextColor(2);
		label.SetNDC();
		label.DrawLatex(0.15, 0.8, tmpchr);
  } 
	
	cc->cd(4);
  gPad->SetGridx(1);
  gPad->SetGridy(0);
  TOF_Error3->GetYaxis()->SetLabelSize(0.07);
  TOF_Error3->GetXaxis()->SetLabelSize(0.055);
  TOF_Error3->SetFillColor(45);
  TOF_Error3->Draw();
  if(nerror3>0) {
		hmax=TOF_Error3->GetMaximum();
		label.SetTextColor(2);
		sprintf(tmpchr,"#scale[1.2]{%d read out errors in %d events!}",nerror3,mNevents);
		label.SetNDC();
		label.DrawLatex( 0.15, 0.8, tmpchr);
  } else {
    hmax=gPad->GetUymax();
    label.SetTextColor(3);
    sprintf(tmpchr,"#scale[1.2]{No read out errors in %d events!}",mNevents);
		label.SetNDC();
    label.DrawLatex( 0.15, 0.8, tmpchr);
  }

  cc->Update();

} 


bool TOFcheckHistogramGroup::fill(evpReader* evp, char* datap) { 
  
  int ret=tofReader(datap);
  if(ret <= 0)   {
    fprintf(stderr,"TOF: problems in data (%d) - continuing...",ret);
    return false;
  }
  // 
  int halftrayid=-1;
  int trayid=-1;
  int allbunchid[2][122];
  for(int i=0;i<2;i++)for(int j=0;j<122;j++)allbunchid[i][j]=-9999;

  int totaltofdataword=0;
  for(int ifib=0;ifib<4;ifib++){
    int ndataword = tof.ddl_words[ifib];    // 
    totaltofdataword=totaltofdataword+ndataword;
    if(ndataword<=0) continue;
    int thebunchid =0; 
    for(int iword=0;iword<ndataword;iword++){
      int dataword=tof.ddl[ifib][iword];
      //cout<<"TOF:: ifib="<<ifib<<" dataword=0x"<<hex<<dataword<<dec<<endl;
      int packetid = (dataword&0xF0000000)>>28;
      if(!ValidDataword(packetid)) TOF_Error1->Fill(trayid+0.5*halftrayid);
      //if(!ValidDataword(packetid)) cout<<"ERROR!!!!"<<hex<<"dataword=0x"<<dataword<<dec<<"tray="<<trayid<<endl;
      //}

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
      //cout<<"tray="<<trayid<<" halftray="<<halftrayid<<endl;
      if(trayid <1 || trayid >122) continue;
      if(trayid >= 123) continue;  // no such tray number.

      // bunch id 
      if(packetid == 0x2) {
        thebunchid = dataword&0xFFF;
        //cout<<"tray="<<trayid<<" halftray="<<halftrayid<<" bunchid="<<thebunchid<<endl;
        allbunchid[halftrayid][trayid-1]=thebunchid;         
       continue;  
      }
			//cout<<"tray="<<trayid<<" halftray="<<halftrayid<<" bunchid="<<thebunchid<<endl;

      //if( (dataword&0xF0000000)>>28 == 0x6) {continue;}
      //
      int edgeid =packetid;
      if((edgeid !=4) && (edgeid!=5)) continue;
      if(halftrayid==0) TOF_Tray_hits1->Fill(trayid-0.5);
      if(halftrayid==1) TOF_Tray_hits2->Fill(trayid);
      
    }  // end loop nword
  }  // end loop fiber

  if(totaltofdataword==0) {
    cout<<"ERROR!!!! TOF data: no data word!"<<endl;
    return false;
  }

  TOF_EventCount->Fill(1);

  // check bunch id shift
  int bunchidref1 =   allbunchid[0][mReferenceTray-1];   // bunchid from tray 1 as reference.
  int bunchidref2 =   allbunchid[1][mReferenceTray-1];   // bunchid from tray 1 as reference.
  if(bunchidref1 != bunchidref2) {TOF_Error2->Fill(3);}

  for(int itray=0;itray<122;itray++){
    int traynum=itray+1;
    if(NotActiveTray[traynum]) continue;
    for(int ihalf=0;ihalf<2;ihalf++){
      int bunchid=allbunchid[ihalf][itray];
      //if(bunchid == -9999) continue;
      int ret=ValidBunchid(traynum,ihalf,bunchid,bunchidref1);
      if(ret>=0 && bunchid!=-9999) TOF_Error2->Fill(traynum); //real bunchid errors
      if(bunchid==-9999) TOF_Error3->Fill(traynum); //missing bunchid
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
void TOFcheckHistogramGroup::ReadTrayList(){
  
  //cout<<"TOFcheckHistogramGroup::TrayList config file:"<<mTrayList<<endl;

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
  } else {cout<<"TOFcheckHistogramGroup::Can not open file:"<<mTrayList<<endl;}
  filein.close();

}

void TOFcheckHistogramGroup::ReadValidBunchidPhase(){
  //cout<<"TOFcheckHistogramGroup::Bunchid phase config file:"<<mBunchShiftList<<endl;

  mReferenceTray=1;
  
  TString buffer;
  ifstream filein(mBunchShiftList);
  int count=0;

  if(filein){
    while(!filein.eof()){
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      int number=atoi(buffer.Data());
      if(count==0) mReferenceTray=number;
      if(count>=1 && count<=8)mValidShiftTray[(count-1)%2][(count-1)/2]=number;

      if(count== 9 || count==10) mValidShift121[0][(count-1)%2]=number;
      if(count==11 || count==12) mValidShift121[1][(count-1)%2]=number;

      if(count==13 || count==14) mValidShift122[0][(count-1)%2]=number;
      if(count==15 || count==16) mValidShift122[1][(count-1)%2]=number;

      count++;
    }
  } else {cout<<"TOFcheckHistogramGroup::Can not open file:"<<mBunchShiftList<<endl;}

}

int TOFcheckHistogramGroup::ValidBunchid(int trayid,int halftrayid,int bunchid,int refbunchid)
{
  if(trayid<1 || trayid>122) return -1;
  if(trayid == 123) return -1;
  // THUB NW
  int trayinTHUB1[30]={21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
  // THUB NE 
  int trayinTHUB2[30]={66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95};
  // THUB SW
  int trayinTHUB3[30]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,51,52,53,54,55,56,57,58,59,60};
  // THUB SE
  int trayinTHUB4[30]={61,62,63,64,65,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120};
 
  // the following numbers are moved into TOF_ValidBunchidPhase.txt 
  /*
  int trayvalidshift[2][4]={{4,-5,0,-7},{5,-4,0,-6}};
  //tray 121 half0: 4 5    half1: 5 6
  //tray 122 half0: -5 -4  half1: -4 -3
  int tray121shift[2][2]={{ 4, 5},{ 5, 6}};
  int tray122shift[2][2]={{-5,-4},{-4,-3}};
  int tray124shift[2]   = {-5,-6};
  */

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

//  cout<<"tray="<<trayid<<" halftrayid="<<halftrayid<<" bunchid="<<bunchid<<" refbunchid="<<refbunchid<<" diff="<<diff<<" nthub="<<nthub<<endl;
  if(trayid>1 && trayid<121){
    if( (diff != mValidShiftTray[0][nthub])  && (diff != mValidShiftTray[1][nthub]) ) ret=nthub;
  } else if(trayid==121){
    if(diff !=mValidShift121[halftrayid][0] && diff != mValidShift121[halftrayid][1]) ret=nthub;
  } else if(trayid==122){
    if(diff !=mValidShift122[halftrayid][0] && diff != mValidShift122[halftrayid][1]) ret=nthub;
  }

  //if(ret>=0)cout<<"ERROR!! tray="<<trayid<<" halftrayid="<<halftrayid<<" bunchid="<<bunchid<<" refbunchid="<<refbunchid<<" diff="<<diff<<" nthub="<<ret<<endl;

  return ret;
}
 
