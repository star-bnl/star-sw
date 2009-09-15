#include "MTDhitsHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <algorithm>

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

ClassImp(MTDhitsHistogramGroup) ;

MTDhitsHistogramGroup::MTDhitsHistogramGroup() {
  // For ROOT I/O
  memset( MTD_hitmap, 0, sizeof(MTD_hitmap));
  MTD_ToT = 0;
  MTD_eastT_westT = 0;
  MTD_eastT_vs_westT = 0;
  MTD_hits_vs_TOF_hits=0;
}

MTDhitsHistogramGroup::MTDhitsHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  char tmpchr[200];
  sprintf(tmpchr,"MTD_LE_hitmap");
  MTD_hitmap[0]=new TH1F(tmpchr,"MTD Leading edge hitmap",36,-0.5,35.5);
  MTD_hitmap[0]->SetXTitle("Chan #");
  //MTD_hitmap[0]->SetYTitle("Counts");
  sprintf(tmpchr,"MTD_TE_hitmap");
  MTD_hitmap[1]=new TH1F(tmpchr,"MTD Trailing edge hitmap",36,-0.5,35.5);
  MTD_hitmap[1]->SetXTitle("Chan #");
  //MTD_hitmap[1]->SetYTitle("Counts");

  MTD_ToT    =new TH2F("MTD_ToT","MTD ToT vs Chan #",36,-0.5,35.5,70,0,70);
  MTD_ToT->SetXTitle("MTD Chan#");
  //MTD_ToT->SetYTitle("ToT (ns)");

  MTD_eastT_vs_westT=new TH2F("MTD_eastT_vs_westT","MTD eastT vs westT",1024,0,51200,1024,0,51200);
  MTD_eastT_vs_westT->SetXTitle("east time (ns)");
  //MTD_eastT_vs_westT->SetYTitle("west time (ns)");

  MTD_eastT_westT=new TH1F("MTD_eastT_westT","MTD eastT - westT",40,-10,10);
  MTD_eastT_westT->SetXTitle("(east-west) time diff (ns)");
  //MTD_eastT_vs_westT->SetYTitle("west time (ns)");

  MTD_hits_vs_TOF_hits=new TH2F("MTD_hits_vs_TOF_hits","MTD chan vs TOF MRPC chan ",36,-0.5,35.5,960,-0.5,959.5);
  MTD_hits_vs_TOF_hits->SetXTitle("MTD chan #");

}


MTDhitsHistogramGroup::~MTDhitsHistogramGroup() {

  for (int i = 0; i < 2; ++i)delete MTD_hitmap[i];
  delete MTD_ToT;
  delete MTD_eastT_vs_westT;
  delete MTD_eastT_westT;
  delete MTD_hits_vs_TOF_hits;
}


void MTDhitsHistogramGroup::reset() {

  for (int i = 0; i < 2; ++i)MTD_hitmap[i]->Reset();
  MTD_ToT->Reset();
  MTD_eastT_vs_westT->Reset();
  MTD_eastT_westT->Reset();
  MTD_hits_vs_TOF_hits->Reset();
}


void MTDhitsHistogramGroup::draw(TCanvas* cc) {

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
  gStyle->SetStatW(0.18); gStyle->SetStatH(0.14);

  gStyle->SetNdivisions(505,"xyz");


  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);

  cc->cd(); cc->SetFillColor(0);
  cc->Clear();
  cc->Divide(2, 3);
  cc->cd(1);

  MTD_hitmap[0]->SetMinimum(0);
  MTD_hitmap[0]->SetFillColor(19);
  MTD_hitmap[0]->GetYaxis()->SetLabelSize(0.07);
  MTD_hitmap[0]->GetXaxis()->SetLabelSize(0.055);
  MTD_hitmap[0]->Draw();

  float hmin=0.;
  float hmax=MTD_hitmap[0]->GetMaximum();
  line.SetLineColor(4);
  line.DrawLine(17.5, hmin, 17.5, 1.05*hmax);
  label.DrawLatex(  7.5, 0.98*hmax, "east end");
  label.DrawLatex( 26.5, 0.98*hmax, "west end");
  labely.DrawLatex(-3.5, 0.6*hmax, "Counts");
  
  cc->cd(2);
  MTD_hitmap[1]->SetMinimum(0);
  MTD_hitmap[1]->SetFillColor(19);
  MTD_hitmap[1]->GetYaxis()->SetLabelSize(0.07);
  MTD_hitmap[1]->GetXaxis()->SetLabelSize(0.055);
  MTD_hitmap[1]->Draw();
  hmin=0;
  hmax=MTD_hitmap[1]->GetMaximum();
  line.SetLineColor(4);
  line.DrawLine(17.5, hmin, 17.5, 1.05*hmax);
  label.DrawLatex(  7.5, 0.98*hmax, "east end");
  label.DrawLatex( 26.5, 0.98*hmax, "west end");
  labely.DrawLatex(-3.5, 0.6*hmax, "Counts");

  cc->cd(3);
  MTD_ToT->GetYaxis()->SetLabelSize(0.07);
  MTD_ToT->GetXaxis()->SetLabelSize(0.055);
  MTD_ToT->Draw("colz");
  line.SetLineColor(44);
  line.DrawLine(17.5, hmin, 17.5, 70);
  label.DrawLatex(  7.5, 0.98*70, "east end");
  label.DrawLatex( 26.5, 0.98*70, "west end");
  labely.DrawLatex(-3.5, 30, "ToT (ns)");

  cc->cd(4);
  MTD_eastT_vs_westT->GetYaxis()->SetLabelSize(0.07);
  MTD_eastT_vs_westT->GetXaxis()->SetLabelSize(0.055);
  MTD_eastT_vs_westT->Draw("col");
  labely.DrawLatex(-5200, 0.6*51200, "west time (ns)");

  cc->cd(5);
  MTD_eastT_westT->GetYaxis()->SetLabelSize(0.07);
  MTD_eastT_westT->GetXaxis()->SetLabelSize(0.055);
  MTD_eastT_westT->Draw();

  cc->cd(6);
  MTD_hits_vs_TOF_hits->GetYaxis()->SetLabelSize(0.07);
  MTD_hits_vs_TOF_hits->GetXaxis()->SetLabelSize(0.055);
  MTD_hits_vs_TOF_hits->Draw("col");
  labely.DrawLatex(-3.5, 400, "TOF MRPC chan");

  cc->Update();

} 


bool MTDhitsHistogramGroup::fill(evpReader* evp, char* datap) { 
  
  int ret=tofReader(datap);
  if(ret <= 0)   {
    fprintf(stderr,"TOF: problems in data (%d) - continuing...",ret);
    return false;
  }
  // 

  int timeinbin=0;
  float time=0.;
  int halftrayid=-1;
  int trayid=-1;
  leadinghits.clear();
  trailinghits.clear();
  for(int ifib=0;ifib<4;ifib++){
    int ndataword = tof.ddl_words[ifib];    // 
    //cout<<"TOF:: ifib="<<ifib<<" ndataword="<< ndataword<<endl;
    if(ndataword<=0) continue;
    for(int iword=0;iword<ndataword;iword++){
      int dataword=tof.ddl[ifib][iword];
      //cout<<"TOF :: dataword=0x"<<hex<<dataword<<dec<<" "<<iword<<"/"<<dec<<ndataword<<" ifiber="<<ifib<<endl;
      if( (dataword&0xF0000000)>>28 == 0x2) continue;  
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

      if(!( (trayid>105 && trayid <111) || trayid ==124)) continue;

      if( (dataword&0xF0000000)>>28 == 0x6) {continue;}
      //
      int edgeid =int( (dataword & 0xf0000000)>>28 );
      if((edgeid !=4) && (edgeid!=5)) continue;

      int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
      //cout<<"tdcid="<<tdcid<<" halftrayid="<<halftrayid<<endl;
      int tdigboardid=tdcid/4;   // 0-3 for half tray.
      int  tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
      int globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray
      timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
      time = timeinbin * 25./1024;   // time in ns 

      //int moduleid=-1;
      int modulechan=-1;
      int globalmodulechan=-1;

      if(trayid==124) {
        modulechan=tdcchan2MTDchan(globaltdcchan,trayid);
      } else if(trayid>0 && trayid<121){
        modulechan=tdcchan2mrpcchan(globaltdcchan);
      }
      globalmodulechan=modulechan;

      //cout<<"MTD===:: tray="<<trayid<<" halftray="<<halftrayid<<" globaltdcchan="<<globaltdcchan<<" modulechan="<<modulechan<<" time="<<time<<" edgeid="<<edgeid<<endl;

      // fill hitmap.
      if(trayid==124) MTD_hitmap[edgeid-4]->Fill(modulechan);

      numberforsort= time+globalmodulechan*1.e5+trayid*1.e8;
      if(edgeid==4)leadinghits.push_back(numberforsort);
      if(edgeid==5)trailinghits.push_back(numberforsort);

    }  // end loop nword
  }  // end loop fiber
  
  sort(leadinghits.begin(),leadinghits.end());
  sort(trailinghits.begin(),trailinghits.end());

  float leadingtime[36],trailingtime[36];  // will only get one hit of each channel
  for(int i=0;i<36;i++){leadingtime[i]=0.;trailingtime[i]=0.;}

  for(int ich=0;ich<36;ich++){
    for(unsigned int ile=0;ile<leadinghits.size();ile++){
      double thisnumber = leadinghits[ile];
      int thistrayid= int(thisnumber/1.e8);
      if(thistrayid !=124)continue; 
      int  thismodule=int((thisnumber-thistrayid*1.e8)/1.e5);
      float thistime=  thisnumber-thistrayid*1.e8-thismodule*1.e5;
      if(thismodule == ich) {leadingtime[ich]= thistime;break;} 
    }
  }
  for(int ich=0;ich<36;ich++){
    for(unsigned int ite=0;ite<trailinghits.size();ite++){
      double thisnumber = trailinghits[ite];
      int thistrayid= int(thisnumber/1.e8);
      if(thistrayid !=124)continue; 
      int  thismodule=int((thisnumber-thistrayid*1.e8)/1.e5);
      float thistime=  thisnumber-thistrayid*1.e8-thismodule*1.e5;
      if(thismodule == ich) {trailingtime[ich]= thistime;break;} 
    }
  }
 
  for(int ich=0;ich<36;ich++){
    if(leadingtime[ich]*trailingtime[ich]<1) continue;
    float ToT = trailingtime[ich]-leadingtime[ich];
    if(ToT<0) ToT = ToT + 51200;
    if(ToT>0)MTD_ToT->Fill(ich,ToT);
  }

  for(int ieast=0;ieast<18;ieast++){
    int iwest=ieast+18;
    if(leadingtime[ieast]*leadingtime[iwest]<1) continue;
    MTD_eastT_vs_westT->Fill(leadingtime[ieast],leadingtime[iwest]);
    float timediff=leadingtime[iwest]-leadingtime[ieast];
    if(timediff>51200/2) timediff = timediff-51200;
    if(timediff<-51200/2) timediff = timediff+51200;
    //cout<<"ieast="<<ieast<<" time="<<leadingtime[ieast]<<" iwest="<<iwest<<" time="<<leadingtime[iwest]<<" diff="<<timediff<<endl;
    MTD_eastT_westT->Fill(timediff);
 }

  // 
  for(unsigned int ile=0;ile<leadinghits.size();ile++){
    double number1 = leadinghits[ile];
    int trayid1= int(number1/1.e8);
    int modulechan1=int((number1-trayid1*1.e8)/1.e5);
    int traymodulechan=modulechan1+ 192*(trayid1-106);
    if(trayid1 ==124)continue; 
    //cout<<" trayid1="<<trayid1<<" modulechan="<<modulechan1<<endl;
    for(unsigned int ile2=0;ile2<leadinghits.size();ile2++){
      double number2 = leadinghits[ile2];
      int trayid2= int(number2/1.e8);
      int  modulechan2=int((number2-trayid2*1.e8)/1.e5);
      if(trayid2 !=124)continue; 
      //cout<<" trayid2="<<trayid2<<" modulechan="<<modulechan2<<endl;
      MTD_hits_vs_TOF_hits->Fill(modulechan2,traymodulechan);
    }
  }


  return true;

}

int MTDhitsHistogramGroup::tdcchan2MTDchan(int globaltdcchan,int trayid)
{

  if(trayid !=124) return -1;

  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

//                      1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
  int MTDTDCchan[36]={  9, 12, 13, 16, 17, 21,  1,  2,  3, 19, 20, 22,  0,  5,  7, 10, 14, 15,
                      105,108,109,112,113,117, 97, 98, 99,115,116,118, 96,101,103,106,110,111};


  int inputglobalchan=globaltdcchan;
  int mtdchan=-1;

  for(int i=0;i<36;i++){
    if(MTDTDCchan[i]==inputglobalchan) {mtdchan=i;break;}
  }

  return mtdchan;
}
int MTDhitsHistogramGroup::tdcchan2mrpcchan(int globaltdcchan)
{
  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

  int tdcidmap[4][6] = { {0,1,0,1,0,1}, {2,0,2,0,1,0}, {1,2,0,2,0,2}, {2,1,2,1,2,1}};
  int tdcchanmap[4][6]={ {7,7,0,2,5,6}, {7,4,4,2,3,6}, {0,2,3,3,1,6}, {0,5,1,4,5,1}};

  int theglobalmodulechan[192];
  int theglobaltdcchan[192];

  for(int isec=0;isec<8;isec++){
    for(int imodule=0;imodule<4;imodule++){
      for(int ipad=0;ipad<6;ipad++){
        int globalmodule  =  isec*24 + imodule*6 + ipad;
        int globaltdc     =  isec*24 + tdcidmap[imodule][ipad]*8+tdcchanmap[imodule][ipad];
        theglobalmodulechan[globalmodule]=globalmodule;
        theglobaltdcchan[globalmodule]=globaltdc;
        //cout<<"global module chan="<<globalmodule<<" global tdc chan="<<globaltdc<<endl;
     }
    }
  }
  int returnthis=0;

  //int thistdcchan=tdig*24+(tdcid%4)*8+tdcchan;
  int thistdcchan=globaltdcchan;
  for(int i=0;i<192;i++){
    if(thistdcchan == theglobaltdcchan[i]) {returnthis = i;break;}
  }
  return returnthis;
}
