#include "TOFupvpdHistogramGroup.h"

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

ClassImp(TOFupvpdHistogramGroup) ;

TOFupvpdHistogramGroup::TOFupvpdHistogramGroup() {
  // For ROOT I/O
  memset( upvpd_hitmap, 0, sizeof(upvpd_hitmap));
  upvpd_ToT = 0;
  upvpd_eastT_vs_westT = 0;
}

TOFupvpdHistogramGroup::TOFupvpdHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
 
  char tmpchr[200];
  sprintf(tmpchr,"upvpd_LE_hitmap");
  upvpd_hitmap[0]=new TH1F(tmpchr,"upvpd Leading edge hitmap",54,-0.5,53.5);
  upvpd_hitmap[0]->SetXTitle("PMT #");
  //upvpd_hitmap[0]->SetYTitle("Counts");
  sprintf(tmpchr,"upvpd_TE_hitmap");
  upvpd_hitmap[1]=new TH1F(tmpchr,"upvpd Trailing edge hitmap",54,-0.5,53.5);
  upvpd_hitmap[1]->SetXTitle("PMT #");
  //upvpd_hitmap[1]->SetYTitle("Counts");

  upvpd_ToT    =new TH2F("upvpd_ToT","upvpd ToT vs PMT#",54,-0.5,53.5,50,0,50);
  upvpd_ToT->SetXTitle("PMT #");
  //upvpd_ToT->SetYTitle("ToT (ns)");

  upvpd_eastT_vs_westT=new TH2F("upvpd_eastT_vs_westT","upvpd eastT vs westT",1024,0,51200,1024,0,51200);
  upvpd_eastT_vs_westT->SetXTitle("east time (ns)");
  //upvpd_eastT_vs_westT->SetYTitle("west time (ns)");


}


TOFupvpdHistogramGroup::~TOFupvpdHistogramGroup() {

  for (int i = 0; i < 2; ++i)delete upvpd_hitmap[i];
  delete upvpd_ToT;
  delete upvpd_eastT_vs_westT;
}


void TOFupvpdHistogramGroup::reset() {

  for (int i = 0; i < 2; ++i)upvpd_hitmap[i]->Reset();
  upvpd_ToT->Reset();
  upvpd_eastT_vs_westT->Reset();
}


void TOFupvpdHistogramGroup::draw(TCanvas* cc) {

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
 
  gStyle->SetOptStat(0);
  gStyle->SetStatX(0.95); gStyle->SetStatY(0.92);
  gStyle->SetStatW(0.42); gStyle->SetStatH(0.20);

  gStyle->SetNdivisions(505,"xyz");


  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);

  cc->cd(); cc->SetFillColor(0);
  cc->Clear();
  cc->Divide(2, 2);
  cc->cd(1);
  upvpd_hitmap[0]->SetFillColor(19);
  upvpd_hitmap[0]->Draw();

  float hmin=0;
  float hmax=upvpd_hitmap[0]->GetMaximum();
  line.SetLineColor(4);
  line.DrawLine(18.5, hmin, 18.5, 1.05*hmax);
  line.DrawLine(37.5, hmin, 37.5, 1.05*hmax);
  label.DrawLatex(  7.5, 0.98*hmax, "west side");
  label.DrawLatex( 26.5, 0.98*hmax, "east side");
  label.DrawLatex( 45.5, 0.98*hmax, "PP2PP");
  labely.DrawLatex(-5.5, 0.6*hmax, "Counts");
  
  cc->cd(2);
  upvpd_hitmap[1]->SetFillColor(19);
  upvpd_hitmap[1]->Draw();

  hmin=0;
  hmax=upvpd_hitmap[1]->GetMaximum();
  line.SetLineColor(4);
  line.DrawLine(18.5, hmin, 18.5, 1.05*hmax);
  line.DrawLine(37.5, hmin, 37.5, 1.05*hmax);
  label.DrawLatex(  7.5, 0.98*hmax, "west side");
  label.DrawLatex( 26.5, 0.98*hmax, "east side");
  label.DrawLatex( 45.5, 0.98*hmax, "PP2PP");
  labely.DrawLatex(-5.5, 0.6*hmax, "Counts");

  cc->cd(3);
  upvpd_ToT->Draw("colz");
  line.SetLineColor(44);
  line.DrawLine(18.5, hmin, 18.5, 50);
  line.DrawLine(37.5, hmin, 37.5, 50);
  label.DrawLatex(  7.5, 47, "west side");
  label.DrawLatex( 26.5, 47, "east side");
  label.DrawLatex( 45.5, 47, "PP2PP");
  labely.DrawLatex(-5.4, 30, "ToT (ns)");
  cc->cd(4);
  upvpd_eastT_vs_westT->Draw("col");
  labely.DrawLatex(-5100, 0.6*51200, "west time (ns)");

  cc->Update();

} 


bool TOFupvpdHistogramGroup::fill(evpReader* evp, char* datap) { 
  
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

      if(trayid !=121 && trayid !=122) continue;  // only start detector here.

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

      int moduleid=-1;
      int modulechan=-1;
      int globalmodulechan=-1;

      moduleid=trayid;
      modulechan=tdcchan2upvpdPMTchan(globaltdcchan,edgeid,trayid);
      globalmodulechan=modulechan;
      // fill hitmap.
      upvpd_hitmap[edgeid-4]->Fill(modulechan);

      numberforsort= time+globalmodulechan*1.e5+trayid*1.e8;
      if(edgeid==4)leadinghits.push_back(numberforsort);
      if(edgeid==5)trailinghits.push_back(numberforsort);

    }  // end loop nword
  }  // end loop fiber
  
  std::sort(leadinghits.begin(),leadinghits.end());
  std::sort(trailinghits.begin(),trailinghits.end());

  float leadingtime[54],trailingtime[54];  // will only get one hit of each channel
  for(int i=0;i<54;i++){leadingtime[i]=0.;trailingtime[i]=0;}

  for(int ich=0;ich<54;ich++){
    for(unsigned int ile=0;ile<leadinghits.size();ile++){
      double thisnumber = leadinghits[ile];
      int thistrayid= int(thisnumber/1.e8);
      int  thismodule=int((thisnumber-thistrayid*1.e8)/1.e5);
      float thistime=  thisnumber-thistrayid*1.e8-thismodule*1.e5;
      if(thismodule == ich) {leadingtime[ich]= thistime;break;} 
    }
  }
  for(int ich=0;ich<54;ich++){
    for(unsigned int ite=0;ite<trailinghits.size();ite++){
      double thisnumber = trailinghits[ite];
      int thistrayid= int(thisnumber/1.e8);
      int  thismodule=int((thisnumber-thistrayid*1.e8)/1.e5);
      float thistime=  thisnumber-thistrayid*1.e8-thismodule*1.e5;
      if(thismodule == ich) {trailingtime[ich]= thistime;break;} 
    }
  }
 
  for(int ich=0;ich<54;ich++){
    if(leadingtime[ich]*trailingtime[ich]<1) continue;
    float ToT = trailingtime[ich]-leadingtime[ich];
    if(ToT<0) ToT = ToT + 51200;
    if(ToT>0)upvpd_ToT->Fill(ich,ToT);
  }
  for(int iwest=0;iwest<19;iwest++){
    int ieast=iwest+19;
    if(leadingtime[ieast]*leadingtime[iwest]<1) continue;
    upvpd_eastT_vs_westT->Fill(leadingtime[ieast],leadingtime[iwest]);
  }

  return true;

}

int TOFupvpdHistogramGroup::tdcchan2upvpdPMTchan(int globaltdcchan, int edgeid,int trayid)
{

  if(trayid<121 || trayid >122) return -1;
  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

//                      1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16 17 18 19
  int upvpdLEchan[54]={142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,  //west
                       142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,  //east
		       48,64,50,70,0,29,5,96,   48,64,50,70,0,29,5,96};                 //pp2pp 
  int upvpdTEchan[54]={129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //west
                       129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //east
		       63,61,59,57,15,38,14,111,    63,61,59,57,15,38,14,111};             //pp2pp 
  int inputglobalchan=globaltdcchan;
  int pmtchan=-1;
  int pmtLEchan =-1;
  int startpoint=-1;
  if(trayid==121) startpoint=0;   // west
  if(trayid==122) startpoint=19;  // east

  for(int i=startpoint;i<startpoint+19;i++){
    if(upvpdLEchan[i]==inputglobalchan) {pmtLEchan=i;break;}
  }
  for(int i=38;i<46;i++){
    if(upvpdLEchan[i]==inputglobalchan) {pmtLEchan=i;if(trayid==122)pmtLEchan=pmtLEchan+8;break;}
  }
  //
  int pmtTEchan=-1;
  for(int i=startpoint;i<startpoint+19;i++){
    if(upvpdTEchan[i]==inputglobalchan) {pmtTEchan=i;break;}
  }
  for(int i=38;i<46;i++){
    if(upvpdTEchan[i]==inputglobalchan) {pmtTEchan=i;if(trayid==122)pmtTEchan=pmtTEchan+8;break;}
  }

  if(edgeid==4) pmtchan = pmtLEchan;
  if(edgeid==5) pmtchan = pmtTEchan;

  //cout<<" inside map:: trayid="<<trayid<<" globaltdcchan="<<globaltdcchan<<" edgeid="<<edgeid<<" return="<<pmtchan<<endl;

  return pmtchan;
}
