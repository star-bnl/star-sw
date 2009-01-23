#include "L2UpsilonCountsHistogramGroup.h"

#include <iostream>
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

//extern static TVirtualPad* gPad;

ClassImp(L2UpsilonCountsHistogramGroup) ;

L2UpsilonCountsHistogramGroup::L2UpsilonCountsHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector) : HistogramGroup(group,subGroup,trigger,detector) {

  hTag  = new TH1D( pre("hTag"), "Tag",5,-0.5,4.5);
  hTime = new TH1D( pre("hTime"), "Time",250,0.5,1000);
  hEvent =  new TH1D( pre("hEventAccepted"),"Events seen/accepted",2,-0.5,1.5);
  hNumberOfHotTowers =  new TH1D( pre("hNumberOfHotTowers"),"number of hot towers",5000,4,5004);
  hNumberOfHotTowers->SetXTitle("events seen");
  hNumberOfHotTowers->SetYTitle("number of hot towers");
  hAbordRate =  new TH1D( pre("hAbordRate"),"abord rate overall",5000,4,5004);
  hAbordRate->SetXTitle("events seed");
  hAbordRate->SetYTitle("overall abord rate");
  hAbordRateCurrent =  new TH1D( pre("hAbordRateCurrent"),"abord rate (25 evts)",5000,4,5004);
  hAbordRateCurrent->SetXTitle("events seed");
  hAbordRateCurrent->SetYTitle("aboard rate (25 evts)");
  hNumberOfHotTowers->SetFillColor(2);
  mNumberOfHotTowers = -1;
  mHotTowerChanges = -1;
}

L2UpsilonCountsHistogramGroup::~L2UpsilonCountsHistogramGroup() {
  //cout << __PRETTY_FUNCTION__ << endl;
  delete hTag;
  delete hTime; 
  delete hEvent;
  delete hNumberOfHotTowers;
  delete hAbordRate;
  delete hAbordRateCurrent;
}

void L2UpsilonCountsHistogramGroup::reset() {
  hTag->Reset();
  hTime->Reset();
  hEvent->Reset();
  hNumberOfHotTowers->Reset();
  hAbordRate->Reset();
  hAbordRateCurrent->Reset();
  mNumberOfHotTowers = -1;
  mHotTowerChanges = -1;
}


void L2UpsilonCountsHistogramGroup::draw(TCanvas* cc) {
  //cout << __PRETTY_FUNCTION__ << endl;
  cc->cd();
  cc->Clear();
  cc->Divide(2,3);
  cc->cd(1);
  hTag->Draw();
  cc->cd(2);
  hTime->Draw();
  cc->cd(3)->SetLogx();  
  hNumberOfHotTowers->Draw("p");
  cc->cd(4)->SetLogx();  
  hAbordRate->Draw("p");
  cc->cd(6)->SetLogx();  
  hAbordRateCurrent->Draw("p");
  
  cc->cd(5);
  hEvent->Draw();
  cc->Update();
} 

#include <stdlib.h>
#include "L2UpsilonResult.h"
bool L2UpsilonCountsHistogramGroup::fill(evpReader* evp, char* datap) { 
  static list<double> seen;
  static list<double> acce;
  int ret = trgReader(datap) ;
  if(ret <= 0) {
    fprintf(stderr,"TRG RAW: problems in data (%d) - continuing...",ret) ;
    return false;
  }  
#ifndef NEW_DAQ_READER
  TRGD *trgd = (TRGD *)trg.trgd;

  if ( !trgd ) return false;

  L2UpsilonResult L2;
  L2UpsilonResult* mL2 = &L2;
  if  (!mL2) return false;
  memcpy(mL2,trgd->sum.L2Result+L2RESULTS_2008_OFFSET_UPS,sizeof(L2UpsilonResult) );
  mL2->swap();

  hTag->Fill( (double)mL2->tag );
  hTime->Fill( (double)mL2->time );
  mNumberOfHotTowers =  mL2->reserved;
  hNumberOfHotTowers->Fill(mL2->eventsSeen,mNumberOfHotTowers);
  seen.push_back(mL2->eventsSeen);
  acce.push_back(mL2->eventsAccepted);
  hAbordRate->SetBinContent(mL2->eventsSeen, (double)(mL2->eventsSeen-mL2->eventsAccepted)/mL2->eventsSeen );
  if ( seen.size() > 25 ) {
    while ( seen.size()>25 ) {
      seen.pop_front();
      acce.pop_front();
    }
    double rate = 1. - ( acce.back()-acce.front() ) / (seen.back()-seen.front() );
    hAbordRateCurrent->SetBinContent(mL2->eventsSeen, rate);
  }
  hEvent->SetBinContent( 1, mL2->eventsSeen );
  hEvent->SetBinContent( 2, mL2->eventsAccepted );
  return true;
#else
  return false;
#endif
}

/*************************************************************************************
 $Id: L2UpsilonCountsHistogramGroup.cxx,v 1.1 2009/01/23 16:07:55 jeromel Exp $
 *************************************************************************************
 $Log: L2UpsilonCountsHistogramGroup.cxx,v $
 Revision 1.1  2009/01/23 16:07:55  jeromel
 Import from online/RTS/src/

 Revision 1.6  2008/12/18 18:10:02  fine
 adjust for the newest DAQ_READER

 Revision 1.5  2007/11/27 11:52:05  psoren
 L2RESULTS_2007 --> L2RESULTS_2008

 Revision 1.4  2007/04/25 17:52:06  laue
 Minor updates to plots.
 Fixed the SVT hybrid and anode mapping

 Revision 1.3  2007/04/05 16:49:25  laue
 *** empty log message ***

 Revision 1.2  2007/04/03 13:18:55  laue
 *** empty log message ***

 Revision 1.1  2007/03/21 17:06:07  laue
 Some new histogram groups.
 Some groups just moved from the Infrastructure folder to this new folder

*************************************************************************************/




 

