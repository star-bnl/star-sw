// K. Yip : Feb. 15, 2008 

#include "pp2ppHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#  include "trgReader.h"
#else
#  include "StEvent/StTriggerData.h"
#  include "TriggerData.h"
#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"


#include "TStyle.h"

ClassImp(pp2ppHistogramGroup) ;

pp2ppHistogramGroup::pp2ppHistogramGroup()
  : HistogramGroup(), mswitch(9999) 
{
   // prepare objt for ROOT I/O
   memset( h_P2P,0,sizeof(h_P2P));
}

pp2ppHistogramGroup::pp2ppHistogramGroup(unsigned int iswitch=0, const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector), mswitch(iswitch) {

  ostringstream so ;

  for ( int ii=0; ii<mMaxBits; ii++) {
    so.str("");
    so << "h" << ii+mswitch*mMaxBits ;  // mswitch=0: 0->15 ; mswitch=1: 16-31
    const char* name = (so.str()).c_str();
    h_P2P[ii] = new TH1D(name,name,256,0.,256);
  }

}


pp2ppHistogramGroup::~pp2ppHistogramGroup() {
  for ( int ii=0; ii<mMaxBits; ii++)
    if ( h_P2P[ii] != 0 ) delete h_P2P[ii];
}


void pp2ppHistogramGroup::reset() {
  for ( int ii=0; ii<mMaxBits; ii++)
    h_P2P[ii]->Reset();
}


void pp2ppHistogramGroup::draw(TCanvas* cc) {

  TLine  line;
  line.SetLineColor(16);
  TLatex label;
  label.SetTextAlign(23);  // center, top
  label.SetTextSize(0.06);
  label.SetTextColor(16);

  gStyle->SetStatFontSize( 0.05 );  
  gStyle->SetStatTextColor( kRed );  
  gStyle->SetStatW(0.38);


  cc->cd();
  cc->Clear();


  gStyle->SetOptStat(1111);

  cc->Divide(4, 4);
  for ( int ii=0; ii<mMaxBits; ii++) {
    cc->cd(ii+1) ; 
    cc->cd(ii+1)->SetLogy() ; 
    h_P2P[ii]->SetFillColor(4) ;
    h_P2P[ii]->Draw();
  }
  cc->Update();


} 


bool pp2ppHistogramGroup::fill(evpReader* evp, char* datap) { 

#ifndef NEW_DAQ_READER
  int ret = trgReader(datap);
  if(ret <= 0) {
    fprintf(stderr, "pp2pp Trigger RAW: problems in data (%d) - continuing...", ret);
    return false;
  }

  int index ;

  for ( int ii = 0; ii<mMaxBits; ii++ ) {

    index = ii+mswitch*mMaxBits ; // mswitch=0: 0->15 ; mswitch=1: 16-31

    h_P2P[ii]->Fill( double( trg.P2P[index] ) ) ;

//    if ( trg.P2P[index] != 0 ) 
//      cout << "pp2ppHistogramGroup : " << ii << " " <<  int( trg.P2P[index] ) << endl ;

  }

#else
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(trgd){
    int i=0;
    for(int vh=0; vh<2; vh++){
      for(int ew=0; ew<2; ew++){
        for(int udio=0; udio<2; udio++){
          for(int ch=0; ch<2; ch++){
            unsigned short adc = trgd->pp2ppADC((StBeamDirection)ew,vh,udio,ch);
            unsigned short tac = trgd->pp2ppTAC((StBeamDirection)ew,vh,udio,ch);
            h_P2P[i   ]->Fill( double(adc) );
            h_P2P[i+16]->Fill( double(tac) );
            i++;
          }
        }
      }
    }
  }
#endif

  return true;
}

