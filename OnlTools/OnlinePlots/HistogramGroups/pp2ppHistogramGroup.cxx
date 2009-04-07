/*
 K. Yip : Apr. 6, 2009 --- Put in the real names for the h_P2P 
 K. Yip : Apr. 3, 2009 --- h_P2P has only a dim. of 16 and both pp2ppHistogramGroup's use the same h_P2P[0..15]
 K. Yip : Feb. 20, 2009 --- correction of Akio's stuff and preparation of making pp2pp detector plots ...
 K. Yip : Feb. 15, 2008 
*/

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

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_PP2PP/daq_pp2pp.h>


#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"




#include "TStyle.h"

ClassImp(pp2ppHistogramGroup) ;

pp2ppHistogramGroup::pp2ppHistogramGroup() {
  // For ROOT I/O
  memset( h_P2P,0,sizeof(h_P2P));
  mswitch = 0;
}

pp2ppHistogramGroup::pp2ppHistogramGroup(unsigned int iswitch, const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector), mswitch(iswitch) {

  /*
  ostringstream so ;

  for ( int ii=0; ii<mMaxBits; ii++) {
    so.str("");
    so << "h" << ii+mswitch*mMaxBits ;  // mswitch=0: 0->15 ; mswitch=1: 16-31
    h_P2P[ii] = new TH1D((so.str()).c_str(),(so.str()).c_str(),256,0.,256);
  }
  */


  const char *hist_name[32] = { "RPEVU1_ADC", "RPEVU2_ADC", "RPEVD1_ADC", "RPEVD2_ADC", "RPWVU1_ADC", "RPWVU2_ADC", "RPWVD1_ADC", "RPWVD2_ADC",
				"RPEHO1_ADC", "RPEHO2_ADC", "RPEHI1_ADC", "RPEHI2_ADC", "RPWHO1_ADC", "RPWHO2_ADC", "RPWHI1_ADC", "RPWHI2_ADC",
				"RPEVU1_TAC", "RPEVU2_TAC", "RPEVD1_TAC", "RPEVD2_TAC", "RPWVU1_TAC", "RPWVU2_TAC", "RPWVD1_TAC", "RPWVD2_TAC",
				"RPEHO1_TAC", "RPEHO2_TAC", "RPEHI1_TAC", "RPEHI2_TAC", "RPWHO1_TAC", "RPWHO2_TAC", "RPWHI1_TAC", "RPWHI2_TAC" };

  for ( int ii=0; ii<mMaxBits; ii++) {
    h_P2P[ii] = new TH1D(hist_name[ii+mswitch*mMaxBits],hist_name[ii+mswitch*mMaxBits],256,0.,256.);
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

  unsigned short adc, tac;

  StTriggerData* trgd = TriggerData::Instance(datap);
  if(trgd){
    int i=0;
    for(int vh=0; vh<2; vh++){
      for(int ew=0; ew<2; ew++){
        for(int udio=0; udio<2; udio++){
          for(int ch=0; ch<2; ch++){
	    if ( mswitch == 0 ) {
	      adc = trgd->pp2ppADC((StBeamDirection)ew,vh,udio,ch);
	      h_P2P[i]->Fill( double(adc) );
	    }
	    else {
	      tac = trgd->pp2ppTAC((StBeamDirection)ew,vh,udio,ch);
	      h_P2P[i]->Fill( double(tac) );
	    }
            i++;
          }
        }
      }
    }
  }

#endif

  return true;

}

