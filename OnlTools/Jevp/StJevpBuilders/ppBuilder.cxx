#define NDEBUG
//#define RUN2009
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <math.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_PP2PP/daq_pp2pp.h"
#include "trgStructures.h"
#include <TH1I.h>
#include <TH2F.h>
#include <TString.h>
#include <TStyle.h>

#include "ppBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//

  TString chain_name[4] = { "_A", "_B", "_C", "_D" };
  TString SEQ_name[8]  = { "SEQ1","SEQ2","SEQ3","SEQ4","SEQ5","SEQ6","SEQ7","SEQ8",};
#ifdef RUN2009
  //             SEQ  |    1      2      3      4      5      6      7      8
  TString RP_name[8]  = { "EHI", "EHO", "EVU", "EVD", "WHI", "WHO", "WVD", "WVU" };
  TString PMT_name[8] = { "PMT0", "PMT1" };
  //  u_short PMT_RP_MAP[8] = { 2, 3, 7, 6, 0, 1, 4, 5 };
  int PMT_RP_MAP[8] = { 2, 3, 7, 6, 1, 0, 5, 4 }; // changed by KY (2015-1-30)
#else
  //              SEQ  |    1      2      3      4      5      6      7      8
  TString RP_name[8] =  { "E1U", "E1D", "E2U", "E2D", "W1U", "W1D", "W2U", "W2D" };
  TString PMT_name[2] = { "PMT1", "PMT2"};
  //  int PMT_RP_MAP[8] = { 2, 3, 6, 7, 0, 1, 4, 5 };
  int PMT_RP_MAP[8] = { 2, 3, 6, 7, 1, 0, 5, 4 }; // changed by KY (2015-1-30)
#endif
const double TAC_TRSHLD = 100.;

ClassImp(ppBuilder);
  

ppBuilder::ppBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"pp";

  // start with histograms undefined...
  memset(&contPMT_ADC, 0, sizeof(contPMT_ADC));
  memset(&contPMT_TAC, 0, sizeof(contPMT_TAC));
  memset(&contVIP, 0, sizeof(contVIP));
  memset(&contentsSVX, 0, sizeof(contentsSVX));
  memset(&mEntriesSVX, 0, sizeof(mEntriesSVX));
  memset(&mEntriesNCH, 0, sizeof(mEntriesNCH));
  memset(&hitperbunch, 0, sizeof(hitperbunch)); // added by KY (2015-3-19)
}

ppBuilder::~ppBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contVIP) / sizeof(TH2 *);
  for(int i=0;i<n;i++) {
    if(contVIP.array[i]) delete contVIP.array[i];
  }
  n = sizeof(contPMT_ADC) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contPMT_ADC.array[i]) delete contPMT_ADC.array[i];
  }
  n = sizeof(contPMT_TAC) / sizeof(TH2 *);
  for(int i=0;i<n;i++) {
    if(contPMT_TAC.array[i]) delete contPMT_TAC.array[i];
  }
  n = sizeof(contentsSVX) / sizeof(TProfile *);
  for(int i=0;i<n;i++) {
    if(contentsSVX.array[i]) delete contentsSVX.array[i];
  }
  if ( hitperbunch ) delete hitperbunch ; // added by KY (2015-3-19)
}

void ppBuilder::initialize(int argc, char *argv[]) {

  // Initialization of histograms.
  //
  gStyle->SetGridStyle(1);
  gStyle->SetGridWidth(0.001);
  gStyle->SetPaintTextFormat("6.2f");
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  int np_Tot = 0;
    contVIP.PMT = new TH2D("PMT_STATUS","PMT_STATUS <ADC>; ; ",8,0,8,2,0,2);
    contVIP.PMT->SetStats(0);
    contVIP.PMT->LabelsDeflate("X");
    contVIP.PMT->LabelsDeflate("Y");
    for (int i=0;i<8;i++) contVIP.PMT->GetXaxis()->SetBinLabel(i+1,RP_name[i]);
                          contVIP.PMT->GetYaxis()->SetBinLabel( 1,"PMT1");
                          contVIP.PMT->GetYaxis()->SetBinLabel( 2,"PMT2");
    //
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    contVIP.SVX = new TH2D("SVX_STATUS","SVX_STATUS <ADC>",6,0,6,32,0,32);
    contVIP.SVX->SetStats(0);
    contVIP.SVX->LabelsDeflate("X");
    contVIP.SVX->LabelsDeflate("Y");

    contVIP.SVX_NCH = new TH2D("SVX_NCH","SVX_NCH <average fraction of chnls read>",6,0,6,32,0,32);
    contVIP.SVX_NCH->SetStats(0);
    contVIP.SVX_NCH->LabelsDeflate("X");
    contVIP.SVX_NCH->LabelsDeflate("Y");

    int bin = 1;
    for (int i=0;i<8;i++){
      for ( int k=0;k<4;k++){
	contVIP.SVX->GetYaxis()->SetBinLabel(bin, RP_name[i]+chain_name[k] );
	contVIP.SVX_NCH->GetYaxis()->SetBinLabel(bin, RP_name[i]+chain_name[k] );
	bin++;
      }
    }
    for ( int i=0;i<6;i++) {
      TString svx ("SVX");
      svx += i;
      contVIP.SVX->GetXaxis()->SetBinLabel( i+1, svx );
      contVIP.SVX_NCH->GetXaxis()->SetBinLabel( i+1, svx );
    }
    //
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=+++++
    contVIP.VTIM = new TH2D("CTIME","PMT Time vs z-vertex; (TAC_{WEST}-TAC_{EAST})/2; (TAC_{WEST}+TAC_{EAST})/2", 50,-700.,700.,50,600.,2000.);
    contVIP.VTIM -> SetStats(0);
    // added by KY (2015-2-13) : To make the Y-axis label unobstructed
    gStyle->SetPadLeftMargin(0.11);
    contVIP.VTIM->GetYaxis()->SetTitleOffset(1.38); 
    //
    //
    np_Tot += sizeof(contVIP)/sizeof( TH2 *);
  
    //
    // for PMT
    int mBinsTAC = 100;
    double maxBinTAC = 2500.;
    int mBinsADC = 100;
    double maxBinADC = 400.;
  for(int i=0;i<8;i++) {
    // PMT's
    //
    //
    //    int k = PMT_RP_MAP[i]; // changed by KY (2015-1-30) : RP_name[k] -> RP_name[i] 
    TString name = SEQ_name[i] + "_PMT1_ADC";
    TString caption = RP_name[i] + " PMT1 ;ADC; Nent";
 
      contPMT_ADC.h1_P2P[i*2] = new TH1D(name, caption, mBinsADC,0.,maxBinADC); 
      np_Tot++;
      contPMT_ADC.h1_P2P[i*2] -> SetFillColor(8); 

      name = SEQ_name[i] + "_PMT2_ADC";
      caption = RP_name[i] +  " PMT2 ;ADC; Nent";
      contPMT_ADC.h1_P2P[i*2+1] = new TH1D(name, caption, mBinsADC,0.,maxBinADC); 
      np_Tot++;
      contPMT_ADC.h1_P2P[i*2+1] -> SetFillColor(8); 


      name = SEQ_name[i] + "_TAC";
      caption = RP_name[i] + " TAC ; PMT1; PMT2";
      contPMT_TAC.h2_P2P[i] = new TH2D(name, caption, mBinsTAC,0.,maxBinTAC,mBinsTAC,0.,maxBinTAC);
      // added by KY (2015-2-13) : To make it look nicer/unobstructed
      contPMT_TAC.h2_P2P[i]->GetXaxis()->SetNdivisions(5,0,0,true);
      contPMT_TAC.h2_P2P[i]->GetXaxis()->CenterTitle();
      contPMT_TAC.h2_P2P[i]->GetYaxis()->CenterTitle();
      contPMT_TAC.h2_P2P[i]->GetYaxis()->SetTitleOffset(1.28);

      np_Tot++;
      //
      //  SVX'es
      for ( int p=0; p<4; p++){
	name = SEQ_name[i] + chain_name[p];
	caption = RP_name[i] + chain_name[p] +" ; channel; < ADC >";
	int Nchan = 128;
	double Chmax = 512.;                   
	if ( p == 1 || p == 3 ) { Nchan = 192;  Chmax = 768.; }
	contentsSVX.hp_P2P[i*4+p] = new TProfile( name, caption, Nchan,0.,Chmax,0.,5000.,"S");
	contentsSVX.hp_P2P[i*4+p] -> SetLineColor(2);
	contentsSVX.hp_P2P[i*4+p] -> SetLineWidth(4);
	contentsSVX.hp_P2P[i*4+p] -> SetStats(0);
	np_Tot++;
      }
  }

  // added by KY (2015-3-19) 
  hitperbunch = new TProfile("hitperbunch", "Hits per bunch", 120,0.,120.,0.,1000.);
  hitperbunch->SetLineColor(4);
  hitperbunch->SetLineWidth(4);
  hitperbunch->SetMarkerStyle(22);
  hitperbunch->SetMarkerColor(4);
  hitperbunch->SetStats(0);
  np_Tot++ ;

  // Add root histograms to Plots

  JevpPlot *plots[np_Tot-1];

  int iNext = 0;
  plots[iNext]=new JevpPlot(contVIP.array[iNext]);
  plots[iNext] -> getHisto(0)->histo->SetMinimum(10.);
  plots[iNext] -> getHisto(0)->histo->SetLineStyle(1);
  plots[iNext] -> setDrawOpts("COLTEXT");
  iNext++;
  plots[iNext]=new JevpPlot(contVIP.array[iNext]);
  plots[iNext] -> getHisto(0)->histo->SetMaximum(150.);
  plots[iNext] -> getHisto(0)->histo->SetMinimum(1.);
  plots[iNext] -> setDrawOpts("COLTEXT");
  iNext++;
  plots[iNext]=new JevpPlot(contVIP.array[iNext]);
  plots[iNext] -> optlogz = 1;
  plots[iNext] -> setDrawOpts("COLZ");
  iNext++;
  plots[iNext]=new JevpPlot(contVIP.array[iNext]);
  plots[iNext] -> getHisto(0)->histo->SetMinimum(0.01);
  plots[iNext] -> getHisto(0)->histo->SetMaximum(0.16);
  plots[iNext] -> getHisto(0)->histo->SetLineStyle(1);
  plots[iNext] -> setDrawOpts("COLTEXT");
  iNext++;

  for(u_int i=0; i<sizeof(contPMT_ADC) / sizeof(TH1 *);i++) {
    plots[iNext] = new JevpPlot(contPMT_ADC.array[i]); 
    plots[iNext]->logy = 1;  
    iNext++;                                      
  }

  for(u_int i=0;i<sizeof(contPMT_TAC) / sizeof(TH2 *);i++) {
    plots[iNext] = new JevpPlot(contPMT_TAC.array[i]);
    plots[iNext] -> optlogz=1; 
    iNext++;                           
  }

  for(u_int i=0;i<sizeof(contentsSVX) / sizeof(TProfile *);i++) {
    plots[iNext] = new JevpPlot(contentsSVX.array[i]);
    plots[iNext]->getHisto(0)->histo->SetMinimum(  0.);
    plots[iNext]->getHisto(0)->histo->SetMaximum(250.);
    plots[iNext]->gridx = 0;
    plots[iNext]->setDrawOpts("HIST");
    iNext++;
  }

  // added by KY (2015-3-19) 
  plots[iNext] = new JevpPlot(hitperbunch);
  plots[iNext]->gridx = 0;
  iNext++;

  // Add Plots to plot set...
  //
  assert ( np_Tot == iNext );
  for(int i=0;i< iNext;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }

}
  
void ppBuilder::startrun(daqReader *rdr) {
  char *htime;
  htime = ctime( (long int*)&rdr->evt_time);
  *(htime+strlen(htime)-1) =0 ;
  LOG("JEFF", "ppBuilder starting run #%d date %s",rdr->run, htime);
  resetAllPlots();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)


void ppBuilder::event(daqReader *rdr)
{
  int bunch_number ; // added by KY (2015-3-19)
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) {  return; }

  if(trgd){
    int i=0;
    double tac_avr[8];
    double adc_avr[8];
    for(int vh=0; vh<2; vh++){
      for(int ew=0; ew<2; ew++){

        for(int udio=0; udio<2; udio++){

          int iSeq = PMT_RP_MAP[i];
	  double adc0 = trgd->pp2ppADC((StBeamDirection)ew,vh,udio,0);
	  double adc1 = trgd->pp2ppADC((StBeamDirection)ew,vh,udio,1);
	  double tac0 = trgd->pp2ppTAC((StBeamDirection)ew,vh,udio,0);
	  double tac1 = trgd->pp2ppTAC((StBeamDirection)ew,vh,udio,1);
          tac_avr[iSeq] = ( tac0 + tac1 )/4.;  
          adc_avr[iSeq] = ( adc0 + adc1 )/2.;  
	  
	  // changed by KY (2015-1-30) : i -> iSeq 
	  contPMT_ADC.h1_P2P[iSeq*2]->Fill( adc0);
	  contPMT_ADC.h1_P2P[iSeq*2+1]->Fill(adc1 );
	  contPMT_TAC.h2_P2P[iSeq]->Fill( tac0, tac1 );

	  contVIP.PMT->SetBinContent( iSeq+1, 1, contPMT_ADC.h1_P2P[iSeq*2]->GetMean()); 
	  contVIP.PMT->SetBinContent( iSeq+1, 2, contPMT_ADC.h1_P2P[iSeq*2+1]->GetMean());
	  i++;
        }
      }
    }
    if ( adc_avr[0]>TAC_TRSHLD && adc_avr[5]>TAC_TRSHLD)          contVIP.VTIM->Fill ( (tac_avr[5]-tac_avr[0]), (tac_avr[0]+tac_avr[5])); 
        else if ( adc_avr[1]>TAC_TRSHLD && adc_avr[4]>TAC_TRSHLD) contVIP.VTIM->Fill ( (tac_avr[4]-tac_avr[1]), (tac_avr[1]+tac_avr[4])); 
        else if ( adc_avr[2]>TAC_TRSHLD && adc_avr[7]>TAC_TRSHLD) contVIP.VTIM->Fill ( (tac_avr[7]-tac_avr[2]), (tac_avr[2]+tac_avr[7])); 
        else if ( adc_avr[3]>TAC_TRSHLD && adc_avr[6]>TAC_TRSHLD) contVIP.VTIM->Fill ( (tac_avr[6]-tac_avr[3]), (tac_avr[3]+tac_avr[6])); 

    // added by KY (2015-3-19) 
    bunch_number = trgd->bunchId7Bit();

    delete trgd;
  }
  //
  // SVX ADCs plots
  //

  //  daq_dta *dd = rdr->det("pp2pp")->get("adc") ;
  // changed by KY (2015-2-13) : read pedestal-subtracted ADC
  daq_dta *dd = rdr->det("pp2pp")->get("adc_ped_sub") ;  

  // added by KY (2015-3-19) 
  int Nhitsum = 0, silicon_bunch ;

  if ( dd ){
    while ( dd->iterate() ){
      pp2pp_t *ds = (pp2pp_t *) dd->Void;  
      u_char rp_id  = ds->seq_id - 1;                // NOTE!  range 1-8
      u_char plane_id = ds->chain_id;
      u_char svx = ds->svx_id;
      // Added by KY (2015-1-30) : Correcting wrong svx_id
      if ( svx == 7 ) svx = 3 ; 

      // added by KY (2015-3-19) 
      silicon_bunch = ds->bunch_xing ;
      double sum = 0.;
      double nch_live = 0.;
	  assert (rp_id >= 0 &&  rp_id < 8 );
	  assert (plane_id >= 0 &&  plane_id < 4);
	  int idh = rp_id*4 + plane_id;
	  if( idh < 0 || idh > 32 ) continue;
	  if ( (plane_id == 0 || plane_id == 2) && (svx < 0 || svx > 3) ) { continue;}
	  if ( (plane_id == 1 || plane_id == 3) && (svx < 0 || svx > 5) ) { continue;}
    
	  for ( int ich=0; ich<PP2PP_SVX_CH; ich++ ){      //  NOTE: channels 0 and 127 are not connected !
	    if ( ds->trace[ich] == 1 ){
	      double chnum = (double)(svx*PP2PP_SVX_CH + ich);
	      double d_adc = double ( ds->adc[ich] );
	    
	  // fill peds profiles
	      contentsSVX.array[idh]->Fill( chnum, d_adc);
              sum += d_adc;
              nch_live += 1.;

	      // added by KY (2015-3-19) 
	      if ( ( silicon_bunch==0 || silicon_bunch>8 ) & ( d_adc > 0 ) )
		Nhitsum++ ;

	    }       
	  }
	  double wt1 = contVIP.SVX->GetBinContent( svx+1, idh+1 );
	  double wt2 = contVIP.SVX_NCH->GetBinContent( svx+1, idh+1 );
	  if ( nch_live > 0.) {
	    double nent1 = (double)mEntriesSVX[svx][idh];
            double nent2 = (double)mEntriesNCH[svx][idh];
	    wt1 = (wt1*nent1 + sum  )/(nent1+nch_live);
	    wt2 = (wt2*nent2 + nch_live/128. )/(nent2+1.);
      	    mEntriesSVX[svx][idh] += (int)nch_live ;
            mEntriesNCH[svx][idh] += 1;
	  }
	  contVIP.SVX->SetBinContent( svx+1, idh+1, wt1 ); 
	  contVIP.SVX_NCH->SetBinContent( svx+1, idh+1, wt2 ); 
    }
  }

  // added by KY (2015-3-19) 
  hitperbunch->Fill(bunch_number, Nhitsum, 1.) ;

}

void ppBuilder::stoprun(daqReader *rdr) {
  LOG("JEFF", "ppBuilder end run #%d",rdr->run);
}

void ppBuilder::main(int argc, char *argv[])
{
  ppBuilder me;
  
  me.Main(argc, argv);
}

