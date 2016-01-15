#include <stdio.h>
#include <stdlib.h>
#include <algorithm>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_TOF/daq_tof.h"
#include <TLatex.h>
#include <TPad.h>
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "tofBuilder.h"
#include <RTS/include/rtsLog.h>
#include <assert.h>

ClassImp(tofBuilder);

int mapTF00X[6][20] = 
{{13,12,11,10,9,8,7,6,5,4,103,104,105,106,107,108,109,110,111,112},
 {3,2,1,60,59,58,57,56,55,54,113,114,115,116,117,118,119,120,61,62},
 {53,52,51,50,49,48,47,46,45,44,63,64,65,66,67,68,69,70,71,72},
 {43,42,41,40,39,38,37,36,35,34,73,74,75,76,77,78,79,80,81,82},
 {33,32,31,30,29,28,27,26,25,24,83,84,85,86,87,88,89,90,91,92},
 {23,22,21,20,19,18,17,16,15,14,93,94,95,96,97,98,99,100,101,102}};
  
tofBuilder::tofBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"tof";

  np = sizeof(contents) / sizeof(TH1 *);
  int npp = sizeof(plots) / sizeof(JevpPlot *);

  assert(np <= npp);

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(&extra, 0, sizeof(extra));

  memset(TOF_L0_trg_labels, 0, sizeof(contents));
  TOF_Error1_label = NULL;
  TOF_Error2_label = NULL;
  TOF_Error3_label = NULL;
  TOF_Error1_list = NULL;
  TOF_Error2_list = NULL;
  TOF_Error3_list = NULL;
  
  hist_maxtofmult_tof	= 2000;
  hist_maxtofmult_trg	= 2000;
  
}

tofBuilder::~tofBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }

  n = sizeof(extra) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(extra.array[i]) delete extra.array[i];
  }
}

void tofBuilder::initialize(int argc, char *argv[]) {

  ReadHistConfig();

  // Initialization of histograms.
  for(int i=0;i<NTRAYS;i++) {
    static char tmpchr[255];
    static char tmpchr1[255];

    sprintf(tmpchr,"TOF_Tray%03d_trg_L0",i+1);
    sprintf(tmpchr1,"Tray %03d L0 (trg)",i+1);
    contents.TOF_L0_trg[i]= new TH1F(tmpchr,tmpchr1,32,-0.5,31.5);

    sprintf(tmpchr,"TOF_Tray%03d_LE_hitmap",i+1);
    sprintf(tmpchr1,"Tray %03d Leading hitmap",i+1);
    contents.TOF_Tray_LEhitmap[i] = new TH1F(tmpchr,tmpchr1,192,-0.5,191.5);
    contents.TOF_Tray_LEhitmap[i]->SetLineColor(2);

    sprintf(tmpchr,"TOF_Tray%03d_TE_hitmap",i+1);
    sprintf(tmpchr1,"Tray %03d Trailing hitmap",i+1);
    extra.TOF_Tray_TEhitmap[i] = new TH1F(tmpchr,tmpchr1,192,-0.5,191.5);
    extra.TOF_Tray_TEhitmap[i]->SetLineColor(4);
  }
  contents.hBunchidShiftVSthub = new TH2F("hBunchidShiftVSthub","hBunchidShiftVSthub",8,0.5,8.5,250,-124.5,125.5);
  
  int nbin_maxtofmult_trg;
  int nbin_maxtofmult_tof;
  if (hist_maxtofmult_trg<250){ nbin_maxtofmult_trg=hist_maxtofmult_trg; }else{ nbin_maxtofmult_trg=hist_maxtofmult_trg/4; } 
  if (hist_maxtofmult_tof<250){ nbin_maxtofmult_tof=hist_maxtofmult_tof; }else{ nbin_maxtofmult_tof=hist_maxtofmult_tof/4; } 
  
  //contents.TOF_L1mult_vs_ZDCadcsum=new TH2F("TOF_L1_vs_ZDCadcsum","TOF_L1_vs_ZDCadcsum",144,0.5,2880.5,150,0,3000);//auau
  contents.TOF_L1mult_vs_ZDCadcsum=new TH2F("TOF_L1_vs_ZDCadcsum","TOF_L1_vs_ZDCadcsum",nbin_maxtofmult_trg,0.,hist_maxtofmult_trg,150,0.,300.5); 
  contents.TOF_L1mult_vs_ZDCadcsum->SetXTitle("TOF Mult in TRG");
  contents.TOF_L1mult_vs_ZDCadcsum->SetYTitle("ZDC hardware adc Sum");

  contents.TOF_L1mult_vs_sumL0=new TH2F("TOF_L1_vs_sumL0","TOF hit mult: sum(TRG) vs sum(TOF)",nbin_maxtofmult_tof,0.,hist_maxtofmult_tof,nbin_maxtofmult_trg,0,hist_maxtofmult_trg);
  contents.TOF_L1mult_vs_sumL0->SetXTitle("TOF Mult in TOF data");
  contents.TOF_L1mult_vs_sumL0->SetYTitle("TOF Mult in TRG data");
  contents.TOF_L1mult=new TH1F("TOF_L1mult","TOFmult: sum(TRG)",nbin_maxtofmult_trg,0.,hist_maxtofmult_trg);
  contents.TOF_L1mult->SetXTitle("TOFmult from TRG data");
  contents.TOF_sumL0=new TH1F("TOF_sumL0","TOFmult: sum(TOF)",nbin_maxtofmult_tof,0.,hist_maxtofmult_tof);
  contents.TOF_sumL0->SetXTitle("TOFmult from TOF data");

  contents.TOF_Error1 = new TH1F("TOF_Error1","TOF electronics errors",244,0.5,122.5);
  contents.TOF_Error1->SetXTitle("Tray #");
  contents.TOF_Error2=new TH1F("TOF_Error2","TOF incorrect bunchid errors",122,0.5,122.5);
  contents.TOF_Error3=new TH1F("TOF_Error3","TOF trays not read out (bunchid not found)",122,0.5,122.5);
  contents.TOF_EventCount=new TH1F("TOF_EventCount","TOF_EventCount",2,0,2);
  contents.TOF_Tray_hits1=new TH1F("TOF_Tray_hits1","TOF Hits by TrayHalf",244,0.5,122.5);
  contents.TOF_Tray_hits2=new TH1F("TOF_Tray_hits2","TOF Hits by TrayHalf",244,0.5,122.5);

  contents.upvpd_hitmap[0] = new TH1F("upvpd_LE_hitmap","upvpd Leading edge hitmap",54,-0.5,53.5);
  contents.upvpd_hitmap[0]->SetXTitle("PMT #");
  contents.upvpd_hitmap[1] = new TH1F("upvpd_TE_hitmap","upvpd Trailing edge hitmap",54,-0.5,53.5);
  contents.upvpd_hitmap[1]->SetXTitle("PMT #");

  contents.upvpd_ToT = new TH2F("upvpd_ToT","upvpd ToT vs PMT#",54,-0.5,53.5,50,0,50);
  contents.upvpd_ToT->SetXTitle("PMT #");

  contents.upvpd_eastT_vs_westT=new TH2F("upvpd_eastT_vs_westT","upvpd eastT vs westT",400,0,51200,400,0,51200);
  contents.upvpd_eastT_vs_westT->SetXTitle("east time (ns)");

  for(int i=0;i<6;i++) {
    static char tmpchr[255];
    static char tmpchr1[255];
    sprintf(tmpchr,"TOF_TF00X%d",i+1);
    sprintf(tmpchr1,"TRG TF00%d TofMult by Tray",i+1);
    contents.TOF_TF00X[i]= new TH2F(tmpchr,tmpchr1,20,-0.5,19.5,32,-0.5,31.5);
  }
  for(int i=0;i<6;i++) {
    for (int it=1;it<=120;it++){
    	int info[2] = {0};
 		GetInfoTF00X(it, info);
 		int itf	= info[0]-1;
 		int ibx	= info[1]+1;
 		//cout<<it<<" "<<itf<<" "<<ibx-1<<" "<<endl;
	    contents.TOF_TF00X[i]->GetXaxis()->LabelsOption("v");
		contents.TOF_TF00X[itf]->GetXaxis()->SetBinLabel(ibx,Form("%d",it));
 		contents.TOF_TF00X[itf]->GetXaxis()->SetLabelSize(0.06);
 		//contents.TOF_TF00X[itf]->GetXaxis()->SetLabelAngle(45);
    }
    //
  }

  //----------------------------------
  int n=0;
  for(n=0;n<NTRAYS;n++) {
    plots[n] = new JevpPlot(contents.TOF_L0_trg[n]);
    plots[n]->optstat = 0;
    plots[n]->logy = 1;
    plots[n]->gridx = 0;
    plots[n]->gridy = 0;
  }
  n--;
  
  //TOF mult plots....
  plots[++n] = new JevpPlot(contents.TOF_L1mult_vs_ZDCadcsum);
  plots[n]->optstat = 0;
  plots[n]->setDrawOpts("colz");
  plots[++n] = new JevpPlot(contents.TOF_L1mult_vs_sumL0);
  plots[n]->optstat = 0;
  plots[n]->setDrawOpts("colz");
  plots[++n] = new JevpPlot(contents.TOF_L1mult);
  plots[n]->optstat = 1;
  plots[n]->logy = 1;
  plots[n]->gridx = 1;
  plots[n]->gridy = 1;
  plots[++n] = new JevpPlot(contents.TOF_sumL0);
  plots[n]->optstat = 1;
  plots[n]->logy = 1;
  plots[n]->gridx = 1;
  plots[n]->gridy = 1;
  
  //error check
  plots[++n] = new JevpPlot(contents.TOF_Error1);
  plots[n]->logy=0;plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  nperror1 = n;
  TOF_Error1_label = new TLatex();
  TOF_Error1_label->SetNDC();
  plots[n]->addElement(TOF_Error1_label);
  TOF_Error1_list = new TLatex();
  TOF_Error1_list->SetNDC();
  plots[n]->addElement(TOF_Error1_list);

  plots[++n] = new JevpPlot(contents.TOF_Error2);
  plots[n]->logy=0;plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  nperror2 = n;
  TOF_Error2_label = new TLatex();
  TOF_Error2_label->SetNDC(); 
  plots[n]->addElement(TOF_Error2_label);
  TOF_Error2_list = new TLatex();
  TOF_Error2_list->SetNDC(); 
  plots[n]->addElement(TOF_Error2_list); 
  
  plots[++n] = new JevpPlot(contents.TOF_Error3);
  plots[n]->logy=0;plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  nperror3 = n;
  TOF_Error3_label = new TLatex();
  TOF_Error3_label->SetNDC();  
  plots[n]->addElement(TOF_Error3_label);
  TOF_Error3_list = new TLatex();
  TOF_Error3_list->SetNDC();  
  plots[n]->addElement(TOF_Error3_list);
  
  //hits vs tray
  plots[++n] = new JevpPlot(contents.TOF_Tray_hits1);
  plots[n]->optstat = 0;
  plots[n]->addHisto(contents.TOF_Tray_hits2);
  plots[n]->optstat = 0;
  plots[n]->logy=1;
  plots[n]->getHisto(0)->histo->SetFillColor(5); plots[n]->optstat = 0;
  plots[n]->getHisto(1)->histo->SetFillColor(7); plots[n]->optstat = 0;  
  
  plots[++n] = new JevpPlot(contents.TOF_EventCount);
  plots[++n] = new JevpPlot(contents.hBunchidShiftVSthub);
  plots[n]->optstat = 1111111;

  for(int i=0;i<NTRAYS;i++) {
    n++;
    plots[n] = new JevpPlot(contents.TOF_Tray_LEhitmap[i]);
    plots[n]->addHisto(extra.TOF_Tray_TEhitmap[i]);
    plots[n]->optstat = 0;
    plots[n]->gridx = 0;
    plots[n]->gridy = 0;
  }

 JLine *ln;
  JLatex *l; 
  l = new JLatex(.25, .12, "THUB1-NW0");
  l->SetTextSize(.035);
  l->SetTextAlign(13);
  l->SetTextAngle(90); 

  plots[++n] = new JevpPlot(contents.upvpd_hitmap[0]);
	  contents.upvpd_hitmap[0]->SetFillColor(19);
	  ln = new JLine(18.5,.1,18.5,.9);
	  ln->SetLineColor(4);
	  plots[n]->addElement(ln);
	  ln = new JLine(37.5,.1,37.5,.9);
	  ln->SetLineColor(4);
	  plots[n]->addElement(ln);
	  l = new JLatex(7.5, .8, "west side");
	  plots[n]->addElement(l);
	  l = new JLatex(26.5, .8, "east side");
	  plots[n]->addElement(l);
	  l = new JLatex(45.5, .8, "PP2PP");
	  plots[n]->addElement(l);
	  plots[n]->optstat = 0;

  plots[++n] = new JevpPlot(contents.upvpd_hitmap[1]);
	  contents.upvpd_hitmap[1]->SetFillColor(19);
	  ln = new JLine(18.5,.1,18.5,.9);
	  ln->SetLineColor(4);
	  plots[n]->addElement(ln);
	  ln = new JLine(37.5,.1,37.5,.9);
	  ln->SetLineColor(4);
	  plots[n]->addElement(ln);
	  l = new JLatex(7.5, .8, "west side");
	  plots[n]->addElement(l);
	  l = new JLatex(26.5, .8, "east side");
	  plots[n]->addElement(l);
	  l = new JLatex(45.5, .8, "PP2PP");
	  plots[n]->addElement(l);
	  plots[n]->optstat = 0;

  plots[++n] = new JevpPlot(contents.upvpd_ToT);
	  plots[n]->setDrawOpts("colz");
	  ln = new JLine(18.5,.1,18.5,.9);
	  ln->SetLineColor(44);
	  plots[n]->addElement(ln);
	  ln = new JLine(37.5,.1,37.5,.9);
	  ln->SetLineColor(44);
	  plots[n]->addElement(ln);
	  l = new JLatex(7.5, .8, "west side");
	  plots[n]->addElement(l);
	  l = new JLatex(26.5, .8, "east side");
	  plots[n]->addElement(l);
	  l = new JLatex(45.5, .8, "PP2PP");
	  plots[n]->addElement(l);
	  plots[n]->optstat = 0;

  plots[++n] = new JevpPlot(contents.upvpd_eastT_vs_westT);
	  plots[n]->setDrawOpts("col");
	  plots[n]->optstat = 0;
 
  plots[n]->optlogz = 1;		// HACK as there's no ->logz option!!!
  for(int i=0;i<6;i++) {
    n++;
    plots[n] = new JevpPlot(contents.TOF_TF00X[i]);
    plots[n]->optstat = 0;
    plots[n]->optlogz = 1;
    plots[n]->gridx = 1;
    plots[n]->gridy = 1;
  }
  plots[n]->optlogz = 0;		// HACK as there's no ->logz option!!!
 
  //---------------------------------------------------
  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
  
  LOG("TOF", "Initialization done.");
}
  
void tofBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "tofBuilder starting run #%d",rdr->run);
  resetAllPlots();

  ReadTrayList();
  ReadValidBunchidPhase();
  ReadTraymaskoutList();

  long TOF_L0_trg_idx = ((long)&contents.TOF_L0_trg[0] - (long)&contents) / 4;

  for(int i=0;i<NTRAYS;i++) {
    LOG(DBG, "tray %d   (idx=%d)",i, TOF_L0_trg_idx);

    if(TOF_L0_trg_labels[i]) {
      // remove element deletes it!
      plots[i+TOF_L0_trg_idx]->removeElement(TOF_L0_trg_labels[i]);
      //delete TOF_L0_trg_labels[i];
      TOF_L0_trg_labels[i] = NULL;
    }

    LOG(DBG, "checking... tray %d",i);

    if(NotActiveTray[i+1]) {
      LOG(DBG, "Tray %d not active",i+1);
      TOF_L0_trg_labels[i] = new TLatex(.2,.8,"Not Active");
      TOF_L0_trg_labels[i]->SetNDC();
      plots[i+TOF_L0_trg_idx]->addElement(TOF_L0_trg_labels[i]);
    }
    else if (MaskoutTray[i+1]) {
      LOG(DBG,"Tray %d masked out\n",i+1);
      TOF_L0_trg_labels[i] = new TLatex(.2,.8,"Masked Out");
      TOF_L0_trg_labels[i]->SetNDC();
      plots[i+TOF_L0_trg_idx]->addElement(TOF_L0_trg_labels[i]);
    } 
    
    LOG(DBG, "checkinged tray %d",i);
    
  }

  // LOG("TOF", "::starrun() done");
}

int tofBuilder::TDIGChan2TINOChan(int tdc,int chan)
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

int tofBuilder::parseData(daqReader *rdr)
{
  //printf("parsedata\n");

  int halftrayid=-1;
  int trayid=-1;
  int bunchid=0;
  memset(tinohit, 0, sizeof(tinohit));
  for(int i=0;i<2;i++) for(int j=0;j<122;j++) allbunchid[i][j] = -9999;

  // Get TOF data...
  daq_dta *dd = rdr->det("tof")->get("legacy");
  tof_t *tof;
  if(dd) {
    while(dd->iterate()) {
      tof = (tof_t *)dd->Void;

      // TOF trigger window per THUB, subject to change anytime.
      //int trigwindowLow[4]={2830,2840,2910,2910};
      //int trigwindowHigh[4]={2910,2920,2990,2990};
//      int trigwindowLowpertray[120]={ //run11
//	2809,2809,2806,2808,2809, 2809,2809,2809,2809,2810, 2809,2809,2809,2809,2809,
//	2809,2808,2809,2809,2808, 2809,2809,2809,2809,2806, 2809,2818,2818,2818,2818,
//	2818,2824,2826,2822,2828, 2826,2827,2825,2827,2819, 2821,2821,2820,2818,2809,
//	2809,2809,2809,2809,2809, 2784,2784,2784,2795,2796, 2797,2797,2797,2809,2809,
//	2884,2884,2871,2873,2872, 2883,2877,2884,2878,2883, 2884,2884,2884,2884,2885,
//	2884,2892,2894,2895,2893, 2894,2893,2892,2894,2884, 2884,2883,2883,2884,2875,
//	2876,2877,2875,2876,2878, 2884,2884,2890,2884,2897, 2897,2897,2897,2896,2897,
//	2896,2896,2894,2896,2897, 2896,2884,2891,2884,2884, 2884,2885,2883,2884,2884
//      };
//      int trigwindowHighpertray[120];
//      for(int iii=0;iii<120;iii++)
//	trigwindowHighpertray[iii]=trigwindowLowpertray[iii]+80;

//       float trigwindowCenterpertray[120]={ //run12 13037090.
//         2910.1,2909.8,2909.4,2910.8,2917.0,2917.3,2917.9,2918.3,2917.3,2918.6,
//         2917.9,2917.7,2916.2,2914.9,2918.4,2917.9,2910.1,2911.2,2911.3,2911.5,
//         2837.2,2837.4,2837.4,2836.0,2835.2,2835.5,2841.8,2844.4,2841.7,2840.9,
//         2842.2,2850.3,2851.0,2849.9,2851.2,2850.4,2851.5,2849.8,2851.3,2843.9,
//         2843.2,2844.2,2843.4,2841.9,2833.3,2834.6,2835.8,2837.2,2836.6,2833.6,
//         2892.3,2895.1,2892.9,2901.6,2901.4,2903.6,2902.7,2902.3,2909.7,2910.3,
//         2904.6,2905.6,2897.0,2897.0,2897.6,2901.6,2899.1,2901.3,2899.5,2902.6,
//         2901.8,2906.5,2909.8,2907.6,2909.5,2906.4,2914.7,2917.3,2917.7,2915.0,
//         2914.2,2914.1,2915.7,2914.9,2905.9,2907.1,2906.5,2907.2,2905.4,2899.4,
//         2899.4,2902.2,2899.2,2899.1,  0.0,2913.2,2913.6,2913.4,2912.6,2921.5,
//         2921.6,  0.0,2920.9,2921.2,2921.0,2920.6,2920.8,2920.6,2920.7,2919.7,
//         2919.2,2912.0,2915.5,2912.2,2911.9,2913.1,2912.3,2902.4,2904.0,2905.5
//       };
//
// now 122 trays... expanded to fix bug in hit pattern plot 3/22/2012 WJL 
//                  (as well as several bugs in the VPD plots)
//       float trigwindowCenterpertray[122]={	 //run12 13082005
// 		2907.9,2908.3,2908.0,2908.4,2915.6,2915.9,2915.2,2915.5,2915.2,2915.7,
// 		2916.0,2915.9,2914.0,2914.2,2916.4,2916.6,2908.8,2908.7,2909.3,2909.2,
// 		2835.9,2835.5,2835.4,2835.0,2833.2,2833.3,2840.7,2843.5,2840.0,2840.0,
// 		2840.3,2848.6,2849.8,2848.4,2849.6,2848.1,2849.6,2848.0,2849.4,2841.9,
// 		2843.0,2842.7,2842.5,2839.5,2832.8,2832.4,2834.7,2835.0,2835.3,2832.7,
// 		2890.6,2893.0,2890.8,2898.6,2899.0,2900.8,2901.0,2900.7,2907.9,2908.2,
// 		2902.7,2902.3,2894.8,2895.3,2895.3,2899.7,2897.8,2899.8,2897.7,2899.8,
// 		2900.0,2905.5,2908.3,2903.9,2906.9,2904.0,2912.6,2914.0,2914.6,2913.3,
// 		2913.0,2913.3,2913.2,2913.2,2904.3,2905.4,2904.8,2904.1,2903.9,2897.7,
// 		2897.9,2898.1,2898.1,2898.4,   0.0,2911.0,2911.1,2911.3,2911.3,2919.8,
// 		2919.9,   0.0,2919.7,2918.4,2918.7,2918.5,2919.8,2918.4,2918.0,2918.4,
// 		2917.8,2910.1,2913.0,2909.8,2910.1,2910.8,2910.2,2901.7,2901.5,2902.4,
// 		259.5,271.7
//       };
 
// run-13 pp500 from 14070026
//       float trigwindowCenterpertray[122]={
// 		2884.6,2883.5,2884.1,2883.8,2891.2,2891.6,2891.0,2892.4,2891.4,2891.6,
// 		2891.4,2891.2,2889.4,2889.6,2892.4,2892.2,2885.0,2884.7,2884.5,2884.8,
// 		2810.9,2811.4,2811.5,2810.6,2808.7,2808.3,2815.9,2818.6,2816.0,2816.0,
// 		2815.9,2824.8,2825.4,2824.0,2825.3,2824.2,2825.3,2824.2,2824.4,2818.5,
// 		2818.6,2819.0,2818.7,2815.1,2807.8,  0.0,2810.8,2810.3,2811.8,2808.6,
// 		2866.6,2869.7,2866.0,2874.8,2874.2,2876.6,2877.1,2876.6,2884.7,2883.6,
// 		2879.2,2877.5,2871.0,2870.1,2870.9,2876.3,2874.2,2875.6,2874.0,2876.2,
// 		2875.9,2881.2,2884.5,2881.2,2883.5,2881.6,2889.4,2890.8,2890.3,2888.6,
// 		2889.0,2890.1,2888.4,2889.1,2880.1,2880.9,2880.1,2880.5,2881.5,2873.3,
// 		2873.3,2874.6,2874.4,2874.2,2874.2,2887.3,2887.1,2887.9,2887.2,2896.1,
// 		2895.5,  0.0,2895.7,2894.3,2894.0,  0.0,2896.1,2896.3,2895.4,2894.5,
// 		2894.2,2885.9,2888.5,2886.3,2886.0,2887.1,2886.6,2878.4,2877.7,2878.1,
// 		234.7,247.1
//       };
//--- 14073012 run13 pp500 after new TCPU code
//         float trigwindowCenterpertray[122]={
// 		2845.0,2844.1,2845.2,2843.0,2851.7,2852.6,2852.9,2852.9,2851.6,2852.9,
// 		2850.9,2851.9,2850.7,2851.3,2853.6,2853.5,2845.3,2845.6,2845.6,2846.0,
// 		2772.0,2771.9,2772.9,2771.4,2769.7,2770.3,2776.7,2780.0,2776.2,2776.9,
// 		2776.6,2786.1,2786.4,2785.4,2786.7,2784.7,2785.9,2785.2,2786.8,2780.0,
// 		2779.3,2778.7,2779.6,2776.7,2770.1,  0.0,2771.4,2772.0,2772.2,2770.1,
// 		2827.3,2828.5,2827.3,2835.6,2835.6,2837.5,2838.2,2838.1,2844.1,2844.9,
// 		2839.2,2839.0,2831.6,2832.2,2832.4,2837.1,2834.9,2837.0,2834.9,2837.3,
// 		2837.5,2842.8,2844.5,2841.6,2844.7,2841.1,2850.2,2851.3,2851.1,2851.5,
// 		2850.7,2850.0,2850.0,2849.0,2841.4,2841.8,2842.4,2841.4,2842.0,2833.5,
// 		2834.2,2834.3,2835.3,2835.1,2835.5,2848.1,2848.5,2848.9,2846.8,2856.8,
// 		2857.1,  0.0,2856.4,2855.6,2855.7,  0.0,2855.7,2856.6,2855.8,2855.6,
// 		2856.1,2846.8,2849.5,2847.2,2846.4,2847.9,2847.4,2839.2,2839.3,2839.3,
// 		195.5,207.5
// 		};
//--- 15048021 run14 auau15
//      float trigwindowCenterpertray[122] = {
//      2938.2,2937.7,2938.5,2939.2,2946.3,2946.6,2945.8,2946.5,2945.4,2945.7,
//      2946.0,2946.2,2944.6,2944.4,2947.1,2946.9,2938.8,2939.5,2939.3,2947.1,
//      2865.5,2878.9,2866.0,2865.5,2863.1,2863.4,2870.9,2873.4,2869.7,2870.5,
//      2870.4,2878.8,2879.9,2878.7,   0.0,2879.2,2879.9,2877.5,2880.1,2872.8,
//      2872.7,2872.8,2873.7,2870.9,2862.8,2863.1,2865.2,2864.8,2864.9,2862.9,
//      2920.5,2923.2,2921.0,2928.7,2928.9,2931.3,2931.5,2931.7,2938.6,2938.5,
//      2935.4,2934.6,2926.8,2927.1,2926.9,2932.2,2929.9,2931.6,2929.5,2932.2,
//      2932.3,2937.6,2940.4,2936.9,2940.0,2937.0,2945.0,2946.8,2946.7,2945.3,
//      2945.0,2945.1,2945.4,2945.6,2936.2,2936.8,2936.7,2936.7,2936.4,2929.5,
//      2929.9,2930.0,2930.3,2929.8,2930.3,2942.7,2942.7,2943.2,2942.8,2951.7,
//      2951.5,   0.0,   0.0,2950.9,2950.6,2950.6,2950.4,2951.5,2950.7,2950.3,
//      2950.7,2942.2,2945.0,2941.9,2942.0,2942.6,2942.2,2934.3,2934.5,2934.8,
//      290.0, 302.0
//      };
//--- 15073033 run14 auau200
//		float trigwindowCenterpertray[122] = {
// 		2955.1,2955.1,2955.3,2955.7,2962.7,2962.8,2962.7,2963.4,2962.3,2962.9,
// 		2962.7,2963.0,2961.2,2961.3,2963.9,2963.6,2956.1,2956.2,2956.2,  0.0,
// 		2882.3,  0.0,2883.1,2882.3,2880.1,2880.4,2887.6,2890.2,2886.8,2887.3,
// 		2887.2,2895.7,2897.2,2895.7,  0.0,2895.6,2897.0,2895.3,2896.9,2889.6,
// 		2889.8,2889.8,2889.6,2887.1,2879.8,2879.9,2882.0,2882.2,2882.7,2879.8,
// 		2937.7,2940.5,2937.7,2946.0,2946.3,2948.4,2948.3,2948.4,2955.2,2955.2,
// 		2950.7,2950.6,2942.3,2942.7,2942.8,2947.8,2945.5,2947.6,2945.5,2947.7,
// 		2947.8,2952.6,2956.1,2952.0,2954.0,2951.9,2960.3,2962.3,2962.2,2960.9,
// 		2960.4,2960.5,2960.4,2960.5,2951.6,2952.0,2951.9,2951.7,2951.7,2945.3,
// 		2945.3,2945.7,2946.1,2945.8,2945.9,2958.8,2958.8,2958.9,2958.9,2967.1,
// 		2967.2,  0.0,  0.0,2966.4,2966.1,2966.2,2966.1,2967.2,2966.4,2966.2,
// 		2966.2,2957.9,2960.4,2958.0,2958.0,2958.4,2958.2,2950.0,2950.0,2950.6,
// 		305.0,314.5
//		};
// 
//--- 16039022 run15 pp200
		float trigwindowCenterpertray[122] = {
		2852.3,2852.9,2853.2,2852.5,2860.3,2859.2,2859.7,2860.9,2860.1,2860.5,
		2860.3,2860.5,2859.3,2859.2,2860.8,2860.9,2853.6,2853.7,2854.4,2853.4,
		2780.1,2779.9,2780.8,2779.7,2776.9,2777.6,2785.6,2787.6,2784.5,2785.0,
		2784.7,2792.9,2794.7,2793.3,2794.2,2793.4,2795.2,  0.0,2793.9,2787.2,
		  0.0,2786.9,2787.1,2784.6,2777.6,2777.1,2778.9,2780.1,2779.0,2776.9,
		2835.2,2837.7,2835.4,2842.9,2843.8,2846.3,2845.7,2846.0,2852.8,2853.3,
		2848.9,2848.4,2840.2,2841.2,2841.3,2845.9,2843.2,2845.2,2843.6,2845.2,
		2844.6,2851.1,2853.4,2849.6,2851.8,2850.5,2858.4,2860.9,2860.6,2859.4,
		2858.5,2859.5,2859.0,2857.9,2850.0,2850.6,2850.3,2850.3,2850.1,2843.4,
		2842.9,2843.5,2844.1,2843.5,2843.8,2857.6,2856.8,2856.3,2856.9,2865.0,
		2865.0,  0.0,2865.2,2865.1,2864.5,2864.5,2863.9,2865.2,2864.8,2863.5,
		2864.3,2855.2,2858.8,2856.1,2855.3,2855.8,2856.7,2848.1,2847.6,2848.8,
		205.0,214.5
		};

      int trigwindowHighpertray[122];
      int trigwindowLowpertray[122];
      int triggeredcrossinghalfwindow = 40;
      for(int iii=0;iii<122;iii++){
      	int center	= trigwindowCenterpertray[iii] + 0.5; 	// convert float to nearest integer "NINT"
		if (iii>=120){ triggeredcrossinghalfwindow = 30; }	// not strictly necessary but better than 40
        trigwindowLowpertray[iii]	= center - triggeredcrossinghalfwindow;
        trigwindowHighpertray[iii]	= center + triggeredcrossinghalfwindow;
      }
			
      for(int ifib=0;ifib<4;ifib++){    
		int ndataword = tof->ddl_words[ifib];
		if(ndataword<=0) continue;
		for(int iword=0;iword<ndataword;iword++){
		  unsigned int dataword = tof->ddl[ifib][iword];
	
		  // error stuff...
		  int packetid = (dataword&0xF0000000)>>28;
		  if(!ValidDataword(packetid)) {
			if(trayid!=46 && trayid!=102 && trayid!=106) {				// IGNORE - should use mask here
			  contents.TOF_Error1->Fill(trayid-0.5+0.5*halftrayid); 
			}
		  }
	
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
	
		  if(trayid < 1 || trayid > 122) continue;
		  if( (dataword&0xF0000000)>>28 == 0x6) {continue;}
		  if( (dataword&0xF0000000)>>28 == 0x2) {
			bunchid=dataword&0xFFF;
			allbunchid[halftrayid][trayid-1] = bunchid;
			continue;  
		  }
		  int edgeid =int( (dataword & 0xf0000000)>>28 );
	
		  int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
		  int tdigboardid=tdcid/4;   // 0-3 for half tray.
		  int tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
		  int globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray
	
		  int timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
		  int time = timeinbin * 25./1024;   // time in ns 
	
		  float trgTime = 25.*bunchid;
		  float timeDiff = time - trgTime;
		  while(timeDiff<0) timeDiff += 51200;
	
		  if((trayid == 121) || (trayid == 122)) {    // handle upvpd...
			int moduleid=-1;
			int modulechan=-1;
			int globalmodulechan=-1;
			moduleid=trayid;
			modulechan=tdcchan2upvpdPMTchan(globaltdcchan,edgeid,trayid);
			if (modulechan<0){
				cout<<"tray="<<trayid<<" tdcid="<<tdcid<<" tdigboardid="<<tdigboardid
					<<" tdcchan="<<tdcchan<<" globch="<<globaltdcchan<<endl;
			}
			globalmodulechan=modulechan;
			if ( timeDiff >= trigwindowLowpertray[trayid-1] 
			  && timeDiff <= trigwindowHighpertray[trayid-1] ){ 
				contents.upvpd_hitmap[edgeid-4]->Fill(modulechan);  
				numberforsort= time+globalmodulechan*1.e5+trayid*1.e8;
				if(edgeid==4) leadinghits.push_back(numberforsort);
				if(edgeid==5) trailinghits.push_back(numberforsort);
			}
		  }
	
		  if(edgeid==4) {
			if((trayid >= 1) && (trayid<=120))
			  contents.TOF_Tray_LEhitmap[trayid-1]->Fill(globaltdcchan);
		  }
	
		  if(edgeid==5) {
			if((trayid >= 1) && (trayid<=120))
			  extra.TOF_Tray_TEhitmap[trayid-1]->Fill(globaltdcchan);
		  }
	
		  if(edgeid !=4) continue;    // leading edge data is enough
	
		  if ( timeDiff<trigwindowLowpertray[trayid-1] 
			|| timeDiff>trigwindowHighpertray[trayid-1] ) continue;
	
		  int atdig = globaltdcchan/24;     // [0,7]
		  int atdcid  = globaltdcchan/8;    // [0,23]    
		  int ahptdcid = atdcid%3;
		  int atdcchan = globaltdcchan%8;
		  int tinoid=TDIGChan2TINOChan(ahptdcid,atdcchan);
		  int tinoidx = atdig*3 + tinoid;
		  
		  if (trayid>=1 && trayid<=120 && tinoidx>=0 && tinoidx<=23){   //WJL
			   tinohit[trayid-1][tinoidx]++; 
		  }                                                             //WJL
		  
		  if((edgeid !=4) && (edgeid != 5)) continue;
		  
		  if(halftrayid==0) contents.TOF_Tray_hits1->Fill(trayid-0.5);
		  if(halftrayid==1) contents.TOF_Tray_hits2->Fill(trayid);
		}  // end loop nword
      }  // end loop fiber
    }
    return 1;
  }
  else {
    return 0;
  }
}

bool tofBuilder::ValidDataword(int packetid)
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

int tofBuilder::ValidBunchid(int trayid,int halftrayid,int bunchid,int refbunchid)
{
  if(trayid<1 || trayid>122) return -1;

  // THUB NW
  int trayinTHUB1[30]={21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
  // THUB NE 
  int trayinTHUB2[30]={66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95};
  // THUB SW
  int trayinTHUB3[30]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,51,52,53,54,55,56,57,58,59,60};
  // THUB SE
  int trayinTHUB4[30]={61,62,63,64,65,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120};

  int nthub=-1;
  int ret=-1;
  for(int itray=0;itray<30;itray++){
    if(trayid == trayinTHUB1[itray]) nthub=0;
    if(trayid == trayinTHUB2[itray]) nthub=1;
    if(trayid == trayinTHUB3[itray]) nthub=2;
    if(trayid == trayinTHUB4[itray]) nthub=3;
  }
  if(trayid == 121) nthub = 4;
  if(trayid == 122) nthub = 5;
  int diff = bunchid - refbunchid;
  if(diff>2048)   {diff =diff-4096;} 
  else if(diff<-2048) {diff =diff+4096;}

  if(trayid>1 && trayid<121){
    if( (diff != mValidShiftTray[0][nthub])  && (diff != mValidShiftTray[1][nthub]) ) ret=nthub;
    contents.hBunchidShiftVSthub->Fill(nthub+1,diff);
  } 
  else if(trayid==121){
    if(diff !=mValidShift121[halftrayid][0] && diff != mValidShift121[halftrayid][1])
      ret=nthub;
    if(halftrayid==0)contents.hBunchidShiftVSthub->Fill(nthub+1,diff);
    if(halftrayid==1)contents.hBunchidShiftVSthub->Fill(nthub+3,diff);
  } 
  else if(trayid==122){
    if(diff !=mValidShift122[halftrayid][0] && diff != mValidShift122[halftrayid][1]) 
      ret=nthub;
    if(halftrayid==0)contents.hBunchidShiftVSthub->Fill(nthub+1,diff);
    if(halftrayid==1)contents.hBunchidShiftVSthub->Fill(nthub+3,diff);
  }

  return ret;
}


void tofBuilder::event(daqReader *rdr)
{

  //printf("event\n");

  // Get Trigger Data...
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  //printf("trgd\n");

  // trigger only histos...
  int npre1=trgd->numberOfPreXing();
  int npost1=trgd->numberOfPostXing();

  float TOF_L1mult		= (float)trgd->tofMultiplicity(0);
  float TOF_L1mult_sum	= 0;
  for( int ipost=-npre1; ipost<npost1+1; ipost++) {
    int prepost=ipost;
    if(prepost != 0) continue;    // only look at prepost =0 data.

    for(int tray=1;tray<=120;tray++){
      int trigger_mult	 = trgd->tofTrayMultiplicity(tray,prepost);
      int info[2] = {0};			// idTF00X, ibinTF00X
      GetInfoTF00X(tray,info);		//
      int idTF00X		 = info[0];	//
      int ixTF00X		 = info[1];	//
      TOF_L1mult_sum	+= trigger_mult;
      if(trigger_mult > 31) trigger_mult=31;
      contents.TOF_L0_trg[tray-1]->Fill(trigger_mult);
      contents.TOF_TF00X[idTF00X-1]->Fill(ixTF00X,trigger_mult);
    }
  }
  //printf("to parsedata\n");

  leadinghits.clear();
  trailinghits.clear();
  int isuccess = parseData(rdr);     //WJL 
  if (isuccess==0){                  //WJL
    //LOG(WARN, "No pointer to TOF data - skipping event");
    if(trgd) delete trgd;            //WJL
    return;                          //WJL
  }                                  //WJL

  int sum_L0_hit=0;
  for(int itray=0;itray<120;itray++){
    //if(Tray_NotInRun(itray+1)) continue;
    // mult. from hits
    int hit_mult=0;
    for(int i=0;i<24;i++){if(tinohit[itray][i]>0) hit_mult++;}
    sum_L0_hit += hit_mult;
  }
  //cout<<TOF_L1mult<<" "<<TOF_L1mult_sum<<endl;

  float zdcHardwaresum = float(trgd->zdcAttenuated(east)) + float(trgd->zdcAttenuated(west));
  contents.TOF_L1mult_vs_ZDCadcsum->Fill(TOF_L1mult, zdcHardwaresum);
  contents.TOF_L1mult_vs_sumL0->Fill(sum_L0_hit, TOF_L1mult);
  contents.TOF_L1mult->Fill(TOF_L1mult);
  contents.TOF_sumL0->Fill(sum_L0_hit);

  contents.TOF_EventCount->Fill(1);

  // check bunch
  int bunchidref1 =   allbunchid[0][mReferenceTray-1];   // bunchid from tray 1 as reference.
  int bunchidref2 =   allbunchid[1][mReferenceTray-1];   // bunchid from tray 1 as reference.
  if(bunchidref1 != bunchidref2) {
    printf("ids %d %d\n",bunchidref1,bunchidref2);
    contents.TOF_Error2->Fill(1);
  }
  
  for(int itray=0;itray<122;itray++){
    int traynum=itray+1;
    if(NotActiveTray[traynum]) continue;
    for(int ihalf=0;ihalf<2;ihalf++){
      int bunchid=allbunchid[ihalf][itray];
      int ret=ValidBunchid(traynum,ihalf,bunchid,bunchidref1);
      if(ret>=0 && bunchid!=-9999) contents.TOF_Error2->Fill(traynum); //real bunchid errors
      if(bunchid==-9999) contents.TOF_Error3->Fill(traynum); //missing bunchid
    }
  }

  char t[256];
  int nev = (int)(contents.TOF_EventCount->GetEntries());
  int err1 = (int)(contents.TOF_Error1->GetEntries());
  int err2 = (int)(contents.TOF_Error2->GetEntries());
  int err3 = (int)(contents.TOF_Error3->GetEntries());
  
  if(err1== 0) {
    sprintf(t, "No electronics errors in %d events",nev);
    TOF_Error1_label->SetTextColor(3);
  }
  else {
    sprintf(t, "%d electronics errors in %d events",err1, nev);
    TOF_Error1_label->SetTextColor(2);
  }
  TOF_Error1_label->SetText(.2,.8,t);

  
  if( err2== 0) {
    sprintf(t, "No incorrrect bunchids in %d events",nev);
    TOF_Error2_label->SetTextColor(3);
  }
  else {
    sprintf(t, "%d incorrect bunchids in %d events!",err2, nev);
    TOF_Error2_label->SetTextColor(2);
  }
  TOF_Error2_label->SetText(.2,.8,t);

  if( err3== 0) {
    sprintf(t, "No read out errors in %d events",nev);
    TOF_Error3_label->SetTextColor(3);
  }
  else {
    sprintf(t, "%d read out errors in %d events!",err3, nev);
    TOF_Error3_label->SetTextColor(2);
  }
  TOF_Error3_label->SetText(.2,.8,t);
 
  std::sort(leadinghits.begin(), leadinghits.end());
  std::sort(trailinghits.begin(), trailinghits.end());

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
    if(ToT>0) contents.upvpd_ToT->Fill(ich,ToT);
  }
  for(int iwest=0;iwest<19;iwest++){
    int ieast=iwest+19;
    if(leadingtime[ieast]*leadingtime[iwest]<1) continue;
    contents.upvpd_eastT_vs_westT->Fill(leadingtime[ieast],leadingtime[iwest]);
  }
  if(contents.upvpd_eastT_vs_westT->GetEntries()==0) contents.upvpd_eastT_vs_westT->Fill(0.,0.); //Geary wants to see this even if it is empty..

  if(trgd) delete trgd;
}

void tofBuilder::GetInfoTF00X(int tray, int* info){
	info[0]	= -1;
	info[1]	= -1;
	for (int itf=0;itf<6;itf++){
		for (int i=0;i<20;i++){
			//cout<<"TF00"<<itf+1<<"  i="<<i<<"   tray="<<mapTF00X[itf][i]<<endl;
			if (tray==mapTF00X[itf][i]){
				info[0]	= itf+1;
				info[1]	= i;
				break;
			}
		}
	}
	if (info[0]<1||info[0]> 6){ cout<<"tofBuilder PROBLEM mapping TF00X ID"<<endl; }
	if (info[1]<0||info[1]>19){ cout<<"tofBuilder PROBLEM mapping TF00X xval"<<endl; }
}



void tofBuilder::stoprun(daqReader *rdr) {
  
  //label error bins
  char tmpchr[255], tmpchr1[255], tmpchr2[255], tmpchr3[255];
  sprintf(tmpchr1, "%s", "List of bad trays: ");
  sprintf(tmpchr2, "%s", "List of bad trays: ");
  sprintf(tmpchr3, "%s", "List of bad trays: ");
  
  int nbad1=0, nbad2=0, nbad3=0;
  for(int traynum=1;traynum<=122;traynum++){
    if(contents.TOF_Error1->GetBinContent(traynum*2-1)>0 || contents.TOF_Error1->GetBinContent(traynum*2)>0){
      nbad1++;
      sprintf(tmpchr, "%d, ", traynum);
      strcat (tmpchr1, tmpchr);
    }
    if(contents.TOF_Error2->GetBinContent(traynum)>0) {
      nbad2++;
      sprintf(tmpchr, "%d, ", traynum);
      strcat (tmpchr2, tmpchr);
    }
    if(contents.TOF_Error3->GetBinContent(traynum)>0) {
      nbad3++; 
      sprintf(tmpchr, "%d, ", traynum); 
      strcat (tmpchr3, tmpchr);
    }
  }
  tmpchr1[strlen(tmpchr1)-2]=0;
  tmpchr2[strlen(tmpchr2)-2]=0;
  tmpchr3[strlen(tmpchr3)-2]=0;
  
  
  if (nbad1>8) {
    sprintf(tmpchr1, "More than 8 bad trays!");
    TOF_Error1_list->SetTextColor(2);
  }
  else if(nbad1<10&&nbad1>0){
    TOF_Error1_list->SetTextColor(2);
  }
  else{
    TOF_Error1_list->SetTextColor(3);
    sprintf(tmpchr1, " ");
  }
  TOF_Error1_list->SetText(.15,.6,tmpchr1);
  
  if (nbad2>8) {
    sprintf(tmpchr2, "More than 8 bad trays!");
    TOF_Error2_list->SetTextColor(2);
  }
  else if(nbad2<10&&nbad2>0){
    TOF_Error2_list->SetTextColor(2);
  }
  else{
    TOF_Error2_list->SetTextColor(3);
    sprintf(tmpchr2," ");
  }
  TOF_Error2_list->SetText(.15,.6,tmpchr2);
  
  if (nbad3>8) {
    sprintf(tmpchr3, "More than 8 bad trays!");
    TOF_Error3_list->SetTextColor(2);
  }
  else if(nbad3<10&&nbad3>0){
    TOF_Error3_list->SetTextColor(2);
  }
  else{
    TOF_Error3_list->SetTextColor(3);
    sprintf(tmpchr3," ");
  }
  TOF_Error3_list->SetText(.15,.6,tmpchr3);
}

void tofBuilder::main(int argc, char *argv[])
{
  tofBuilder me;
  
  me.Main(argc, argv);
}

void tofBuilder::ReadHistConfig(){
  TString buffer;
  char mHistConfigFile[256];
  sprintf(mHistConfigFile, "%s/tof/%s",confdatadir,"TOF_HistConfig.txt");
  ifstream filein(mHistConfigFile); 
  //try local if not in conf dir
  if(!filein) {
    filein.close(); 
    sprintf(mHistConfigFile, "tofconfig/%s","TOF_HistConfig.txt");
    filein.open(mHistConfigFile);
  }
  int count=0;
  if(filein){
    while(!filein.eof()){
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      float number=atof(buffer.Data());
      if (count==0){ 
      	hist_maxtofmult_trg = number; 
        cout<<"====TOF==== Set hist_maxtofmult_trg = "<<hist_maxtofmult_trg<<endl;
      }
      if (count==1){ 
      	hist_maxtofmult_tof = number; 
        cout<<"====TOF==== Set hist_maxtofmult_tof = "<<hist_maxtofmult_tof<<endl;
      }
      count++;
    }
  } else {
      LOG("====TOF====", "Can not open file: TOF_HistConfig.txt");
  }
  filein.close();
}

void tofBuilder::ReadValidBunchidPhase(){
  
  TString buffer;
  char mBunchShiftList[256];
  char mBunchShiftListLocal[256];
  
  sprintf(mBunchShiftList, "%s/tof/%s",confdatadir,"TOF_ValidBunchidPhase.txt");
  ifstream filein(mBunchShiftList);
  
  mReferenceTray=1;
  int count=0;
  
  //try local if not in conf dir
  if(!filein) {
    filein.close(); 
    sprintf(mBunchShiftListLocal, "tofconfig/%s","TOF_ValidBunchidPhase.txt");
    filein.open(mBunchShiftListLocal);
  }

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
  } else
      LOG("====TOF====", "Can not open file: %s or %s", mBunchShiftList, mBunchShiftListLocal);

}


void tofBuilder::ReadTraymaskoutList(){
  
  TString buffer;
  char mTraymaskoutList[256];
  char mTraymaskoutListLocal[256];
  
  sprintf(mTraymaskoutList, "%s/tof/%s",confdatadir,"TOF_TrayMaskout.txt");
  ifstream filein(mTraymaskoutList);
  for(int i=0;i<122;i++){MaskoutTray[i]=false;}
  
  //try local if not in conf dir
  if(!filein) {
    filein.close(); 
    sprintf(mTraymaskoutListLocal, "tofconfig/%s","TOF_TrayMaskout.txt");
    filein.open(mTraymaskoutListLocal);
  }
  
  if(filein){ 
    while(!filein.eof()) {
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      int trayid = atoi(buffer.Data());
      if(trayid<1 || trayid>122) continue;
      MaskoutTray[trayid]=true;
    }   
  } else
    LOG("====TOF====", "Can not open file: %s or %s", mTraymaskoutList, mTraymaskoutListLocal);
  filein.close();

}

void tofBuilder::ReadTrayList(){
  
  TString buffer;
  char mTrayList[256];
  char mTrayListLocal[256];
  sprintf(mTrayList, "%s/tof/%s",confdatadir,"TOF_TrayNotInRun.txt");

  ifstream filein(mTrayList);
  for(int i=0;i<122;i++){NotActiveTray[i]=false;}
  
  //try local if not in conf dir
  if(!filein) {
    filein.close(); 
    sprintf(mTrayListLocal, "tofconfig/%s","TOF_TrayNotInRun.txt");
    filein.open(mTrayListLocal);
  }
  
  if(filein){ 
    while(!filein.eof()) {
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      int trayid = atoi(buffer.Data());
      if(trayid<1 || trayid>122) continue;
      NotActiveTray[trayid]=true;
    }   
  } else
    LOG("====TOF====", "Can not open file: %s or %s", mTrayList, mTrayListLocal);
  filein.close();

}


int tofBuilder::tdcchan2upvpdPMTchan(int globaltdcchan, int edgeid,int trayid)
{

  if(trayid<121 || trayid >122) return -1;
  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

  // run 9 ---->
  //                     1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16 17 18 19
  //int upvpdLEchan[54]={142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,     //west
  //                     142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,     //east
  // 	                 48,64,50,70,0,29,5,96,   48,64,50,70,0,29,5,96};                    //pp2pp 
  //int upvpdTEchan[54]={129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //west
  //                     129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //east
  //	                 63,61,59,57,15,38,14,111,    63,61,59,57,15,38,14,111};             //pp2pp 
  // run10--->
  //                    1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16  17  18 19
  //int upvpdLEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,    //west
  //	 	               5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,    //east
  //		              -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};                   //pp2pp 
  //int upvpdTEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,139,133,48,96,    //west
  //		               5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,139,133,48,96,  //east
  //		                -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};                 //pp2pp 
  // run14--->
  //                     1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16  17  18 19
  //int upvpdLEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,139,133,48,96,  //west
  //			             5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,139,133,48,96,  //east
  //		                -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};                 //pp2pp 
  //int upvpdTEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,139,133,48,96,  //west
  //			             5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,139,133,48,96,  //east
  //		                -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};                 //pp2pp 
  // run15 (15039022 pp200) --->
  //                     1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16  17  18 19
  int upvpdLEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //west
			             5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //east
		                -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};                 //pp2pp 
  int upvpdTEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //west
			             5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //east
		                -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};                 //pp2pp 

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
  //if (pmtLEchan<0) cout<<trayid<<" "<<inputglobalchan<<endl;
  
  int pmtTEchan=-1;
  for(int i=startpoint;i<startpoint+19;i++){
    if(upvpdTEchan[i]==inputglobalchan) {pmtTEchan=i;break;}
  }
  for(int i=38;i<46;i++){
    if(upvpdTEchan[i]==inputglobalchan) {pmtTEchan=i;if(trayid==122)pmtTEchan=pmtTEchan+8;break;}
  }

  if(edgeid==4) pmtchan = pmtLEchan;
  if(edgeid==5) pmtchan = pmtTEchan;

  return pmtchan;
}
