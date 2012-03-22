#include <stdio.h>
#include <stdlib.h>   
#include <algorithm>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
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
  
tofBuilder::tofBuilder(JevpServer *parent) : JevpPlotSet(parent) {
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
    //cout<<"booking "<<i<<"\t"<<contents.TOF_Tray_LEhitmap[i]<<endl;
    contents.TOF_Tray_LEhitmap[i]->SetLineColor(2);

    sprintf(tmpchr,"TOF_Tray%03d_TE_hitmap",i+1);
    sprintf(tmpchr1,"Tray %03d Trailing hitmap",i+1);
    extra.TOF_Tray_TEhitmap[i] = new TH1F(tmpchr,tmpchr1,192,-0.5,191.5);
    extra.TOF_Tray_TEhitmap[i]->SetLineColor(4);
  }
  contents.hBunchidShiftVSthub = new TH2F("hBunchidShiftVSthub","hBunchidShiftVSthub",8,0.5,8.5,250,-124.5,125.5);
  
  //contents.TOF_L1mult_vs_ZDCadcsum=new TH2F("TOF_L1_vs_ZDCadcsum","TOF_L1_vs_ZDCadcsum",144,0.5,2880.5,150,0,3000);//auau
  contents.TOF_L1mult_vs_ZDCadcsum=new TH2F("TOF_L1_vs_ZDCadcsum","TOF_L1_vs_ZDCadcsum",200,0.5,200.5,200,0.5,200.5);//pp200
  contents.TOF_L1mult_vs_ZDCadcsum->SetXTitle("TOF L1 Mult");
  contents.TOF_L1mult_vs_ZDCadcsum->SetYTitle("ZDC hardware adc Sum");

  //contents.TOF_L1mult_vs_sumL0=new TH2F("TOF_L1_vs_sumL0","TOF_L1_vs_sumL0",144,0.5,2880.5,144,0.5,2880.5);//auau
  contents.TOF_L1mult_vs_sumL0=new TH2F("TOF_L1_vs_sumL0","TOF_L1_vs_sumL0",200,0.5,200.5,200,0.5,200.5);//pp
  contents.TOF_L1mult_vs_sumL0->SetXTitle("TOF L1 Mult");
  contents.TOF_L1mult_vs_sumL0->SetYTitle("Sum L0 of hits");


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

  int n=0;
  for(n=0;n<NTRAYS;n++) {
    plots[n] = new JevpPlot(contents.TOF_L0_trg[n]);
    plots[n]->optstat = 0;
    plots[n]->logy = 1;
    plots[n]->gridx = 0;
    plots[n]->gridy = 0;
  }
  n--;
  
  //L1mult
  plots[++n] = new JevpPlot(contents.TOF_L1mult_vs_ZDCadcsum);
  plots[n]->setDrawOpts("colz");
  plots[++n] = new JevpPlot(contents.TOF_L1mult_vs_sumL0);
  plots[n]->setDrawOpts("colz");
  
  //error check
  plots[++n] = new JevpPlot(contents.TOF_Error1);
  plots[n]->logy=0;plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  TOF_Error1_label = new TLatex();
  TOF_Error1_label->SetNDC();
  plots[n]->addElement(TOF_Error1_label);

  plots[++n] = new JevpPlot(contents.TOF_Error2);
  plots[n]->logy=0;plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  TOF_Error2_label = new TLatex();
  TOF_Error2_label->SetNDC(); 
  plots[n]->addElement(TOF_Error2_label); 
  
  plots[++n] = new JevpPlot(contents.TOF_Error3);
  plots[n]->logy=0;plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  TOF_Error3_label = new TLatex();
  TOF_Error3_label->SetNDC();  
  plots[n]->addElement(TOF_Error3_label);
  
  //hits vs tray
  plots[++n] = new JevpPlot(contents.TOF_Tray_hits1);
  plots[n]->addHisto(contents.TOF_Tray_hits2);
  plots[n]->logy=1;
  plots[n]->getHisto(0)->histo->SetFillColor(5);plots[n]->optstat = 0;
  plots[n]->getHisto(1)->histo->SetFillColor(7);plots[n]->optstat = 0;  
  
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
  
  plots[++n] = new JevpPlot(contents.upvpd_hitmap[0]);

  JLine *ln;
  JLatex *l; 
  l = new JLatex(.25, .12, "THUB1-NW0");
  l->SetTextSize(.035);
  l->SetTextAlign(13);
  l->SetTextAngle(90); 

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
      float trigwindowCenterpertray[122]={	 //run12 13082005
		2907.9,2908.3,2908.0,2908.4,2915.6,2915.9,2915.2,2915.5,2915.2,2915.7,
		2916.0,2915.9,2914.0,2914.2,2916.4,2916.6,2908.8,2908.7,2909.3,2909.2,
		2835.9,2835.5,2835.4,2835.0,2833.2,2833.3,2840.7,2843.5,2840.0,2840.0,
		2840.3,2848.6,2849.8,2848.4,2849.6,2848.1,2849.6,2848.0,2849.4,2841.9,
		2843.0,2842.7,2842.5,2839.5,2832.8,2832.4,2834.7,2835.0,2835.3,2832.7,
		2890.6,2893.0,2890.8,2898.6,2899.0,2900.8,2901.0,2900.7,2907.9,2908.2,
		2902.7,2902.3,2894.8,2895.3,2895.3,2899.7,2897.8,2899.8,2897.7,2899.8,
		2900.0,2905.5,2908.3,2903.9,2906.9,2904.0,2912.6,2914.0,2914.6,2913.3,
		2913.0,2913.3,2913.2,2913.2,2904.3,2905.4,2904.8,2904.1,2903.9,2897.7,
		2897.9,2898.1,2898.1,2898.4,  0.0,2911.0,2911.1,2911.3,2911.3,2919.8,
		2919.9,  0.0,2919.7,2918.4,2918.7,2918.5,2919.8,2918.4,2918.0,2918.4,
		2917.8,2910.1,2913.0,2909.8,2910.1,2910.8,2910.2,2901.7,2901.5,2902.4,
		259.5,271.7
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
	    // ignore tray95-0 error until bad HPTDC replaced!!!!! 
      // Contact kefeng.xin@rice.edu
	    if(trayid != 95) {
	      contents.TOF_Error1->Fill(trayid+0.5*halftrayid); 
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
	    globalmodulechan=modulechan;
		if (timeDiff<trigwindowLowpertray[trayid-1] || timeDiff>trigwindowHighpertray[trayid-1]){ 
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

  for( int ipost=-npre1; ipost<npost1+1; ipost++) {
    int prepost=ipost;
    if(prepost != 0) continue;    // only look at prepost =0 data.

    for(int tray=1;tray<=120;tray++){
      int trigger_mult=trgd->tofTrayMultiplicity(tray,prepost);
      if(trigger_mult > 31) trigger_mult=31;
      contents.TOF_L0_trg[tray-1]->Fill(trigger_mult);
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

  float TOF_L1mult = (float)trgd->tofMultiplicity(0);

  contents.TOF_L1mult_vs_ZDCadcsum->Fill(TOF_L1mult, trgd->zdcHardwareSum());
  contents.TOF_L1mult_vs_sumL0->Fill(TOF_L1mult, sum_L0_hit);

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


  if(trgd) delete trgd;
}

void tofBuilder::stoprun(daqReader *rdr) {
}

void tofBuilder::main(int argc, char *argv[])
{
  tofBuilder me;
  
  me.Main(argc, argv);
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

  /* run 9 ---->
//                      1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16 17 18 19
  int upvpdLEchan[54]={142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,  //west
                       142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,  //east
		       48,64,50,70,0,29,5,96,   48,64,50,70,0,29,5,96};                 //pp2pp 
  int upvpdTEchan[54]={129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //west
                       129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //east
		       63,61,59,57,15,38,14,111,    63,61,59,57,15,38,14,111};             //pp2pp 
  */
  // run10--->
//                      1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16  17  18 19
  int upvpdLEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //west
			 5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //east
		        -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};               //pp2pp 
  int upvpdTEchan[54]={  5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //west
			 5, 22, 12,29,46,36,53,70,60,101, 0,24,118,108,125,142,132,48,96,  //east
		        -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1};               //pp2pp 

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
