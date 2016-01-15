#include <stdio.h>
#include <stdlib.h>
#include <algorithm>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_MTD/daq_mtd.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>
#include <TString.h>

#include <math.h>
#include "mtdBuilder.h"
#include <RTS/include/rtsLog.h>

// Backleg lists
const int nTray3bl			=  9;
      int tray3bl[nTray3bl]	= {12,13,14,15,16,17,18,19,20};
const int nTray5bl			= 21;
      int tray5bl[nTray5bl]	= {21,22,23,24,25,26,27,28,29,30,1,2,3,4,5,6,7,8,9,10,11};

int tray[30]			= {1,2, 3, 4, 5, 6, 7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30};

//                   BL        1 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
const int HitmapXbyTray[30] = {1,2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30};
const int TrayToRDO[30]     = {2,2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};

int nGlobalSlot	= nTray3bl*3 + nTray5bl*5;
int ntray		= nTray3bl   + nTray5bl;

ClassImp(mtdBuilder);
  
typedef JevpPlot * ptrJevpPlot;

mtdBuilder::mtdBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"mtd";
  
  int np = sizeof(contents) / sizeof(TH1 *);

  plots = new ptrJevpPlot[np]();

  memset(&contents, 0, sizeof(contents));
  memset(plots, 0, sizeof(JevpPlot *) * np);
}

mtdBuilder::~mtdBuilder() {
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(plots[i]) delete plots[i];
  }
  delete plots;
}

void mtdBuilder::initialize(int argc, char *argv[]) {

  //---- get bunchid offsets...
  //
  ReadValidBunchidPhase();

  //---- get QT board map information...
  //
  for (int i=0;i<nMTDtrig;i++){ isADC[i] = isTAC[i] = 0; }
  SetMtdQTmap();

  //---- build Root Histograms...
  //
  char tmpchr[300];
  char tmpchrt[300];
  
  sprintf(tmpchr,"MTD strips vs BL");
  contents.hMTD_hitmap2D = new TH2F(tmpchr,tmpchr,30,0.5,30.5,120.,0.5,120.5);		// 30 active backlegs
  for (int i=0;i<ntray;i++){
	sprintf(tmpchr, "%d", tray[i]);
  	contents.hMTD_hitmap2D->GetXaxis()->SetBinLabel(i+1,tmpchr);
  }
  
  contents.hMTD_hitmap = new TH1 **[nMTDtrays];
  for(int itray=0;itray<nMTDtrays;itray++){
    contents.hMTD_hitmap[itray] = new TH1*[5];
  }
  for(int i=0;i<nMTDtrays;i++){
    for(int j=0; j<5; j++){
      sprintf(tmpchr, "MTD_tray_%d_position_%d", i+1, j+1);
      contents.hMTD_hitmap[i][j] = new TH1F(tmpchr, tmpchr, 24, 0.5, 24.5);
    }
  }
  
	sprintf(tmpchr,"MTD_Trig2D");
//    contents.hMTD_trig2D = new TH2F(tmpchr,tmpchr,96,-0.5,95.5,60,0,3000);
    contents.hMTD_trig2D     = new TH2F(tmpchr,tmpchr,512,-0.5,511.5,164, 0, 4100);
	sprintf(tmpchr,"MTD_Trig2D_adc");
    contents.hMTD_trig2D_adc = new TH2F(tmpchr,tmpchr,56,0.5,56.5,164, 0, 4100);
	sprintf(tmpchr,"MTD_Trig2D_tac");
    contents.hMTD_trig2D_tac = new TH2F(tmpchr,tmpchr,56,0.5,56.5,164, 0, 4100);
	//
	int kadc,ktac,kadctac;
	TString adctac[2];
			adctac[0] = TString("ADC");
			adctac[1] = TString("TAC");
	TString names[nMTDtrig];
 	for (int i=0;i<nMTDtrig;i++){
 		//
 		kadc = ktac = 0; kadctac = -1;
 		if (isADC[i]){ kadc=isADC[i]; }else{ ktac=isTAC[i]; }
 		if (kadc){ kadctac=0; } else if (ktac){ kadctac=1; }
		if (kadctac>=0){
	 		sprintf(tmpchrt,"%s %s %s",QTboard[i].Data(),adctac[kadctac].Data(),QTchanstring[i].Data()); 
	 		//cout<<i<<" "<<tmpchrt<<endl;
	 		names[i] = TString(tmpchrt);
	 	} else {
	 		names[i] = TString("no QT connection");
	 	}
 		//
 	}
    contents.hMTD_trig = new TH1 *[nMTDtrig];
	for (int i=0;i<nMTDtrig;i++){
		sprintf(tmpchr,"MTD_Trig%d",i);
		contents.hMTD_trig[i] = new TH1F(tmpchr, names[i].Data(), 164, 0, 4100);
	}

  // Run12 MTD trigger
  // keep the same format as the trigger experts'
// 
//   // for backleg 27-2
//   sprintf(tmpchr,"MTD_Trigger27_2_outadc");contents.hMTD_trig[0] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_2_outtdc");contents.hMTD_trig[1] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_2_inadc"); contents.hMTD_trig[2] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_2_intdc"); contents.hMTD_trig[3] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   
//   //for backleg 27-3
//   sprintf(tmpchr,"MTD_Trigger27_3_outadc");contents.hMTD_trig[4] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_3_outtdc");contents.hMTD_trig[5] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_3_inadc"); contents.hMTD_trig[6] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_3_intdc"); contents.hMTD_trig[7] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   
//   //for backleg 27-4
//   sprintf(tmpchr,"MTD_Trigger27_4_outadc");contents.hMTD_trig[8] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_4_outtdc");contents.hMTD_trig[9] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_4_inadc"); contents.hMTD_trig[10] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_4_intdc"); contents.hMTD_trig[11] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   
//   // for backleg 27-1
//   sprintf(tmpchr,"MTD_Trigger27_1_outadc");contents.hMTD_trig[12] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_1_outtdc");contents.hMTD_trig[13] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_1_inadc"); contents.hMTD_trig[14] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_1_intdc"); contents.hMTD_trig[15] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   
//   //for backleg 27-5
//   sprintf(tmpchr,"MTD_Trigger27_5_outadc");contents.hMTD_trig[16] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_5_outtdc");contents.hMTD_trig[17] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_5_inadc"); contents.hMTD_trig[18] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
//   sprintf(tmpchr,"MTD_Trigger27_5_intdc"); contents.hMTD_trig[19] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);

  //mtd bunchid
  contents.MTD_bunchid = new TH2F("MTD_bunchid", "MTD_bunchid", 30, 0.5, 30.5, 30, -14.5, 15.5);

  //mtd error check
  //
  sprintf(tmpchr,"MTD_Error1");contents.MTD_Error1 = new TH1F(tmpchr, "MTD electronics errors (by RDO)", 2, 0.5, 2.5);
  //contents.MTD_Error1->GetXaxis()->SetBinLabel(1, "THUB1");  contents.MTD_Error1->GetXaxis()->SetBinLabel(2, "THUB2");
  //
  sprintf(tmpchr,"MTD_Error2");contents.MTD_Error2 = new TH1F(tmpchr, "MTD incorrect bunchid errors (by Tray)", 30, 0.5, 30.5);
  //for(int i=0;i<ntray;i++){
  //  sprintf(tmpchr,"Tray%d", tray[i]); 
  //  contents.MTD_Error2->GetXaxis()->SetBinLabel(i+1, tmpchr); 
  //}
  //
  sprintf(tmpchr,"MTD_Error3");contents.MTD_Error3 = new TH1F(tmpchr, "MTD trays not read out (bunchid not found)", 30, 0.5, 30.5);
  //for(int i=0;i<ntray;i++){
  //  sprintf(tmpchr,"Tray%d", tray[i]); 
  //  contents.MTD_Error3->GetXaxis()->SetBinLabel(i+1, tmpchr); 
  //}
  
//  contents.MTD_Tray_hits=new TH1F("MTD_Tray_hits","MTD Hits by Tray",150,0.5,150.5);
  contents.MTD_Tray_hits=new TH1F("MTD_Tray_hits","MTD Hits by Tray",150,0.5,150.5);
  contents.MTD_Tray_hitsEvenOdd=new TH1F("MTD_Tray_hitsEvenOdd","MTD Hits by Tray",150,0.5,150.5);
  contents.MTD_Tray_hitsBinEven=new TH1F("MTD_Tray_hitsBinEven","MTD Hits by Tray",150,0.5,150.5);
  
//  int i=0;
//   for (unsigned int itray3bl=0; itray3bl<nTray3bl; itray3bl++) {
//     int val3tray=tray3bl[itray3bl];
//     for(int islot=2;islot<=4;islot++){
//       sprintf(tmpchr, "Tray%d-%d", val3tray, islot);
//       contents.MTD_Tray_hits->GetXaxis()->SetBinLabel(++i, tmpchr);
//     }
//   }
//   for (unsigned int itray5bl=0; itray5bl<nTray5bl; itray5bl++) {
//     int val5tray=tray5bl[itray5bl];
//     for(int islot=1;islot<=5;islot++){
//       sprintf(tmpchr, "Tray%d-%d", val5tray, islot);
//       contents.MTD_Tray_hits->GetXaxis()->SetBinLabel(++i, tmpchr);
//     }
//   }
  
  //Counter
  contents.MTD_EventCount=new TH1F("MTD_EventCount","MTD_Events by RDO number",2,0.5,2.5);

  // Add root histograms to Plots
  LOG("====MTD====", "Adding Plots...........................");
  JevpPlot *plots[300];				// was 200
  int nhhit=0;
  int nhtrig=0;
  int n=0;

  JLatex *latexW, *latexE;
  latexW = new JLatex(4., 0.8, "West");
  latexE = new JLatex(16., 0.8, "East");
  JLine *ln = new JLine(12.5, 0.1, 12.5, 0.9 );
  		 ln -> SetLineColor(4);

  TLatex *qtid[56];						// Run-14 - 56 connections to TRG.
  int kadctacind;
  //cout<<"nMTDtrig="<<nMTDtrig<<endl;
  for (int i=0;i<nMTDtrig;i++){
    kadctacind	= 0;
	if (isADC[i]){ kadctacind=isADC[i]; }else if (isTAC[i]){ kadctacind=isTAC[i]; }
	if (kadctacind){		
		sprintf(tmpchr,"%s %s",QTboard[i].Data(),QTchanstring[i].Data());
		//cout<<kadctacind<<" "<<tmpchr<<endl;
		qtid[kadctacind-1]	= new TLatex(kadctacind+0.3,3000.,tmpchr);
		qtid[kadctacind-1]->SetTextAngle(90);
		qtid[kadctacind-1]->SetTextSize(0.02);
	}
  }
  TLine *qtlines[3];
  for (int i=0;i<3;i++){
  	float xval = 16.5+14.*i; 						// Run-14 config....
  		if (i==2){ xval-=2.; }						// Run-14 config....
	qtlines[i] = new TLine(xval,0.,xval,4095.);		// Run-14 config....
	qtlines[i]->SetLineColor(1);
	qtlines[i]->SetLineWidth(1);
  }
  
  plots[nhhit++] = new JevpPlot(contents.hMTD_hitmap2D);
  //
  for (int itray3bl=0; itray3bl<nTray3bl; itray3bl++) {
    int val3tray=tray3bl[itray3bl];
    for(int islot=2;islot<=4;islot++){
      plots[nhhit++] = new JevpPlot(contents.hMTD_hitmap[val3tray-1][islot-1]);
    }
  }  
  for (int itray5bl=0; itray5bl<nTray5bl; itray5bl++) {
    int val5tray=tray5bl[itray5bl];
    for(int islot=1;islot<=5;islot++){
      plots[nhhit++] = new JevpPlot(contents.hMTD_hitmap[val5tray-1][islot-1]);
    }
  }
  LOG("====MTD====", "n=%d nhhit=%d nhtrig=%d", nhhit+nhtrig, nhhit, nhtrig);
  //
  plots[nhhit+(nhtrig++)] = new JevpPlot(contents.hMTD_trig2D);
  plots[nhhit+(nhtrig++)] = new JevpPlot(contents.hMTD_trig2D_adc);
  plots[nhhit+(nhtrig++)] = new JevpPlot(contents.hMTD_trig2D_tac);
  for (int i=0; i<nMTDtrig; i++) {
    plots[nhhit+(nhtrig++)] = new JevpPlot(contents.hMTD_trig[i]);
  }
  LOG("====MTD====", "n=%d nhhit=%d nhtrig=%d", nhhit+nhtrig, nhhit, nhtrig);

  // Add Plots to plot set...
  //
  for(int i=0;i<nhhit;i++) {		// 1D hitmaps are after the 2D hitmap		
    //LOG("MTD", "Adding plot %d",i);
    addPlot(plots[i]);
	plots[i]->gridx = 0;
	plots[i]->gridy = 0;
    if (i==0){
		plots[i]->optstat=0;
	} else {
		plots[i]->getHisto(0)->histo->SetFillColor(19);
		plots[i]->getHisto(0)->histo->SetMinimum(0);
		plots[i]->getHisto(0)->histo->SetFillStyle(1001);
		plots[i]->optstat=10;
		plots[i]->addElement(latexW);
		plots[i]->addElement(latexE);
		plots[i]->addElement(ln);
    }
  }
  LOG("====MTD====", "%d hitmap plots added",nhhit);
  //
  for(int i=nhhit;i<nhhit+nhtrig;i++) {
    //LOG("MTD", "Adding plot %d",i);
    addPlot(plots[i]);
	plots[i]->gridx = 0;
	plots[i]->gridy = 0;
	if (i>=nhhit&&i<nhhit+3){				// TRG data 2D plots...
		plots[i]->optstat=0;
		if (i==nhhit+1||i==nhhit+2){
			for (int j=0;j<56;j++){			// Run-14 56 trg connections...
				plots[i]->addElement(qtid[j]);
			}
			for (int j=0;j<3;j++){			// Run-14 4 QT boards...
				//cout<<qtlines[j]<<endl;
				plots[i]->addElement(qtlines[j]);
			}
		}
	} else {								// TRG data 1D plots...
		plots[i]->getHisto(0)->histo->SetFillColor(19);
		plots[i]->getHisto(0)->histo->SetMinimum(0);
		plots[i]->getHisto(0)->histo->SetFillStyle(1001);
		plots[i]->logy		= 1;
		plots[i]->optstat	= 10;
	}
  }
  LOG("====MTD====", "%d trig plots added", nhtrig);

  n = nhhit+nhtrig-1;
  plots[++n] = new JevpPlot(contents.MTD_bunchid);
  //LOG("MTD", "Adding plot %d",n);
  addPlot(plots[n]);
  plots[n]->setDrawOpts("colz");
  plots[n]->optstat=1111111;
  LOG("====MTD====", "mtd bunchid plots added %d",n);
  
  plots[++n] = new JevpPlot(contents.MTD_Error1);
  //LOG("MTD", "Adding plot %d",n);
  addPlot(plots[n]);
  plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  MTD_Error1_label = new TLatex();
  MTD_Error1_label->SetNDC(); 
  plots[n]->addElement(MTD_Error1_label);
  LOG("====MTD====", "MTD_Error1 added %d",n);
  
  plots[++n] = new JevpPlot(contents.MTD_Error2);
  //LOG("MTD", "Adding plot %d",n);
  addPlot(plots[n]);
  plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  MTD_Error2_label = new TLatex();
  MTD_Error2_label->SetNDC(); 
  plots[n]->addElement(MTD_Error2_label);
  LOG("====MTD====", "MTD_Error2 added %d",n);
  
  plots[++n] = new JevpPlot(contents.MTD_Error3);
  //LOG("MTD", "Adding plot %d",n);
  addPlot(plots[n]);
  plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 1111111;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  MTD_Error3_label = new TLatex();
  MTD_Error3_label->SetNDC(); 
  plots[n]->addElement(MTD_Error3_label);
  LOG("====MTD====", "MTD_Error3 added %d",n);
  
  plots[++n] = new JevpPlot(contents.MTD_EventCount);
  //LOG("MTD", "Adding plot %d",n);
  addPlot(plots[n]);
  plots[n]->optstat=1111111;
  plots[n]->getHisto(0)->histo->SetFillColor(19);
  plots[n]->getHisto(0)->histo->SetFillStyle(1001);
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  LOG("====MTD====", "MTD_EventCount added %d",n);
  
  plots[++n] = new JevpPlot(contents.MTD_Tray_hits);
  //LOG("MTD", "Adding plot %d",n);
  addPlot(plots[n]);
  plots[n]->optstat=0;
  plots[n]->gridx = 0;
  plots[n]->gridy = 0;
  plots[n]->logy=1;
  plots[n]->getHisto(0)->histo->SetFillColor(18);
//  JLine *vlines[30];								// Run-13, 15 backlegs
//  for (int i=0;i<30;i++){			
// 	vlines[i] = new JLine(5.5+i*5,1.,5.5+i*5,100000);
//	vlines[i]->SetLineColor(3);
//	plots[n]->addElement(vlines[i]);
//  }
  LOG("====MTD====", "MTD_Tray_hits added %d",n);

  plots[++n] = new JevpPlot(contents.MTD_Tray_hitsEvenOdd);
  //LOG("MTD", "Adding plot %d",n);
  addPlot(plots[n]);
  plots[n]->optstat=0;
  plots[n]->addHisto(contents.MTD_Tray_hitsBinEven);
  plots[n]->gridx = 0;
  plots[n]->gridy = 0;
  plots[n]->logy=1;
  plots[n]->getHisto(0)->histo->SetFillColor(7);
  plots[n]->getHisto(1)->histo->SetFillColor(5);
  plots[n]->getHisto(0)->histo->SetMinimum(0.5);
  plots[n]->getHisto(1)->histo->SetMinimum(0.5);
  for (int i=0;i<30;i++){
	sprintf(tmpchr, "%d", tray[i]);
  	MTD_BL_label[i]=new TLatex(3.+i*5,1.1,tmpchr);
  	MTD_BL_label[i]->SetTextAlign(21);
  	MTD_BL_label[i]->SetTextSize(0.03);
    plots[n]->addElement(MTD_BL_label[i]);
  }
  LOG("====MTD====", "MTD_Tray_hits(odd/even) added %d",n);
  
  LOG("====MTD====", "MTD initialization done");
}
  
void mtdBuilder::startrun(daqReader *rdr) {
  //
  LOG("MTD", "TriggerPlotBuilder starting run #%d",rdr->run);
  resetAllPlots();
  //
  ReadTraymaskoutList();
  //
}
void mtdBuilder::ReadTraymaskoutList(){
  //
  TString buffer;
  char mTraymaskoutList[256];
  char mTraymaskoutListLocal[256];
  //
  for(int i=0;i<30;i++){MaskoutTray[i]=false;}
  //
  sprintf(mTraymaskoutList, "%s/mtd/%s",confdatadir,"MTD_TrayMaskout.txt");
  ifstream filein(mTraymaskoutList);
  //  
  //try local if not in conf dir
  if(!filein) {
    filein.close(); 
    sprintf(mTraymaskoutListLocal, "mtdconfig/%s","MTD_TrayMaskout.txt");
    filein.open(mTraymaskoutListLocal);
  }
  if(filein){ 
    while(!filein.eof()) {
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      int trayid = atoi(buffer.Data());
      if(trayid<1 || trayid>30) continue;
      LOG("====MTD====", "...Masking out Backleg %d...", trayid);
      MaskoutTray[trayid]=true;
    }   
  } else {
    LOG("====MTD====", "Can not open file: %s or %s", mTraymaskoutList, mTraymaskoutListLocal);
  }
  filein.close();
  //
}

void mtdBuilder::event(daqReader *rdr) {
  int timeinbin=0;
  float time=0.;
  int halftrayid=-1;
  int trayid=-1;
  int bunchid=0;
  leadinghits.clear();
  trailinghits.clear();
  int allbunchid[2][30];
  for(int i=0;i<2;i++){ for(int j=0;j<30;j++){ allbunchid[i][j] = -9999; }}
	
  daq_dta *dd = rdr->det("mtd")->get("legacy");
  mtd_t *mtd;
  if (!dd){		//WJL ...pointer to mtd data not found
  	return;
  } else {
    while(dd->iterate()) {
      mtd = (mtd_t *)dd->Void;
	  for (int ifib=0;ifib<2;ifib++){				// THUB-S is fiber 0, THUB-N is fiber 1
		  int ndataword = mtd->ddl_words[ifib];    
		  if(ndataword<=0) continue;
		  contents.MTD_EventCount->Fill(ifib+1);
		  for(int iword=0;iword<ndataword;iword++){
			int dataword=mtd->ddl[ifib][iword];
						
			int packetid = (dataword&0xF0000000)>>28;
			if(!ValidDataword(packetid)){ contents.MTD_Error1->Fill(ifib); }
		
		//	if( (dataword&0xF0000000)>>28 == 0x2) continue;  //TDC header, moved to later
			if( (dataword&0xF0000000)>>28 == 0xD) continue;  //Header tag
			if( (dataword&0xF0000000)>>28 == 0xE) continue;  //TDIG Separator
			if( (dataword&0xF0000000)>>28 == 0xA) {  // header trigger data flag
			  // do nothing at this moment.
			  continue;
			}
						
			// geographical data words for tray number.
			if( (dataword&0xF0000000)>>28 == 0xC) { //Geographical Data
			  halftrayid = dataword&0x01;    
			  trayid     = (dataword&0x0FE)>>1;
			  continue;
			}
		
			if(!istray3bl(trayid) && !istray5bl(trayid)) continue;
						
			if( (dataword&0xF0000000)>>28 == 0x6) {continue;} //error
		
			if( (dataword&0xF0000000)>>28 == 0x2) {
			  bunchid=dataword&0xFFF;
			  allbunchid[halftrayid][trayid-1] = bunchid;
			  continue;  
			}
		
			int edgeid =int( (dataword & 0xf0000000)>>28 );
			//if((edgeid !=4) && (edgeid!=5)) continue; //leading edge or trailing edge
			if (edgeid != 4) continue; //kx: plot LE only. Requested by Bill Llope
						
			int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
			int tdigboardid= ( (tdcid & 0xC) >> 2) + halftrayid*4;
			int tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
			//int globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray
			timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
			time = timeinbin * 25./1024;   // time in ns 
						
			//int moduleid=-1;
			int globalstripid=-1;
			int stripid=-1;
			int zendid=-1;
			int slot=-1;
			//				
			if(trayid){
			  globalstripid=tdcchan2globalstrip(tdigboardid,tdcid,tdcchan);
			  stripid=(globalstripid-1)%12+1;
			  zendid=(globalstripid-1)/12; //0 for Lo Z end; 1 for Hi Z end
			  slot=tdig2slot(tdigboardid, trayid);
			}				
		
			if( istray3bl(trayid) && (slot<2||slot>4) ) continue;
			if( istray5bl(trayid) && (slot<1||slot>5) ) continue;
			if(!istray3bl(trayid) && !istray5bl(trayid)) continue;
			
//			contents.hMTD_hitmap2D->Fill(trayid,24*(slot-1)+globalstripid);
			contents.hMTD_hitmap2D->Fill(HitmapXbyTray[trayid-1],24*(slot-1)+globalstripid);
			contents.hMTD_hitmap[trayid-1][slot-1]->Fill(globalstripid);        
//			contents.MTD_Tray_hits->Fill(iGlobalSlot(trayid,slot));
			int ntrayonbl	= 5;
			//if (trayid>=12&&trayid<=20) ntrayonbl = 3;
			contents.MTD_Tray_hits->Fill(ntrayonbl*(HitmapXbyTray[trayid-1]-1)+slot);
			if (HitmapXbyTray[trayid-1]%2==0){
				contents.MTD_Tray_hitsBinEven->Fill(ntrayonbl*(HitmapXbyTray[trayid-1]-1)+slot);
			} else {
				contents.MTD_Tray_hitsEvenOdd->Fill(ntrayonbl*(HitmapXbyTray[trayid-1]-1)+slot);
			}
		  }  // end loop nword
      } // end loop over fibers
    } //end dd iterate
  } //end if dd
	  
  //check bunchid
//  int mReferenceTray=26;				// now read from config file
  int bunchidref1 	= allbunchid[0][mReferenceTray-1];
  int bunchidref2 	= allbunchid[1][mReferenceTray-1];
  int diff 			= bunchidref2-bunchidref1;
  if (diff> 2048){ diff = diff-4096; } else 
  if (diff<-2048){ diff = diff+4096; }
  
  contents.MTD_bunchid->Fill(mReferenceTray, diff);
  if(bunchidref2!=-9999 && diff) contents.MTD_Error2->Fill(mReferenceTray);
  if(bunchidref1==-9999 || bunchidref2==-9999) contents.MTD_Error3->Fill(mReferenceTray);

  int BunchIdError	= 1;
  for(int ihalf=0; ihalf<2; ihalf++){
    for(int i=0; i<ntray; i++){
	  int traynum	= tray[i]; 
	  if (traynum != mReferenceTray){		// skip reference tray here...
		  int itray		= traynum-1;
          int irdo		= TrayToRDO[itray] - 1;
		  diff 			= allbunchid[ihalf][itray]-allbunchid[0][mReferenceTray-1];
		  if (diff>= 2048){ diff -= 4096; }
		  if (diff<=-2048){ diff += 4096; }
		  //
		  BunchIdError		= 0;
		  if (diff!=mValidShiftTray[0][irdo]&&diff!=mValidShiftTray[1][irdo]){
			  BunchIdError	= 1;
		  }
          //
          if (MaskoutTray[traynum]) continue;
          //
		  if (BunchIdError){
		    LOG("====MTD====","bunchid error or not found ... tray=%d   ref=%d,%d   bunchid=%d   diff=%d",
		  			traynum,allbunchid[0][mReferenceTray-1],allbunchid[1][mReferenceTray-1],
		  			allbunchid[ihalf][itray],diff);
		  }
		  //
		  if(allbunchid[ihalf][itray]!=-9999)                 contents.MTD_bunchid->Fill(traynum, diff);
		  if(allbunchid[ihalf][itray]!=-9999 && BunchIdError) contents.MTD_Error2->Fill(traynum); 	// real bunchid errors
		  if(allbunchid[ihalf][itray]==-9999)                 contents.MTD_Error3->Fill(traynum); 	// missing bunchids
      }
    }
  }
  
  //painting label.. //can move to end of run
  char t[256];
  int nev = (int)(contents.MTD_EventCount->GetEntries());
  int err1 = (int)(contents.MTD_Error1->GetEntries());
  int err2 = (int)(contents.MTD_Error2->GetEntries());
  int err3 = (int)(contents.MTD_Error3->GetEntries());
  
  //error1 label
  if(err1== 0) {
    sprintf(t, "No electronics errors in %d events",nev);
    MTD_Error1_label->SetTextColor(3);
  }
  else {
    sprintf(t, "%d electronics errors in %d events",err1, nev);
    MTD_Error1_label->SetTextColor(2);
  }
  MTD_Error1_label->SetText(.2,.8,t);
  
  //error2 label
  if( err2== 0) {
    sprintf(t, "No incorrrect bunchids in %d events",nev);
    MTD_Error2_label->SetTextColor(3);
  }
  else {
    sprintf(t, "%d incorrect bunchids in %d events!",err2, nev);
    MTD_Error2_label->SetTextColor(2);
  }
  MTD_Error2_label->SetText(.2,.8,t);
  
  //error3 label
  if( err3== 0) {
    sprintf(t, "No read out errors in %d events",nev);
    MTD_Error3_label->SetTextColor(3);
  }
  else {
    sprintf(t, "%d read out errors in %d events!",err3, nev);
    MTD_Error3_label->SetTextColor(2);
  }
  MTD_Error3_label->SetText(.2,.8,t);
  
  // now do the trigger plots...
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) {
    //LOG(WARN, "No trigger data");	//WJL should not be a warning. perfectly normal for some configs
    return;
  }

  //MTD trigger Run12
  // for backleg 27-2
//   float outadc0= trgd->mtdAtAddress(8, 0); if(outadc0) contents.hMTD_trig[0]->Fill(outadc0);
//   float outtdc0= trgd->mtdAtAddress(12, 0); if(outtdc0) contents.hMTD_trig[1]->Fill(outtdc0);
//   float inadc0= trgd->mtdAtAddress(9, 0); if(inadc0) contents.hMTD_trig[2]->Fill(inadc0);
//   float intdc0= trgd->mtdAtAddress(13, 0); if(intdc0) contents.hMTD_trig[3]->Fill(intdc0);
//   
//   //for backleg 27-3
//   float outadc1= trgd->mtdAtAddress(16, 0); if(outadc1) contents.hMTD_trig[4]->Fill(outadc1);
//   float outtdc1= trgd->mtdAtAddress(20, 0); if(outtdc1) contents.hMTD_trig[5]->Fill(outtdc1);
//   float inadc1= trgd->mtdAtAddress(17, 0); if(inadc1) contents.hMTD_trig[6]->Fill(inadc1);
//   float intdc1= trgd->mtdAtAddress(21, 0); if(intdc1) contents.hMTD_trig[7]->Fill(intdc1);
//   
//   //for backleg 27-4
//   float outadc2= trgd->mtdAtAddress(24, 0); if(outadc2) contents.hMTD_trig[8]->Fill(outadc2);
//   float outtdc2= trgd->mtdAtAddress(28, 0); if(outtdc2) contents.hMTD_trig[9]->Fill(outtdc2);
//   float inadc2= trgd->mtdAtAddress(25, 0); if(inadc2) contents.hMTD_trig[10]->Fill(inadc2);
//   float intdc2= trgd->mtdAtAddress(29, 0);  if(intdc2) contents.hMTD_trig[11]->Fill(intdc2);
//   
//   // for backleg 27-1
//   float outadc3= trgd->mtdgemAtAddress(8, 0); if(outadc3) contents.hMTD_trig[12]->Fill(outadc3);
//   float outtdc3= trgd->mtdgemAtAddress(12, 0); if(outtdc3) contents.hMTD_trig[13]->Fill(outtdc3);
//   float inadc3= trgd->mtdgemAtAddress(9, 0); if(inadc3) contents.hMTD_trig[14]->Fill(inadc3);
//   float intdc3= trgd->mtdgemAtAddress(13, 0); if(intdc3) contents.hMTD_trig[15]->Fill(intdc3);
//   
//   //for backleg 27-5
//   float outadc4= trgd->mtdgemAtAddress(16, 0); if(outadc4) contents.hMTD_trig[16]->Fill(outadc4);
//   float outtdc4= trgd->mtdgemAtAddress(20, 0); if(outtdc4) contents.hMTD_trig[17]->Fill(outtdc4);
//   float inadc4= trgd->mtdgemAtAddress(17, 0); if(inadc4) contents.hMTD_trig[18]->Fill(inadc4);
//   float intdc4= trgd->mtdgemAtAddress(21, 0); if(intdc4) contents.hMTD_trig[19]->Fill(intdc4);

//	mtdAtAddress(ich,iprepost=0) -> mxq[iprepost=0][0][ich]			ich=[0,31]
//	mtdgemAtAddress(ich,iprepost=0) -> mxq[iprepost=0][10][ich]		ich=[0,31]

 	const int nslots 	= 4;
 	int slots[nslots]	= {0,10,12,14};
 	int mh				= 0;
 	int kbin;
 	for (int kslot=0;kslot<nslots;kslot++){
 		int islot		= slots[kslot];
 		for (int iaddr=0;iaddr<32;iaddr++){
 			int kh		= islot*32 + iaddr;
 			if (trgd->mxqAtSlotAddress(iaddr,0,islot)){
 				contents.hMTD_trig2D->Fill(kh,trgd->mxqAtSlotAddress(iaddr,0,islot)); 				
 				contents.hMTD_trig[mh]->Fill(trgd->mxqAtSlotAddress(iaddr,0,islot));
 				if (isADC[mh]){
 					kbin	= isADC[mh];
	 				contents.hMTD_trig2D_adc->Fill(kbin,trgd->mxqAtSlotAddress(iaddr,0,islot)); 				
 				} else if (isTAC[mh]){
 					kbin	= isTAC[mh];
	 				contents.hMTD_trig2D_tac->Fill(kbin,trgd->mxqAtSlotAddress(iaddr,0,islot)); 				
 				} 
 			}
 			++mh;
 		}
 	}
 
	
  if(trgd) delete trgd;
  return;
}

void mtdBuilder::stoprun(daqReader *rdr) {
//   printf("Stopping run #%d\n",run);
//   status.setEndOfRun(1);
//   send((TObject *)&status);
}

void mtdBuilder::main(int argc, char *argv[])
{
  mtdBuilder me;
  
  me.Main(argc, argv);
}

int mtdBuilder::tdcchan2globalstrip(int tdigboardid, int tdcid, int tdcchan)
{
  int globalstripid=-1;
    int Hnum=tdcid%4+1; // kx: check
    int globaltdcchan=Hnum*10+tdcchan;
    int mtdstrip[24]={21,12,32,20,14,35,25,13,30,24,11,31,
      34,22,10,37,27,17,33,23,16,36,26,15};
    for(int i=0;i<24;i++){
      if(mtdstrip[i]==globaltdcchan) {globalstripid=i+1;break;}
    }
  if(tdigboardid>3)
    globalstripid = (globalstripid>12)? globalstripid-12:globalstripid+12;
	
  return globalstripid;
}

int mtdBuilder::tdig2slot(int tdigboardid, int trayid){
  int slot=-1;  
  if(istray3bl(trayid)){
  	 slot = (tdigboardid<4)?(tdigboardid+2):(tdigboardid); 
  } else {
    slot = (tdigboardid<4)?(tdigboardid+1):(tdigboardid);
    if(tdigboardid==4) slot=5;
    if(tdigboardid==5) slot=4;
  }
  return slot;
}

int mtdBuilder::istray3bl(int trayid){
  int is3 = 0;
   for (int itray3bl=0; itray3bl<nTray3bl; itray3bl++) {
    if (trayid==tray3bl[itray3bl]) is3 = 1;
   }
  return is3;
}

int mtdBuilder::istray5bl(int trayid){
  int is5 = 0;
   for (int itray5bl=0; itray5bl<nTray5bl; itray5bl++) {
    if (trayid==tray5bl[itray5bl]) is5 = 1;
   }
  return is5;
}

bool mtdBuilder::ValidDataword(int packetid)
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
      
int mtdBuilder::iGlobalSlot(int trayid, int slot){ 
  int igs = -1;
  if (trayid>=1&&trayid<=30){ igs = (trayid-1)*5 + slot; } 
                       else { LOG("====MTD====", "MTD Tray ID issue: %d",trayid); }
  return igs;
}

void mtdBuilder::ReadValidBunchidPhase(){
  
  TString buffer;
  char mBunchShiftList[256];
  char mBunchShiftListLocal[256];
  
  sprintf(mBunchShiftList, "%s/mtd/%s",confdatadir,"MTD_ValidBunchidPhase.txt");
  ifstream filein(mBunchShiftList);
  
  mReferenceTray=26;
  int count=0;
  
  //try local if not in conf dir
  if(!filein) {
    filein.close(); 
    sprintf(mBunchShiftListLocal, "mtdconfig/%s","MTD_ValidBunchidPhase.txt");
    filein.open(mBunchShiftListLocal);
  }

  if(filein){
    while(!filein.eof()){
      buffer.ReadLine(filein);
      if(buffer.BeginsWith("/")) continue;
      if(buffer.BeginsWith("#")) continue;
      int number=atoi(buffer.Data());
      if(count==0){ 
        mReferenceTray = number;
        LOG("====MTD====", "mReferenceTray=%d",mReferenceTray);
      }
      if(count>=1 && count<=4){
      	mValidShiftTray[(count-1)%2][(count-1)/2]=number;
        LOG("====MTD====", "Found BunchId Shift RDO=%d, Value=%d", 1+((count-1)/2), number);
      }
      count++;
    }
  } else {
      LOG("====MTD====", "Can not open file: %s or %s", mBunchShiftList, mBunchShiftListLocal);
  }
}

int mtdBuilder::SetMtdQTmap(){
	char buf[200];
// 	const char* MtdQTmap[96] = {
// 	"06 (0x10) MT001 QT8A-J1 (ch01) MTD 25-1-J2 (TPC sectors 21,22)",
// 	"06 (0x10) MT001 QT8A-J2 (ch02) MTD 25-1-J3 (TPC sectors 21,22)",
// 	"06 (0x10) MT001 QT8A-J3 (ch03) MTD 25-5-J2 (TPC sectors 2,3)",
// 	"06 (0x10) MT001 QT8A-J4 (ch04) MTD 25-5-J3 (TPC sectors 2,3)",
// 	"06 (0x10) MT001 QT8A-J5 (ch05) MTD TAC 25-1-J2",
// 	"06 (0x10) MT001 QT8A-J6 (ch06) MTD TAC 25-1-J3",
// 	"06 (0x10) MT001 QT8A-J7 (ch07) MTD TAC 25-5-J2",
// 	"06 (0x10) MT001 QT8A-J8 (ch08) MTD TAC 25-5-J3",
// 	"06 (0x10) MT001 QT8B-J1 (ch09) MTD 25-2-J2 (TPC sectors 21,22)",
// 	"06 (0x10) MT001 QT8B-J2 (ch10) MTD 25-2-J3 (TPC sectors 21,22)",
// 	"06 (0x10) MT001 QT8B-J3 (ch11) MTD 25-4-J2 (TPC sectors 2,3)",
// 	"06 (0x10) MT001 QT8B-J4 (ch12) MTD 25-4-J3 (TPC sectors 2,3)",
// 	"06 (0x10) MT001 QT8B-J5 (ch13) MTD TAC 25-2-J2",
// 	"06 (0x10) MT001 QT8B-J6 (ch14) MTD TAC 25-2-J3",
// 	"06 (0x10) MT001 QT8B-J7 (ch15) MTD TAC 25-4-J2",
// 	"06 (0x10) MT001 QT8B-J8 (ch16) MTD TAC 25-4-J3",
// 	"06 (0x10) MT001 QT8C-J1 (ch17) MTD 25-3-J2 (TPC sectors 21,22,2,3)",
// 	"06 (0x10) MT001 QT8C-J2 (ch18) MTD 25-3-J3 (TPC sectors 21,22,2,3)",
// 	"06 (0x10) MT001 QT8C-J3 (ch19) MTD 30-3-J2 (TPC sectors 23,24,1,12)",
// 	"06 (0x10) MT001 QT8C-J4 (ch20) MTD 30-3-J3 (TPC sectors 23,24,1,12)",
// 	"06 (0x10) MT001 QT8C-J5 (ch21) MTD TAC 25-3-J2",
// 	"06 (0x10) MT001 QT8C-J6 (ch22) MTD TAC 25-3-J3",
// 	"06 (0x10) MT001 QT8C-J7 (ch23) MTD TAC 30-3-J2",
// 	"06 (0x10) MT001 QT8C-J8 (ch24) MTD TAC 30-3-J3",
// 	"06 (0x10) MT001 QT8D-J1 (ch25) MTD 30-1-J2 (TPC sectors 23,24)",
// 	"06 (0x10) MT001 QT8D-J2 (ch26) MTD 30-1-J3 (TPC sectors 23,24)",
// 	"06 (0x10) MT001 QT8D-J3 (ch27) MTD 30-5-J2 (TPC sectors 1,12)",
// 	"06 (0x10) MT001 QT8D-J4 (ch28) MTD 30-5-J3 (TPC sectors 1,12)",
// 	"06 (0x10) MT001 QT8D-J5 (ch29) MTD TAC 30-1-J2",
// 	"06 (0x10) MT001 QT8D-J6 (ch30) MTD TAC 30-1-J3",
// 	"06 (0x10) MT001 QT8D-J7 (ch31) MTD TAC 30-5-J2",
// 	"06 (0x10) MT001 QT8D-J8 (ch32) MTD TAC 30-5-J3",
// 	"16 (0x1a) MT002 QT8A-J1 (ch01) MTD 05-1-J2 (TPC sectors 13,14)",
// 	"16 (0x1a) MT002 QT8A-J2 (ch02) MTD 05-1-J3 (TPC sectors 13,14)",
// 	"16 (0x1a) MT002 QT8A-J3 (ch03) MTD 05-5-J2 (TPC sectors 10,11)",
// 	"16 (0x1a) MT002 QT8A-J4 (ch04) MTD 05-5-J3 (TPC sectors 10,11)",
// 	"16 (0x1a) MT002 QT8A-J5 (ch05) MTD TAC 05-1-J2",
// 	"16 (0x1a) MT002 QT8A-J6 (ch06) MTD TAC 05-1-J2",
// 	"16 (0x1a) MT002 QT8A-J7 (ch07) MTD TAC 05-5-J2",
// 	"16 (0x1a) MT002 QT8A-J8 (ch08) MTD TAC 05-5-J3",
// 	"16 (0x1a) MT002 QT8B-J1 (ch09) MTD 05-2-J2 (TPC sectors 13,14)",
// 	"16 (0x1a) MT002 QT8B-J2 (ch10) MTD 05-2-J3 (TPC sectors 13,14)",
// 	"16 (0x1a) MT002 QT8B-J3 (ch11) MTD 05-4-J2 (TPC sectors 10,11)",
// 	"16 (0x1a) MT002 QT8B-J4 (ch12) MTD 05-4-J3 (TPC sectors 10,11)",
// 	"16 (0x1a) MT002 QT8B-J5 (ch13) MTD TAC 05-2-J2",
// 	"16 (0x1a) MT002 QT8B-J6 (ch14) MTD TAC 05-2-J3",
// 	"16 (0x1a) MT002 QT8B-J7 (ch15) MTD TAC 05-4-J2",
// 	"16 (0x1a) MT002 QT8B-J8 (ch16) MTD TAC 05-4-J3",
// 	"16 (0x1a) MT002 QT8C-J1 (ch17) MTD 05-3-J2 (TPC sectors 13,14,10,11)",
// 	"16 (0x1a) MT002 QT8C-J2 (ch18) MTD 05-3-J3 (TPC sectors 13,14,10,11)",
// 	"16 (0x1a) MT002 QT8C-J3 (ch19) MTD",
// 	"16 (0x1a) MT002 QT8C-J4 (ch20) MTD",
// 	"16 (0x1a) MT002 QT8C-J5 (ch21) MTD TAC 05-3-J2",
// 	"16 (0x1a) MT002 QT8C-J6 (ch22) MTD TAC 05-3-J3",
// 	"16 (0x1a) MT002 QT8C-J7 (ch23) MTD",
// 	"16 (0x1a) MT002 QT8C-J8 (ch24) MTD",
// 	"16 (0x1a) MT002 QT8D-J1 (ch25) MTD 30-2-J2 (TPC sectors 23,24)",
// 	"16 (0x1a) MT002 QT8D-J2 (ch26) MTD 30-2-J3 (TPC sectors 23,24)",
// 	"16 (0x1a) MT002 QT8D-J3 (ch27) MTD 30-4-J2 (TPC sectors 1,12)",
// 	"16 (0x1a) MT002 QT8D-J4 (ch28) MTD 30-4-J3 (TPC sectors 1,12)",
// 	"16 (0x1a) MT002 QT8D-J5 (ch29) MTD TAC 30-2-J2",
// 	"16 (0x1a) MT002 QT8D-J6 (ch30) MTD TAC 30-2-J3",
// 	"16 (0x1a) MT002 QT8D-J7 (ch31) MTD TAC 30-4-J2",
// 	"16 (0x1a) MT002 QT8D-J8 (ch32) MTD TAC 30-4-J3",
// 	"18 (0x1c) MT003 QT8A-J1 (ch09) MTD 10-2-J2 (TPC sectors 8,16)",
// 	"18 (0x1c) MT003 QT8A-J2 (ch10) MTD 10-2-J3 (TPC sectors 8,16)",
// 	"18 (0x1c) MT003 QT8A-J3 (ch11) MTD 22-2-J2 (TPC sectors 4,20)",
// 	"18 (0x1c) MT003 QT8A-J4 (ch12) MTD 22-2-J3 (TPC sectors 4,20)",
// 	"18 (0x1c) MT003 QT8A-J5 (ch13) MTD TAC 10-2-J2",
// 	"18 (0x1c) MT003 QT8A-J6 (ch14) MTD TAC 10-2-J3",
// 	"18 (0x1c) MT003 QT8A-J7 (ch15) MTD TAC 22-2-J2",
// 	"18 (0x1c) MT003 QT8A-J8 (ch16) MTD TAC 22-2-J3",
// 	"18 (0x1c) MT003 QT8B-J1 (ch09) MTD 10-3-J2 (TPC sectors 8,16)",
// 	"18 (0x1c) MT003 QT8B-J2 (ch10) MTD 10-3-J3 (TPC sectors 8,16)",
// 	"18 (0x1c) MT003 QT8B-J3 (ch11) MTD 22-3-J2 (TPC sectors 4,20)",
// 	"18 (0x1c) MT003 QT8B-J4 (ch12) MTD 22-3-J3 (TPC sectors 4,20)",
// 	"18 (0x1c) MT003 QT8B-J5 (ch13) MTD TAC 10-3-J2",
// 	"18 (0x1c) MT003 QT8B-J6 (ch14) MTD TAC 10-3-J3",
// 	"18 (0x1c) MT003 QT8B-J7 (ch15) MTD TAC 22-3-J2",
// 	"18 (0x1c) MT003 QT8B-J8 (ch16) MTD TAC 22-3-J3",
// 	"18 (0x1c) MT003 QT8C-J1 (ch17) MTD",
// 	"18 (0x1c) MT003 QT8C-J2 (ch18) MTD",
// 	"18 (0x1c) MT003 QT8C-J3 (ch19) MTD",
// 	"18 (0x1c) MT003 QT8C-J4 (ch20) MTD",
// 	"18 (0x1c) MT003 QT8C-J5 (ch21) MTD",
// 	"18 (0x1c) MT003 QT8C-J6 (ch22) MTD",
// 	"18 (0x1c) MT003 QT8C-J7 (ch23) MTD",
// 	"18 (0x1c) MT003 QT8C-J8 (ch24) MTD",
// 	"18 (0x1c) MT003 QT8D-J1 (ch25) MTD 10-4-J2 (TPC sectors 8,16)",
// 	"18 (0x1c) MT003 QT8D-J2 (ch26) MTD 10-4-J3 (TPC sectors 8,16)",
// 	"18 (0x1c) MT003 QT8D-J3 (ch27) MTD 22-4-J2 (TPC sectors 4,20)",
// 	"18 (0x1c) MT003 QT8D-J4 (ch28) MTD 22-4-J3 (TPC sectors 4,20)",
// 	"18 (0x1c) MT003 QT8D-J5 (ch29) MTD TAC 10-4-J2",
// 	"18 (0x1c) MT003 QT8D-J6 (ch30) MTD TAC 10-4-J3",
// 	"18 (0x1c) MT003 QT8D-J7 (ch31) MTD TAC 22-4-J2",
// 	"18 (0x1c) MT003 QT8D-J8 (ch32) MTD TAC 22-4-J3"
// 	};

	const char* MtdQTmap[128] = {
		"06 (0x10) MT001 QT8A-J1 (ch1) ADC 25-1 (J2)",
		"06 (0x10) MT001 QT8A-J2 (ch2) ADC 25-1 (J3)",
		"06 (0x10) MT001 QT8A-J3 (ch3) ADC 25-5 (J2)",
		"06 (0x10) MT001 QT8A-J4 (ch4) ADC 25-5 (J3)",
		"06 (0x10) MT001 QT8A-J5 (ch5) TAC 25-1 (J2)",
		"06 (0x10) MT001 QT8A-J6 (ch6) TAC 25-1 (J3)",
		"06 (0x10) MT001 QT8A-J7 (ch7) TAC 25-5 (J2)",
		"06 (0x10) MT001 QT8A-J8 (ch8) TAC 25-5 (J3)",
		"06 (0x10) MT001 QT8B-J1 (ch1) ADC 25-2 (J2)",
		"06 (0x10) MT001 QT8B-J2 (ch2) ADC 25-2 (J3)",
		"06 (0x10) MT001 QT8B-J3 (ch3) ADC 25-4 (J2)",
		"06 (0x10) MT001 QT8B-J4 (ch4) ADC 25-4 (J3)",
		"06 (0x10) MT001 QT8B-J5 (ch5) TAC 25-2 (J2)",
		"06 (0x10) MT001 QT8B-J6 (ch6) TAC 25-2 (J3)",
		"06 (0x10) MT001 QT8B-J7 (ch7) TAC 25-4 (J2)",
		"06 (0x10) MT001 QT8B-J8 (ch8) TAC 25-4 (J3)",
		"06 (0x10) MT001 QT8C-J1 (ch1) ADC 25-3 (J2)",
		"06 (0x10) MT001 QT8C-J2 (ch2) ADC 25-3 (J3)",
		"06 (0x10) MT001 QT8C-J3 (ch3) ADC 30-3 (J2)",
		"06 (0x10) MT001 QT8C-J4 (ch4) ADC 30-3 (J3)",
		"06 (0x10) MT001 QT8C-J5 (ch5) TAC 25-3 (J2)",
		"06 (0x10) MT001 QT8C-J6 (ch6) TAC 25-3 (J3)",
		"06 (0x10) MT001 QT8C-J7 (ch7) TAC 30-3 (J2)",
		"06 (0x10) MT001 QT8C-J8 (ch8) TAC 30-3 (J3)",
		"06 (0x10) MT001 QT8D-J1 (ch1) ADC 30-1 (J2)",
		"06 (0x10) MT001 QT8D-J2 (ch2) ADC 30-1 (J3)",
		"06 (0x10) MT001 QT8D-J3 (ch3) ADC 30-5 (J2)",
		"06 (0x10) MT001 QT8D-J4 (ch4) ADC 30-5 (J3)",
		"06 (0x10) MT001 QT8D-J5 (ch5) TAC 30-1 (J2)",
		"06 (0x10) MT001 QT8D-J6 (ch6) TAC 30-1 (J3)",
		"06 (0x10) MT001 QT8D-J7 (ch7) TAC 30-5 (J2)",
		"06 (0x10) MT001 QT8D-J8 (ch8) TAC 30-5 (J3)",
		"16 (0x1A) MT002 QT8A-J1 (ch1) ADC 5-1 (J2)",
		"16 (0x1A) MT002 QT8A-J2 (ch2) ADC 5-1 (J3)",
		"16 (0x1A) MT002 QT8A-J3 (ch3) ADC 5-5 (J2)",
		"16 (0x1A) MT002 QT8A-J4 (ch4) ADC 5-5 (J3)",
		"16 (0x1A) MT002 QT8A-J5 (ch5) TAC 5-1 (J2)",
		"16 (0x1A) MT002 QT8A-J6 (ch6) TAC 5-1 (J3)",
		"16 (0x1A) MT002 QT8A-J7 (ch7) TAC 5-5 (J2)",
		"16 (0x1A) MT002 QT8A-J8 (ch8) TAC 5-5 (J3)",
		"16 (0x1A) MT002 QT8B-J1 (ch1) ADC 5-2 (J2)",
		"16 (0x1A) MT002 QT8B-J2 (ch2) ADC 5-2 (J3)",
		"16 (0x1A) MT002 QT8B-J3 (ch3) ADC 5-4 (J2)",
		"16 (0x1A) MT002 QT8B-J4 (ch4) ADC 5-4 (J3)",
		"16 (0x1A) MT002 QT8B-J5 (ch5) TAC 5-2 (J2)",
		"16 (0x1A) MT002 QT8B-J6 (ch6) TAC 5-2 (J3)",
		"16 (0x1A) MT002 QT8B-J7 (ch7) TAC 5-4 (J2)",
		"16 (0x1A) MT002 QT8B-J8 (ch8) TAC 5-4 (J3)",
		"16 (0x1A) MT002 QT8C-J1 (ch1) ADC 5-3 (J2)",
		"16 (0x1A) MT002 QT8C-J2 (ch2) ADC 5-3 (J3)",
		"16 (0x1A) MT002 QT8C-J3 (ch3) ",
		"16 (0x1A) MT002 QT8C-J4 (ch4) ",
		"16 (0x1A) MT002 QT8C-J5 (ch5) TAC 5-3 (J2)",
		"16 (0x1A) MT002 QT8C-J6 (ch6) TAC 5-3 (J3)",
		"16 (0x1A) MT002 QT8C-J7 (ch7) ",
		"16 (0x1A) MT002 QT8C-J8 (ch8) ",
		"16 (0x1A) MT002 QT8D-J1 (ch1) ADC 30-2 (J2)",
		"16 (0x1A) MT002 QT8D-J2 (ch2) ADC 30-2 (J3)",
		"16 (0x1A) MT002 QT8D-J3 (ch3) ADC 30-4 (J2)",
		"16 (0x1A) MT002 QT8D-J4 (ch4) ADC 30-4 (J3)",
		"16 (0x1A) MT002 QT8D-J5 (ch5) TAC 30-2 (J2)",
		"16 (0x1A) MT002 QT8D-J6 (ch6) TAC 30-2 (J3)",
		"16 (0x1A) MT002 QT8D-J7 (ch7) TAC 30-4 (J2)",
		"16 (0x1A) MT002 QT8D-J8 (ch8) TAC 30-4 (J3)",
		"18 (0x1C) MT003 QT8A-J1 (ch1) ADC 10-1 (J2)",
		"18 (0x1C) MT003 QT8A-J2 (ch2) ADC 10-1 (J3)",
		"18 (0x1C) MT003 QT8A-J3 (ch3) ADC 10-5 (J2)",
		"18 (0x1C) MT003 QT8A-J4 (ch4) ADC 10-5 (J3)",
		"18 (0x1C) MT003 QT8A-J5 (ch5) TAC 10-1 (J2)",
		"18 (0x1C) MT003 QT8A-J6 (ch6) TAC 10-1 (J3)",
		"18 (0x1C) MT003 QT8A-J7 (ch7) TAC 10-5 (J2)",
		"18 (0x1C) MT003 QT8A-J8 (ch8) TAC 10-5 (J3)",
		"18 (0x1C) MT003 QT8B-J1 (ch1) ADC 10-2 (J2)",
		"18 (0x1C) MT003 QT8B-J2 (ch2) ADC 10-2 (J3)",
		"18 (0x1C) MT003 QT8B-J3 (ch3) ADC 10-4 (J2)",
		"18 (0x1C) MT003 QT8B-J4 (ch4) ADC 10-4 (J3)",
		"18 (0x1C) MT003 QT8B-J5 (ch5) TAC 10-2 (J2)",
		"18 (0x1C) MT003 QT8B-J6 (ch6) TAC 10-2 (J3)",
		"18 (0x1C) MT003 QT8B-J7 (ch7) TAC 10-4 (J2)",
		"18 (0x1C) MT003 QT8B-J8 (ch8) TAC 10-4 (J3)",
		"18 (0x1C) MT003 QT8C-J1 (ch1) ADC 10-3 (J2)",
		"18 (0x1C) MT003 QT8C-J2 (ch2) ADC 10-3 (J3)",
		"18 (0x1C) MT003 QT8C-J3 (ch3) ADC 15-3 (J2)",
		"18 (0x1C) MT003 QT8C-J4 (ch4) ADC 15-3 (J3)",
		"18 (0x1C) MT003 QT8C-J5 (ch5) TAC 10-3 (J2)",
		"18 (0x1C) MT003 QT8C-J6 (ch6) TAC 10-3 (J3)",
		"18 (0x1C) MT003 QT8C-J7 (ch7) TAC 15-3 (J2)",
		"18 (0x1C) MT003 QT8C-J8 (ch8) TAC 15-3 (J3)",
		"18 (0x1C) MT003 QT8D-J1 (ch1) ",
		"18 (0x1C) MT003 QT8D-J2 (ch2) ",
		"18 (0x1C) MT003 QT8D-J3 (ch3) ",
		"18 (0x1C) MT003 QT8D-J4 (ch4) ",
		"18 (0x1C) MT003 QT8D-J5 (ch5) ",
		"18 (0x1C) MT003 QT8D-J6 (ch6) ",
		"18 (0x1C) MT003 QT8D-J7 (ch7) ",
		"18 (0x1C) MT003 QT8D-J8 (ch8) ",
		"20 (0x1E) MT004 QT8A-J1 (ch1) ADC 21-1 (J2)",
		"20 (0x1E) MT004 QT8A-J2 (ch2) ADC 21-1 (J3)",
		"20 (0x1E) MT004 QT8A-J3 (ch3) ADC 21-5 (J2)",
		"20 (0x1E) MT004 QT8A-J4 (ch4) ADC 21-5 (J3)",
		"20 (0x1E) MT004 QT8A-J5 (ch5) TAC 21-1 (J2)",
		"20 (0x1E) MT004 QT8A-J6 (ch6) TAC 21-1 (J3)",
		"20 (0x1E) MT004 QT8A-J7 (ch7) TAC 21-5 (J2)",
		"20 (0x1E) MT004 QT8A-J8 (ch8) TAC 21-5 (J3)",
		"20 (0x1E) MT004 QT8B-J1 (ch1) ADC 20-2 (J2)",
		"20 (0x1E) MT004 QT8B-J2 (ch2) ADC 20-2 (J3)",
		"20 (0x1E) MT004 QT8B-J3 (ch3) ADC 20-4 (J2)",
		"20 (0x1E) MT004 QT8B-J4 (ch4) ADC 20-4 (J3)",
		"20 (0x1E) MT004 QT8B-J5 (ch5) TAC 20-2 (J2)",
		"20 (0x1E) MT004 QT8B-J6 (ch6) TAC 20-2 (J3)",
		"20 (0x1E) MT004 QT8B-J7 (ch7) TAC 20-4 (J2)",
		"20 (0x1E) MT004 QT8B-J8 (ch8) TAC 20-4 (J3)",
		"20 (0x1E) MT004 QT8C-J1 (ch1) ADC 20-3 (J2)",
		"20 (0x1E) MT004 QT8C-J2 (ch2) ADC 20-3 (J3)",
		"20 (0x1E) MT004 QT8C-J3 (ch3) ",
		"20 (0x1E) MT004 QT8C-J4 (ch4) ",
		"20 (0x1E) MT004 QT8C-J5 (ch5) TAC 20-3 (J2)",
		"20 (0x1E) MT004 QT8C-J6 (ch6) TAC 20-3 (J3)",
		"20 (0x1E) MT004 QT8C-J7 (ch7) ",
		"20 (0x1E) MT004 QT8C-J8 (ch8) ",
		"20 (0x1E) MT004 QT8D-J1 (ch1) ADC 15-2 (J2)",
		"20 (0x1E) MT004 QT8D-J2 (ch2) ADC 15-2 (J3)",
		"20 (0x1E) MT004 QT8D-J3 (ch3) ADC 15-4 (J2)",
		"20 (0x1E) MT004 QT8D-J4 (ch4) ADC 15-4 (J3)",
		"20 (0x1E) MT004 QT8D-J5 (ch5) TAC 15-2 (J2)",
		"20 (0x1E) MT004 QT8D-J6 (ch6) TAC 15-2 (J3)",
		"20 (0x1E) MT004 QT8D-J7 (ch7) TAC 15-4 (J2)",
		"20 (0x1E) MT004 QT8D-J8 (ch8) TAC 15-4 (J3)"
	};
	
	TString QTmap[128];
	//
	int nTAC=0,nADC=0,nOPEN=0;
	bool thisisADC,thisisOPEN;
	//
	for (int i=0;i<128;i++){
		sprintf(buf,"%s",MtdQTmap[i]);
		QTmap[i]	= TString(buf);
		//cout<<QTmap[i].Data()<<endl;
		//
		QTslot[i]		= QTmap[i](0,2);
		QTslothex[i]	= QTmap[i](4,4);
		QTboard[i]		= QTmap[i](10,5);
		QTchanname[i]	= QTmap[i](16,7);
		QTchanno[i]		= QTmap[i](25,3);
		QTchanstring[i]	= QTmap[i](30,QTmap[i].Length()-1);
		//
		thisisADC = thisisOPEN = false;
 		if (QTchanstring[i].Length()==0){ ++nOPEN; thisisOPEN=true; } else 
		if (QTchanstring[i].Contains("TAC", TString::kIgnoreCase)){ 
			++nTAC; isTAC[i]=nTAC; 
		} else {
			thisisADC = true;
 		    ++nADC; isADC[i]=nADC; 
 		}
 		//if (!thisisOPEN){
 		//	if (thisisADC){
 		//		QTcable[i] = QTchanstring[i](0,7);
 		//		QTtpcsector[i] = QTchanstring[i](21,QTchanstring[i].Length()-21-1);
 		//	} else {
 		//		QTcable[i] = QTchanstring[i](4,7);
 		//	}
 		//}
		//
		//cout<<QTmap[i].Length()<<"  ..."<<QTslot[i].Data()<<"..."<<QTslothex[i].Data()<<"..."
		//	<<QTboard[i].Data()<<"..."
		//	<<QTchanname[i].Data()<<"..."
		//	<<QTchanno[i].Data()<<"..."
		//	<<QTchanstring[i].Data()<<"\t"
		//	<<isADC[i]<<" "<<isTAC[i]<<"\t..."
		//	//<<QTcable[i].Data()<<"..."
		//	//<<QTtpcsector[i].Data()<<"..."			
		//	<<endl;
		//
	}
	//cout<<nADC<<" "<<nTAC<<" "<<nOPEN<<endl;
	return 1;
}

