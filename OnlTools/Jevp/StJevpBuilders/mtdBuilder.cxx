#include <stdio.h>
#include <stdlib.h>
#include <algorithm>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_MTD/daq_mtd.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "mtdBuilder.h"
#include <RTS/include/rtsLog.h>

// Backleg lists
int tray3bl[1] = {26};
int tray5bl[2] = {27, 28};
int tray[3]={26, 27, 28};

int nGlobalSlot=sizeof(tray3bl)/sizeof(int)*3+sizeof(tray5bl)/sizeof(int)*5;
int ntray=sizeof(tray)/sizeof(int);

ClassImp(mtdBuilder);
  
typedef JevpPlot * ptrJevpPlot;

mtdBuilder::mtdBuilder(JevpServer *parent) : JevpPlotSet(parent) {
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
  // Build Root Histograms...
  char tmpchr[200];
  
  contents.hMTD_hitmap = new TH1 **[nMTDtrays];
  for(int itray=0;itray<nMTDtrays;itray++)
    contents.hMTD_hitmap[itray] = new TH1*[5];
  for(int i=0;i<nMTDtrays;i++){
    for(int j=0; j<5; j++){
      sprintf(tmpchr, "MTD_tray_%d_position_%d", i+1, j+1);
      contents.hMTD_hitmap[i][j] = new TH1F(tmpchr, tmpchr, 24, 0.5, 24.5);
    }
  }
  // Run12 MTD trigger
  contents.hMTD_trig = new TH1 *[nMTDtrig];
  // keep the same format as the trigger experts'
  // for backleg 27-2
  sprintf(tmpchr,"MTD_Trigger27_2_outadc");contents.hMTD_trig[0] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_2_outtdc");contents.hMTD_trig[1] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_2_inadc"); contents.hMTD_trig[2] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_2_intdc"); contents.hMTD_trig[3] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  
  //for backleg 27-3
  sprintf(tmpchr,"MTD_Trigger27_3_outadc");contents.hMTD_trig[4] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_3_outtdc");contents.hMTD_trig[5] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_3_inadc"); contents.hMTD_trig[6] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_3_intdc"); contents.hMTD_trig[7] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  
  //for backleg 27-4
  sprintf(tmpchr,"MTD_Trigger27_4_outadc");contents.hMTD_trig[8] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_4_outtdc");contents.hMTD_trig[9] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_4_inadc"); contents.hMTD_trig[10] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_4_intdc"); contents.hMTD_trig[11] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  
  // for backleg 27-1
  sprintf(tmpchr,"MTD_Trigger27_1_outadc");contents.hMTD_trig[12] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_1_outtdc");contents.hMTD_trig[13] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_1_inadc"); contents.hMTD_trig[14] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_1_intdc"); contents.hMTD_trig[15] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  
  //for backleg 27-5
  sprintf(tmpchr,"MTD_Trigger27_5_outadc");contents.hMTD_trig[16] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_5_outtdc");contents.hMTD_trig[17] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_5_inadc"); contents.hMTD_trig[18] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);
  sprintf(tmpchr,"MTD_Trigger27_5_intdc"); contents.hMTD_trig[19] = new TH1F(tmpchr, tmpchr, 150, 0, 3000);

  //mtd bunchid
  contents.MTD_bunchid = new TH2F("MTD_bunchid", "MTD_bunchid", 3, 25.5, 28.5, 30, -14.5, 15.5);

  //mtd error check
  sprintf(tmpchr,"MTD_Error1");contents.MTD_Error1 = new TH1F(tmpchr, "MTD electronics errors", 2, -0.5, 1.5);
  contents.MTD_Error1->GetXaxis()->SetBinLabel(1, "THUB1");  contents.MTD_Error1->GetXaxis()->SetBinLabel(2, "THUB2");
  sprintf(tmpchr,"MTD_Error2");contents.MTD_Error2 = new TH1F(tmpchr, "MTD incorrect bunchid errors", 3, 25.5, 28.5);
  for(int i=0;i<ntray;i++){
    sprintf(tmpchr,"Tray%d", tray[i]); 
    contents.MTD_Error2->GetXaxis()->SetBinLabel(i+1, tmpchr); 
  }
  sprintf(tmpchr,"MTD_Error3");contents.MTD_Error3 = new TH1F(tmpchr, "MTD trays not read out (bunchid not found)", 3, 25.5, 28.5);
  for(int i=0;i<ntray;i++){
    sprintf(tmpchr,"Tray%d", tray[i]); 
    contents.MTD_Error3->GetXaxis()->SetBinLabel(i+1, tmpchr); 
  }
  
  contents.MTD_Tray_hits=new TH1F("MTD_Tray_hits","MTD Hits by Tray",nGlobalSlot, 1, nGlobalSlot+1);
  int i=0;
  for (unsigned int itray3bl=0; itray3bl<sizeof(tray3bl)/sizeof(int); itray3bl++) {
    int val3tray=tray3bl[itray3bl];
    for(int islot=2;islot<=4;islot++){
      sprintf(tmpchr, "Tray%d-%d", val3tray, islot);
      contents.MTD_Tray_hits->GetXaxis()->SetBinLabel(++i, tmpchr);
    }
  }
  for (unsigned int itray5bl=0; itray5bl<sizeof(tray5bl)/sizeof(int); itray5bl++) {
    int val5tray=tray5bl[itray5bl];
    for(int islot=1;islot<=5;islot++){
      sprintf(tmpchr, "Tray%d-%d", val5tray, islot);
      contents.MTD_Tray_hits->GetXaxis()->SetBinLabel(++i, tmpchr);
    }
  }
  
  //Counter
  contents.MTD_EventCount=new TH1F("MTD_EventCount","MTD_EventCount",2,0,2);

  // Add root histograms to Plots
  JevpPlot *plots[100];
  int nhhit=0;
  int nhtrig=0;
  int n=0;

  JLatex *latexW, *latexE;
  latexW = new JLatex(4., 0.8, "West");
  latexE = new JLatex(16., 0.8, "East");
  JLine *ln;
  ln = new JLine(12.5, 0.1, 12.5, 0.9 );
  ln -> SetLineColor(4);
  
  for (unsigned int itray3bl=0; itray3bl<sizeof(tray3bl)/sizeof(int); itray3bl++) {
    int val3tray=tray3bl[itray3bl];
    for(int islot=2;islot<=4;islot++){
      plots[nhhit++] = new JevpPlot(contents.hMTD_hitmap[val3tray-1][islot-1]);
    }
  }
  for (unsigned int itray5bl=0; itray5bl<sizeof(tray5bl)/sizeof(int); itray5bl++) {
    int val5tray=tray5bl[itray5bl];
    for(int islot=1;islot<=5;islot++){
      plots[nhhit++] = new JevpPlot(contents.hMTD_hitmap[val5tray-1][islot-1]);
    }
  }
  
  for (int i=0; i<20; i++) {
    plots[nhhit+(nhtrig++)] = new JevpPlot(contents.hMTD_trig[i]);
  }
  LOG("====MTD====", "n=%d nhhit=%d nhtrig=%d", nhhit+nhtrig, nhhit, nhtrig);
  // Add Plots to plot set...
  for(int i=0;i<nhhit;i++) {
    LOG("MTD", "Adding plot %d",i);
    addPlot(plots[i]);
    plots[i]->getHisto(0)->histo->SetFillColor(19);
    plots[i]->getHisto(0)->histo->SetMinimum(0);
    plots[i]->getHisto(0)->histo->SetFillStyle(1001);
    plots[i]->optstat=1111111;
    plots[i]->gridx = 0;
    plots[i]->gridy = 0;
    plots[i]->addElement(latexW);
    plots[i]->addElement(latexE);
    plots[i]->addElement(ln);
  }
  LOG("====MTD====", "%d hitmap plots added",nhhit);
  for(int i=nhhit;i<nhhit+nhtrig;i++) {
    LOG("MTD", "Adding plot %d",i);
    addPlot(plots[i]);
    plots[i]->getHisto(0)->histo->SetFillColor(19);
    plots[i]->getHisto(0)->histo->SetMinimum(0);
    plots[i]->getHisto(0)->histo->SetFillStyle(1001);
    plots[i]->optstat=1111111;
    plots[i]->gridx = 0;
    plots[i]->gridy = 0;
  }
  LOG("====MTD====", "%d trig plots added", nhtrig);

  n = nhhit+nhtrig-1;
  plots[++n] = new JevpPlot(contents.MTD_bunchid);
  addPlot(plots[n]);
  plots[n]->setDrawOpts("colz");
  plots[n]->optstat=1111111;
  LOG("====MTD====", "mtd bunchid plots added");
  
  plots[++n] = new JevpPlot(contents.MTD_Error1);
  addPlot(plots[n]);
  plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  MTD_Error1_label = new TLatex();
  MTD_Error1_label->SetNDC(); 
  plots[n]->addElement(MTD_Error1_label);
  LOG("====MTD====", "MTD_Error1 added");
  
  plots[++n] = new JevpPlot(contents.MTD_Error2);
  addPlot(plots[n]);
  plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 0;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  MTD_Error2_label = new TLatex();
  MTD_Error2_label->SetNDC(); 
  plots[n]->addElement(MTD_Error2_label);
  LOG("====MTD====", "MTD_Error2 added");
  
  plots[++n] = new JevpPlot(contents.MTD_Error3);
  addPlot(plots[n]);
  plots[n]->getHisto(0)->histo->SetFillColor(45);plots[n]->optstat = 1111111;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  MTD_Error3_label = new TLatex();
  MTD_Error3_label->SetNDC(); 
  plots[n]->addElement(MTD_Error3_label);
  LOG("====MTD====", "MTD_Error3 added");
  
  plots[++n] = new JevpPlot(contents.MTD_EventCount);
  addPlot(plots[n]);
  plots[n]->optstat=1111111;
  plots[n]->getHisto(0)->histo->SetMinimum(0);
  LOG("====MTD====", "MTD_EventCount added");
  
  plots[++n] = new JevpPlot(contents.MTD_Tray_hits);
  addPlot(plots[n]);
  plots[n]->logy=1;
  plots[n]->getHisto(0)->histo->SetFillColor(18);plots[n]->optstat = 0;
  LOG("====MTD====", "MTD_Tray_hits added");
  
  LOG("====MTD====", "MTD initialization done");
}
  
void mtdBuilder::startrun(daqReader *rdr) {
  LOG("MTD", "TriggerPlotBuilder starting run #%d",rdr->run);
  resetAllPlots();
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
  for(int i=0;i<2;i++) for(int j=0;j<30;j++) allbunchid[i][j] = -9999;
	
  daq_dta *dd = rdr->det("mtd")->get("legacy");
  mtd_t *mtd;
  if (!dd){		//WJL ...pointer to mtd data not found
  	return;
  } else {
    while(dd->iterate()) {
      mtd = (mtd_t *)dd->Void;
			
      int ifib=0;
      int ndataword = mtd->ddl_words[ifib];    
      if(ndataword<=0) continue;
      for(int iword=0;iword<ndataword;iword++){
		int dataword=mtd->ddl[ifib][iword];
					
		int packetid = (dataword&0xF0000000)>>28;
		if(!ValidDataword(packetid))
		  contents.MTD_Error1->Fill(ifib); 
	
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
		
        contents.hMTD_hitmap[trayid-1][slot-1]->Fill(globalstripid);        
  		contents.MTD_Tray_hits->Fill(iGlobalSlot(trayid,slot));

      }  // end loop nword
    } //end dd iterate
  } //end if dd
	
  contents.MTD_EventCount->Fill(1);
  
  //check bunchid
  int mReferenceTray=26;
  int bunchidref1 =   allbunchid[0][mReferenceTray-1];
  int bunchidref2 =   allbunchid[1][mReferenceTray-1];
  int diff = bunchidref2-bunchidref1;
  if(diff>2048)   {diff =diff-4096;} 
  else if(diff<-2048) {diff =diff+4096;}
  
  contents.MTD_bunchid->Fill(mReferenceTray, diff);
  if(bunchidref2!=-9999 && diff) contents.MTD_Error2->Fill(mReferenceTray);
  if(bunchidref1==-9999 || bunchidref2==-9999) contents.MTD_Error3->Fill(mReferenceTray);

  for(int ihalf=0; ihalf<2; ihalf++){
    for(int i=1; i<ntray; i++){ // skip mReferenceTray
      int traynum=tray[i]; 
      int itray=traynum-1;
      diff = allbunchid[ihalf][itray]-allbunchid[0][mReferenceTray-1];
      if(allbunchid[ihalf][itray]!=-9999) contents.MTD_bunchid->Fill(traynum, diff);
      if(allbunchid[ihalf][itray]!=-9999 && diff) contents.MTD_Error2->Fill(traynum); // real bunchid errors
      if(allbunchid[ihalf][itray]==-9999) contents.MTD_Error3->Fill(traynum); //missing bunchids
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
  float outadc0= trgd->mtdAtAddress(8, 0); if(outadc0) contents.hMTD_trig[0]->Fill(outadc0);
  float outtdc0= trgd->mtdAtAddress(12, 0); if(outtdc0) contents.hMTD_trig[1]->Fill(outtdc0);
  float inadc0= trgd->mtdAtAddress(9, 0); if(inadc0) contents.hMTD_trig[2]->Fill(inadc0);
  float intdc0= trgd->mtdAtAddress(13, 0); if(intdc0) contents.hMTD_trig[3]->Fill(intdc0);
  
  //for backleg 27-3
  float outadc1= trgd->mtdAtAddress(16, 0); if(outadc1) contents.hMTD_trig[4]->Fill(outadc1);
  float outtdc1= trgd->mtdAtAddress(20, 0); if(outtdc1) contents.hMTD_trig[5]->Fill(outtdc1);
  float inadc1= trgd->mtdAtAddress(17, 0); if(inadc1) contents.hMTD_trig[6]->Fill(inadc1);
  float intdc1= trgd->mtdAtAddress(21, 0); if(intdc1) contents.hMTD_trig[7]->Fill(intdc1);
  
  //for backleg 27-4
  float outadc2= trgd->mtdAtAddress(24, 0); if(outadc2) contents.hMTD_trig[8]->Fill(outadc2);
  float outtdc2= trgd->mtdAtAddress(28, 0); if(outtdc2) contents.hMTD_trig[9]->Fill(outtdc2);
  float inadc2= trgd->mtdAtAddress(25, 0); if(inadc2) contents.hMTD_trig[10]->Fill(inadc2);
  float intdc2= trgd->mtdAtAddress(29, 0);  if(intdc2) contents.hMTD_trig[11]->Fill(intdc2);
  
  // for backleg 27-1
  float outadc3= trgd->mtdgemAtAddress(8, 0); if(outadc3) contents.hMTD_trig[12]->Fill(outadc3);
  float outtdc3= trgd->mtdgemAtAddress(12, 0); if(outtdc3) contents.hMTD_trig[13]->Fill(outtdc3);
  float inadc3= trgd->mtdgemAtAddress(9, 0); if(inadc3) contents.hMTD_trig[14]->Fill(inadc3);
  float intdc3= trgd->mtdgemAtAddress(13, 0); if(intdc3) contents.hMTD_trig[15]->Fill(intdc3);
  
  //for backleg 27-5
  float outadc4= trgd->mtdgemAtAddress(16, 0); if(outadc4) contents.hMTD_trig[16]->Fill(outadc4);
  float outtdc4= trgd->mtdgemAtAddress(20, 0); if(outtdc4) contents.hMTD_trig[17]->Fill(outtdc4);
  float inadc4= trgd->mtdgemAtAddress(17, 0); if(inadc4) contents.hMTD_trig[18]->Fill(inadc4);
  float intdc4= trgd->mtdgemAtAddress(21, 0); if(intdc4) contents.hMTD_trig[19]->Fill(intdc4);
	
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
  if(istray3bl(trayid)) slot = (tdigboardid<4)?(tdigboardid+2):(tdigboardid); 
  else {
    slot = (tdigboardid<4)?(tdigboardid+1):(tdigboardid);
    if(tdigboardid==4) slot=5;
    if(tdigboardid==5) slot=4;
  }
    return slot;
}

int mtdBuilder::istray3bl(int trayid){
  int is3 = 0;
   for (unsigned int itray3bl=0; itray3bl<sizeof(tray3bl)/sizeof(int); itray3bl++) {
    if (trayid==tray3bl[itray3bl]) is3 = 1;
   }
  return is3;
}

int mtdBuilder::istray5bl(int trayid){
  int is5 = 0;
   for (unsigned int itray5bl=0; itray5bl<sizeof(tray5bl)/sizeof(int); itray5bl++) {
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
      
int mtdBuilder::iGlobalSlot(int trayid, int slot){ // 1-13, subject to change with detector update
  if(trayid==26) return slot-1;
  if(trayid==27) return 3+slot;
  if(trayid==28) return 8+slot;
  return -1;
}

