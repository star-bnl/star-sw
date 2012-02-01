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
int tray3bl[] = {26};
int tray5bl[] = {27, 28};

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

  // Add root histograms to Plots
  JevpPlot *plots[100];
  int nhhit=0;
  int nhtrig=0;

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
    plots[i]->optstat=0;
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
    plots[i]->optstat=0;
  }
  LOG("====MTD====", "%d trig plots added", nhtrig);
  
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
  leadinghits.clear();
  trailinghits.clear();
	
  daq_dta *dd = rdr->det("mtd")->get("legacy");
  mtd_t *mtd;
  if(dd) {
    while(dd->iterate()) {
      mtd = (mtd_t *)dd->Void;
			
      int ifib=0;
      int ndataword = mtd->ddl_words[ifib];    
      if(ndataword<=0) continue;
      for(int iword=0;iword<ndataword;iword++){
	int dataword=mtd->ddl[ifib][iword];
				
	if( (dataword&0xF0000000)>>28 == 0x2) continue;  //TDC header
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
				
	if( (dataword&0xF0000000)>>28 == 0x6) {continue;} //error
	//
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

  for(int i=0;i<nMTDtrays;i++){
      for(int j=0; j<5; j++)
        contents.hMTD_hitmap[trayid-1][slot-1]->Fill(globalstripid);
  }
      }  // end loop nword
    }
  }
	
  // now do the trigger plots...
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) {
    LOG(WARN, "No trigger data");
    return;
  }

  //MTD trigger Run12
  // for backleg 27-2
  float outadc0= trgd->mtdAtAddress(8, 0); contents.hMTD_trig[0]->Fill(outadc0);
  float outtdc0= trgd->mtdAtAddress(12, 0);contents.hMTD_trig[1]->Fill(outtdc0);
  float inadc0= trgd->mtdAtAddress(9, 0);  contents.hMTD_trig[2]->Fill(inadc0);
  float intdc0= trgd->mtdAtAddress(13, 0); contents.hMTD_trig[3]->Fill(intdc0);
  
  //for backleg 27-3
  float outadc1= trgd->mtdAtAddress(16, 0);contents.hMTD_trig[4]->Fill(outadc1);
  float outtdc1= trgd->mtdAtAddress(20, 0);contents.hMTD_trig[5]->Fill(outtdc1);
  float inadc1= trgd->mtdAtAddress(17, 0); contents.hMTD_trig[6]->Fill(inadc1);
  float intdc1= trgd->mtdAtAddress(21, 0); contents.hMTD_trig[7]->Fill(intdc1);
  
  //for backleg 27-4
  float outadc2= trgd->mtdAtAddress(24, 0);contents.hMTD_trig[8]->Fill(outadc2);
  float outtdc2= trgd->mtdAtAddress(28, 0);contents.hMTD_trig[9]->Fill(outtdc2);
  float inadc2= trgd->mtdAtAddress(25, 0); contents.hMTD_trig[10]->Fill(inadc2);
  float intdc2= trgd->mtdAtAddress(29, 0); contents.hMTD_trig[11]->Fill(intdc2);
  
  // for backleg 27-1
  float outadc3= trgd->mtdgemAtAddress(8, 0); contents.hMTD_trig[12]->Fill(outadc3);
  float outtdc3= trgd->mtdgemAtAddress(12, 0);contents.hMTD_trig[13]->Fill(outtdc3);
  float inadc3= trgd->mtdgemAtAddress(9, 0);  contents.hMTD_trig[14]->Fill(inadc3);
  float intdc3= trgd->mtdgemAtAddress(13, 0); contents.hMTD_trig[15]->Fill(intdc3);
  
  //for backleg 27-5
  float outadc4= trgd->mtdgemAtAddress(16, 0);contents.hMTD_trig[16]->Fill(outadc4);
  float outtdc4= trgd->mtdgemAtAddress(20, 0);contents.hMTD_trig[17]->Fill(outtdc4);
  float inadc4= trgd->mtdgemAtAddress(17, 0); contents.hMTD_trig[18]->Fill(inadc4);
  float intdc4= trgd->mtdgemAtAddress(21, 0); contents.hMTD_trig[19]->Fill(intdc4);
	
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