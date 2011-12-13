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

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(mtdBuilder);
  
typedef JevpPlot * ptrJevpPlot;

mtdBuilder::mtdBuilder() {
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
  sprintf(tmpchr,"MTD26E_EastEnd_hitmap");
  contents.MTD26E_hitmap[0][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26E_EastEnd_hitmapB");
  contents.MTD26E_hitmap[0][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26E_WestEnd_hitmap");
  contents.MTD26E_hitmap[1][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26E_WestEnd_hitmapB");
  contents.MTD26E_hitmap[1][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	
  sprintf(tmpchr,"MTD26C_EastEnd_hitmap");
  contents.MTD26C_hitmap[0][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26C_EastEnd_hitmapB");
  contents.MTD26C_hitmap[0][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26C_WestEnd_hitmap");
  contents.MTD26C_hitmap[1][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26C_WestEnd_hitmapB");
  contents.MTD26C_hitmap[1][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	
  //the 3rd tray is rotated: HiZ - WestEnd
  sprintf(tmpchr,"MTD26W_WestEnd_hitmap");
  contents.MTD26W_hitmap[0][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26W_WestEnd_hitmapB");
  contents.MTD26W_hitmap[0][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26W_EastEnd_hitmap");
  contents.MTD26W_hitmap[1][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
  sprintf(tmpchr,"MTD26W_EastEnd_hitmapB");
  contents.MTD26W_hitmap[1][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	
  sprintf(tmpchr,"MTD1_EastEnd_hitmap");
  contents.MTD1_hitmap[0][0]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
  sprintf(tmpchr,"MTD1_EastEnd_hitmapB");
  contents.MTD1_hitmap[0][1]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
  sprintf(tmpchr,"MTD1_WestEnd_hitmap");
  contents.MTD1_hitmap[1][0]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
  sprintf(tmpchr,"MTD1_WestEnd_hitmapB");
  contents.MTD1_hitmap[1][1]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
	
  contents.MTD_ToT    =new TH2F("MTD_ToT","MTD ToT vs Chan #",36,-0.5,35.5,70,0,70);
  contents.MTD_ToT->SetXTitle("MTD Chan#");
  //contents.MTD_ToT->SetYTitle("ToT (ns)");
	
  contents.MTD_eastT_vs_westT=new TH2F("contents.MTD_eastT_vs_westT","MTD eastT vs westT",1024,0,51200,1024,0,51200);
  contents.MTD_eastT_vs_westT->SetXTitle("east time (ns)");
  //MTD_eastT_vs_westT->SetYTitle("west time (ns)");
	
  contents.MTD_eastT_westT=new TH1F("MTD_eastT_westT","MTD eastT - westT",40,-10,10);
  contents.MTD_eastT_westT->SetXTitle("(east-west) time diff (ns)");
  //MTD_eastT_vs_westT->SetYTitle("west time (ns)");
	
  contents.MTD_hits_vs_TOF_hits=new TH2F("MTD_hits_vs_TOF_hits","MTD chan vs TOF MRPC chan ",36,-0.5,35.5,960,-0.5,959.5);
  contents.MTD_hits_vs_TOF_hits->SetXTitle("MTD chan #");

  char imtd[200];
  for (int itray=0; itray<nMTDtrays; itray++){
    
    if (itray==0) sprintf(imtd,"1");
    if (itray==1) sprintf(imtd,"26C");
    if (itray==2) sprintf(imtd,"26EW");
    sprintf(tmpchr,"MTD%s_east_adc",imtd);
    contents.MTD_adc[itray][0]=new TH1F(tmpchr,tmpchr,100,0,2048);
    contents.MTD_adc[itray][0]->SetXTitle("ADC");
    sprintf(tmpchr,"MTD%s_west_adc",imtd);
    contents.MTD_adc[itray][1]=new TH1F(tmpchr,tmpchr,100,0,2048);
    contents.MTD_adc[itray][1]->SetXTitle("ADC");
    
    sprintf(tmpchr,"MTD%s_east_tac",imtd);
    contents.MTD_tac[itray][0]=new TH1F(tmpchr,tmpchr,150,0,3000);
    contents.MTD_tac[itray][0]->SetXTitle("TAC");
    sprintf(tmpchr,"MTD%s_west_tac",imtd);
    contents.MTD_tac[itray][1]=new TH1F(tmpchr,tmpchr,150,0,3000);
    contents.MTD_tac[itray][1]->SetXTitle("TAC");
  }//tray loop over


  // Add root histograms to Plots
  JevpPlot *plots[100];
  int n=0;

  //JLatex *l;
  //JLine *ln;

  plots[n] = new JevpPlot(contents.MTD26E_hitmap[0][0]);
  plots[++n] = new JevpPlot(contents.MTD26E_hitmap[0][1]);
  plots[++n] = new JevpPlot(contents.MTD26E_hitmap[1][0]);
  plots[++n] = new JevpPlot(contents.MTD26E_hitmap[1][1]);
  
  plots[++n] = new JevpPlot(contents.MTD26C_hitmap[0][0]);
  plots[++n] = new JevpPlot(contents.MTD26C_hitmap[0][1]);
  plots[++n] = new JevpPlot(contents.MTD26C_hitmap[1][0]);
  plots[++n] = new JevpPlot(contents.MTD26C_hitmap[1][1]);

  plots[++n] = new JevpPlot(contents.MTD26W_hitmap[0][0]);
  plots[++n] = new JevpPlot(contents.MTD26W_hitmap[0][1]);
  plots[++n] = new JevpPlot(contents.MTD26W_hitmap[1][0]);
  plots[++n] = new JevpPlot(contents.MTD26W_hitmap[1][1]);
  
  plots[++n] = new JevpPlot(contents.MTD1_hitmap[0][0]);
  plots[++n] = new JevpPlot(contents.MTD1_hitmap[0][1]);
  plots[++n] = new JevpPlot(contents.MTD1_hitmap[1][0]);
  plots[++n] = new JevpPlot(contents.MTD1_hitmap[1][1]);

  plots[++n] = new JevpPlot(contents.MTD_ToT);
  plots[++n] = new JevpPlot(contents.MTD_eastT_vs_westT);
  plots[++n] = new JevpPlot(contents.MTD_eastT_westT);
  plots[++n] = new JevpPlot(contents.MTD_hits_vs_TOF_hits);

  plots[++n] = new JevpPlot(contents.MTD_adc[0][0]);
  plots[++n] = new JevpPlot(contents.MTD_adc[0][1]);
  plots[++n] = new JevpPlot(contents.MTD_adc[1][0]);
  plots[++n] = new JevpPlot(contents.MTD_adc[1][1]);
  plots[++n] = new JevpPlot(contents.MTD_adc[2][0]);
  plots[++n] = new JevpPlot(contents.MTD_adc[2][1]);

  plots[++n] = new JevpPlot(contents.MTD_tac[0][0]);
  plots[++n] = new JevpPlot(contents.MTD_tac[0][1]);
  plots[++n] = new JevpPlot(contents.MTD_tac[1][0]);
  plots[++n] = new JevpPlot(contents.MTD_tac[1][1]);
  plots[++n] = new JevpPlot(contents.MTD_tac[2][0]);
  plots[++n] = new JevpPlot(contents.MTD_tac[2][1]);

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    // LOG("JEFF", "Adding plot %d",i);
    addPlot(plots[i]);
    plots[i]->getHisto(0)->histo->SetFillColor(19);
    plots[i]->optstat=110110;
  }
  
}
  
void mtdBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "TriggerPlotBuilder starting run #%d",rdr->run);
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
	if((edgeid !=4) && (edgeid!=5)) continue; //leading edge or trailing edge
				
	int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
	int tdigboardid=tdcid/4;   // for halftray 0, 0-2 for tdig0; 4-6 for tdig1
	if (halftrayid==1) tdigboardid=4; //for halftray1, 0-2 for tdig 4
	int  tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
	//int globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray
	timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
	time = timeinbin * 25./1024;   // time in ns 
				
	//int moduleid=-1;
	int globalstripid=-1;
	int stripid=-1;
	int zendid=-1;
	//				
	if(trayid==26){
	  globalstripid=tdcchan2globalstrip(tdigboardid,tdcid,tdcchan,trayid);
	  stripid=(globalstripid-1)%12+1;
	  zendid=(globalstripid-1)/12; //0 for Hi Z end; 1 for Lo Z end
	}
	if(trayid==1){
	  globalstripid=tdcchan2globalstrip(tdigboardid,tdcid,tdcchan,trayid);
	  stripid=globalstripid;
	  zendid=tdigboardid/4; //0 for east end; 1 for west end
	}
				
	//				cout<<"=======MTD========:: trayid="<<trayid<<" tdigboardid="<<tdigboardid<<" zendid="<<zendid<<
	//				" halftray="<<halftrayid<<" tdcid="<<tdcid<<" tdcchan="<<tdcchan<<" stripid="<<stripid <<endl;
				
	if(trayid==26&&tdigboardid==0) contents.MTD26E_hitmap[zendid][edgeid-4]->Fill(stripid);
	if(trayid==26&&tdigboardid==1) contents.MTD26C_hitmap[zendid][edgeid-4]->Fill(stripid);
	if(trayid==26&&tdigboardid==4) contents.MTD26W_hitmap[zendid][edgeid-4]->Fill(stripid);
	if(trayid==1&&tdigboardid==0) contents.MTD1_hitmap[0][edgeid-4]->Fill(stripid);
	if(trayid==1&&tdigboardid==4) contents.MTD1_hitmap[1][edgeid-4]->Fill(stripid);
      }  // end loop nword
    }
  }
	

  // now do the trigger plots...
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) {
    LOG(WARN, "No trigger data");
    return;
  }

  //MTD for run 11
  //0 for MTD-1; 1 for MTD-26, the center tray; 2 for MTD-26, the left and right trays. 
  float west_adc[3];
  float west_tac[3];
  float east_adc[3];
  float east_tac[3];
  for(int itray=0;itray<nMTDtrays;itray++){ 
    west_adc[itray]=-9999.;
    west_tac[itray]=-9999.;
    east_adc[itray]=-9999.;
    east_tac[itray]=-9999.;
  }
	
  west_adc[0]= trgd->mtdAtAddress(8, 0);
  west_tac[0]= trgd->mtdAtAddress(12, 0);
  east_adc[0]= trgd->mtdAtAddress(9, 0);
  east_tac[0]= trgd->mtdAtAddress(13, 0);
	
  west_adc[1]= trgd->mtdAtAddress(16, 0);
  west_tac[1]= trgd->mtdAtAddress(20, 0);
  east_adc[1]= trgd->mtdAtAddress(17, 0);
  east_tac[1]= trgd->mtdAtAddress(21, 0);
	
  west_adc[2]= trgd->mtdAtAddress(24, 0);
  west_tac[2]= trgd->mtdAtAddress(28, 0);
  east_adc[2]= trgd->mtdAtAddress(25, 0);
  east_tac[2]= trgd->mtdAtAddress(29, 0);
	
  for (int itray=0;itray<nMTDtrays; itray++){
    if(east_adc[itray]>0)contents.MTD_adc[itray][0]->Fill(east_adc[itray]);
    if(west_adc[itray]>0)contents.MTD_adc[itray][1]->Fill(west_adc[itray]);
    if(east_tac[itray]>0)contents.MTD_tac[itray][0]->Fill(east_tac[itray]);
    if(west_tac[itray]>0)contents.MTD_tac[itray][1]->Fill(west_tac[itray]);
		
  }
	
  // for (int itray=0;itray<=2; itray++){cout <<"itray="<<itray<<endl;cout<<"eastadc="<<east_adc[itray]<<" westadc[itray]="<<west_adc[itray]<<" easttac="<<east_tac[itray]<<" westtac0="<<west_tac[itray]<<endl;}
	
  //if (west_adc[1]>0) cout<<"west_adc[1]="<<west_adc[1]<<"!!!!!!!!!!!<"endl;
	
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



int mtdBuilder::tdcchan2globalstrip(int tdigboardid,int tdcid,int tdcchan,int trayid)
{
  int globalstripid=-1;
  if(trayid==26){
    if (tdcid>3) tdcid=tdcid-4; //scale to H#
    int globaltdcchan=(tdcid+1)*10+tdcchan;
    int mtdstrip[24]={34,22,10,37,27,17,33,23,16,36,26,15,
		      21,12,32,20,14,35,25,13,30,24,11,31};
    for(int i=0;i<24;i++){
      if(mtdstrip[i]==globaltdcchan) {globalstripid=i+1;break;}
    }
  }
	
  if(trayid==1){
    int globaltdcchan=(tdcid+1)*10+tdcchan;
    int mtdstrip[18]=  {34,22,10,37,27,17, 32,20,30,24,11,31, 33,23,16,36,26,15};
    for(int i=0;i<18;i++){
      if(mtdstrip[i]==globaltdcchan) {globalstripid=i+1;break;}
    }
  }
	
  return globalstripid;
}
