#include <stdio.h>
#include <stdlib.h>
#include <algorithm>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_TOF/daq_tof.h"
#include "StDaqLib/TRG/trgStructures2009.h"
#include "StEvent/StTriggerData2009.h"

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


int mtdBuilder::tdcchan2MTDchan(int globaltdcchan,int trayid)
{

  if(trayid !=124) return -1;

  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

//                      1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
  int MTDTDCchan[36]={  9, 12, 13, 16, 17, 21,  1,  2,  3, 19, 20, 22,  0,  5,  7, 10, 14, 15,
                      105,108,109,112,113,117, 97, 98, 99,115,116,118, 96,101,103,106,110,111};


  int inputglobalchan=globaltdcchan;
  int mtdchan=-1;

  for(int i=0;i<36;i++){
    if(MTDTDCchan[i]==inputglobalchan) {mtdchan=i;break;}
  }

  return mtdchan;
}
int mtdBuilder::tdcchan2mrpcchan(int globaltdcchan)
{
  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

  int tdcidmap[4][6] = { {0,1,0,1,0,1}, {2,0,2,0,1,0}, {1,2,0,2,0,2}, {2,1,2,1,2,1}};
  int tdcchanmap[4][6]={ {7,7,0,2,5,6}, {7,4,4,2,3,6}, {0,2,3,3,1,6}, {0,5,1,4,5,1}};

  int theglobalmodulechan[192];
  int theglobaltdcchan[192];

  for(int isec=0;isec<8;isec++){
    for(int imodule=0;imodule<4;imodule++){
      for(int ipad=0;ipad<6;ipad++){
        int globalmodule  =  isec*24 + imodule*6 + ipad;
        int globaltdc     =  isec*24 + tdcidmap[imodule][ipad]*8+tdcchanmap[imodule][ipad];
        theglobalmodulechan[globalmodule]=globalmodule;
        theglobaltdcchan[globalmodule]=globaltdc;
        //cout<<"global module chan="<<globalmodule<<" global tdc chan="<<globaltdc<<endl;
     }
    }
  }
  int returnthis=0;

  //int thistdcchan=tdig*24+(tdcid%4)*8+tdcchan;
  int thistdcchan=globaltdcchan;
  for(int i=0;i<192;i++){
    if(thistdcchan == theglobaltdcchan[i]) {returnthis = i;break;}
  }
  return returnthis;
}


mtdBuilder::mtdBuilder() {
  plotsetname = (char *)"mtd";
  
  int np = sizeof(contents) / sizeof(TH1 *);

  plots = new ptrJevpPlot[np]();
  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(plots, 0, sizeof(JevpPlot *) * np);
}

mtdBuilder::~mtdBuilder() {
  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(plots[i]) delete plots[i];
  }
  
  delete plots;
}

void mtdBuilder::initialize(int argc, char *argv[]) {

  // Build Root Histograms...
  contents.MTD_hitmap[0] = new TH1F("MTD_LE_hitmap","MTD Leading edge hitmap",36,-0.5,35.5);
  contents.MTD_hitmap[0]->SetXTitle("Chan #");
 
  contents.MTD_hitmap[1]= new TH1F("MTD_TE_hitmap","MTD Trailing edge hitmap",36,-0.5,35.5);
  contents.MTD_hitmap[1]->SetXTitle("Chan #");
  
  contents.MTD_ToT = new TH2F("MTD_ToT","MTD ToT vs Chan #",36,-0.5,35.5,70,0,70);
  contents.MTD_ToT->SetXTitle("MTD Chan#");
  contents.MTD_ToT->SetYTitle("MTD Tot");

  contents.MTD_eastT_vs_westT=new TH2F("MTD_eastT_vs_westT","MTD eastT vs westT",1024,0,51200,1024,0,51200);
  contents.MTD_eastT_vs_westT->SetXTitle("east time (ns)");
  contents.MTD_eastT_vs_westT->SetYTitle("west time (ns)");

  contents.MTD_eastT_westT=new TH1F("MTD_eastT_westT","MTD eastT - westT",40,-10,10);
  contents.MTD_eastT_westT->SetXTitle("(east-west) time diff (ns)");
  contents.MTD_eastT_vs_westT->SetYTitle("west time (ns)");

  contents.MTD_hits_vs_TOF_hits=new TH2F("MTD_hits_vs_TOF_hits","MTD chan vs TOF MRPC chan ",36,-0.5,35.5,960,-0.5,959.5);
  contents.MTD_hits_vs_TOF_hits->SetXTitle("MTD chan #");


  contents.MTD_adc[0]=new TH1F("MTD_east_adc","MTD ADC (east)",100,0,2048);
  contents.MTD_adc[0]->SetXTitle("ADC");
  contents.MTD_adc[1]=new TH1F("MTD_west_adc","MTD ADC (west)",100,0,2048);
  contents.MTD_adc[1]->SetXTitle("ADC");

  contents.MTD_tac[0]=new TH1F("MTD_east_tac","MTD TAC (east)",150,0,3000);
  contents.MTD_tac[0]->SetXTitle("TAC");
  contents.MTD_tac[1]=new TH1F("MTD_west_tac","MTD TAC (west)",150,0,3000);
  contents.MTD_tac[1]->SetXTitle("TAC");

  contents.MTD_eastTac_vs_westTac=new TH2F("MTD_eastTac_vs_westTac","MTD eastTac vs westTac",150,0,3000,150,0,3000);
  contents.MTD_eastTac_vs_westTac->SetXTitle("east TAC");
  contents.MTD_eastTac_vs_westTac->SetYTitle("west TAC");

  contents.MTD_aveTac_vs_vpd_aveTac=new TH2F("MTD_aveTac_vs_vpd_aveTac","MTD aveTac vs vpd aveTac",150,0,3000,100,500,2048);
  contents.MTD_aveTac_vs_vpd_aveTac->SetXTitle("MTD aveTAC");


  // Add root histograms to Plots
  JevpPlot *plots[100];
  int n=0;

  JLatex *l;
  JLine *ln;

  plots[n] = new JevpPlot(contents.MTD_hitmap[0]);
  l = new JLatex(7.5, .8, "east end");
  plots[n]->addElement(l);
  l = new JLatex(26.5, .8, "east end");
  plots[n]->addElement(l);
  ln = new JLine(17.5,.1,17.5,.9);
  plots[n]->addElement(ln);

  plots[++n] = new JevpPlot(contents.MTD_hitmap[1]);
  l = new JLatex(7.5, .8, "east end");
  plots[n]->addElement(l);
  l = new JLatex(26.5, .8, "east end");
  plots[n]->addElement(l);
  ln = new JLine(17.5,.1,17.5,.9);
  plots[n]->addElement(ln);

  plots[++n] = new JevpPlot(contents.MTD_ToT);
  plots[n]->setDrawOpts("colz");  
  l = new JLatex(7.5, .8, "east end");
  plots[n]->addElement(l);
  l = new JLatex(26.5, .8, "east end");
  plots[n]->addElement(l);
  ln = new JLine(17.5,.1,17.5,.9);
  plots[n]->addElement(ln);
  
  plots[++n] = new JevpPlot(contents.MTD_eastT_vs_westT);
  plots[n]->setDrawOpts("col");

  plots[++n] = new JevpPlot(contents.MTD_eastT_westT);
  plots[++n] = new JevpPlot(contents.MTD_hits_vs_TOF_hits);
  plots[n]->setDrawOpts("col");
  plots[++n] = new JevpPlot(contents.MTD_adc[0]);
  plots[++n] = new JevpPlot(contents.MTD_adc[1]);
  plots[++n] = new JevpPlot(contents.MTD_tac[0]);
  plots[++n] = new JevpPlot(contents.MTD_tac[1]);
  plots[++n] = new JevpPlot(contents.MTD_eastTac_vs_westTac);
  plots[n]->setDrawOpts("col");
  plots[++n] = new JevpPlot(contents.MTD_aveTac_vs_vpd_aveTac);

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG("JEFF", "Adding plot %d",i);
    addPlot(plots[i]);
    plots[i]->getHisto(0)->histo->SetFillColor(19);
    plots[i]->optstat=110110;
  }
  
}
  
void mtdBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "TriggerPlotBuilder starting run #%d",rdr->run);
  resetAllPlots();
}

void mtdBuilder::event(daqReader *rdr)
{
  StTriggerData2009 *trgd2009;
  int run = rdr->run;

  daq_dta *dd = rdr->det("trg")->get("raw");
  if(dd && dd->iterate()) {
    char *td = (char *)dd->Void;
    
    if(td[3] != 0x40) {
      LOG("ERR", "TRG RAW: version mismatch 0x%2x-0x%2x-0x%2x-0x%2x", td[0], td[1], td[2], td[3]);
      return;
    }

    TriggerDataBlk2009 *trgdatablock2009 = (TriggerDataBlk2009 *)td;
    trgd2009 = new StTriggerData2009(trgdatablock2009, run);
  }
  else {
    LOG(ERR, "No trigger data exists...");
    return;
  }
  
  StTriggerData *trgd = (StTriggerData *)trgd2009;


  // triggerinfo...
  float east_adc=trgd->mtdAdc(east,0);
  float west_adc=trgd->mtdAdc(west,0);
  float east_tac=trgd->mtdTdc(east,0);
  float west_tac=trgd->mtdTdc(west,0);

  if(east_adc>0)contents.MTD_adc[0]->Fill(east_adc);
  if(west_adc>0)contents.MTD_adc[1]->Fill(west_adc);
  if(east_tac>0)contents.MTD_tac[0]->Fill(east_tac);
  if(west_tac>0)contents.MTD_tac[1]->Fill(west_tac);

  if((east_tac+west_tac)>0) contents.MTD_eastTac_vs_westTac->Fill(east_tac,west_tac);

  int vpd_maxTacEast = trgd->vpdEarliestTDC((StBeamDirection)0);
  int vpd_maxTacWest = trgd->vpdEarliestTDC((StBeamDirection)1);

  //cout<<" eastadc="<<east_adc<<" westadc="<<west_adc<<" easttac="<<east_tac<<" westtac="<<west_tac<<" vpdeast="<<vpd_maxTacEast<<" vpdwest="<<vpd_maxTacWest<<endl;
  if((east_tac+west_tac)>0)contents.MTD_aveTac_vs_vpd_aveTac->Fill( (east_tac+west_tac)/2.,(vpd_maxTacEast+vpd_maxTacWest)/2.);

  if(trgd) delete trgd;

  leadinghits.clear();
  trailinghits.clear();

  // MTD data...
  int timeinbin = 0;
  float time = 0;
  int halftrayid = -1;
  int trayid = -1;
  dd = rdr->det("tof")->get("legacy");
  tof_t *tof;
  if(dd) {
    while(dd->iterate()) {
      tof = (tof_t *)dd->Void;
      
      for(int ifib=0;ifib<4;ifib++){
	int ndataword = tof->ddl_words[ifib];    
   	if(ndataword<=0) continue;
	for(int iword=0;iword<ndataword;iword++){
	  int dataword=tof->ddl[ifib][iword];
	  if( (dataword&0xF0000000)>>28 == 0x2) continue;  
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

	  if(!( (trayid>105 && trayid <111) || trayid ==124)) continue;

	  if( (dataword&0xF0000000)>>28 == 0x6) {continue;}
	  //
	  int edgeid =int( (dataword & 0xf0000000)>>28 );
	  if((edgeid !=4) && (edgeid!=5)) continue;

	  int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
	  int tdigboardid=tdcid/4;   // 0-3 for half tray.
	  int  tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
	  int globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray
	  timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
	  time = timeinbin * 25./1024;   // time in ns 

	  //int moduleid=-1;
	  int modulechan=-1;
	  int globalmodulechan=-1;

	  if(trayid==124) {
	    modulechan=tdcchan2MTDchan(globaltdcchan,trayid);
	  } else if(trayid>0 && trayid<121){
	    modulechan=tdcchan2mrpcchan(globaltdcchan);
	  }
	  globalmodulechan=modulechan;

	  //cout<<"MTD===:: tray="<<trayid<<" halftray="<<halftrayid<<" globaltdcchan="<<globaltdcchan<<" modulechan="<<modulechan<<" time="<<time<<" edgeid="<<edgeid<<endl;

	  // fill hitmap.
	  if(trayid==124) contents.MTD_hitmap[edgeid-4]->Fill(modulechan);

	  double numberforsort= time+globalmodulechan*1.e5+trayid*1.e8;
	  if(edgeid==4)leadinghits.push_back(numberforsort);
	  if(edgeid==5)trailinghits.push_back(numberforsort);

	}  // end loop nword
      }  // end loop fiber
  
      sort(leadinghits.begin(),leadinghits.end());
      sort(trailinghits.begin(),trailinghits.end());

      float leadingtime[36],trailingtime[36];  // will only get one hit of each channel
      for(int i=0;i<36;i++){leadingtime[i]=0.;trailingtime[i]=0.;}

      for(int ich=0;ich<36;ich++){
	for(unsigned int ile=0;ile<leadinghits.size();ile++){
	  double thisnumber = leadinghits[ile];
	  int thistrayid= int(thisnumber/1.e8);
	  if(thistrayid !=124)continue; 
	  int  thismodule=int((thisnumber-thistrayid*1.e8)/1.e5);
	  float thistime=  thisnumber-thistrayid*1.e8-thismodule*1.e5;
	  if(thismodule == ich) {leadingtime[ich]= thistime;break;} 
	}
      }
      for(int ich=0;ich<36;ich++){
	for(unsigned int ite=0;ite<trailinghits.size();ite++){
	  double thisnumber = trailinghits[ite];
	  int thistrayid= int(thisnumber/1.e8);
	  if(thistrayid !=124)continue; 
	  int  thismodule=int((thisnumber-thistrayid*1.e8)/1.e5);
	  float thistime=  thisnumber-thistrayid*1.e8-thismodule*1.e5;
	  if(thismodule == ich) {trailingtime[ich]= thistime;break;} 
	}
      }
 
      for(int ich=0;ich<36;ich++){
	if(leadingtime[ich]*trailingtime[ich]<1) continue;
	float ToT = trailingtime[ich]-leadingtime[ich];
	if(ToT<0) ToT = ToT + 51200;
	if(ToT>0) contents.MTD_ToT->Fill(ich,ToT);
      }

      for(int ieast=0;ieast<18;ieast++){
	int iwest=ieast+18;
	if(leadingtime[ieast]*leadingtime[iwest]<1) continue;
	contents.MTD_eastT_vs_westT->Fill(leadingtime[ieast],leadingtime[iwest]);
	float timediff=leadingtime[iwest]-leadingtime[ieast];
	if(timediff>51200/2) timediff = timediff-51200;
	if(timediff<-51200/2) timediff = timediff+51200;
	//cout<<"ieast="<<ieast<<" time="<<leadingtime[ieast]<<" iwest="<<iwest<<" time="<<leadingtime[iwest]<<" diff="<<timediff<<endl;
	contents.MTD_eastT_westT->Fill(timediff);
      }

      // 
      for(unsigned int ile=0;ile<leadinghits.size();ile++){
	double number1 = leadinghits[ile];
	int trayid1= int(number1/1.e8);
	int modulechan1=int((number1-trayid1*1.e8)/1.e5);
	int traymodulechan=modulechan1+ 192*(trayid1-106);
	if(trayid1 ==124)continue; 
	//cout<<" trayid1="<<trayid1<<" modulechan="<<modulechan1<<endl;
	for(unsigned int ile2=0;ile2<leadinghits.size();ile2++){
	  double number2 = leadinghits[ile2];
	  int trayid2= int(number2/1.e8);
	  int  modulechan2=int((number2-trayid2*1.e8)/1.e5);
	  if(trayid2 !=124)continue; 
	  //cout<<" trayid2="<<trayid2<<" modulechan="<<modulechan2<<endl;
	  contents.MTD_hits_vs_TOF_hits->Fill(modulechan2,traymodulechan);
	}
      }
    }
  }  
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

