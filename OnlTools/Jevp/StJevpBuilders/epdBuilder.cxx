#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1D.h>
#include <TH2D.h>

#include <TMath.h>
#include <math.h>
#include "epdBuilder.h"
#include <RTS/include/rtsLog.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//
// QT board address for EPQ crate 0x1...
int qtbrdaddr[4] = {0,2,6,8};


// temp names used to assign hist titiels ans names
char buff1[255];
char buff2[255];
const static std::string ewstring[2] = {"East","West"};

ClassImp(epdBuilder);

typedef JevpPlot * ptrJevpPlot;

epdBuilder::epdBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"epd";

  int np = sizeof(contents) / sizeof(TH1 *);

  plots = new ptrJevpPlot[np]();
  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(plots, 0, sizeof(JevpPlot *) * np);
}

epdBuilder::~epdBuilder() {
  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(plots[i]) delete plots[i];
  }

  delete plots;
}

void epdBuilder::initialize(int argc, char *argv[]) {

  // Build Root Histograms...

  LOG(NOTE, "init");





  for(int ew=0;ew<2;ew++){
    for(int pp=0;pp<12;pp++){

      sprintf(buff1,"TAC_%s_PP_%d",ewstring[ew].c_str(),pp+1);
      sprintf(buff2,"TAC %s PP-%d",ewstring[ew].c_str(),pp+1);
      contents.hTAC[ew][pp] = new TH2F(buff1,buff2,31,0,31,750,0,3000);
      contents.hTAC[ew][pp]->GetXaxis()->SetTitle("Tile #");
      contents.hTAC[ew][pp]->GetXaxis()->SetTitleOffset(1.21);
      contents.hTAC[ew][pp]->GetYaxis()->SetTitle("TAC counts");
      contents.hTAC[ew][pp]->GetYaxis()->SetTitleOffset(1.21);

      sprintf(buff1,"ADC_%s_PP-%d",ewstring[ew].c_str(),pp+1);
      sprintf(buff2,"ADC %s PP %d",ewstring[ew].c_str(),pp+1);
      contents.hADC[ew][pp] = new TH2D(buff1,buff2,31,0,31,100,0,500);
      contents.hADC[ew][pp]->GetXaxis()->SetTitle("Tile #");
      contents.hADC[ew][pp]->GetXaxis()->SetTitleOffset(1.21);
      contents.hADC[ew][pp]->GetYaxis()->SetTitle("ADC counts");
      contents.hADC[ew][pp]->GetYaxis()->SetTitleOffset(1.21);
    }
  }

  // QT data
  for(int ibrd=0;ibrd<4;ibrd++){
    int qtbrd = qtbrdaddr[ibrd];
    sprintf(buff1,"QT board_0x1%d",qtbrd);
    sprintf(buff2,"QT_board-0x1%d",qtbrd);
    contents.hQT[ibrd] = new TH2D(buff1,buff2,32,0,32,1000,0,4095);
    contents.hQT[ibrd]->GetXaxis()->SetTitle("Ch #");
    contents.hQT[ibrd]->GetYaxis()->SetTitle("ADC or TAC"); 
    contents.hQT[ibrd]->GetYaxis()->SetTitleOffset(1.21);
  } 

  // Add root histograms to Plots
  JevpPlot *plots[1000];
  int n=-1;

  for(int ew=0;ew<2;ew++){
    for(int pp=0;pp<12;pp++){
      plots[++n] = new JevpPlot(contents.hTAC[ew][pp]);
      plots[n]->setOptStat(0);
      plots[n]->setDrawOpts("COLZ");
      plots[++n] = new JevpPlot(contents.hADC[ew][pp]);
      plots[n]->setOptStat(0);
      plots[n]->setDrawOpts("COLZ");
    }
  }

  for(int ibrd=0;ibrd<4;ibrd++){
    plots[++n] = new JevpPlot(contents.hQT[ibrd]);
    plots[n]->setOptStat(0);
    plots[n]->setDrawOpts("COLZ");
  } 

  for(int i=0;i<=n;i++) {
    addPlot(plots[i]);
  }
}

void epdBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "epdBuilder starting run #%d",rdr->run);
  //Read EPD mapping
  char fn[256];
  sprintf(fn, "%s/epd/epdMap.txt",clientdatadir);
  FILE *fp = fopen(fn,"r");
  if(fp==NULL) {
      LOG("EPD","Mapping file does not exist");
      disable_builder = 1;
      return;
  }
  else {
      disable_builder = 0;
  }

  short ew;
  short pp;
  short tile;
  short ss_number;
  short qt_board_address;
  short qt_channel_ADC;
  short qt_channel_TAC;
  short tuff_id;
  short tuff_group;

  char line[255];
  fgets(line,255,fp);
  LOG("EPD","EPD is using mapping as on %s",line);
  while (fgets(line,255,fp)) {
    if(line[0]=='#') continue;
    sscanf(&line[0],"%hd %hd %hd %hd %hd %hd %hd %hd %hd",&ew,&pp,&tile,&ss_number,&qt_board_address,&qt_channel_ADC,&qt_channel_TAC,&tuff_id,&tuff_group);
    //cout<<ew<<"\t"<<pp<<"\t"<<tile<<"\t"<<ss_number<<"\t"<<qt_board_address<<"\t"<<qt_channel_ADC<<"\t"<<qt_channel_TAC<<"\t"<<tuff_id<<"\t"<<tuff_group<<endl;

    mEPDMap[ew][pp-1][tile-1].qt_board_address = qt_board_address;
    mEPDMap[ew][pp-1][tile-1].qt_channel_ADC = qt_channel_ADC-1;
    mEPDMap[ew][pp-1][tile-1].qt_channel_TAC = qt_channel_TAC-1;

  }

  fclose(fp);



  resetAllPlots();
}

void epdBuilder::event(daqReader *rdr) {

    if(disable_builder) return;

    StTriggerData *trgd = getStTriggerData(rdr);
    if(!trgd) return;



    for(int ibrd=0;ibrd<4;ibrd++){
	for(int ch=0;ch<32;ch++){
	    contents.hQT[ibrd]->Fill(ch, trgd->fmsADC(5,qtbrdaddr[ibrd],ch,0));
	}
    }



    for(int ew=0;ew<2;ew++){
	for(int pp=0;pp<12;pp++){
	    for(int tile=0;tile<31;tile++){

		if( mEPDMap[ew][pp][tile].qt_channel_ADC > -900){
		    contents.hADC[ew][pp]->Fill(tile, trgd->fmsADC(5,mEPDMap[ew][pp][tile].qt_board_address, mEPDMap[ew][pp][tile].qt_channel_ADC,0) );
		}
		if( mEPDMap[ew][pp][tile].qt_channel_TAC > -900){
		    contents.hTAC[ew][pp]->Fill(tile, trgd->fmsADC(5,mEPDMap[ew][pp][tile].qt_board_address, mEPDMap[ew][pp][tile].qt_channel_TAC,0) );
		}
	    }
	}
    }



    if(trgd) delete trgd;
}

void epdBuilder::stoprun(daqReader *rdr) {
  //   printf("Stopping run #%d\n",run);
  //   status.setEndOfRun(1);
  //   send((TObject *)&status);
}

void epdBuilder::main(int argc, char *argv[])
{
  epdBuilder me;

  me.Main(argc, argv);
}

