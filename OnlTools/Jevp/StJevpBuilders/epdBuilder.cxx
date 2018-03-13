#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TStyle.h>
#include <TPave.h>
#include <TGaxis.h>
#include <TPaletteAxis.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TProfile2D.h>

#include <TMath.h>
#include <math.h>
#include "epdBuilder.h"
#include <RTS/include/rtsLog.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>


// temp names used to assign hist titiels ans names
char buff1[255];
char buff2[255];
const static std::string ewstring[2] = {"East","West"};

// Arrays to convert [ew,pp,tt] to [phi,r] of polar plot
float pi  = TMath::Pi();

float phiTiles[2][12][32];
float rTiles[32] ={0.0, 6.8,10.608, 10.61, 14.93, 14.93, 19.80, 19.80, 25.22, 25.22, 
  30.65, 30.65, 36.11, 36.11, 41.52, 41.52, 46.95, 46.95, 
  52.39, 52.39, 57.82, 57.82, 63.26, 63.26, 68.70, 68.70, 
  74.13, 74.13, 79.57, 79.57, 84.99, 84.99};

float phiTT[2][32] ={ 
  {0.0, 2.617994e-01,  1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01,
    1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 
    1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01,
    1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01,
    1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01}
  ,
    {0.0, 2.617994e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01,
      1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 
      1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01,
      1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01,
      1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01, 3.926991e-01, 1.308997e-01 }
};


float phiSS[2][12] = {  
  { 2*pi/6, pi/6, 0.0, 11*pi/6, 10*pi/6, 9*pi/6, 8*pi/6, 7*pi/6, 6*pi/6, 5*pi/6, 4*pi/6, 3*pi/6}
  ,
    { 3*pi/6, 4*pi/6, 5*pi/6, 6*pi/6, 7*pi/6, 8*pi/6, 9*pi/6, 10*pi/6, 11*pi/6, 0.0, pi/6, 2*pi/6}
};

Int_t colors[] = {0,3,8,7,9,6,4,2}; // Colors for TPalette 


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

  // Initialise polar plots
  gStyle->SetOptStat(0);
  gStyle->SetPalette((sizeof(colors)/sizeof(Int_t)), colors);

  for(int ew=0;ew<2;ew++){
    for(int pp=0;pp<12;pp++){
      for(int tt=0;tt<32;tt++){
	phiTiles[ew][pp][tt] = phiSS[ew][pp] + phiTT[ew][tt];
      }
    }
  }
  contents.hDummyAdc[0] = new TH2D("EPD_East_ADC", "EPD East <ADC>", 32,-100,100, 32,-100,100);
  contents.hDummyAdc[1] = new TH2D("EPD_West_ADC", "EPD West <ADC>", 32,-100,100, 32,-100,100);
  contents.hDummyTac[0] = new TH2D("EPD_East_TAC", "EPD East <TAC>", 32,-100,100, 32,-100,100);
  contents.hDummyTac[1] = new TH2D("EPD_West_TAC", "EPD West <TAC>", 32,-100,100, 32,-100,100);
  contents.hDummyHit[0] = new TH2D("EPD_East_Hit", "EPD East Hit count", 32,-100,100, 32,-100,100);
  contents.hDummyHit[1] = new TH2D("EPD_West_Hit", "EPD West Hit count", 32,-100,100, 32,-100,100);

  contents.hDummyAdc[0]->GetXaxis()->SetTitle("x(cm)");
  contents.hDummyAdc[0]->GetXaxis()->CenterTitle(true);
  contents.hDummyAdc[0]->GetYaxis()->SetTitle("y(cm)");
  contents.hDummyAdc[0]->GetYaxis()->CenterTitle(true);
  contents.hDummyAdc[1]->GetXaxis()->SetTitle("x(cm)");
  contents.hDummyAdc[1]->GetXaxis()->CenterTitle(true);
  contents.hDummyAdc[1]->GetYaxis()->SetTitle("y(cm)");
  contents.hDummyAdc[1]->GetYaxis()->CenterTitle(true);

  contents.hDummyTac[0]->GetXaxis()->SetTitle("x(cm)");
  contents.hDummyTac[0]->GetXaxis()->CenterTitle(true);
  contents.hDummyTac[0]->GetYaxis()->SetTitle("y(cm)");
  contents.hDummyTac[0]->GetYaxis()->CenterTitle(true);
  contents.hDummyTac[1]->GetXaxis()->SetTitle("x(cm)");
  contents.hDummyTac[1]->GetXaxis()->CenterTitle(true);
  contents.hDummyTac[1]->GetYaxis()->SetTitle("y(cm)");
  contents.hDummyTac[1]->GetYaxis()->CenterTitle(true);

  contents.hDummyHit[0]->GetXaxis()->SetTitle("x(cm)");
  contents.hDummyHit[0]->GetXaxis()->CenterTitle(true);
  contents.hDummyHit[0]->GetYaxis()->SetTitle("y(cm)");
  contents.hDummyHit[0]->GetYaxis()->CenterTitle(true);
  contents.hDummyHit[1]->GetXaxis()->SetTitle("x(cm)");
  contents.hDummyHit[1]->GetXaxis()->CenterTitle(true);
  contents.hDummyHit[1]->GetYaxis()->SetTitle("y(cm)");
  contents.hDummyHit[1]->GetYaxis()->CenterTitle(true);

  contents.hPolPlotAdc[0][0] = new TProfile2D("EPD_East_Rg1_Adc", "EPD_East_Rg1_Adc", 12, 0., 2.*TMath::Pi(), 1,4.2,8.6); 
  contents.hPolPlotAdc[0][1] = new TProfile2D("EPD_East_Rg2_Adc", "EPD_East_Rg2_Adc", 24, 0., 2.*TMath::Pi(),15,8.6,89.27); 
  contents.hPolPlotAdc[1][0] = new TProfile2D("EPD_West_Rg1_Adc", "EPD_West_Rg1_Adc", 12, 0., 2.*TMath::Pi(), 1,4.2,8.6); 
  contents.hPolPlotAdc[1][1] = new TProfile2D("EPD_West_Rg2_Adc", "EPD_West_Rg2_Adc", 24, 0., 2.*TMath::Pi(),15,8.6,89.27); 

  contents.hPolPlotTac[0][0] = new TProfile2D("EPD_East_Rg1_Tac", "EPD_East_Rg1_Tac", 12, 0., 2.*TMath::Pi(), 1,4.2,8.6); 
  contents.hPolPlotTac[0][1] = new TProfile2D("EPD_East_Rg2_Tac", "EPD_East_Rg2_Tac", 24, 0., 2.*TMath::Pi(),15,8.6,89.27); 
  contents.hPolPlotTac[1][0] = new TProfile2D("EPD_West_Rg1_Tac", "EPD_West_Rg1_Tac", 12, 0., 2.*TMath::Pi(), 1,4.2,8.6); 
  contents.hPolPlotTac[1][1] = new TProfile2D("EPD_West_Rg2_Tac", "EPD_West_Rg2_Tac", 24, 0., 2.*TMath::Pi(),15,8.6,89.27); 

  contents.hPolPlotHit[0][0] = new TH2D("EPD_East_Rg1_Hit", "EPD_East_Rg1_Hit", 12, 0., 2.*TMath::Pi(), 1,4.2,8.6); 
  contents.hPolPlotHit[0][1] = new TH2D("EPD_East_Rg2_Hit", "EPD_East_Rg2_Hit", 24, 0., 2.*TMath::Pi(),15,8.6,89.27); 
  contents.hPolPlotHit[1][0] = new TH2D("EPD_West_Rg1_Hit", "EPD_West_Rg1_Hit", 12, 0., 2.*TMath::Pi(), 1,4.2,8.6); 
  contents.hPolPlotHit[1][1] = new TH2D("EPD_West_Rg2_Hit", "EPD_West_Rg2_Hit", 24, 0., 2.*TMath::Pi(),15,8.6,89.27); 

  contents.hPolPlotAdc[0][0]->GetZaxis()->SetRangeUser(0,300);
  contents.hPolPlotAdc[0][1]->GetZaxis()->SetRangeUser(0,300);
  contents.hPolPlotAdc[1][0]->GetZaxis()->SetRangeUser(0,300);
  contents.hPolPlotAdc[1][1]->GetZaxis()->SetRangeUser(0,300);

  contents.hPolPlotTac[0][0]->GetZaxis()->SetRangeUser(0,4000);
  contents.hPolPlotTac[0][1]->GetZaxis()->SetRangeUser(0,4000);
  contents.hPolPlotTac[1][0]->GetZaxis()->SetRangeUser(0,4000);
  contents.hPolPlotTac[1][1]->GetZaxis()->SetRangeUser(0,4000);

  contents.hPolPlotHit[0][0]->GetZaxis()->SetRangeUser(0,1000000);
  contents.hPolPlotHit[0][1]->GetZaxis()->SetRangeUser(0,1000000);
  contents.hPolPlotHit[1][0]->GetZaxis()->SetRangeUser(0,1000000);
  contents.hPolPlotHit[1][1]->GetZaxis()->SetRangeUser(0,1000000);




  for(int ew=0;ew<2;ew++){
    for(int pp=0;pp<12;pp++){
      for(int tile=0;tile<32;tile++){
	sprintf(buff1,"adc_%d_%d_%d",ew,pp,tile);
	sprintf(buff2,"ADC %s PP-%d Tile-%d",ewstring[ew].c_str(),pp+1,tile);
	contents.hADC[ew][pp][tile] = new TH1D(buff1,buff2,1200,0,1200);
	contents.hADC[ew][pp][tile] ->SetFillColor(20);

	if(tile>9) continue;
	sprintf(buff1,"tac_%d_%d_%d",ew,pp,tile);
	sprintf(buff2,"TAC %s PP-%d Tile-%d",ewstring[ew].c_str(),pp+1,tile);
	contents.hTAC[ew][pp][tile] = new TH1D(buff1,buff2,400,0,4000);
	contents.hTAC[ew][pp][tile] ->SetFillColor(20);

      }
    }
  }


  contents.hHitCountEast = new TH1D("hHitCountEast","Hit Count East(EP101)",256,0,256);
  contents.hHitCountWest = new TH1D("hHitCountWest","Hit Count West(EP101)",256,0,256);
  contents.hTacDiff  = new TH1D("hTacDiff","Earliest TAC Diff",250,0,9000);
  contents.hEarliestTacEast = new TH1D("hEarliestTacEast", "Earliest TAC East",100,0,4000);
  contents.hEarliestTacWest = new TH1D("hEarliestTacWest", "Earliest TAC West",100,0,4000);
  contents.hEarliestTacEvsW = new TH2D("hEarliestTacEvsW", "Earliest TAC (E vs. W)",100,0,4000,100,0,4000);



  //TPaletteAxis *paletteE = new TPaletteAxis(101.1905,-99.83871,108.396,100.1613,contents.hPolPlot[0][0]);
  //TPaletteAxis *paletteW = new TPaletteAxis(101.1905,-99.83871,108.396,100.1613,contents.hPolPlot[1][0]);
  //contents.hPolPlot[0][0]->GetListOfFunctions()->Add(paletteE,"br");
  //contents.hPolPlot[1][0]->GetListOfFunctions()->Add(paletteW,"br");


  // Add root histograms to Plots
  JevpPlot *plots[1200];
  int n=-1;

  plots[++n] = new JevpPlot();
  PlotHisto *ph = new PlotHisto(contents.hDummyAdc[0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotAdc[0][0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotAdc[0][1]);
  plots[n]->addHisto(ph);
  plots[n]->setDrawOpts("COLZ POL"); 
  plots[n]->setOptStat(0);

  plots[++n] = new JevpPlot();
  ph = new PlotHisto(contents.hDummyAdc[1]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotAdc[1][0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotAdc[1][1]);
  plots[n]->addHisto(ph);
  plots[n]->setDrawOpts("COLZ POL"); 
  plots[n]->setOptStat(0);

  plots[++n] = new JevpPlot();
  ph = new PlotHisto(contents.hDummyTac[0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotTac[0][0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotTac[0][1]);
  plots[n]->addHisto(ph);
  plots[n]->setDrawOpts("COLZ POL"); 
  plots[n]->setOptStat(0);

  plots[++n] = new JevpPlot();
  ph = new PlotHisto(contents.hDummyTac[1]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotTac[1][0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotTac[1][1]);
  plots[n]->addHisto(ph);
  plots[n]->setDrawOpts("COLZ POL"); 
  plots[n]->setOptStat(0);

  plots[++n] = new JevpPlot();
  ph = new PlotHisto(contents.hDummyHit[0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotHit[0][0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotHit[0][1]);
  plots[n]->addHisto(ph);
  plots[n]->setDrawOpts("COLZ POL"); 
  plots[n]->setOptStat(0);
  plots[n]->optlogz =1;

  plots[++n] = new JevpPlot();
  ph = new PlotHisto(contents.hDummyHit[1]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotHit[1][0]);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(contents.hPolPlotHit[1][1]);
  plots[n]->addHisto(ph);
  plots[n]->setDrawOpts("COLZ POL"); 
  plots[n]->setOptStat(0);
  plots[n]->optlogz =1;


  plots[++n] = new JevpPlot(contents.hHitCountEast);
  plots[n]->setOptStat(0);
  plots[n]->logx =1;
  plots[n]->logy =1;
  plots[++n] = new JevpPlot(contents.hHitCountWest);
  plots[n]->setOptStat(0);
  plots[n]->logx =1;
  plots[n]->logy =1;
  plots[++n] = new JevpPlot(contents.hTacDiff);
  plots[n]->setOptStat(0);
  plots[++n] = new JevpPlot(contents.hEarliestTacEast);
  plots[n]->setOptStat(0);
  plots[n]->logy =1;
  plots[++n] = new JevpPlot(contents.hEarliestTacWest);
  plots[n]->setOptStat(0);
  plots[n]->logy =1;
  plots[++n] = new JevpPlot(contents.hEarliestTacEvsW);
  plots[n]->setOptStat(0);



  for(int ew=0;ew<2;ew++){
    for(int pp=0;pp<12;pp++){
      for(int tt=0;tt<32;tt++){
	plots[++n] = new JevpPlot(contents.hADC[ew][pp][tt]);
	plots[n]->setOptStat(0);
	if(tt>9) continue;
	plots[++n] = new JevpPlot(contents.hTAC[ew][pp][tt]);
	plots[n]->setOptStat(0);
      }
    }
  }




  for(int i=0;i<=n;i++) {
    addPlot(plots[i]);
  }
}

void epdBuilder::startrun(daqReader *rdr) {

  short ew;
  short pp;
  short tile;

  Short_t qt_crate_adc;
  Short_t qt_board_adc;
  Short_t qt_channel_adc;

  Short_t qt_crate_tac;
  Short_t qt_board_tac;
  Short_t qt_channel_tac;

  Float_t adc_min;
  Float_t adc_max;
  Float_t tac_min;
  Float_t tac_max;

  LOG("JEFF", "epdBuilder starting run #%d",rdr->run);

  //Read EPD QT mapping
  char fn[256];
  sprintf(fn, "%s/epd/EPD_QTmap.txt",clientdatadir);
  FILE *fp = fopen(fn,"r");
  if(fp==NULL) {
    LOG("EPD","QT Mapping file does not exist");
    disable_builder = 1;
    return;
  }
  else {
    disable_builder = 0;
  }

  char line[255];
  fgets(line,255,fp);
  LOG("EPD","EPD is using mapping as on %s",line);
  while (fgets(line,255,fp)) {
    if(line[0]=='#') continue;
    sscanf(&line[0],"%hd %hd %hd %hd %hd %hd %hd %hd %hd",&ew,&pp,&tile,&qt_crate_adc, &qt_board_adc, &qt_channel_adc, &qt_crate_tac, &qt_board_tac, &qt_channel_tac);
    //cout<<ew<<"\t"<<pp<<"\t"<<tile<<"\t"<<qt_crate_adc<<"\t"<<qt_board_adc<<"\t"<<qt_channel_adc<<"\t"<<qt_crate_tac<<"\t"<<qt_board_tac<<"\t"<<qt_channel_tac<<endl;

    mEPDMap[ew][pp-1][tile].qt_crate_adc = qt_crate_adc;
    mEPDMap[ew][pp-1][tile].qt_board_adc = qt_board_adc;
    mEPDMap[ew][pp-1][tile].qt_channel_adc = qt_channel_adc;

    mEPDMap[ew][pp-1][tile].qt_crate_tac = qt_crate_tac;
    mEPDMap[ew][pp-1][tile].qt_board_tac = qt_board_tac;
    mEPDMap[ew][pp-1][tile].qt_channel_tac = qt_channel_tac;

  }
  fclose(fp);


  //Read EPD QT mapping
  sprintf(fn, "%s/epd/EPD_threshold.txt",clientdatadir);
  FILE *fp2 = fopen(fn,"r");
  if(fp2==NULL) {
    LOG("EPD","Threshold file does not exist");
    disable_builder = 1;
    return;
  }
  else {
    disable_builder = 0;
  }

  fgets(line,255,fp2);
  LOG("EPD","EPD is using mapping as on %s",line);
  while (fgets(line,255,fp2)) {
    if(line[0]=='#') continue;
    sscanf(&line[0],"%hd %hd %hd %f %f %f %f",&ew,&pp,&tile,&adc_min, &adc_max, &tac_min, &tac_max);
    //cout<<ew<<"\t"<<pp<<"\t"<<tile<<"\t"<<adc_min<<"\t"<<adc_max<<"\t"<<tac_min<<"\t"<<tac_max<<endl;

    mEPDMap[ew][pp-1][tile].adc_min = adc_min;
    mEPDMap[ew][pp-1][tile].adc_max = adc_max;
    mEPDMap[ew][pp-1][tile].tac_min = tac_min;
    mEPDMap[ew][pp-1][tile].tac_max = tac_max;
  }
  fclose(fp2);

  resetAllPlots();
  gStyle->SetPalette((sizeof(colors)/sizeof(Int_t)), colors);

  for(int ew=0;ew<2;ew++){
    for(int pp=0;pp<12;pp++){
      for(int tt=0;tt<32;tt++){
	if(tt==1){
	  contents.hPolPlotAdc[0][0]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);
	  contents.hPolPlotAdc[1][0]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);

	  contents.hPolPlotTac[0][0]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);
	  contents.hPolPlotTac[1][0]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);

	  contents.hPolPlotHit[0][0]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);
	  contents.hPolPlotHit[1][0]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);
	}else if(tt>1){
	  contents.hPolPlotAdc[0][1]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);
	  contents.hPolPlotAdc[1][1]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);

	  contents.hPolPlotTac[0][1]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);
	  contents.hPolPlotTac[1][1]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);

	  contents.hPolPlotHit[0][1]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);
	  contents.hPolPlotHit[1][1]->Fill(phiTiles[ew][pp][tt],rTiles[tt],0);

	}
      }
    }
  }

  for(Int_t i=1; i<=contents.hPolPlotHit[0][1]->GetNbinsX(); i++) {
    for(Int_t j=1; j<=contents.hPolPlotHit[0][1]->GetNbinsY(); j++) {
      contents.hPolPlotHit[0][0]->SetBinContent(i, j, 1 );
      contents.hPolPlotHit[0][1]->SetBinContent(i, j, 1 );
      contents.hPolPlotHit[1][0]->SetBinContent(i, j, 1 );
      contents.hPolPlotHit[1][1]->SetBinContent(i, j, 1 );
    }
  }

}

void epdBuilder::event(daqReader *rdr) {

  if(disable_builder) return;

  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  for(int ew=0;ew<2;ew++){
    for(int pp=0;pp<12;pp++){
      for(int tt=1;tt<32;tt++){ // tile stats from 1

	float adc = trgd->epdADC(mEPDMap[ew][pp][tt].qt_crate_adc, mEPDMap[ew][pp][tt].qt_board_adc, mEPDMap[ew][pp][tt].qt_channel_adc);
	float tac = trgd->epdADC(mEPDMap[ew][pp][tt].qt_crate_tac, mEPDMap[ew][pp][tt].qt_board_tac, mEPDMap[ew][pp][tt].qt_channel_tac);
	float adc_min = mEPDMap[ew][pp][tt].adc_min;
	float adc_max = mEPDMap[ew][pp][tt].adc_max;
	float tac_min = mEPDMap[ew][pp][tt].tac_min;
	float tac_max = mEPDMap[ew][pp][tt].tac_max;

	bool isGoodHit = (adc >= adc_min) && (adc <= adc_max ) && (tac >= tac_min) && (tac <= tac_max);
	bool isGoodHitwoTac = (adc >= adc_min) && (adc <= adc_max );

	if( (tt<10 && !isGoodHit) || ( tt>=10 && !isGoodHitwoTac) )continue;

	if(tt==1){
	  contents.hPolPlotAdc[ew][0] -> Fill(phiTiles[ew][pp][tt],rTiles[tt],adc);
	  contents.hPolPlotTac[ew][0] -> Fill(phiTiles[ew][pp][tt],rTiles[tt],tac);
	  contents.hPolPlotHit[ew][0] -> Fill(phiTiles[ew][pp][tt],rTiles[tt],1);
	}else if(tt>1){
	  contents.hPolPlotAdc[ew][1] -> Fill(phiTiles[ew][pp][tt],rTiles[tt],adc);
	  contents.hPolPlotTac[ew][1] -> Fill(phiTiles[ew][pp][tt],rTiles[tt],tac);
	  contents.hPolPlotHit[ew][1] -> Fill(phiTiles[ew][pp][tt],rTiles[tt],1);
	}

	contents.hADC[ew][pp][tt]->Fill(adc);
	if(tt<10)contents.hTAC[ew][pp][tt]->Fill(tac);

      }
    }
  }

  // From Next layers
  int hitCountEast = 0;
  int hitCountWest = 0;

  int maxTacEast = 0;
  int maxTacWest = 0;
  unsigned short tacDiff = 0;

  //East EP001
  unsigned short dsmEP001OutR = trgd->epdLayer1(0, 0);
  unsigned short dsmEP001OutL = trgd->epdLayer1(1, 0);
  unsigned int dsmEP001Out = (dsmEP001OutL<<16) + dsmEP001OutR;
  int maxTac03 = (dsmEP001Out >> 12) & 0xfff; // max tac from channels 0:3
  int maxTac47 = (dsmEP001Out >> 0 ) & 0xfff; // max tac from channels 4:7
  hitCountEast = (dsmEP001Out >> 24) & 0xff;
  maxTacEast   = (maxTac03>maxTac47)?maxTac03:maxTac47;

  //East EP002
  unsigned short dsmEP002OutR = trgd->epdLayer1(2, 0);
  unsigned short dsmEP002OutL = trgd->epdLayer1(3, 0);
  unsigned int dsmEP002Out = (dsmEP002OutL<<16) + dsmEP002OutR;
  maxTac03 = (dsmEP002Out >> 12) & 0xfff; // max tac from channels 0:3
  maxTac47 = (dsmEP002Out >> 0 ) & 0xfff; // max tac from channels 4:7
  hitCountWest = (dsmEP002Out >> 24) & 0xff;
  maxTacWest   = (maxTac03>maxTac47)?maxTac03:maxTac47;

  //From EPD-DSM EP101
  unsigned short dsmL2EpdOutput = trgd->vertexDSM(5);
  tacDiff = (unsigned short)(dsmL2EpdOutput & 0x1fff); //(0-12)   EPD TAC-Difference

  contents.hEarliestTacEast -> Fill(maxTacEast);
  contents.hEarliestTacWest -> Fill(maxTacWest);
  contents.hEarliestTacEvsW -> Fill(maxTacEast,maxTacWest);
  if(tacDiff>0)     contents.hTacDiff -> Fill(tacDiff);
  if(hitCountEast>0)contents.hHitCountEast -> Fill(hitCountEast);
  if(hitCountWest>0)contents.hHitCountWest -> Fill(hitCountWest);


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

