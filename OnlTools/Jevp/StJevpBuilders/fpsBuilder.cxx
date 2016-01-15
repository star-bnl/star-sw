#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_FPS/daq_fps.h"
#include "StRoot/StFmsDbMaker/StFmsDbMaker.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "fpsBuilder.h"
#include <RTS/include/rtsLog.h>

ClassImp(fpsBuilder);
  

fpsBuilder::fpsBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"fps";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

fpsBuilder::~fpsBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void fpsBuilder::initialize(int argc, char *argv[]) {
  
  // mFmsDbMkr = static_cast< StFmsDbMaker*>(GetMaker("fmsDb"));
  // if(!mFmsDbMkr){
  //   LOG_FATAL << "Error finding StFmsDbMaker"<< endm;
  //   return kStFatal;
  // }
  // mFmsDbMkr->SetDateTime(date,time);
  
  pre_trig = 0; // first crossing in data
  evt = 0;

  for( int i=0; i<23; i++) {
    xypos[i] = i*4.0;
    if( i>9 ) xypos[i] += (i-9)*1.8;
  }

  float ADCMAX = 400;

  // Initialization of histograms.
  contents.h0_evt_size = new TH1F("h0_evt_size","log of Event Size; log(evt size)",50,1,10);
  contents.h1_fps_size = new TH1F("h1_fps_size","log of FPS Size; log(FPS size)",50,1,10);
  contents.h10_multi1 = new TH1F("h10_multi1","channel multiplicity; channel multiplicity",84,0,84);
  contents.h11_multi2 = new TH1F("h11_multi2","channel multiplicity; channel multiplicity",84,0,84);
  contents.h12_multi3 = new TH1F("h12_multi3","channel multiplicity; channel multiplicity",84,0,84);
  contents.h10_multi1->SetLineColor(kBlue);
  contents.h11_multi2->SetLineColor(kGreen);
  contents.h12_multi3->SetLineColor(kRed);
  contents.h13_ch_rdo = new TH2F("h13_ch_rdo","channel vs. rdo; channel; QT board",32, 0, 32, 8, 0, 8);
  contents.h20_ch_adc = new TH2F("h20_ch_adc", "ADC vs. channel; QT channel; ADC", 256, 0, 256, 50, 0, ADCMAX);
  contents.h21_ch_adc_full = new TH2F("h21_ch_adc_full", "ADC vs. channel; QT channel; ADC", 256, 0, 256, 42, 0, 4200);
  contents.h30_adc1 = new TH1F("h30_adc","ADC spectra; ADC",ADCMAX,0,ADCMAX);
  contents.h31_adc2 = new TH1F("h31_adc","ADC spectra; ADC",ADCMAX,0,ADCMAX);
  contents.h32_adc3 = new TH1F("h32_adc","ADC spectra; ADC",ADCMAX,0,ADCMAX);
  contents.h30_adc1->SetLineColor(kBlue);
  contents.h31_adc2->SetLineColor(kGreen);
  contents.h32_adc3->SetLineColor(kRed);
  contents.h33_adc1_full = new TH1F("h33_adc_full","ADC spectra; ADC", 210, 0, 4200);
  contents.h34_adc2_full = new TH1F("h34_adc_full","ADC spectra; ADC", 210, 0, 4200);
  contents.h35_adc3_full = new TH1F("h35_adc_full","ADC spectra; ADC", 210, 0, 4200);
  contents.h33_adc1_full->SetLineColor(kBlue);
  contents.h34_adc2_full->SetLineColor(kGreen);
  contents.h35_adc3_full->SetLineColor(kRed);
 
  float xyrange[45];
  for(int i=0; i<23; i++) {
    xyrange[22-i] = -xypos[i];
    xyrange[22+i] = xypos[i];
  }
  contents.h40_hits12 = new TH2F("h40_hits12", "hits in layers 1 & 2; FPS1: x (cm); FPS2: y (cm)", 44, xyrange, 44, xyrange);

  contents.h155_time_size_2min = new TH2F("h155_time_size_2min","Log10(event size) vs time", 60,0,120,60,0,8);

  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  char *c_title[] = { "FPS1", "FPS2", "FPS3" };
  int n=0;
  plots[n]   = new JevpPlot(contents.h0_evt_size);
  plots[++n] = new JevpPlot(contents.h1_fps_size);
  plots[++n] = new JevpPlot(contents.h10_multi1);
  plots[n]->addHisto(contents.h11_multi2);
  plots[n]->addHisto(contents.h12_multi3);
  plots[n]->setLegend(0.62,0.72,0.76,.88);
  for(int i=0; i<3; i++) {
    plots[n]->getHisto(i)->setLegText(c_title[i]);
    plots[n]->getHisto(i)->setLegArgs("l");
  }
  plots[++n] = new JevpPlot(contents.h13_ch_rdo);
  plots[++n] = new JevpPlot(contents.h20_ch_adc);
  plots[++n] = new JevpPlot(contents.h21_ch_adc_full);
  plots[++n] = new JevpPlot(contents.h30_adc1);
  plots[n]->addHisto(contents.h31_adc2);
  plots[n]->addHisto(contents.h32_adc3);
  plots[n]->setLegend(0.62,0.72,0.76,.88);
  for(int i=0; i<3; i++) {
    plots[n]->getHisto(i)->setLegText(c_title[i]);
    plots[n]->getHisto(i)->setLegArgs("l");
  }
  plots[++n] = new JevpPlot(contents.h33_adc1_full);
  plots[n]->addHisto(contents.h34_adc2_full);
  plots[n]->addHisto(contents.h35_adc3_full);
  plots[n]->setLegend(0.62,0.72,0.76,.88);
  for(int i=0; i<3; i++) {
    plots[n]->getHisto(i)->setLegText(c_title[i]);
    plots[n]->getHisto(i)->setLegArgs("l");
  }
  plots[++n] = new JevpPlot(contents.h40_hits12);
  plots[++n] = new JevpPlot(contents.h155_time_size_2min);
  plots[n]->setDrawOpts("col");
  for(int i=0; i<n; i++)
    plots[i]->optstat = 0;

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void fpsBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "fpsBuilder starting run #%d",rdr->run);
  resetAllPlots();

  t_2min = time(NULL);
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void fpsBuilder::event(daqReader *rdr)
{
  // Fill Histograms...
  int fps_size = rdr->getDetectorSize("fps");
  int sz = rdr->getDetectorSize("/");
  // printf("rdr->getDetectorSize(): %d  evtSize : %d  (diff=%d)\n", sz, rdr->event_size,rdr->event_size-sz);

  contents.h0_evt_size->Fill(safelog(sz));
  contents.h1_fps_size->Fill(safelog(fps_size));
  
  // Reset rolling histos if necessary..
  int tm = time(NULL);
  if(tm > t_2min + 120) {
    t_2min = tm;
    contents.h155_time_size_2min->Reset();
  }
  contents.h155_time_size_2min->Fill(tm-t_2min, safelog(sz));

  daq_dta *dd = rdr->det("fps")->get("adc");
  int first = 0;
  int ndata = 0;
  int qt_ch = 0;
  int quadrant = 0;
  int layer = 0;
  int slat = 0;
  int n1 = 0;
  int n2 = 0;
  int n3 = 0;
  vector<int> slats[2][4];
  while(dd && dd->iterate()) {
    int xing=(char)dd->sec;  // crossing (trigger was on sec=0)
    if(xing>=128) xing-=256;
    if(first==0) {
      pre_trig = xing;
      first=-1;
    }
    if( xing-pre_trig != 0 ) continue;
    int qt=dd->rdo;
    int n=dd->ncontent;
    // printf("FPS: sec %2d, rdo %d, pad %d\n",xing,qt,dd->pad);
    fps_adc_t *a = (fps_adc_t *) dd->Void;
    for(int i=0;i<n;i++) {
      ndata++;
      int ch=a[i].ch;
      int adc=a[i].adc;
      //int tdc=a[i].tdc;

      contents.h13_ch_rdo->Fill(ch, qt);

      qt_ch = qt*32+ch;

      // get quadrant, layer, slat from qt_ch
      if( qt_ch<=252 ) { quadrant=4; layer=3; slat=qt_ch-232; };
      if( qt_ch<=232 ) { quadrant=3; layer=3; slat=qt_ch-211; };
      if( qt_ch<=211 ) { quadrant=4; layer=2; slat=qt_ch-188; };
      if( qt_ch<=188 ) { quadrant=3; layer=2; slat=qt_ch-167; };
      if( qt_ch<=167 ) { quadrant=4; layer=1; slat=qt_ch-148; };
      if( qt_ch<=148 ) { quadrant=3; layer=1; slat=qt_ch-124; };
      if( qt_ch<=124 ) { quadrant=2; layer=3; slat=qt_ch-104; };
      if( qt_ch<=104 ) { quadrant=1; layer=3; slat=qt_ch-83; };
      if( qt_ch<= 83 ) { quadrant=2; layer=2; slat=qt_ch-60; };
      if( qt_ch<= 60 ) { quadrant=1; layer=2; slat=qt_ch-39; };
      if( qt_ch<= 39 ) { quadrant=2; layer=1; slat=qt_ch-20; };
      if( qt_ch<= 20 ) { quadrant=1; layer=1; slat=qt_ch+1; };

      if( qt_ch>80  && qt_ch<84  ) continue;
      if( qt_ch>145 && qt_ch<149 ) continue;
      if( qt_ch>207 && qt_ch<212 ) continue;
      if( qt_ch>252 )              continue;
      // printf("     sec %2d  QT %1d, ch %2d (%3d) --- Q%1d L%1d C%2d --- ADC %d \n", xing, qt, ch, qt_ch, quadrant, layer, slat, adc);

      contents.h20_ch_adc->Fill(qt_ch,adc);
      contents.h21_ch_adc_full->Fill(qt_ch,adc);

      if( layer==1 ) { n1++; contents.h30_adc1->Fill(adc); contents.h33_adc1_full->Fill(adc); }
      if( layer==2 ) { n2++; contents.h31_adc2->Fill(adc); contents.h34_adc2_full->Fill(adc);  }
      if( layer==3 ) { n3++; contents.h32_adc3->Fill(adc); contents.h35_adc3_full->Fill(adc);  }

      if( layer<3 && adc>50 )
	slats[layer-1][quadrant-1].push_back(slat);

      //   printf("FPS: xing %2d, QT %4d, ch %2d: ADC %4d, TDC %2d\n",xing,qt,ch,adc,tdc);
      //   int slatid = mFmsDbMkr->fpsSlatidFromQT(qt,ch);
      //   int q,l,s;
      //   mFmsDbMkr->fpsQLSfromSlatId(slatid,&q,&l,&s);
      //   int flag=0;
      //   if(slatid<0)          { /* LOG_WARN << "Invalid SlatId = "<<slatid<<endm;*/      flag=1; }
      //   if(q<0 || l<1 || s<1) { /* LOG_WARN << Form("Invalid Q/L/S = %d/%d/%d",q,l,s);*/ flag=1; }
      //   if(flag==0){
      // 	StFmsHit* hit = new StFmsHit();
      // 	hit->setDetectorId(15);
      // 	hit->setChannel(slatid);
      // 	hit->setQtCrate(6);
      // 	hit->setQtSlot(qt);
      // 	hit->setQtChannel(ch);
      // 	hit->setAdc(adc);
      // 	hit->setTdc((char)xing);  //! Hack to keep xing# as tdc for now
      // 	hit->setEnergy(0.0);
      // 	mFmsCollectionPtr->addHit(hit);
      // 	if(Debug()) hit->print();
      //   }
    }
  }

  contents.h10_multi1->Fill(n1);
  contents.h11_multi2->Fill(n2);
  contents.h12_multi3->Fill(n3);


  // printf("\nmultiplicities in layers: %d %d %d\n\n", n1, n2, n3);

  float xpos = 0.0;
  float ypos = 0.0;
  // get cross points from layers 1 & 2
  for(int q=0; q<4; q++) {
    if( slats[0][q].size()>0 && slats[1][q].size()>0) {
      for(u_int i=0; i<slats[0][q].size(); i++) {
  	if( slats[0][q][i] < 1 || slats[0][q][i] > 21 ) continue;
  	for(u_int j=0; j<slats[1][q].size(); j++) {
  	  if( slats[1][q][j] < 1 || slats[1][q][j] > 21 ) continue;
  	  xpos = xypos[slats[0][q][i]] - 1.0;
  	  if( q==1 || q==3) xpos = xypos[slats[0][q][i]+2] - 1.0; // bottom, beam pipe support shadow
  	  if( q==0 || q==1) xpos = -xpos;  // left
  	  ypos = xypos[slats[1][q][j]] - 1.0;
  	  if( q==1 || q==3) ypos = -ypos;  // bottom
  	  // printf("hit: Q%d (%2d,%2d) -> (%4.1f,%4.1f) \n", q, slats[0][q][i], slats[1][q][j], xpos, ypos);
  	  contents.h40_hits12->Fill(xpos, ypos);
  	}
      }
    }
  }
  





  // End Fill Histograms...
}

void fpsBuilder::stoprun(daqReader *rdr) {
  
}

void fpsBuilder::main(int argc, char *argv[])
{
  fpsBuilder me;
  
  me.Main(argc, argv);
}

