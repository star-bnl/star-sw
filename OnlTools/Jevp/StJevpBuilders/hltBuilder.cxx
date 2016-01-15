////////////////////////////////////////////////////////////
///// Liang Xue Run10 HLT(di-electron,Heavy Fragment, //////
/////       High Pt Triggered Evevt) online OA        //////
/////            using Jeff's new FramWork            //////
////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_L3/daq_l3.h>
#include <TStyle.h>
#include "TVector3.h"
#include <fstream>
#include <iostream>
#include <iomanip>
#include <TF1.h>
#include <TH1I.h>
#include <TH1D.h>
#include <TH2F.h>
#include <TProfile.h>
#include <math.h>
#include <rtsSystems.h>
#include <rtsLog.h>
#include "hltBuilder.h"

using namespace std;

ClassImp(hltBuilder);    		

//timeval PeriodicStart;
//timeval PeriodicResetTest;
//timeval PeriodicResult;

void hltBuilder::initialize(int argc, char *argv[]) {
  PlotHisto *ph;

  //Set the starting time for resetting periodic histos
  last_time = time(NULL);
  nHours = 4.0;
  //gettimeofday(&PeriodicStart,NULL);
  //timeval PeriodicStart;
  //timeval PeriodicResetTest;
  //timeval PeriodicResult;
  
  HBTCALC = kTRUE;
  V2CALC = kTRUE;
  eventCounter = 0;
  FILL_VPD_HISTOS = kFALSE;  //Just to start.
  
  mPion = 0.13957018;  	//pion mass in gev/c^2
  mKaon = 0.493677;    	//kaon mass in gev/c^2
  mProton = 0.93827203;	//proton mass in gev/c^2
  Nhbtmixing = 1;  //must be the same as the first array index in the hbt_event_info hbtbuffer[Nhbtmixing] structure declared above.
  NmultMixingBins = 3;
  NvertexMixingBins = 3;
  vmb = 0;
  mmb = 0;
  UPDATE_SWITCH = 0;
  multRange=1000;
  vertRange=150.; //units are cm.  
 
  //////////////////////////////Initialize JevpPlot////////////////////////////////
  gStyle->SetPalette(1);
  gStyle->SetOptLogz(1);
  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);
  
  for(int i=0;i<156;i++) {
    HltPlots[i] = new JevpPlot();
    HltPlots[i]->gridx=0;
    HltPlots[i]->gridy=0;
    HltPlots[i]->setPalette(1);
  } 

  ///////////////////////////////HltPlots histograms//////////////////////////////
  int index=0;    
  
  HltPlots[index]->setDrawOpts((char*)"hist");  //HltPlots[index]->gridx=-1;
  Vz1 = new TH1F("hlt_vertexZ_1", "Set A - Single Run Z vertex (No Cuts)",500,-250,250);
  Vz1->Sumw2();  ph = new PlotHisto();  ph->histo = Vz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vz2 = new TH1F("hlt_vertexZ_2", "Set A - Single Run Z vertex (Vr < 2)",500,-250,250);
  Vz2->Sumw2();  ph = new PlotHisto();  ph->histo = Vz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vz3 = new TH1F("hlt_vertexZ_3", "Set A - Single Run Z vertex (Vr < 2, Vz < 70)",500,-250,250);
  Vz3->Sumw2();  ph = new PlotHisto();  ph->histo = Vz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");  
  Vz4 = new TH1F("hlt_vertexZ_4", "Set A - Single Run Z vertex (Vr > 2, Vz < 70)",500,-250,250);
  Vz4->Sumw2();  ph = new PlotHisto();  ph->histo = Vz4;  HltPlots[index]->addHisto(ph);
        
  addPlot(HltPlots[index]);

  LOG(DBG, "here");

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vx1 = new TH1F("hlt_vertexX_1", "Set A - Single Run X vertex (No Cuts)",100,-5.,5.);
  Vx1->Sumw2();  ph = new PlotHisto();  ph->histo = Vx1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vx2 = new TH1F("hlt_vertexX_2", "Set A - Single Run X vertex (Vr < 2)",100,-5.,5.);
  Vx2->Sumw2();  ph = new PlotHisto();  ph->histo = Vx2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vx3 = new TH1F("hlt_vertexX_3", "Set A - Single Run X vertex (Vr < 2, Vz < 70)",100,-5.,5.);
  Vx3->Sumw2();  ph = new PlotHisto();  ph->histo = Vx3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vx4 = new TH1F("hlt_vertexX_4", "Set A - Single Run X vertex (Vr > 2, Vz < 70)",100,-5.,5.);
  Vx4->Sumw2();  ph = new PlotHisto();  ph->histo = Vx4;  HltPlots[index]->addHisto(ph);
        
  addPlot(HltPlots[index]);

  index++;
    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vy1 = new TH1F("hlt_vertexY_1", "Set A - Single Run Y vertex (No Cuts)",100,-5.,5.);
  Vy1->Sumw2();  ph = new PlotHisto();  ph->histo = Vy1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vy2 = new TH1F("hlt_vertexY_2", "Set A - Single Run Y vertex (Vr < 2)",100,-5.,5.);
  Vy2->Sumw2();  ph = new PlotHisto();  ph->histo = Vy2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vy3 = new TH1F("hlt_vertexY_3", "Set A - Single Run Y vertex (Vr < 2, Vz < 70)",100,-5.,5.);
  Vy3->Sumw2();  ph = new PlotHisto();  ph->histo = Vy3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  Vy4 = new TH1F("hlt_vertexY_4", "Set A - Single Run Y vertex (Vr > 2, Vz < 70)",100,-5.,5.);
  Vy4->Sumw2();  ph = new PlotHisto();  ph->histo = Vy4;  HltPlots[index]->addHisto(ph);
        
  addPlot(HltPlots[index]);

  index++;
   
  HltPlots[index]->setDrawOpts((char*)"hist");  HltPlots[index]->logy=1;
  Vr1 = new TH1F("hlt_vertexR_1","Set A - Single Run R vertex (No Cuts)",50,0.,5.);
  Vr1->Sumw2();  ph = new PlotHisto();  ph->histo = Vr1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");  HltPlots[index]->logy=1;
  Vr2 = new TH1F("hlt_vertexR_2","Set A - Single Run R vertex (Vr < 2)",50,0.,5.);
  Vr2->Sumw2();  ph = new PlotHisto();  ph->histo = Vr2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");  HltPlots[index]->logy=1;
  Vr3 = new TH1F("hlt_vertexR_3","Set A - Single Run R vertex (Vr < 2, Vz < 70)",50,0.,5.);
  Vr3->Sumw2();  ph = new PlotHisto();  ph->histo = Vr3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");  HltPlots[index]->logy=1;
  Vr4 = new TH1F("hlt_vertexR_4","Set A - Single Run R vertex (Vr > 2, Vz < 70)",50,0.,5.);
  Vr4->Sumw2();  ph = new PlotHisto();  ph->histo = Vr4;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);

  index++;

  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  Vxy1 = new TH2F("hlt_xyvertex_1","Set A - Single Run vertexY (cm) vs vertexX (cm) (No Cuts)",100,-5.,5.,100,-5.,5.);
  Vxy1->Sumw2();  ph = new PlotHisto();  ph->histo = Vxy1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  Vxy2 = new TH2F("hlt_xyvertex_2","Set A - Single Run vertexY (cm) vs vertexX (cm) (Vr < 2)", 100,-5.,5.,100,-5.,5.);
  Vxy2->Sumw2();  ph = new PlotHisto();  ph->histo = Vxy2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  Vxy3 = new TH2F("hlt_xyvertex_3","Set A - Single Run vertexY (cm) vs vertexX (cm) (Vr < 2, Vz < 70)", 100,-5.,5.,100,-5.,5.);
  Vxy3->Sumw2();  ph = new PlotHisto();  ph->histo = Vxy3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  Vxy4 = new TH2F("hlt_xyvertex_4","Set A - Single Run vertexY (cm) vs vertexX (cm) (Vr > 2, Vz < 70)",100,-5.,5.,100,-5.,5.);
  Vxy4->Sumw2();  ph = new PlotHisto();  ph->histo = Vxy4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);
    
  index++;

  HltPlots[index]->setDrawOpts((char*)"hist");
  Mult1 = new TH1F("hlt_Mult1","Set A - Single Run Multiplicity (No Cuts)",150,0,1500);
  Mult1->Sumw2();  ph = new PlotHisto();  ph->histo = Mult1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  Mult2 = new TH1F("hlt_Mult2","Set A - Single Run Multiplicity (Vr < 2)",150,0,1500);
  Mult2->Sumw2();  ph = new PlotHisto();  ph->histo = Mult2;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  Mult3 = new TH1F("hlt_Mult3","Set A - Single Run Multiplicity (Vr < 2, Vz < 70)",150,0,1500);
  Mult3->Sumw2();  ph = new PlotHisto();  ph->histo = Mult3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  Mult4 = new TH1F("hlt_Mult4","Set A - Single Run Multiplicity (Vr > 2, Vz < 70)",150,0,1500);
  Mult4->Sumw2();  ph = new PlotHisto();  ph->histo = Mult4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);

  index++;

  HltPlots[index]->setDrawOpts((char*)"hist");
  Eta1 = new TH1F("hlt_Pseudorapidity_1","Set A - Single Run Pseudorapidity (No Cuts)",50,-5,5);
  Eta1->Sumw2();  ph = new PlotHisto();  ph->histo = Eta1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  Eta2 = new TH1F("hlt_Pseudorapidity_2","Set A - Single Run Pseudorapidity (Vr < 2)",50,-5,5);
  Eta2->Sumw2();  ph = new PlotHisto();  ph->histo = Eta2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  Eta3 = new TH1F("hlt_Pseudorapidity_3","Set A - Single Run Pseudorapidity (Vr < 2, Vz < 70)",50,-5,5);
  Eta3->Sumw2();  ph = new PlotHisto();  ph->histo = Eta3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  Eta4 = new TH1F("hlt_Pseudorapidity_4","Set A - Single Run Pseudorapidity (Vr > 2, Vz < 70)",50,-5,5);
  Eta4->Sumw2();  ph = new PlotHisto();  ph->histo = Eta4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);

//Analysis Histos
  index++;

  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1; 
  Timevsmultiplicity = new TH2F("hlt_Timevsmultiplicity","Single Run processing time (to fill these HLT histos) vs multiplicity",100,0.,1000.,100,0.,200.);
  Timevsmultiplicity->Sumw2();  ph = new PlotHisto();  ph->histo = Timevsmultiplicity;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;

  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  Ratevsmultiplicity = new TH2F("hlt_Ratevsmultiplicity","Single Run rate/event (to fill these HLT histos) vs multiplicity",100,0.,1000.,100,0.,1000.);
  Ratevsmultiplicity->Sumw2();  ph = new PlotHisto();  ph->histo = Ratevsmultiplicity;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;   

  //flow
  v2_pt = new TProfile("hlt_v2pt","Single Run v2_pt",100,0.,10.);
  v2_pt->Sumw2();  ph = new PlotHisto();  ph->histo = v2_pt;  HltPlots[index]->addHisto(ph);
    
  
  //v2ptCounter = new TH1F("hlt_v2ptCounter","v2ptCounter",100,0.,10.);
  //v2ptCounter->Sumw2();
  //ph = new PlotHisto();
  //ph->histo = v2ptCounter;
  //    HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]); 
   
  index++;

  resolution = new TProfile("hlt_resolution","Single Run v2 resolution",1,-100.,100.);
  resolution->Sumw2();
//  resolution->SetMinimum(-1.);
//  resolution->SetMaximum(1.);  
  ph = new PlotHisto();  ph->histo = resolution;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
   
  index++;    

  corrected_v2_pt = new TProfile("hlt_v2corrected","Single Run v2_pt_corrected",100,0.,10.);
  corrected_v2_pt->Sumw2();  ph = new PlotHisto();  ph->histo = corrected_v2_pt;
  HltPlots[index]->addHisto(ph);
  
  LOG(DBG, "HERE");
   
  //corrected_v2ptCounter = new TH1F("hlt_v2correctedCounter","v2correctedCounter",100,0,10);
  //corrected_v2ptCounter->Sumw2();
  //ph = new PlotHisto();
  //ph->histo = corrected_v2ptCounter;
  //    HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);   

  index++; 

  //HltPlots[index]->logz=1;
  //HltPlots[index]->optstat = 0;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  dedx = new TH2F("hlt_dEdxpt","dEdx vs pt",100,0.,5.,100,0.,5.e-5);
  dedx->Sumw2();  ph = new PlotHisto();  ph->histo = dedx;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++; 

  //Pt SPECTRA PLOTS AND HISTOS    i
  HltPlots[index]->logy=1;
  piplus = new TH1F("hlt_PiPlusPtSpectrum","Single Run PiPlusPtSpectrum",20,0,2);
  piplus->Sumw2();  ph = new PlotHisto();  ph->histo = piplus;  HltPlots[index]->addHisto(ph);

  piminus = new TH1F("hlt_PiMinusPtSpectrum","Single Run PiMinusPtSpectrum",20,0,2);
  piminus->Sumw2();  ph = new PlotHisto();  ph->histo = piminus;  HltPlots[index]->addHisto(ph);

  kplus = new TH1F("hlt_KPlusPtSpectrum","Single Run KPlusPtSpectrum",20,0,2);
  kplus->Sumw2();  ph = new PlotHisto();  ph->histo = kplus;  HltPlots[index]->addHisto(ph);

  kminus = new TH1F("hlt_KMinusPtSpectrum","Single Run KMinusPtSpectrum",20,0,2);
  kminus->Sumw2();  ph = new PlotHisto();  ph->histo = kminus;  HltPlots[index]->addHisto(ph);

  pplus = new TH1F("hlt_ProPlusPtSpectrum","Single Run ProPlusPtSpectrum",20,0,2);
  pplus->Sumw2();  ph = new PlotHisto();  ph->histo = pplus;  HltPlots[index]->addHisto(ph);

  pminus = new TH1F("hlt_ProMinusPtSpectrum","Single Run ProMinusPtSpectrum",20,0,2);
  pminus->Sumw2();  ph = new PlotHisto();  ph->histo = pminus;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;  

  Yield = new TH1F("hlt_yields","Single Run Yields",20,0,20);
  Yield->Sumw2();  ph = new PlotHisto();  ph->histo = Yield;  HltPlots[index]->addHisto(ph);

  corrected_Yield = new TH1F("hlt_yieldsCorrected","Single Run Yields from corrected pt spectra",20,0,20);
  corrected_Yield->Sumw2();  ph = new PlotHisto();  ph->histo = corrected_Yield;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]); 

  index++;    

  //HBT HISTOGRAMS
  hbtnum = new TH1F("hlt_hbtnum","Single Run hbtnum",50,0,0.5);
  hbtnum->Sumw2();  ph = new PlotHisto();  ph->histo = hbtnum;  HltPlots[index]->addHisto(ph);

  hbtden = new TH1F("hlt_hbtden","Single Run hbtden",50,0,0.5);
  hbtden->Sumw2();  ph = new PlotHisto();  ph->histo = hbtden;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;

  hbtCF_qinv = new TH1F("hlt_hbtCF_qinv","Single Run hbtCF_qinv",50,0,0.5);
  hbtCF_qinv->Sumw2();  ph = new PlotHisto();  ph->histo = hbtCF_qinv;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  //LETTER REFERS TO BEMC CUT, NUMBER REFERS TO VERTEX CUT    
  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_1a = new TH2F("BEMC_1a","BEMC e vs w (No BEMC cuts, No Vertex cuts) 1a",100,0.,200.,100,0.,200.);
  BEMC_1a->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_1a;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);    jml--> did this twice!
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_2a = new TH2F("BEMC_2a","BEMC e vs w (No BEMC cuts, Vr < 2) 2a",100,0.,200.,100,0.,200.);
  BEMC_2a->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_2a;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_3a = new TH2F("BEMC_3a","BEMC e vs w (No BEMC cuts, Vr < 2, Vz < 70) 3a",100,0.,200.,100,0.,200.);
  BEMC_3a->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_3a;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_4a = new TH2F("BEMC_4a","BEMC e vs w (No BEMC cuts, Vr > 2, Vz < 70) 4a",100,0.,200.,100,0.,200.);
  BEMC_4a->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_4a;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_1b = new TH2F("BEMC_1b","BEMC e vs w (BEMC e & w > 20, No Vertex cuts) 1b",100,0.,200.,100,0.,200.);
  BEMC_1b->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_1b;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_2b = new TH2F("BEMC_2b","BEMC e vs w (BEMC e & w > 20, Vr < 2) 2b",100,0.,200.,100,0.,200.);
  BEMC_2b->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_2b;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_3b = new TH2F("BEMC_3b","BEMC e vs w (BEMC e & w > 20, Vr < 2, Vz < 70) 3b",100,0.,200.,100,0.,200.);
  BEMC_3b->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_3b;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_4b = new TH2F("BEMC_4b","BEMC e vs w (BEMC e & w > 20, Vr > 2, Vz < 70) 4b",200,0.,200.,200,0.,200.);
  BEMC_4b->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_4b;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_1c = new TH2F("BEMC_1c","BEMC e vs w (BEMC e & w > 20, BEMC |e-w| < 6, No Vertex cuts) 1c",100,0.,200.,100,0.,200.);
  BEMC_1c->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_1c;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_2c = new TH2F("BEMC_2c","BEMC e vs w (BEMC e & w > 20, BEMC |e-w| < 6, Vr < 2) 2c",100,0.,200.,100,0.,200.);
  BEMC_2c->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_2c;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_3c = new TH2F("BEMC_3c","BEMC e vs w (BEMC e & w > 20, BEMC |e-w| < 6, Vr < 2, Vz < 70) 3c",100,0.,200.,100,0.,200.);
  BEMC_3c->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_3c;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_4c = new TH2F("BEMC_4c","BEMC e vs w (BEMC e & w > 20, BEMC |e-w| < 6, Vr > 2, Vz < 70) 4c",200,0.,200.,200,0.,200.);
  BEMC_4c->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_4c;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);
 
  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_1d = new TH2F("BEMC_1d","BEMC e vs w (BEMC e & w > 1 GeV, No Vertex cuts) 1d",100,0.,200.,100,0.,200.);
  BEMC_1d->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_1d;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_2d = new TH2F("BEMC_2d","BEMC e vs w (BEMC e & w > 1 GeV, Vr < 2) 2d",100,0.,200.,100,0.,200.);
  BEMC_2d->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_2d;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_3d = new TH2F("BEMC_3d","BEMC e vs w (BEMC e & w > 1 GeV, Vr < 2, Vz < 70) 3d",200,0.,200.,200,0.,200.);
  BEMC_3d->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_3d;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

  index++;             HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  BEMC_4d = new TH2F("BEMC_4d","BEMC e vs w (BEMC e & w > 1 GeV, Vr > 2, Vz < 70) 4d",100,0.,200.,100,0.,200.);
  BEMC_4d->Sumw2(); ph = new PlotHisto(); ph->histo = BEMC_4d;  HltPlots[index]->addHisto(ph);    
  //addPlot(HltPlots[index]);
  addPlot(HltPlots[index]);

//**************************** Histos for Vertex Method of Event Counting ******************************
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVz1 = new TH1F("hlt_cVertexZ_1", "Set B - Single Run Z vertex (No Cuts)",500,-250.,250.);
  cVz1->Sumw2();  ph = new PlotHisto();  ph->histo = cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVz2 = new TH1F("hlt_cVertexZ_2", "Set B - Single Run Z vertex (Vr < 2, Vz < 70, pTracks > 5)",500,-250.,250.);
  cVz2->Sumw2();  ph = new PlotHisto();  ph->histo = cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVz3 = new TH1F("hlt_cVertexZ_3", "Set B - Single Run Z vertex (Vr < 2, pTracks > 5)",500,-250.,250.);
  cVz3->Sumw2();  ph = new PlotHisto();  ph->histo = cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVz4 = new TH1F("hlt_cVertexZ_4", "Set B - Single Run Z vertex (Vr > 2)",500,-250.,250.);
  cVz4->Sumw2();  ph = new PlotHisto();  ph->histo = cVz4;  HltPlots[index]->addHisto(ph);
        
  addPlot(HltPlots[index]);

  LOG(DBG, "here");

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVx1 = new TH1F("hlt_cVertexX_1", "Set B - Single Run X vertex (No Cuts)",100,-5.,5.);
  cVx1->Sumw2();  ph = new PlotHisto();  ph->histo = cVx1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVx2 = new TH1F("hlt_cVertexX_2", "Set B - Single Run X vertex (Vr < 2, Vz < 70, pTracks > 5)",100,-5.,5.);
  cVx2->Sumw2();  ph = new PlotHisto();  ph->histo = cVx2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVx3 = new TH1F("hlt_cVertexX_3", "Set B - Single Run X vertex (Vr > 2)",100,-5.,5.);
  cVx3->Sumw2();  ph = new PlotHisto();  ph->histo = cVx3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVy1 = new TH1F("hlt_cVertexY_1", "Set B - Single Run Y vertex (No Cuts)",100,-5.,5.);
  cVy1->Sumw2();  ph = new PlotHisto();  ph->histo = cVy1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVy2 = new TH1F("hlt_cVertexY_2", "Set B - Single Run Y vertex (Vr < 2, Vz < 70, pTracks > 5)",100,-5.,5.);
  cVy2->Sumw2();  ph = new PlotHisto();  ph->histo = cVy2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVy3 = new TH1F("hlt_cVertexY_3", "Set B - Single Run Y vertex (Vr > 2)",100,-5.,5.);
  cVy3->Sumw2();  ph = new PlotHisto();  ph->histo = cVy3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  
  HltPlots[index]->setDrawOpts((char*)"hist"); 
  cVr1 = new TH1F("hlt_cVertexR_1","Set B - Single Run R vertex (No Cuts)",50,0.,5.);
  cVr1->Sumw2();  ph = new PlotHisto();  ph->histo = cVr1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVr2 = new TH1F("hlt_cVertexR_2","Set B - Single Run R vertex (Vr < 2, Vz < 70, pTracks > 5)",50,0.,5.);
  cVr2->Sumw2();  ph = new PlotHisto();  ph->histo = cVr2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVr3 = new TH1F("hlt_cVertexR_3","Set B - Single Run R vertex (Vz < 70, pTracks > 5)",50,0.,5.);
  cVr3->Sumw2();  ph = new PlotHisto();  ph->histo = cVr3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cVr4 = new TH1F("hlt_cVertexR_4","Set B - Single Run R vertex (Vr > 2)",50,0.,5.);
  cVr4->Sumw2();  ph = new PlotHisto();  ph->histo = cVr4;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);

    
  index++;

  HltPlots[index]->setDrawOpts((char*)"hist");
  cM1 = new TH1F("hlt_cMult1","Set B - Single Run Mult [nPrimaryTracks] (No Cuts)",150,0,1500);
  cM1->Sumw2();  ph = new PlotHisto();  ph->histo = cM1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cM2 = new TH1F("hlt_cMult2","Set B - Single Run Mult [nPrimaryTracks] (Vr < 2, Vz < 70, pTracks > 5)",150,0,1500);
  cM2->Sumw2();  ph = new PlotHisto();  ph->histo = cM2;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cM3 = new TH1F("hlt_cMult3","Set B - Single Run Mult [nPrimaryTracks] (Vr < 2, Vz < 70)",150,0,1500);
  cM3->Sumw2();  ph = new PlotHisto();  ph->histo = cM3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  cM4 = new TH1F("hlt_cMult4","Set B - Single Run Mult [nPrimaryTracks] (Vr > 2)",150,0,1500);
  cM4->Sumw2();  ph = new PlotHisto();  ph->histo = cM4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);

//************************* End of Histos for Vertex Method of Event Counting **************************


//********************************** Beginning of Cumulative Histos ************************************

//**************************** Histos for Vertex Method of Event Counting ******************************
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVz1 = new TH1F("hlt_tot_cVertexZ_1","Set B - Accumulated Z vertex (No Cuts)",500,-250.,250.);
  tot_cVz1->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVz2 = new TH1F("hlt_tot_cVertexZ_2","Set B - Accumulated Z vertex (Vr < 2, Vz < 70, pTracks > 5)",500,-250.,250.);
  tot_cVz2->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVz3 = new TH1F("hlt_tot_cVertexZ_3","Set B - Accumulated Z vertex (Vr < 2, pTracks > 5)",500,-250.,250.);
  tot_cVz3->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVz4 = new TH1F("hlt_tot_cVertexZ_4","Set B - Accumulated Z vertex (Vr > 2)",500,-250.,250.);
  tot_cVz4->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVz4;  HltPlots[index]->addHisto(ph);
        
  addPlot(HltPlots[index]);

  LOG(DBG, "here");

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVx1 = new TH1F("hlt_tot_cVertexX_1","Set B - Accumulated X vertex (No Cuts)",100,-5.,5.);
  tot_cVx1->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVx1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVx2 = new TH1F("hlt_tot_cVertexX_2","Set B - Accumulated X vertex (Vr < 2, Vz < 70, pTracks > 5)",100,-5.,5.);
  tot_cVx2->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVx2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVx3 = new TH1F("hlt_tot_cVertexX_3","Set B - Accumulated X vertex (Vr > 2)",100,-5.,5.);
  tot_cVx3->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVx3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVy1 = new TH1F("hlt_tot_cVertexY_1","Set B - Accumulated Y vertex (No Cuts)",100,-5.,5.);
  tot_cVy1->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVy1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVy2 = new TH1F("hlt_tot_cVertexY_2","Set B - Accumulated Y vertex (Vr < 2, Vz < 70, pTracks > 5)",100,-5.,5.);
  tot_cVy2->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVy2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVy3 = new TH1F("hlt_tot_cVertexY_3","Set B - Accumulated Y vertex (Vr > 2)",100,-5.,5.);
  tot_cVy3->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVy3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
   
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVr1 = new TH1F("hlt_tot_cVertexR_1","Set B - Accumulated R vertex (No Cuts)",50,0.,5.);
  tot_cVr1->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVr1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVr2 = new TH1F("hlt_tot_cVertexR_2","Set B - Accumulated R vertex (Vr < 2, Vz < 70, pTracks > 5)",50,0.,5.);
  tot_cVr2->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVr2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVr3 = new TH1F("hlt_tot_cVertexR_3","Set B - Accumulated R vertex (Vz < 70, pTracks > 5)",50,0.,5.);
  tot_cVr3->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVr3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cVr4 = new TH1F("hlt_tot_cVertexR_4","Set B - Accumulated R vertex (Vr > 2)",50,0.,5.);
  tot_cVr4->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cVr4;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);

    
  index++;

  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cM1 = new TH1F("hlt_tot_cMult1","Set B - Accumulated Mult [nPrimaryTracks] (No Cuts)",150,0,1500);
  tot_cM1->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cM1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cM2 = new TH1F("hlt_tot_cMult2","Set B - Accumulated Mult [nPrimaryTracks] (Vr < 2, Vz < 70, pTracks > 5)",150,0,1500);
  tot_cM2->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cM2;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cM3 = new TH1F("hlt_tot_cMult3","Set B - Accumulated Mult [nPrimaryTracks] (Vr < 2, Vz < 70)",150,0,1500);
  tot_cM3->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cM3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  tot_cM4 = new TH1F("hlt_tot_cMult4","Set B - Accumulated Mult [nPrimaryTracks] (Vr > 2)",150,0,1500);
  tot_cM4->Sumw2();  ph = new PlotHisto();  ph->histo = tot_cM4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);

  index++;

//NEW Vy vs Vx CUMULATIVE HISTOGRAMS *************
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  tot_Vxy1 = new TH2F("hlt_tot_xyvertex_1","Set A - Accumulated vertexY (cm) vs vertexX (cm) (No Cuts)",100,-5.,5.,100,-5.,5.);
  tot_Vxy1->Sumw2();  ph = new PlotHisto();  ph->histo = tot_Vxy1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  tot_Vxy2 = new TH2F("hlt_tot_xyvertex_2","Set A - Accumulated vertexY (cm) vs vertexX (cm) (Vr < 2)", 100,-5.,5.,100,-5.,5.);
  tot_Vxy2->Sumw2();  ph = new PlotHisto();  ph->histo = tot_Vxy2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  tot_Vxy3 = new TH2F("hlt_tot_xyvertex_3","Set A - Accumulated vertexY (cm) vs vertexX (cm) (Vr < 2, Vz < 70)", 100,-5.,5.,100,-5.,5.);
  tot_Vxy3->Sumw2();  ph = new PlotHisto();  ph->histo = tot_Vxy3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  tot_Vxy4 = new TH2F("hlt_tot_xyvertex_4","Set A - Accumulated vertexY (cm) vs vertexX (cm) (Vr > 2, Vz < 70)",100,-5.,5.,100,-5.,5.);
  tot_Vxy4->Sumw2();  ph = new PlotHisto();  ph->histo = tot_Vxy4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);
//END of new vy vs vx histograms *************

//Analysis Histos
  index++;

  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  tot_Timevsmultiplicity = new TH2F("hlt_tot_Timevsmultiplicity","Accumulated processing time (to fill these HLT histos) vs multiplicity",100,0.,1000.,100,0.,200.);
  tot_Timevsmultiplicity->Sumw2();  ph = new PlotHisto();  ph->histo = tot_Timevsmultiplicity;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;

  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  tot_Ratevsmultiplicity = new TH2F("hlt_tot_Ratevsmultiplicity","Accumulated rate/event (to fill these HLT histos) vs multiplicity",100,0.,1000.,100,0.,1000.);
  tot_Ratevsmultiplicity->Sumw2();  ph = new PlotHisto();  ph->histo = tot_Ratevsmultiplicity;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;   

  //flow
  tot_v2_pt = new TProfile("hlt_tot_v2pt","Accumulated v2_pt",100,0.,10.);
  tot_v2_pt->Sumw2();  ph = new PlotHisto();  ph->histo = tot_v2_pt;  HltPlots[index]->addHisto(ph);
    
  
  //tot_v2ptCounter = new TH1F("hlt_tot_v2ptCounter","v2ptCounter",100,0.,10.);
  //tot_v2ptCounter->Sumw2();
  //ph = new PlotHisto();
  //ph->histo = tot_v2ptCounter;
  //    HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]); 
   
  index++;

  tot_resolution = new TProfile("hlt_tot_resolution","Accumulated v2 resolution",1,-100.,100.);
  tot_resolution->Sumw2();
//  resolution->SetMinimum(-1.);
//  resolution->SetMaximum(1.);  
  ph = new PlotHisto();  ph->histo = tot_resolution;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
   
  index++;    

  tot_corrected_v2_pt = new TProfile("hlt_tot_v2corrected","Accumulated v2_pt_corrected",100,0.,10.);
  tot_corrected_v2_pt->Sumw2();  ph = new PlotHisto();  ph->histo = tot_corrected_v2_pt;
  HltPlots[index]->addHisto(ph);
  
  LOG(DBG, "HERE");
   
  //corrected_v2ptCounter = new TH1F("hlt_v2correctedCounter","v2correctedCounter",100,0,10);
  //corrected_v2ptCounter->Sumw2();
  //ph = new PlotHisto();
  //ph->histo = corrected_v2ptCounter;
  //    HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);   

  index++; 

  //HltPlots[index]->logz=1;
  //HltPlots[index]->optstat = 0;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  tot_dedx = new TH2F("hlt_tot_dEdxpt","Accumulated dEdx vs pt",100,0.,5.,100,0.,5.e-5);
  tot_dedx->Sumw2();  ph = new PlotHisto();  ph->histo = tot_dedx;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++; 

  //Pt SPECTRA PLOTS AND HISTOS    i
  HltPlots[index]->logy=1;
  tot_piplus = new TH1F("hlt_tot_PiPlusPtSpectrum","Accumulated PiPlusPtSpectrum",20,0,2);
  tot_piplus->Sumw2();  ph = new PlotHisto();  ph->histo = tot_piplus;  HltPlots[index]->addHisto(ph);

  tot_piminus = new TH1F("hlt_tot_PiMinusPtSpectrum","Accumulated PiMinusPtSpectrum",20,0,2);
  tot_piminus->Sumw2();  ph = new PlotHisto();  ph->histo = tot_piminus;  HltPlots[index]->addHisto(ph);

  tot_kplus = new TH1F("hlt_tot_KPlusPtSpectrum","Accumulated KPlusPtSpectrum",20,0,2);
  tot_kplus->Sumw2();  ph = new PlotHisto();  ph->histo = tot_kplus;  HltPlots[index]->addHisto(ph);

  tot_kminus = new TH1F("hlt_tot_KMinusPtSpectrum","Accumulated KMinusPtSpectrum",20,0,2);
  tot_kminus->Sumw2();  ph = new PlotHisto();  ph->histo = tot_kminus;  HltPlots[index]->addHisto(ph);

  tot_pplus = new TH1F("hlt_tot_ProPlusPtSpectrum","Accumulated ProPlusPtSpectrum",20,0,2);
  tot_pplus->Sumw2();  ph = new PlotHisto();  ph->histo = tot_pplus;  HltPlots[index]->addHisto(ph);

  tot_pminus = new TH1F("hlt_tot_ProMinusPtSpectrum","Accumulated ProMinusPtSpectrum",20,0,2);
  tot_pminus->Sumw2();  ph = new PlotHisto();  ph->histo = tot_pminus;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;  

  tot_Yield = new TH1F("hlt_tot_yields","Accumulated Yields",20,0,20);
  tot_Yield->Sumw2();  ph = new PlotHisto();  ph->histo = tot_Yield;  HltPlots[index]->addHisto(ph);

  tot_corrected_Yield = new TH1F("hlt_tot_yieldsCorrected","Accumulated Yields from corrected pt spectra",20,0,20);
  tot_corrected_Yield->Sumw2();  ph = new PlotHisto();  ph->histo = tot_corrected_Yield;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]); 

  index++;    

  //HBT HISTOGRAMS
  tot_hbtnum = new TH1F("hlt_tot_hbtnum","Accumulated hbtnum",50,0,0.5);
  tot_hbtnum->Sumw2();  ph = new PlotHisto();  ph->histo = tot_hbtnum;  HltPlots[index]->addHisto(ph);

  tot_hbtden = new TH1F("hlt_tot_hbtden","Accumulated hbtden",50,0,0.5);
  tot_hbtden->Sumw2();  ph = new PlotHisto();  ph->histo = tot_hbtden;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;

  tot_hbtCF_qinv = new TH1F("hlt_tot_hbtCF_qinv","Accumulated hbtCF_qinv",50,0,0.5);
  tot_hbtCF_qinv->Sumw2();  ph = new PlotHisto();  ph->histo = tot_hbtCF_qinv;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

//************************************** End of Cumulative Histos **************************************


//#################################### START OF PERIODIC HISTOS HERE ###################################

//************************ Periodic Histos for Vertex Method of Event Counting *************************
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVz1 = new TH1F("hlt_periodic_cVertexZ_1","Set C - Periodic Z vertex (No Cuts)",500,-250.,250.);
  periodic_cVz1->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVz2 = new TH1F("hlt_periodic_cVertexZ_2","Set C - Periodic Z vertex (Vr < 2, Vz < 70, pTracks > 5)",500,-250.,250.);
  periodic_cVz2->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVz3 = new TH1F("hlt_periodic_cVertexZ_3","Set C - Periodic Z vertex (Vr < 2, pTracks > 5)",500,-250.,250.);
  periodic_cVz3->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVz4 = new TH1F("hlt_periodic_cVertexZ_4","Set C - Periodic Z vertex (Vr > 2)",500,-250.,250.);
  periodic_cVz4->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVz4;  HltPlots[index]->addHisto(ph);
        
  addPlot(HltPlots[index]);

  LOG(DBG, "here");

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVx1 = new TH1F("hlt_periodic_cVertexX_1","Set C - Periodic X vertex (No Cuts)",100,-5.,5.);
  periodic_cVx1->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVx1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVx2 = new TH1F("hlt_periodic_cVertexX_2","Set C - Periodic X vertex (Vr < 2, Vz < 70, pTracks > 5)",100,-5.,5.);
  periodic_cVx2->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVx2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVx3 = new TH1F("hlt_periodic_cVertexX_3","Set C - Periodic X vertex (Vr > 2)",100,-5.,5.);
  periodic_cVx3->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVx3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVy1 = new TH1F("hlt_periodic_cVertexY_1","Set C - Periodic Y vertex (No Cuts)",100,-5.,5.);
  periodic_cVy1->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVy1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVy2 = new TH1F("hlt_periodic_cVertexY_2","Set C - Periodic Y vertex (Vr < 2, Vz < 70, pTracks > 5)",100,-5.,5.);
  periodic_cVy2->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVy2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;    
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVy3 = new TH1F("hlt_periodic_cVertexY_3","Set C - Periodic Y vertex (Vr > 2)",100,-5.,5.);
  periodic_cVy3->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVy3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
   
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVr1 = new TH1F("hlt_periodic_cVertexR_1","Set C - Periodic R vertex (No Cuts)",50,0.,5.);
  periodic_cVr1->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVr1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVr2 = new TH1F("hlt_periodic_cVertexR_2","Set C - Periodic R vertex (Vr < 2, Vz < 70, pTracks > 5)",50,0.,5.);
  periodic_cVr2->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVr2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVr3 = new TH1F("hlt_periodic_cVertexR_3","Set C - Periodic R vertex (Vz < 70, pTracks > 5)",50,0.,5.);
  periodic_cVr3->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVr3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cVr4 = new TH1F("hlt_periodic_cVertexR_4","Set C - Periodic R vertex (Vr > 2)",50,0.,5.);
  periodic_cVr4->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVr4;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);

    
  index++;

  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cM1 = new TH1F("hlt_periodic_cMult1","Set C - Periodic Mult [nPrimaryTracks] (No Cuts)",150,0,1500);
  periodic_cM1->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cM1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cM2 = new TH1F("hlt_periodic_cMult2","Set C - Periodic Mult [nPrimaryTracks] (Vr < 2, Vz < 70, pTracks > 5)",150,0,1500);
  periodic_cM2->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cM2;  HltPlots[index]->addHisto(ph);    
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cM3 = new TH1F("hlt_periodic_cMult3","Set C - Periodic Mult [nPrimaryTracks] (Vr < 2, Vz < 70)",150,0,1500);
  periodic_cM3->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cM3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  periodic_cM4 = new TH1F("hlt_periodic_cMult4","Set C - Periodic Mult [nPrimaryTracks] (Vr > 2)",150,0,1500);
  periodic_cM4->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cM4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);

  index++;

//NEW Vy vs Vx Periodic HISTOGRAMS *************
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  periodic_cVxy1 = new TH2F("hlt_periodic_cxyvertex_1","Set C - Periodic vertexY (cm) vs vertexX (cm) (No Cuts)",100,-5.,5.,100,-5.,5.);
  periodic_cVxy1->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVxy1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  periodic_cVxy2 = new TH2F("hlt_periodic_cxyvertex_2","Set C - Periodic vertexY (cm) vs vertexX (cm) (Vr < 2, Vz < 70, pTracks > 5)", 100,-5.,5.,100,-5.,5.);
  periodic_cVxy2->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVxy2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  periodic_cVxy3 = new TH2F("hlt_periodic_cxyvertex_3","Set C - Periodic vertexY (cm) vs vertexX (cm) (Vz < 70, pTracks > 5)", 100,-5.,5.,100,-5.,5.);
  periodic_cVxy3->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVxy3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  index++;
  //HltPlots[index]->logz=1;
  HltPlots[index]->setDrawOpts((char *)"colz");  HltPlots[index]->optlogz=1;
  periodic_cVxy4 = new TH2F("hlt_periodic_cxyvertex_4","Set C - Periodic vertexY (cm) vs vertexX (cm) (Vr > 2)",100,-5.,5.,100,-5.,5.);
  periodic_cVxy4->Sumw2();  ph = new PlotHisto();  ph->histo = periodic_cVxy4;  HltPlots[index]->addHisto(ph);
    
  addPlot(HltPlots[index]);
//END of new vy vs vx and other Periodic histograms *************

//##################################### END OF PERIODIC HISTOS HERE ####################################

//######### SET D - VPD_TRIGGERED VERTEX Z HISTOS #####
  //single run
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_singlerun_cVz1 = new TH1F("hlt_setD_singlerun_cVz1","Set D - VPD trigger Single Run Vz (no cuts)",500,-250.,250.);
  setD_singlerun_cVz1->Sumw2();  ph = new PlotHisto(); ph->histo = setD_singlerun_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_singlerun_cVz2 = new TH1F("hlt_setD_singlerun_cVz2","Set D - VPD trigger Single Run Vz (Vz < 100)",500,-250.,250.);
  setD_singlerun_cVz2->Sumw2();  ph = new PlotHisto(); ph->histo = setD_singlerun_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);  
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_singlerun_cVz3 = new TH1F("hlt_setD_singlerun_cVz3","Set D - VPD trigger Single Run Vz (Vz < 70)",500,-250.,250.);
  setD_singlerun_cVz3->Sumw2();  ph = new PlotHisto(); ph->histo = setD_singlerun_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_singlerun_cVz4 = new TH1F("hlt_setD_singlerun_cVz4","Set D - VPD trigger Single Run Vz (Vz < 100, pTracks > 5)",500,-250.,250.);
  setD_singlerun_cVz4->Sumw2();  ph = new PlotHisto(); ph->histo = setD_singlerun_cVz4;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  //periodic
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_periodic_cVz1 = new TH1F("hlt_setD_periodic_cVz1","Set D - VPD trigger Periodic Vz (no cuts)",500,-250.,250.);
  setD_periodic_cVz1->Sumw2();  ph = new PlotHisto(); ph->histo = setD_periodic_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_periodic_cVz2 = new TH1F("hlt_setD_periodic_cVz2","Set D - VPD trigger Periodic Vz (Vz < 100)",500,-250.,250.);
  setD_periodic_cVz2->Sumw2();  ph = new PlotHisto(); ph->histo = setD_periodic_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);  
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_periodic_cVz3 = new TH1F("hlt_setD_periodic_cVz3","Set D - VPD trigger PeriodicVz (Vz < 70)",500,-250.,250.);
  setD_periodic_cVz3->Sumw2();  ph = new PlotHisto(); ph->histo = setD_periodic_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_periodic_cVz4 = new TH1F("hlt_setD_periodic_cVz4","Set D - VPD trigger PeriodicVz (Vz < 100, pTracks > 5)",500,-250.,250.);
  setD_periodic_cVz4->Sumw2();  ph = new PlotHisto(); ph->histo = setD_periodic_cVz4;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  //accumulated
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_accumulated_cVz1 = new TH1F("hlt_setD_accumulated_cVz1","Set D - VPD trigger Accumulated Vz (no cuts)",500,-250.,250.);
  setD_accumulated_cVz1->Sumw2();  ph = new PlotHisto(); ph->histo = setD_accumulated_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_accumulated_cVz2 = new TH1F("hlt_setD_accumulated_cVz2","Set D - VPD trigger Accumulated Vz (Vz < 100)",500,-250.,250.);
  setD_accumulated_cVz2->Sumw2();  ph = new PlotHisto(); ph->histo = setD_accumulated_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);  
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_accumulated_cVz3 = new TH1F("hlt_setD_accumulated_cVz3","Set D - VPD trigger Accumulated Vz (Vz < 70)",500,-250.,250.);
  setD_accumulated_cVz3->Sumw2();  ph = new PlotHisto(); ph->histo = setD_accumulated_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setD_accumulated_cVz4 = new TH1F("hlt_setD_accumulated_cVz4","Set D - VPD trigger Accumulated Vz (Vz < 100, pTracks > 5)",500,-250.,250.);
  setD_accumulated_cVz4->Sumw2();  ph = new PlotHisto(); ph->histo = setD_accumulated_cVz4;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

//######### End of SET D - VPD_TRIGGERED VERTEX Z HISTOS #####

//######### SET D - VPD_TRIGGERED VERTEX Z HISTOS #####
  //single run
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_singlerun_cVz1 = new TH1F("hlt_setE_singlerun_cVz1","Set E - VPD trigger Single Run Vz (no cuts)",500,-250.,250.);
  setE_singlerun_cVz1->Sumw2();  ph = new PlotHisto(); ph->histo = setE_singlerun_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_singlerun_cVz2 = new TH1F("hlt_setE_singlerun_cVz2","Set E - VPD trigger Single Run Vz (Vr < 2, Vz < 70, pTracks > 5)",500,-250.,250.);
  setE_singlerun_cVz2->Sumw2();  ph = new PlotHisto(); ph->histo = setE_singlerun_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);  
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_singlerun_cVz3 = new TH1F("hlt_setE_singlerun_cVz3","Set E - VPD trigger Single Run Vz (Vr < 2, pTracks > 5)",500,-250.,250.);
  setE_singlerun_cVz3->Sumw2();  ph = new PlotHisto(); ph->histo = setE_singlerun_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_singlerun_cVz4 = new TH1F("hlt_setE_singlerun_cVz4","Set E - VPD trigger Single Run Vz (Vr > 2)",500,-250.,250.);
  setE_singlerun_cVz4->Sumw2();  ph = new PlotHisto(); ph->histo = setE_singlerun_cVz4;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  //periodic
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_periodic_cVz1 = new TH1F("hlt_setE_periodic_cVz1","Set E - VPD trigger Periodic Vz (no cuts)",500,-250.,250.);
  setE_periodic_cVz1->Sumw2();  ph = new PlotHisto(); ph->histo = setE_periodic_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_periodic_cVz2 = new TH1F("hlt_setE_periodic_cVz2","Set E - VPD trigger Periodic Vz (Vr < 2, Vz < 70, pTracks > 5)",500,-250.,250.);
  setE_periodic_cVz2->Sumw2();  ph = new PlotHisto(); ph->histo = setE_periodic_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);  
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_periodic_cVz3 = new TH1F("hlt_setE_periodic_cVz3","Set E - VPD trigger PeriodicVz (Vr < 2, pTracks > 5)",500,-250.,250.);
  setE_periodic_cVz3->Sumw2();  ph = new PlotHisto(); ph->histo = setE_periodic_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_periodic_cVz4 = new TH1F("hlt_setE_periodic_cVz4","Set E - VPD trigger PeriodicVz (Vr > 2)",500,-250.,250.);
  setE_periodic_cVz4->Sumw2();  ph = new PlotHisto(); ph->histo = setE_periodic_cVz4;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
    
  //accumulated
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_accumulated_cVz1 = new TH1F("hlt_setE_accumulated_cVz1","Set E - VPD trigger Accumulated Vz (no cuts)",500,-250.,250.);
  setE_accumulated_cVz1->Sumw2();  ph = new PlotHisto(); ph->histo = setE_accumulated_cVz1;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_accumulated_cVz2 = new TH1F("hlt_setE_accumulated_cVz2","Set E - VPD trigger Accumulated Vz (Vr < 2, Vz < 70, pTracks > 5)",500,-250.,250.);
  setE_accumulated_cVz2->Sumw2();  ph = new PlotHisto(); ph->histo = setE_accumulated_cVz2;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);  
  
  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_accumulated_cVz3 = new TH1F("hlt_setE_accumulated_cVz3","Set E - VPD trigger Accumulated Vz (Vr < 2, pTracks > 5)",500,-250.,250.);
  setE_accumulated_cVz3->Sumw2();  ph = new PlotHisto(); ph->histo = setE_accumulated_cVz3;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);

  index++;
  HltPlots[index]->setDrawOpts((char*)"hist");
  setE_accumulated_cVz4 = new TH1F("hlt_setE_accumulated_cVz4","Set E - VPD trigger Accumulated Vz (Vr > 2)",500,-250.,250.);
  setE_accumulated_cVz4->Sumw2();  ph = new PlotHisto(); ph->histo = setE_accumulated_cVz4;  HltPlots[index]->addHisto(ph);
  addPlot(HltPlots[index]);



//######### End of SET E - VPD_TRIGGERED VERTEX Z HISTOS #####


  LOG(DBG, "HERE");    
  /*
    h = new TH3F("hlt_hbt3dnum","hbt3dnum",50,0,0.5,50,0,0.5,50,0,0.5);
    h->Sumw2();
    ph = new PlotHisto();
    ph->histo = h;
    hbt->addHisto(ph);
    h = new TH3F("hlt_hbt3dden","hbt3dden",50,0,0.5,50,0,0.5,50,0,0.5);
    h->Sumw2();
    ph = new PlotHisto();
    ph->histo = h;
    hbt->addHisto(ph);
  */

  ///////////////////////////////////////////////////////////////////////  
  //------------Set Marker---------------//   
  piplus->SetLineColor(2);
  piplus->SetMarkerStyle(20);
  piplus->SetMarkerSize(1.0);
  piplus->SetMarkerColor(2);

  piminus->SetLineColor(3); 
  piminus->SetMarkerStyle(21);
  piminus->SetMarkerSize(1.0);
  piminus->SetMarkerColor(3);

  kplus->SetLineColor(4);
  kplus->SetMarkerStyle(20);
  kplus->SetMarkerSize(1.0);
  kplus->SetMarkerColor(2);

  kminus->SetLineColor(5); 
  kminus->SetMarkerStyle(21);
  kminus->SetMarkerSize(1.0);
  kminus->SetMarkerColor(5);

  pplus->SetLineColor(6);
  pplus->SetMarkerStyle(20);
  pplus->SetMarkerSize(1.0);
  pplus->SetMarkerColor(6);

  pminus->SetLineColor(7);
  pminus->SetMarkerStyle(21);
  pminus->SetMarkerSize(1.0);
  pminus->SetMarkerColor(7);
     
  Yield->SetLineColor(2);
  corrected_Yield->SetLineColor(4);
     
  LOG(DBG, "HERE");

  hbtnum->SetLineColor(2);
  hbtnum->SetMarkerStyle(21);
  hbtnum->SetMarkerSize(1.0);
  hbtnum->SetMarkerColor(2);

  hbtden->SetLineColor(4);
  hbtden->SetMarkerStyle(21);
  hbtden->SetMarkerSize(1.0);
  hbtden->SetMarkerColor(4);

  LOG(DBG, "HERE");
  //-------------Set X Y Title--------------//
  Vz1->GetXaxis()->SetTitle("Primary Vz");
  Vxy1->GetXaxis()->SetTitle("Primary Vx");
  Vxy1->GetYaxis()->SetTitle("Primary Vy");
  Mult1->GetXaxis()->SetTitle("Multiplicity");
  Eta1->GetXaxis()->SetTitle("Pseudorapidity");  
  Timevsmultiplicity->GetXaxis()->SetTitle("Multiplicity");     
  Timevsmultiplicity->GetYaxis()->SetTitle("Time (ms)");
  Ratevsmultiplicity->GetXaxis()->SetTitle("Multiplicity");
  Ratevsmultiplicity->GetYaxis()->SetTitle("rate/event (Hz)");

  LOG(DBG, "HERE 0x%x 0x%x",v2_pt,v2ptCounter);
  v2_pt->GetXaxis()->SetTitle("Pt GeV") ;
  LOG(DBG, "HERE");
  v2_pt->GetYaxis()->SetTitle("v2") ;
  v2_pt->GetYaxis()->CenterTitle();
  v2_pt->SetName("v2 vs pt");             //*************************
  v2_pt->SetTitle("Single Run v2 vs pt"); //*************************
  v2_pt->SetMinimum(-0.1);
  v2_pt->SetMaximum(0.6);
  //v2ptCounter->GetXaxis()->SetTitle("Pt GeV") ;
  //v2ptCounter->SetName("v2 vs pt ");
  //v2ptCounter->SetTitle("v2 vs pt");

  LOG(DBG, "HERE");

  resolution->SetTitle("Single Run v2 resolution") ;
     
  corrected_v2_pt->GetXaxis()->SetTitle("Pt GeV") ;
  corrected_v2_pt->GetYaxis()->SetTitle("v2") ;
  corrected_v2_pt->GetYaxis()->CenterTitle();
  corrected_v2_pt->SetName("corrected v2 vs pt");             //********
  corrected_v2_pt->SetTitle("Single Run corrected v2 vs pt"); //********
  corrected_v2_pt->SetMinimum(-0.1);
  corrected_v2_pt->SetMaximum(0.6);
  //corrected_v2ptCounter->GetXaxis()->SetTitle("Pt GeV") ;
  //corrected_v2ptCounter->SetNamey("v2 vs pt ");   
  //corrected_v2ptCounter->SetTitle("v2 vs pt");

  LOG(DBG, "HERE");

  dedx->GetXaxis()->SetTitle("Primary Momentum pt") ;
  dedx->GetYaxis()->SetTitle("dEdx in GeV/cm") ;
  dedx->GetYaxis()->CenterTitle();

  LOG(DBG, "EHER");

  piplus->GetXaxis()->SetTitle("Pt GeV") ;
  piminus->GetXaxis()->SetTitle("Pt GeV") ;
  kplus->GetXaxis()->SetTitle("Pt GeV") ;
  kminus->GetXaxis()->SetTitle("Pt GeV") ;
  pplus->GetXaxis()->SetTitle("Pt GeV") ;
  pminus->GetXaxis()->SetTitle("Pt GeV") ;
     
  piplus->SetTitle("Single Run Pi K P pt distrbution") ;
  piplus->SetName("Pt spectra") ;
  piminus->SetTitle("Single Run Pi K P pt distrbution") ;
  piminus->SetName("Pt spectra") ;
  kplus->SetTitle("Single Run Pi K P pt distrbution") ;
  kplus->SetName("Pt spectra") ;
  kminus->SetTitle("Single Run Pi K P pt distrbution") ;
  kminus->SetName("Pt spectra") ;
  pplus->SetTitle("Single Run Pi K P pt distrbution") ;
  pplus->SetName("Pt spectra") ;
  pminus->SetTitle("Single Run Pi K P pt distrbution") ;
  pminus->SetName("Pt spectra") ;

  Yield->GetXaxis()->SetTitle("bin 1=pi-/pi+, 2=k-/k+, 3=pbar/p, 4=k-/pi-, 5=k+/pi+, 6=pbar/pi-, 7=p/pi+") ;
  corrected_Yield->GetXaxis()->SetTitle("Pt GeV") ;
  Yield->SetTitle("Single Run Yield and corrected_Yield") ;
  corrected_Yield->SetTitle("Single Run Yield and corrected_Yield") ;
 

  LOG(DBG, "HERE");
     
  hbtnum->SetTitle("Single Run HBT num and den") ;
  hbtnum->SetName("HBTa") ;
  hbtnum->GetXaxis()->SetTitle("Qinv (GeV/c)");
  hbtden->SetTitle("Single Run HBT num and den") ;
  hbtden->SetName("HBTa") ;
  hbtden->GetXaxis()->SetTitle("Qinv (GeV/c)");
  hbtCF_qinv->SetTitle("Single Run HBT C(Qinv) - normalization range was 0.15 to 0.20");
  hbtCF_qinv->SetName("HBT");
  hbtCF_qinv->GetXaxis()->SetTitle("Qinv (GeV/c)");
  hbtCF_qinv->GetYaxis()->SetTitle("C(Qinv)");
     
  ///////////////////////////////////////////////////////////////////////

//******************* Set Styles for Cumulative Analysis Histos ******************

  //------------Set Marker---------------//   
  tot_piplus->SetLineColor(2);
  tot_piplus->SetMarkerStyle(20);
  tot_piplus->SetMarkerSize(1.0);
  tot_piplus->SetMarkerColor(2);

  tot_piminus->SetLineColor(3); 
  tot_piminus->SetMarkerStyle(21);
  tot_piminus->SetMarkerSize(1.0);
  tot_piminus->SetMarkerColor(3);

  tot_kplus->SetLineColor(4);
  tot_kplus->SetMarkerStyle(20);
  tot_kplus->SetMarkerSize(1.0);
  tot_kplus->SetMarkerColor(2);

  tot_kminus->SetLineColor(5); 
  tot_kminus->SetMarkerStyle(21);
  tot_kminus->SetMarkerSize(1.0);
  tot_kminus->SetMarkerColor(5);

  tot_pplus->SetLineColor(6);
  tot_pplus->SetMarkerStyle(20);
  tot_pplus->SetMarkerSize(1.0);
  tot_pplus->SetMarkerColor(6);

  tot_pminus->SetLineColor(7);
  tot_pminus->SetMarkerStyle(21);
  tot_pminus->SetMarkerSize(1.0);
  tot_pminus->SetMarkerColor(7);
     
  tot_Yield->SetLineColor(2);
  tot_corrected_Yield->SetLineColor(4);
     
  LOG(DBG, "HERE");

  tot_hbtnum->SetLineColor(2);
  tot_hbtnum->SetMarkerStyle(21);
  tot_hbtnum->SetMarkerSize(1.0);
  tot_hbtnum->SetMarkerColor(2);

  tot_hbtden->SetLineColor(4);
  tot_hbtden->SetMarkerStyle(21);
  tot_hbtden->SetMarkerSize(1.0);
  tot_hbtden->SetMarkerColor(4);

  LOG(DBG, "HERE");
  //-------------Set X Y Title--------------//
  //tot_Vz1->GetXaxis()->SetTitle("Primary Vz");
  //tot_Vxy1->GetXaxis()->SetTitle("Primary Vx");
  //tot_Vxy1->GetYaxis()->SetTitle("Primary Vy");
  //tot_Mult1->GetXaxis()->SetTitle("Multiplicity");
  //tot_Eta1->GetXaxis()->SetTitle("Pseudorapidity");  
  tot_Timevsmultiplicity->GetXaxis()->SetTitle("Multiplicity");     
  tot_Timevsmultiplicity->GetYaxis()->SetTitle("Time (ms)");
  tot_Ratevsmultiplicity->GetXaxis()->SetTitle("Multiplicity");
  tot_Ratevsmultiplicity->GetYaxis()->SetTitle("rate/event (Hz)");

  LOG(DBG, "HERE 0x%x 0x%x",v2_pt,v2ptCounter);
  tot_v2_pt->GetXaxis()->SetTitle("Pt GeV") ;
  LOG(DBG, "HERE");
  tot_v2_pt->GetYaxis()->SetTitle("v2") ;
  tot_v2_pt->GetYaxis()->CenterTitle();
  tot_v2_pt->SetName("hlt_tot_v2 vs pt");               //*********************
  tot_v2_pt->SetTitle("Accumulated v2 vs pt");  //*********************
  tot_v2_pt->SetMinimum(-0.1);
  tot_v2_pt->SetMaximum(0.6);
  //tot_v2ptCounter->GetXaxis()->SetTitle("Pt GeV") ;
  //tot_v2ptCounter->SetName("tot_v2 vs pt ");
  //tot_v2ptCounter->SetTitle("v2 vs pt");

  LOG(DBG, "HERE");

  tot_resolution->SetTitle("Accumulated v2 resolution") ;
     
  tot_corrected_v2_pt->GetXaxis()->SetTitle("Pt GeV") ;
  tot_corrected_v2_pt->GetYaxis()->SetTitle("v2") ;
  tot_corrected_v2_pt->GetYaxis()->CenterTitle();
  tot_corrected_v2_pt->SetName("hlt_tot_corrected v2 vs pt");              //******
  tot_corrected_v2_pt->SetTitle("Accumulated corrected v2 vs pt"); //******
  tot_corrected_v2_pt->SetMinimum(-0.1);
  tot_corrected_v2_pt->SetMaximum(0.6);
  //tot_corrected_v2ptCounter->GetXaxis()->SetTitle("Pt GeV") ;
  //tot_corrected_v2ptCounter->SetNamey("v2 vs pt ");   
  //tot_corrected_v2ptCounter->SetTitle("v2 vs pt");

  LOG(DBG, "HERE");

  tot_dedx->GetXaxis()->SetTitle("Primary Momentum pt") ;
  tot_dedx->GetYaxis()->SetTitle("dEdx in GeV/cm") ;
  tot_dedx->GetYaxis()->CenterTitle();

  LOG(DBG, "EHER");

  tot_piplus->GetXaxis()->SetTitle("Pt GeV") ;
  tot_piminus->GetXaxis()->SetTitle("Pt GeV") ;
  tot_kplus->GetXaxis()->SetTitle("Pt GeV") ;
  tot_kminus->GetXaxis()->SetTitle("Pt GeV") ;
  tot_pplus->GetXaxis()->SetTitle("Pt GeV") ;
  tot_pminus->GetXaxis()->SetTitle("Pt GeV") ;
     
  tot_piplus->SetTitle("Accumulated Pi K P pt distrbution") ;
  tot_piplus->SetName("tot_Pt spectra") ;
  tot_piminus->SetTitle("Accumulated Pi K P pt distrbution") ;
  tot_piminus->SetName("tot_Pt spectra") ;
  tot_kplus->SetTitle("Accumulated Pi K P pt distrbution") ;
  tot_kplus->SetName("tot_Pt spectra") ;
  tot_kminus->SetTitle("Accumulated Pi K P pt distrbution") ;
  tot_kminus->SetName("tot_Pt spectra") ;
  tot_pplus->SetTitle("Accumulated Pi K P pt distrbution") ;
  tot_pplus->SetName("tot_Pt spectra") ;
  tot_pminus->SetTitle("Accumulated Pi K P pt distrbution") ;
  tot_pminus->SetName("tot_Pt spectra") ;

  tot_Yield->GetXaxis()->SetTitle("bin 1=pi-/pi+, 2=k-/k+, 3=pbar/p, 4=k-/pi-, 5=k+/pi+, 6=pbar/pi-, 7=p/pi+") ;
  tot_corrected_Yield->GetXaxis()->SetTitle("Pt GeV") ;
  tot_Yield->SetTitle("Accumulated Yield and corrected_Yield") ;
  tot_corrected_Yield->SetTitle("Accumulated Yield and corrected_Yield") ;
 

  LOG(DBG, "HERE");
     
  tot_hbtnum->SetTitle("Accumulated HBT num and den") ;
  tot_hbtnum->SetName("tot_HBTa") ;
  tot_hbtnum->GetXaxis()->SetTitle("Qinv (GeV/c)");
  tot_hbtden->SetTitle("Accumulated HBT num and den") ;
  tot_hbtden->SetName("tot_HBTa") ;
  tot_hbtden->GetXaxis()->SetTitle("Qinv (GeV/c)");
  tot_hbtCF_qinv->SetTitle("Accumulated HBT C(Qinv) - normalization range was 0.15 to 0.20");
  tot_hbtCF_qinv->SetName("tot_HBT");
  tot_hbtCF_qinv->GetXaxis()->SetTitle("Qinv (GeV/c)");
  tot_hbtCF_qinv->GetYaxis()->SetTitle("C(Qinv)");
     
  ///////////////////////////////////////////////////////////////////////

//****************** End Set Styles for Cumulative Analysis Histos ***************
  //INITIALIZE THE HBT BUFFER VARIABLES
  for(int vv=0;vv<NvertexMixingBins;vv++) {
    for(int mm=0;mm<NmultMixingBins;mm++) {
      for(int k=0;k<Nhbtmixing;k++) {
	hbt_buffer[k][vv][mm].mult = 0;
	hbt_buffer[k][vv][mm].zvertex = 0.0;
	hbt_buffer[k][vv][mm].ntracks = 0;
	for(int i=0;i<10000;i++) {
	  for(int j=0;j<5;j++) {
	    hbt_buffer[k][vv][mm].track[i][j] = 0.0;
	  }
	}
      }
    }}
  hbt_current.mult = 0;
  hbt_current.zvertex = 0.0;
  hbt_current.ntracks = 0;
  for(int i=0;i<10000;i++) {
    for(int j=0;j<5;j++) {
      hbt_current.track[i][j] = 0.0;
    }
  }
  printf("n\n");
  
  //set cut information here
  //vertexCuts
  eventCuts.zvertexMin = -75.0;		eventCuts.zvertexMax = 75.0;
  eventCuts.rvertexMin = 0.;		eventCuts.rvertexMax = 2.0;
  eventCuts.BEMCeastEnergyMin = 20.;	eventCuts.BEMCeastEnergyMax = 1000000.;
  eventCuts.BEMCwestEnergyMin = 20.;	eventCuts.BEMCwestEnergyMax = 1000000.;
  eventCuts.BEMC_EastWestDiffMin=0;	eventCuts.BEMC_EastWestDiffMax=6;
  eventCuts.multMin = 17.;  		eventCuts.multMax = 10000.;
  
  multTrackCuts.nHitsMin = 20;   	multTrackCuts.nHitsMax = 48.; //I think max is ~48.
  multTrackCuts.dcaMin = 1.0e-05;	multTrackCuts.dcaMax = 3.;
  multTrackCuts.ptMin = 0.1;		multTrackCuts.ptMax = 100.;
  multTrackCuts.etaMin = -1.;		multTrackCuts.etaMax = 1.;
  multTrackCuts.rapMin = 1.;		multTrackCuts.rapMax = 0.; //no rap cut for mult.
  
  v2ptTrackCuts.nHitsMin = 20;   	v2ptTrackCuts.nHitsMax = 48.; //I think max is ~48.
  v2ptTrackCuts.dcaMin = 1.0e-05;	v2ptTrackCuts.dcaMax = 3.;
  v2ptTrackCuts.ptMin = 0.1;		v2ptTrackCuts.ptMax = 10.;
  v2ptTrackCuts.etaMin = -1.;		v2ptTrackCuts.etaMax = 1.; //eta cut for v2(pt)
  v2ptTrackCuts.rapMin = 1.;		v2ptTrackCuts.rapMax = 0.; //should it be a rap cut?
      
  ptSpectraTrackCuts.nHitsMin = 20;   	ptSpectraTrackCuts.nHitsMax = 48.; //I think max is ~48.
  ptSpectraTrackCuts.dcaMin = 1.0e-05;	ptSpectraTrackCuts.dcaMax = 3.;
  ptSpectraTrackCuts.ptMin = 0.1;	ptSpectraTrackCuts.ptMax = 10000.;
  ptSpectraTrackCuts.etaMin = -1.;	ptSpectraTrackCuts.etaMax = 1.;  //is it rap or eta cut for ptspectra
  ptSpectraTrackCuts.rapMin = -0.5;	ptSpectraTrackCuts.rapMax = 0.5; 
  //hbtnHitsMin was 15
  hbtTrackCuts.nHitsMin = 25;   	hbtTrackCuts.nHitsMax = 48.; //I think max is ~48.
  hbtTrackCuts.dcaMin = 1.0e-05;	hbtTrackCuts.dcaMax = 3.;
  hbtTrackCuts.ptMin = 0.17;		hbtTrackCuts.ptMax = 0.35;
  hbtTrackCuts.etaMin = 1.;		hbtTrackCuts.etaMax = -1.; //no eta cut for hbt
  hbtTrackCuts.rapMin = -0.5;		hbtTrackCuts.rapMax = 0.5;
 
  LOG(DBG, "ERE");

};
  
  void hltBuilder::startrun(daqReader *rdr) {
  printf("hello there. This is startrun\n");
      cVz1->Reset();  cVz2->Reset();  cVz3->Reset();  cVz4->Reset();
      cVx1->Reset();  cVx2->Reset();  cVx3->Reset();
      cVy1->Reset();  cVy2->Reset();  cVy3->Reset();
      cVr1->Reset();  cVr2->Reset();  cVr3->Reset();  cVr4->Reset();
      cM1->Reset();  cM2->Reset();  cM3->Reset();  cM4->Reset();      
      
      Vz1->Reset();  Vz2->Reset();  Vz3->Reset();  Vz4->Reset();
      Vx1->Reset();  Vx2->Reset();  Vx3->Reset();  Vx4->Reset();
      Vy1->Reset();  Vy2->Reset();  Vy3->Reset();  Vy4->Reset();
      Vr1->Reset();  Vr2->Reset();  Vr3->Reset();  Vr4->Reset();
      Vxy1->Reset();  Vxy2->Reset();  Vxy3->Reset();  Vxy4->Reset();
      Mult1->Reset();  Mult2->Reset();  Mult3->Reset();  Mult4->Reset();
      Eta1->Reset();  Eta2->Reset();  Eta3->Reset();  Eta4->Reset();
      Timevsmultiplicity->Reset();
      Ratevsmultiplicity->Reset();     
      v2_pt->Reset();
      //v2ptCounter->Reset();
      resolution->Reset(); 
      corrected_v2_pt->Reset();   
      //corrected_v2ptCounter->Reset();
      dedx->Reset();
      piplus->Reset();
      piminus->Reset();
      kplus->Reset();
      kminus->Reset();
      pplus->Reset();
      pminus->Reset();
      Yield->Reset();
      corrected_Yield->Reset();
      hbtnum->Reset();
      hbtden->Reset();
      hbtCF_qinv->Reset();
      //Vr_dist->Reset();
      BEMC_1a->Reset();	BEMC_2a->Reset(); BEMC_3a->Reset(); BEMC_4a->Reset();
      BEMC_1b->Reset();	BEMC_2b->Reset(); BEMC_3b->Reset(); BEMC_4b->Reset();
      BEMC_1c->Reset();	BEMC_2c->Reset(); BEMC_3c->Reset(); BEMC_4c->Reset();
      BEMC_1d->Reset();	BEMC_2d->Reset(); BEMC_3d->Reset(); BEMC_4d->Reset();
      
      
      setD_singlerun_cVz1->Reset();  setD_singlerun_cVz2->Reset();  setD_singlerun_cVz3->Reset();  setD_singlerun_cVz4->Reset();
      setE_singlerun_cVz1->Reset();  setE_singlerun_cVz2->Reset();  setE_singlerun_cVz3->Reset();  setE_singlerun_cVz4->Reset();
      
      
     //****************** New Periodic histos for Vertex Method of counting - added Apr 27, 2010
     //Bool_t PeriodicConditionIsTrue = kFALSE;  //don't reset unless its time
     //gettimeofday(&PeriodicResetTest,NULL);
     //timersub(&PeriodicResetTest,&PeriodicStart,&PeriodicResult);
       //if( PeriodicConditionIsTrue ) 
       //printf("start 1 hour loop\n");
       //while( double(PeriodicResult.tv_sec)  < 10 ) { //3610.*1.) {
         //gettimeofday(&PeriodicResetTest,NULL);
         //timersub(&PeriodicResetTest,&PeriodicStart,&PeriodicResult);
	 //printf("time = %f\n",double(PeriodicResult.tv_sec));
       //}
       //printf("finished 1 hour loop\n");
     //printf("\n %f   -   %f   =   %f\n",double(PeriodicResetTest.tv_sec),double(PeriodicStart.tv_sec),double(PeriodicResult.tv_sec));
     //if( (double(PeriodicResult.tv_sec)/3600.) > 3. ) { //may need some math with result.tv_sec.
     //if( (double(PeriodicResult.tv_sec) > 3599.) ) {
     if( (last_time + 3600*nHours) < time(NULL) ) {
       printf("resetting periodic histos\n");
       last_time = time(NULL);  //reset the starting time for the next period.
       //gettimeofday(&PeriodicStart,NULL); //reset the starting time for the next period.
       periodic_cVz1->Reset();  periodic_cVz2->Reset();  periodic_cVz3->Reset();  periodic_cVz4->Reset();  //plot1-plot4
       periodic_cVx1->Reset();  periodic_cVx2->Reset();  periodic_cVx3->Reset();		   //plot5-plot8
       periodic_cVy1->Reset();  periodic_cVy2->Reset();  periodic_cVy3->Reset();		   //plot9-plot12
       periodic_cVr1->Reset();  periodic_cVr2->Reset();  periodic_cVr3->Reset();  periodic_cVr4->Reset();  //plot13-plot16
       periodic_cVxy1->Reset();  periodic_cVxy2->Reset();  periodic_cVxy3->Reset();  periodic_cVxy4->Reset();  //plot17-plot20
       periodic_cM1->Reset();  periodic_cM2->Reset();  periodic_cM3->Reset();  periodic_cM4->Reset();  //plot21-plot24
       
       setD_periodic_cVz1->Reset();  setD_periodic_cVz2->Reset();  setD_periodic_cVz3->Reset();  setD_periodic_cVz4->Reset();
       setE_periodic_cVz1->Reset();  setE_periodic_cVz2->Reset();  setE_periodic_cVz3->Reset();  setE_periodic_cVz4->Reset();
       
     }
     //**************** End new Periodic histos for Vertex Method of counting

    printf("Starting run #%d\n",rdr->run);
    
  };

  void hltBuilder::stoprun(daqReader *rdr) {
	printf("Number of events processed in daq file = %d\n",eventCounter);

        printf("Stopping run #%d\n",rdr->run);
  };
  

void hltBuilder::event(daqReader *rdr) {
  
//************************************** SET THE TRIGGER BIT HERE to min bias value *************  
//We want all events right now (not just min-bias), min-bias is our main trigger.
  u_int trg = rdr->daqbits;
  //int minbias = 0x20;
  int vpdtag = 0x200;
  FILL_VPD_HISTOS = kFALSE;
  if (trg & vpdtag) FILL_VPD_HISTOS = kTRUE;
  else FILL_VPD_HISTOS = kFALSE;
  //if (trg & minbias) { //start of check for minbias.
//***********************************************************************************************  
  

  //printf("Event %d/%d: 0x%x\n",rdr->event_number,rdr->seq,trg);
  //    for(int i=0;i<32;i++) {
  //      if(trg & (1<<i)) {
  //	((TH1I *)getPlotByIndex(0)->getHisto(0)->histo)->Fill(i);
  //      }
  //    }

  //ofstream mydedxfile;
  //mydedxfile.open("mydedxfile.dat",std::ios::app);

  //EXTRACT L3 TRACK INFO FROM DAQ FILE
  //daq_dta *dd = rdr->det("l3")->get("legacy");
  daq_dta *dd = rdr->det("hlt")->get("gl3");

  if (!dd) { 
    LOG(DBG, "No HLT in this event");
    //printf("The daq_dta objected was not initiated, the line daq_dta *dd = rdr->det->get ... did not work.\n");
    return;
  }
  eventCounter++;
  //printf("event number %d\n",eventCounter);

  int MULTIPLICITY = 0;

  int trkindx=0;
  //	int species = 8;  //initialized to 8 because this is a particle that is not a pion, kaon, or proton
  //	bool FILL_NUMERATOR_SWITCH = false;	
	
  HLT_EVE  *hlt_eve ; HLT_TOF  *hlt_tof ; HLT_PVPD *hlt_pvpd ; HLT_EMC  *hlt_emc ; HLT_GT   *hlt_gt ;
  HLT_PT   *hlt_pt ;  HLT_NODE *hlt_node ; HLT_HIPT *hlt_hipt ;HLT_DIEP *hlt_diep ;HLT_HF *hlt_hf ;

  while(dd && dd->iterate()){
    hlt_gl3_t *hlt = (hlt_gl3_t *) dd->Void ;

    if(strcmp(hlt->name,"HLT_EVE")==0) hlt_eve = (HLT_EVE *)hlt->data;
    else if(strcmp(hlt->name,"HLT_TOF")==0) hlt_tof = (HLT_TOF *)hlt->data;
    else if(strcmp(hlt->name,"HLT_PVPD")==0) hlt_pvpd = (HLT_PVPD *)hlt->data;
    else if(strcmp(hlt->name,"HLT_EMC")==0) hlt_emc = (HLT_EMC *)hlt->data;
    else if(strcmp(hlt->name,"HLT_GT")==0) hlt_gt = (HLT_GT *)hlt->data;
    else if(strcmp(hlt->name,"HLT_PT")==0) hlt_pt = (HLT_PT *)hlt->data;
    else if(strcmp(hlt->name,"HLT_NODE")==0) hlt_node = (HLT_NODE *)hlt->data;
    else if(strcmp(hlt->name,"HLT_HIPT")==0) hlt_hipt = (HLT_HIPT *)hlt->data;
    else if(strcmp(hlt->name,"HLT_DIEP")==0) hlt_diep = (HLT_DIEP *)hlt->data;
    else if(strcmp(hlt->name,"HLT_HF")==0) hlt_hf = (HLT_HF *)hlt->data;
  }

  //PRINT SOME EVENT INFORMATION
  //	  printf("sequence %d: gTracks %d: tofHits: %d: pvpdHits %d: eePairs %d: emcTowers %d: highPt %d : heavyFrag %d \n",rdr->seq, hlt_gt->nGlobalTracks, hlt_tof->nTofHits, hlt_pvpd->nPvpdHits, hlt_diep->nEPairs, hlt_emc->nEmcTowers, hlt_hipt->nHighPt, hlt_hf->nHeavyFragments) ;
  //	  printf("pTracks %d\n",hlt_pt->nPrimaryTracks);

  for(int aloop=0; aloop<1 ;aloop++) {	  

    timeval start, stop, result;
    gettimeofday(&start,NULL);
	  
    //FILL VertexZ HISTOGRAM WITH DEFAULT L3 Z VERTEX POSITION
    //Vz1->Fill(hlt_eve->vertexZ);
    //Vxy->Fill(hlt_eve->vertexX, hlt_eve->vertexY);
    //Vr_dist->Fill( sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)) );
    //if ( vertexEventCut(hlt_eve, &eventCuts) ) { continue; }
    NO_CUTS_VERTEX = kTRUE; //always have this true
    VzLT200_VxLT2_VyLT2_VERTEX = kFALSE; //always start event with this false
    VzLT70_VxLT2_VyLT2_VERTEX = kFALSE;  //always start event with this false
    VzLT70_VxGT2_VyGT2_VERTEX = kFALSE;  //always start event with this false
    float VrValue = sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2));
    //if( (fabs(hlt_eve->vertexZ) < 200) && (fabs(hlt_eve->vertexX) < 2) && (fabs(hlt_eve->vertexY) < 2) ) VzLT200_VxLT2_VyLT2_VERTEX = kTRUE;
    //if( (fabs(hlt_eve->vertexZ) < 70 ) && (fabs(hlt_eve->vertexX) < 2) && (fabs(hlt_eve->vertexY) < 2) ) VzLT70_VxLT2_VyLT2_VERTEX = kTRUE;
    //if( (fabs(hlt_eve->vertexZ) < 70 ) && ((fabs(hlt_eve->vertexX) > 2) || (fabs(hlt_eve->vertexY) > 2)) ) VzLT70_VxGT2_VyGT2_VERTEX = kTRUE;
    //Use a Vr < 2 cut instead of Vx<2, Vy<2
    VrLT2_VERTEX = kFALSE;
    VzLT70_VERTEX = kFALSE;
    PRIM_TRACK_GT5_MULT = kFALSE;
    if( VrValue < 2. ) VrLT2_VERTEX = kTRUE;
    if( fabs(hlt_eve->vertexZ) < 70. ) VzLT70_VERTEX = kTRUE;
    if( hlt_pt->nPrimaryTracks > 5 )  PRIM_TRACK_GT5_MULT = kTRUE; //Is this what we want?
    //These are the original cuts that used a square.  They now use Vr less than 2 cm.
    if( (fabs(hlt_eve->vertexZ) < 200) && (VrValue < 2) ) VzLT200_VxLT2_VyLT2_VERTEX = kTRUE;
    if( (fabs(hlt_eve->vertexZ) < 70 ) && (VrValue < 2) ) VzLT70_VxLT2_VyLT2_VERTEX = kTRUE;
    if( (fabs(hlt_eve->vertexZ) < 70 ) && (VrValue > 2) )  VzLT70_VxGT2_VyGT2_VERTEX = kTRUE;    
//    if( (fabs(hlt_eve->vertexZ) < 200) && (fabs(hlt_eve->vertexX) < 2) && (fabs(hlt_eve->vertexY) < 2) ) VzLT200_VxLT2_VyLT2_VERTEX = kTRUE;
//    if( (fabs(hlt_eve->vertexZ) < 70 ) && (fabs(hlt_eve->vertexX) < 2) && (fabs(hlt_eve->vertexY) < 2) ) VzLT70_VxLT2_VyLT2_VERTEX = kTRUE;
//    if( (fabs(hlt_eve->vertexZ) < 70 ) && ((fabs(hlt_eve->vertexX) > 2) || (fabs(hlt_eve->vertexY) > 2)) ) VzLT70_VxGT2_VyGT2_VERTEX = kTRUE;

//printf("testing if the compiler sees this code\n");
    //MULTIPLICITY (only pi+, pi-, k+, k-, p, pbar from PID function)
    MULTIPLICITY = 0;
    UPDATE_SWITCH = 0; //for hbt buffer
    for(u_int i=0; i < hlt_pt->nPrimaryTracks; i++) {
      hlt_track ptrack = hlt_pt->primaryTrack[i];
      if( NO_CUTS_VERTEX ) Eta1->Fill(getEta(&ptrack));
      if( VzLT200_VxLT2_VyLT2_VERTEX ) Eta2->Fill(getEta(&ptrack));
      if( VzLT70_VxLT2_VyLT2_VERTEX ) Eta3->Fill(getEta(&ptrack));
      if( VzLT70_VxGT2_VyGT2_VERTEX ) Eta4->Fill(getEta(&ptrack));
      if ( trackCut(&ptrack, hlt_eve, &multTrackCuts) ) {
	int species = getPID(&ptrack); //ptrack.pt, ptrack.dedx, ptrack.q, ptrack.tanl);
	if( (species != 0) && (species != 8) && (species != 7) ) { 
	  MULTIPLICITY++;
	  dedx->Fill(ptrack.pt,ptrack.dedx);                                
	  tot_dedx->Fill(ptrack.pt,ptrack.dedx);                                
	  //				mydedxfile << ptrack.pt << "    " << ptrack.pt*ptrack.tanl << "    " << sqrt(pow(ptrack.pt,2)+pow(ptrack.pt*ptrack.tanl,2)) << "     " << ptrack.dedx << std::endl;
	}
      }		
    }
    //((TH1F *)getPlotByIndex(1)->getHisto(0)->histo)->Fill(MULTIPLICITY);	
    //OPENING A FILE TO WRITE DEDX INFO. (ITS FASTER THIS WAY.)
    //	  ofstream mydedxfile;
    //	  mydedxfile.open("mydedxfile.dat",std::ios::app);

//********************* Fill Histos for Vertex Method of Counting Good Events ************************

//The new vpd_tac triggered histos.
if(FILL_VPD_HISTOS) {
  setD_singlerun_cVz1->Fill(hlt_eve->vertexZ);
  setD_periodic_cVz1->Fill(hlt_eve->vertexZ);
  setD_accumulated_cVz1->Fill(hlt_eve->vertexZ);
  if(fabs(hlt_eve->vertexZ) < 100) {
    setD_singlerun_cVz2->Fill(hlt_eve->vertexZ);
    setD_periodic_cVz2->Fill(hlt_eve->vertexZ);
    setD_accumulated_cVz2->Fill(hlt_eve->vertexZ);
    if( hlt_pt->nPrimaryTracks > 5 ) {
      setD_singlerun_cVz4->Fill(hlt_eve->vertexZ);
      setD_periodic_cVz4->Fill(hlt_eve->vertexZ);
      setD_accumulated_cVz4->Fill(hlt_eve->vertexZ);  
    }  
  }
  if(fabs(hlt_eve->vertexZ) < 70) {
    setD_singlerun_cVz3->Fill(hlt_eve->vertexZ);
    setD_periodic_cVz3->Fill(hlt_eve->vertexZ);
    setD_accumulated_cVz3->Fill(hlt_eve->vertexZ);
  }
}  

//no cuts on Vr, Vz, or M
cVz1->Fill(hlt_eve->vertexZ);
cVx1->Fill(hlt_eve->vertexX);
cVy1->Fill(hlt_eve->vertexY);
cVr1->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
cM1->Fill(hlt_pt->nPrimaryTracks);
tot_cVz1->Fill(hlt_eve->vertexZ);
tot_cVx1->Fill(hlt_eve->vertexX);
tot_cVy1->Fill(hlt_eve->vertexY);
tot_cVr1->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
tot_cM1->Fill(hlt_pt->nPrimaryTracks);
periodic_cVz1->Fill(hlt_eve->vertexZ);
periodic_cVx1->Fill(hlt_eve->vertexX);
periodic_cVy1->Fill(hlt_eve->vertexY);
periodic_cVr1->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
periodic_cVxy1->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
periodic_cM1->Fill(hlt_pt->nPrimaryTracks);
if( FILL_VPD_HISTOS ) {
  setE_singlerun_cVz1->Fill(hlt_eve->vertexZ);
  setE_periodic_cVz1->Fill(hlt_eve->vertexZ);
  setE_accumulated_cVz1->Fill(hlt_eve->vertexZ);
}
//all cuts for Vr<2, Vz<70, M>5 imposed
if( VrLT2_VERTEX && VzLT70_VERTEX && PRIM_TRACK_GT5_MULT ){
  cVz2->Fill(hlt_eve->vertexZ);
  cVx2->Fill(hlt_eve->vertexX);
  cVy2->Fill(hlt_eve->vertexY);
  cVr2->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  cM2->Fill(hlt_pt->nPrimaryTracks);
  tot_cVz2->Fill(hlt_eve->vertexZ);
  tot_cVx2->Fill(hlt_eve->vertexX);
  tot_cVy2->Fill(hlt_eve->vertexY);
  tot_cVr2->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  tot_cM2->Fill(hlt_pt->nPrimaryTracks);
  periodic_cVz2->Fill(hlt_eve->vertexZ);
  periodic_cVx2->Fill(hlt_eve->vertexX);
  periodic_cVy2->Fill(hlt_eve->vertexY);
  periodic_cVr2->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  periodic_cVxy2->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
  periodic_cM2->Fill(hlt_pt->nPrimaryTracks);
  if( FILL_VPD_HISTOS ) {
    setE_singlerun_cVz2->Fill(hlt_eve->vertexZ);
    setE_periodic_cVz2->Fill(hlt_eve->vertexZ);
    setE_accumulated_cVz2->Fill(hlt_eve->vertexZ);
  }
}
//turn the Vr cut off for Vr histo, Vz off for Vz histo, M off for M histo, but keep other cuts
if ( VrLT2_VERTEX && PRIM_TRACK_GT5_MULT ) {
  cVz3->Fill(hlt_eve->vertexZ);
  tot_cVz3->Fill(hlt_eve->vertexZ);
  periodic_cVz3->Fill(hlt_eve->vertexZ);
  if( FILL_VPD_HISTOS ) {
    setE_singlerun_cVz3->Fill(hlt_eve->vertexZ);
    setE_periodic_cVz3->Fill(hlt_eve->vertexZ);
    setE_accumulated_cVz3->Fill(hlt_eve->vertexZ);
  }
}
if ( VzLT70_VERTEX && PRIM_TRACK_GT5_MULT) {
  cVr3->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  tot_cVr3->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  periodic_cVr3->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  periodic_cVxy3->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
}
if ( VrLT2_VERTEX && VzLT70_VERTEX ) {
  cM3->Fill(hlt_pt->nPrimaryTracks);
  tot_cM3->Fill(hlt_pt->nPrimaryTracks);
  periodic_cM3->Fill(hlt_pt->nPrimaryTracks);
}
//only cut is Vr>2 now
if ( ! VrLT2_VERTEX ) {
  cVz4->Fill(hlt_eve->vertexZ);
  cVx3->Fill(hlt_eve->vertexX);
  cVy3->Fill(hlt_eve->vertexY);
  cVr4->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  cM4->Fill(hlt_pt->nPrimaryTracks);
  tot_cVz4->Fill(hlt_eve->vertexZ);
  tot_cVx3->Fill(hlt_eve->vertexX);
  tot_cVy3->Fill(hlt_eve->vertexY);
  tot_cVr4->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  tot_cM4->Fill(hlt_pt->nPrimaryTracks);
  periodic_cVz4->Fill(hlt_eve->vertexZ);
  periodic_cVx3->Fill(hlt_eve->vertexX);
  periodic_cVy3->Fill(hlt_eve->vertexY);
  periodic_cVxy4->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
  periodic_cVr4->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
  periodic_cM4->Fill(hlt_pt->nPrimaryTracks);
  if( FILL_VPD_HISTOS ) {
    setE_singlerun_cVz4->Fill(hlt_eve->vertexZ);
    setE_periodic_cVz4->Fill(hlt_eve->vertexZ);
    setE_accumulated_cVz4->Fill(hlt_eve->vertexZ);
  }
}

//******************End of Fill Histos for Vertex Method of Counting Good Events *********************

	  
    //FILL BEMC PLOT HISTOS
    int BEMCeastEnergy = 0;
    int  BEMCwestEnergy = 0;
    for(u_int i=0 ; i < hlt_emc->nEmcTowers ; i++){
      //		  int adc = hlt_emc->emcTower[i].adc;
      if(hlt_emc->emcTower[i].eta > 0) BEMCeastEnergy += hlt_emc->emcTower[i].energy;
      if(hlt_emc->emcTower[i].eta <=0) BEMCwestEnergy += hlt_emc->emcTower[i].energy;
      //float phi   = hlt_emc->emcTower[i].phi;
      //float  eta   = hlt_emc->emcTower[i].eta;
      //		  float  z     = hlt_emc->emcTower[i].z; 
      //int softId  = hlt_emc->emcTower[i].softId;
      //int daqId   = hlt_emc->emcTower[i].daqId;

      //towerEnergy->Fill(energy);  //run
      //towerDaqId->Fill(daqId);  //run
      //towerSoftId->Fill(softId);  //run
      //towerEtaPhi->Fill(phi,eta);  //run
    }
    //FLAGS FOR BEMC CUTS
    NO_CUTS_BEMC = kTRUE;  //always should be true
    eGT1_wGT1_BEMC = kFALSE;			//always start event false
    eGT20_wGT20_BEMC = kFALSE;			//always start event false
    eGT20_wGT20_diffLT6_BEMC = kFALSE;		//always start event false
    eGT20_wGT20_diffLT6_bbcGT100_BEMC = kFALSE;	//always start event false
    if( (BEMCeastEnergy > 1) && (BEMCwestEnergy > 1) ) {
      eGT1_wGT1_BEMC = kTRUE;
      if( (BEMCeastEnergy > 20) && (BEMCwestEnergy > 20) ) eGT20_wGT20_BEMC = kTRUE;
      if( (BEMCeastEnergy > 20) && (BEMCwestEnergy > 20) && (fabs(BEMCeastEnergy-BEMCwestEnergy) < 6) ) eGT20_wGT20_diffLT6_BEMC = kTRUE;
      //if( (BEMCeastEnergy > 20) && (BEMCwestEnergy > 20) && (fabs(BEMCeastEnergy-BEMCwestEnergy) < 6) (BBC cut not implemented)  eGT20_wGT20_diffLT6_bbcGT100_BEMC = kTRUE;
    }
	  
    //FILL BEMC PLOTS
    if( NO_CUTS_BEMC ) {
      if (NO_CUTS_VERTEX ) {
	Vz1->Fill(hlt_eve->vertexZ);
	Vx1->Fill(hlt_eve->vertexX);
	Vy1->Fill(hlt_eve->vertexY);
	Vr1->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
	Vxy1->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	tot_Vxy1->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	Mult1->Fill(MULTIPLICITY);
	BEMC_1a->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT200_VxLT2_VyLT2_VERTEX ) {
	Vz2->Fill(hlt_eve->vertexZ);
	Vx2->Fill(hlt_eve->vertexX);
	Vy2->Fill(hlt_eve->vertexY);
	Vr2->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
	Vxy2->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	tot_Vxy2->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	Mult2->Fill(MULTIPLICITY);
	BEMC_2a->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxLT2_VyLT2_VERTEX ) {
	Vz3->Fill(hlt_eve->vertexZ);
	Vx3->Fill(hlt_eve->vertexX);
	Vy3->Fill(hlt_eve->vertexY);
	Vr3->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
	Vxy3->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	tot_Vxy3->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	Mult3->Fill(MULTIPLICITY);
	BEMC_3a->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxGT2_VyGT2_VERTEX ) {
	Vz4->Fill(hlt_eve->vertexZ);
	Vx4->Fill(hlt_eve->vertexX);
	Vy4->Fill(hlt_eve->vertexY);
	Vr4->Fill(sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2)));
	Vxy4->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	tot_Vxy4->Fill(hlt_eve->vertexX,hlt_eve->vertexY);
	Mult4->Fill(MULTIPLICITY);
	BEMC_4a->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
    }
    if( eGT1_wGT1_BEMC ) {
      if (NO_CUTS_VERTEX ) {
	BEMC_1d->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT200_VxLT2_VyLT2_VERTEX ) {
	BEMC_2d->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxLT2_VyLT2_VERTEX ) {
	BEMC_3d->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxGT2_VyGT2_VERTEX ) {
	BEMC_4d->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
    }	  	  
    if( eGT20_wGT20_BEMC ) {
      if (NO_CUTS_VERTEX ) {
	BEMC_1b->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT200_VxLT2_VyLT2_VERTEX ) {
	BEMC_2b->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxLT2_VyLT2_VERTEX ) {
	BEMC_3b->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxGT2_VyGT2_VERTEX ) {
	BEMC_4b->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
    }	  
    if( eGT20_wGT20_diffLT6_BEMC ) {
      if (NO_CUTS_VERTEX ) {
	BEMC_1c->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT200_VxLT2_VyLT2_VERTEX ) {
	BEMC_2c->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxLT2_VyLT2_VERTEX ) {
	BEMC_3c->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
      if( VzLT70_VxGT2_VyGT2_VERTEX ) {
	BEMC_4c->Fill(BEMCwestEnergy, BEMCeastEnergy);
      }
    }	  
	    
    //(TH2F*)getPlotByIndex(7)->getHisto(4)->histo->Fill(BEMCwestEnergy,BEMCeastEnergy);
    //if( BemcEventCut(BEMCeastEnergy, BEMCwestEnergy, &eventCuts) )
    //  (TH2F*)getPlotByIndex(7)->getHisto(6)->histo->Fill(BEMCwestEnergy,BEMCeastEnergy); //Bad event
    //else (TH2F*)getPlotByIndex(7)->getHisto(5)->histo->Fill(BEMCwestEnergy,BEMCeastEnergy); //Good event

    //EVENT CUTS ARE APPLIED HERE FOR THE REST OF THE ANALYSIS (SAME WAY AS BEFORE)
    if ( vertexEventCut(hlt_eve, &eventCuts) ) { continue; }
    if ( multiplicityEventCut(MULTIPLICITY, &eventCuts) ) continue;
    //Mult1->Fill(MULTIPLICITY);
	  
    //2ND ORDER REACTION PLANE VECTOR (EVENT AND 2 RANDOM SUBEVENTS)
    float Qx = 0.;	float Qy = 0.;
    float Qax = 0.;	float Qay = 0.;
    float Qbx = 0.;	float Qby = 0.;
    float weight = 0.;	
    if( V2CALC ) {
      for(u_int i=0; i < hlt_pt->nPrimaryTracks; i++) {
	hlt_track ptrack = hlt_pt->primaryTrack[i];
	if ( trackCut(&ptrack, hlt_eve, &v2ptTrackCuts) ) {
	  int species = getPID(&ptrack); //ptrack.pt, ptrack.dedx, ptrack.q, ptrack.tanl);
	  if( (species != 0) && (species != 8) && (species != 7) ) {
	    weight = 1.; // could make it pt
	    Qx += weight*cos(2.*ptrack.psi);
	    Qy += weight*sin(2.*ptrack.psi);
	    srand(time(NULL)*rand()*rand());
	    float randN = (rand() % 10 + 1);
	    if( randN < 5.0 ) {
	      Qax += weight*cos(2.*ptrack.psi);
	      Qay += weight*sin(2.*ptrack.psi);
	    } else {
	      Qbx += weight*cos(2.*ptrack.psi);
	      Qby += weight*sin(2.*ptrack.psi);
	    }
	    srand(time(NULL)*rand()+rand()*rand()*rand());
	  }
	}
      }
    } //END if(V2CALC)
    //printf("got RP vector\n");
	
    //V2(pt) NUMERATOR, PT SPECTRA, HBT NUMERATOR, HBT DENOMINATOR
    float Ux = 0.;	float Uy = 0.;
    for(u_int i=0; i < hlt_pt->nPrimaryTracks; i++) {
      hlt_track ptrack = hlt_pt->primaryTrack[i];
      int species = getPID(&ptrack); //ptrack.pt, ptrack.dedx, ptrack.q, ptrack.tanl);
      //V2(pt) numerator
      if ( trackCut(&ptrack, hlt_eve, &v2ptTrackCuts) && V2CALC ) {
	if( (species != 0) && (species != 8) && (species != 7) ) {
	  weight = 1.; // could make it pt
	  Ux = weight*cos(2.*ptrack.psi);
	  Uy = weight*sin(2.*ptrack.psi);
	  if( ptrack.pt < 2.0 ) {
	    v2_pt->Fill(ptrack.pt,(Ux*(Qx-Ux)+Uy*(Qy-Uy))/sqrt((Ux*Ux+Uy*Uy)*((Qx-Ux)*(Qx-Ux)+(Qy-Uy)*(Qy-Uy))) ) ;
	    corrected_v2_pt->Fill(ptrack.pt, (Ux*(Qx-Ux)+Uy*(Qy-Uy))/sqrt((Ux*Ux+Uy*Uy)*((Qx-Ux)*(Qx-Ux)+(Qy-Uy)*(Qy-Uy))) );
	    tot_v2_pt->Fill(ptrack.pt,(Ux*(Qx-Ux)+Uy*(Qy-Uy))/sqrt((Ux*Ux+Uy*Uy)*((Qx-Ux)*(Qx-Ux)+(Qy-Uy)*(Qy-Uy))) ) ;
	    tot_corrected_v2_pt->Fill(ptrack.pt, (Ux*(Qx-Ux)+Uy*(Qy-Uy))/sqrt((Ux*Ux+Uy*Uy)*((Qx-Ux)*(Qx-Ux)+(Qy-Uy)*(Qy-Uy))) );	  } 
	  else {
	    v2_pt->Fill(ptrack.pt,(Ux*Qx+Uy*Qy)/sqrt((Ux*Ux+Uy*Uy)*(Qx*Qx+Qy*Qy)));
	    corrected_v2_pt->Fill(ptrack.pt,(Ux*Qx+Uy*Qy)/sqrt((Ux*Ux+Uy*Uy)*(Qx*Qx+Qy*Qy)));
	    tot_v2_pt->Fill(ptrack.pt,(Ux*Qx+Uy*Qy)/sqrt((Ux*Ux+Uy*Uy)*(Qx*Qx+Qy*Qy)));
	    tot_corrected_v2_pt->Fill(ptrack.pt,(Ux*Qx+Uy*Qy)/sqrt((Ux*Ux+Uy*Uy)*(Qx*Qx+Qy*Qy)));
	  }
	  //v2ptCounter->Fill(ptrack.pt);
	  //corrected_v2ptCounter->Fill(ptrack.pt);
	}
      }
      //Pt Spectra
      if ( trackCut(&ptrack, hlt_eve, &ptSpectraTrackCuts) ) {
	if(species == 1) piplus->Fill(ptrack.pt,1);
	if(species == 2) piminus->Fill(ptrack.pt,1);
	if(species == 3) kplus->Fill(ptrack.pt,1);
	if(species == 4) kminus->Fill(ptrack.pt,1);
	if(species == 5) pplus->Fill(ptrack.pt,1);
	if(species == 6) pminus->Fill(ptrack.pt,1);
	if(species == 1) tot_piplus->Fill(ptrack.pt,1);
	if(species == 2) tot_piminus->Fill(ptrack.pt,1);
	if(species == 3) tot_kplus->Fill(ptrack.pt,1);
	if(species == 4) tot_kminus->Fill(ptrack.pt,1);
	if(species == 5) tot_pplus->Fill(ptrack.pt,1);
	if(species == 6) tot_pminus->Fill(ptrack.pt,1);
      }
      //printf("filled pt spectra\n");

      //HBT numerator
      if ( trackCut(&ptrack, hlt_eve, &hbtTrackCuts) && ((species == 1) || (species == 2)) && HBTCALC ) { //track cut on first particle
	//Loop over tracks again to make pairs
	for(u_int j=i+1; j < hlt_pt->nPrimaryTracks ; j++) {
	  hlt_track ptrackB = hlt_pt->primaryTrack[j];
	  if ( trackCut(&ptrackB, hlt_eve, &hbtTrackCuts)  && ((getPID(&ptrackB) == 1) || (getPID(&ptrackB) == 2)) ) { //track cut on second particle
	    //if( dipAngleCut(&ptrack, &ptrackB ) ) {  //optional dip angle cut
	    //mix only same sign pions
	    if( (sameSignCheck(&ptrack, &ptrackB) == -1) || ( sameSignCheck(&ptrack, &ptrackB) == 1 ) ) {
	      //if( pairCut(&ptrack, &ptrackB)) {  //pair cut
	      if( fullBuffer(getVertexMixingBin((float(hlt_eve->vertexZ))), getMultMixingBin(MULTIPLICITY) ) ) { //only fill hbt histos if buffer is full
		hbtnum->Fill(getQinv(&ptrack, &ptrackB));
		tot_hbtnum->Fill(getQinv(&ptrack, &ptrackB));
		//      ((TH3F*)getPlotByIndex(11)->getHisto(2)->histo)->Fill(getQout(&ptrack, &ptrackB),
		//  						           getQside(&ptrack, &ptrackB),
		//						           getQlong(&ptrack, &ptrackB));
	      } else setUpdateSwitch(kTRUE);//printf("didn't pass fullBuffer() check\n");
	      //}
	    }
	    //}
	  }
	}
	updateCurrentHbtEvent(&hbt_current, MULTIPLICITY, &ptrack, hlt_eve, trkindx);  //add track to current hbt event
	trkindx++;
      } //end of hbt numerator code block
		
      //HBT denominator
      if ( trackCut(&ptrack, hlt_eve, &hbtTrackCuts) && ((species == 1) || (species == 2)) && HBTCALC) { //track cut on first particle
	mmb = getMultMixingBin(MULTIPLICITY);  //returns 999999999 (nine nine's) if out of range
	vmb = getVertexMixingBin(hlt_eve->vertexZ);  //returns 999999999 (nine nine's if out of range
	// /////////////printf("main event: mmb = %d, vmb = %d\n",mmb,vmb);
	//update buffer if event is in range
	if( (vmb < NvertexMixingBins) && (mmb < NmultMixingBins) ) {
	  setUpdateSwitch(kTRUE); //its in range
	  for(int n=0; n < Nhbtmixing; n++) { //loop over number of events to mix
	    if( fullBuffer(vmb, mmb) ) { //only fill hbt histos if buffer is full
	      for(int m=0;m < hbt_buffer[n][vmb][mmb].ntracks; m++) { //loop over tracks in nth buffer event
		//if( dipAngleCut( &ptrack, hbt_buffer[n][vmb][mmb].track[m] ) ) { //optional dip angle cut
		//if( antiTrackSplittingCut( &ptrack, hbt_buffer[n][vmb][mmb].track[m] ) ) { //optional antisplitting cut
		//if( pairCut( &ptrack, hbt_buffer[n][vmb][mmb].track[m] ) ) {
		if( (sameSignCheck(&ptrack, hbt_buffer[n][vmb][mmb].track[m]) == -1) ||
		    (sameSignCheck(&ptrack, hbt_buffer[n][vmb][mmb].track[m]) == 1) ) {
		  setUpdateSwitch(kTRUE);
		  //printf("hola muchachos\n");
		  hbtden->Fill(getQinv(&ptrack, hbt_buffer[n][vmb][mmb].track[n]));
		  tot_hbtden->Fill(getQinv(&ptrack, hbt_buffer[n][vmb][mmb].track[n]));
		  //     ((TH3F*)getPlotByIndex(11)->getHisto(3)->histo)->Fill(getQout(&ptrack,hbt_buffer[n][vmb][mmb].track[n]),
		  // 							       getQside(&ptrack,hbt_buffer[n][vmb][mmb].track[n]),
		  //							       getQlong(&ptrack,hbt_buffer[n][vmb][mmb].track[n]));
		}
		//}
		//}
		//}
	      }
	    }
	  }
	}
	//else { printf("don't update\n"); setUpdateSwitch(kFALSE);  } //its out of range
      } //end of hbt denominator code block

    } //end of loop over primary tracks
	
    //V2 DENOMINATOR (THE RESOLUTION FACTOR) 
    if( V2CALC ) {
      float resolutionFactor = (Qax*Qbx+Qay*Qby)/sqrt((Qax*Qax+Qay*Qay)*(Qbx*Qbx+Qby*Qby)) ;
      //resolution->Fill(resolutionFactor,resolutionFactor);
      resolution->Fill(0.,fabs(resolutionFactor)); //2.*sqrt(resolutionFactor*resolutionFactor));  //************I need to check this math more carefully.
      tot_resolution->Fill(0.,fabs(resolutionFactor)); //2.*sqrt(resolutionFactor*resolutionFactor));  //************I need to check this math more carefully.
    }
	
    //UPDATE THE HBT BUFFER MAYBE...
    if( HBTCALC ) {
      if( getUpdateSwitch() ) {
	//printf("updating HbtEventBuffer\n");
	updateHbtEventBuffer(vmb, mmb, &hbt_current); 
      } //else printf("NOT updat(ing)HbtEventBuffer()\n");
    }

    //TRY COMPUTING HISTOS EVENT BY EVENT (OR MAYBE EVERY FEW EVENTS LATER)
    computeYieldsHistogram();
    computeV2Corrected();
    computeHbtCorrelationFunction();
	
    //	mydedxfile.close();
	
    gettimeofday(&stop,NULL);
    timersub(&stop,&start,&result);
    Timevsmultiplicity->Fill(float(MULTIPLICITY),float(result.tv_usec)/1000.);  //Convert time to milliseconds
    Ratevsmultiplicity->Fill(float(MULTIPLICITY),1000000./float(result.tv_usec));  //Rate is in Hz
    tot_Timevsmultiplicity->Fill(float(MULTIPLICITY),float(result.tv_usec)/1000.);  //Convert time to milliseconds
    tot_Ratevsmultiplicity->Fill(float(MULTIPLICITY),1000000./float(result.tv_usec));  //Rate is in Hz
    //printf("sec = %f, usec = %f, mult = %d\n",float(result.tv_sec), float(result.tv_usec)/1000.,MULTIPLICITY);

  } //end loop over event

//At the moment we are taking all triggers (not just min-bias)
//} // end of if statement checking for a minbias triggered event.


}; //end of event() function


  int hltBuilder::selectEvent(daqReader *rdr) {
    return 1;
  };

  int hltBuilder::selectRun(daqReader *rdr) {
    return 1;
  };  

  int hltBuilder::getPID(hlt_track *track) { //double Pt, double dEdx, int charge, double Tanl) {
    //The function returns a number to identify each particle according to the following codes
    //0 = e-/e+, 1 = pi+, 2=pi-, 3=k+, 4=k-, 5=p, 6=pbar, 7=d(not implimented currently), 8=other
double Pt = track->pt;
double dEdx = track->dedx;
double Tanl = track->tanl;
double charge = track->q;
//New PID scheme
double P = sqrt(Pt*Pt+Pt*Tanl*Pt*Tanl);
double A=2.3;
double dedxpion = A*(1+0.13957018*0.13957018/P/P)/1000000.;
double dedxkaon = A*(1+0.493677*0.493677/P/P)/1000000.;
double dedxproton = A*(1+0.93827203*0.93827203/P/P)/1000000.;
double dedxdeuteron = A*(1+2*0.93827203*2*0.93827203/P/P)/1000000.;
if(dEdx == 0) return 8;
if( (dEdx > 0) && (dEdx < (dedxpion+0.5*(dedxkaon-dedxpion))) ) {
  if(charge > 0) return 1; //pi+
  if(charge < 0) return 2; //pi-
}
if( (dEdx >= (dedxpion+0.5*(dedxkaon-dedxpion))) &&
    (dEdx < (dedxkaon+(0.5*(dedxproton-dedxkaon)))) ) {
    if(charge > 0) return 3; //k+
    if(charge < 0) return 4; //k-
}
if( (dEdx >= (dedxkaon+(0.5*(dedxproton-dedxkaon)))) &&
    (dEdx < (dedxproton+(0.5*(dedxdeuteron-dedxproton)))) ) {
    if(charge > 0) return 5; //p
    if(charge < 0) return 6; //pbar
}

//These functions optimized for run 10 AuAu 200 gev data
double ecutmax = (0.4/1.95*log(Pt)+2.9)/1000000.;
double ecutmin = (0.4/1.95*log(Pt)+2.5)/1000000.;
double pikline = (-0.3*log(0.2*Pt)/Pt+1.7)/1000000.;
double kpline = (-1.9*log(1.05*Pt)/Pt+3.3)/1000000.;
  
//These functions optimized for run 9 pp data  
//double ecutmax = (0.4/1.95*log(Pt)+3.1)/1000000.;
//double ecutmin = (0.4/1.95*log(Pt)+2.7)/1000000.;
//double pikline = (-0.3*log(0.2*Pt)/Pt+1.7)/1000000.;
//double kpline = (-1.9*log(1.05*Pt)/Pt+3.3)/1000000.;

    //These are the same functions used for 9gev AuAu data from run 8 but shifted down for the run 9 pp data
    if(dEdx == 0) return 8;  //make sure there is a dedx computed.
    if((dEdx > ecutmin) && (dEdx < ecutmax)) return 0;  //e+ or e-
    if(dEdx < pikline || dEdx < ecutmin) {
	if(charge > 0) return 1;  //pi+
	if(charge < 0) return 2;  //pi-
    }
    if( (dEdx < kpline) && (dEdx >= pikline)) {
	if(charge > 0) return 3;  //k+
	if(charge < 0) return 4;  //k-
    }
    if( (dEdx > kpline) ) {
	if(charge > 0) return 5;  //p
	if(charge < 0) return 6;  //pbar
    }
    //currently no cut on deuterons, they are included in the protons
    

    //The functions used to separate dEdx bands are found empirically using 9gev AuAu data from run 8.    
    if((dEdx > (0.4/1.95*log(Pt)+3.4-0.1)*1.0e-06) && (dEdx < (0.4/1.95*log(Pt)+3.8)*1.0e-06)) return 0;  //e+ or e-
    if(dEdx < (-0.35*log(0.2*Pt)/Pt+2.7)/1000000.) {
	if(charge > 0) return 1;  //pi+
	if(charge < 0) return 2;  //pi-
    }
    if( (dEdx < (-1.9*log(0.55*Pt)/Pt+3.)/1000000.) && (dEdx >= (-0.35*log(0.2*Pt)/Pt+2.7)/1000000.)) {
	if(charge > 0) return 3;  //k+
	if(charge < 0) return 4;  //k-
    }
    if( (dEdx > (-1.9*log(0.55*Pt)/Pt+3.)/1000000.) ) {
	if(charge > 0) return 5;  //p
	if(charge < 0) return 6;  //pbar
    }
    //currently no cut on deuterons, they are included in the protons
    return 8;
  };  //end of getPID function

  //THIS FUNCTION COMPUTES QINV FOR 1D HBT
  float hltBuilder::getQinv(hlt_track *trackA, hlt_track *trackB) {
       float px1 = trackA->pt*cos(trackA->psi);
       float py1 = trackA->pt*sin(trackA->psi);
       float pz1 = trackA->pt*trackA->tanl;
       float px2 = trackB->pt*cos(trackB->psi);
       float py2 = trackB->pt*sin(trackB->psi);
       float pz2 = trackB->pt*trackB->tanl;
       return ( sqrt(pow(px1-px2,2)+pow(py1-py2,2)+pow(pz1-pz2,2)) );
  };
  float hltBuilder::getQinv(hlt_track *trackA, float *trackB) {
       float px1 = trackA->pt*cos(trackA->psi);
       float py1 = trackA->pt*sin(trackA->psi);
       float pz1 = trackA->pt*trackA->tanl;
       float px2 = trackB[0]; //trackB.pt*cos(trackB.psi);
       float py2 = trackB[1]; //trackB.pt*sin(trackB.psi);
       float pz2 = trackB[2]; //trackB.pt*trackB.tanl;
       return ( sqrt(pow(px1-px2,2)+pow(py1-py2,2)+pow(pz1-pz2,2)) );
  };

  //THIS FUNCTION COMPUTES THE QOUT HBT COMPONENT
  //  float getQout(float px1, float px2, float py1, float py2) {  
  float hltBuilder::getQout(hlt_track *trackA, hlt_track *trackB) {
       float px1 = trackA->pt*cos(trackA->psi);
       float py1 = trackA->pt*sin(trackA->psi);
       float px2 = trackB->pt*cos(trackB->psi);
       float py2 = trackB->pt*sin(trackB->psi);
       float dx = px1-px2;
       float xt = px1+px2;
       float dy = py1-py2;
       float yt = py1+py2;
       float k1 = sqrt(xt*xt+yt*yt);
       float k2 = (dx*xt+dy*yt);
       return (k2/k1); //qout
  };
  //float getQout(hlt_track *trackA, hbt_event_info hbt_object, int trackNumber) {
  float hltBuilder::getQout(hlt_track *trackA, float *trackB) {
       float px1 = trackA->pt*cos(trackA->psi);
       float py1 = trackA->pt*sin(trackA->psi);
       float px2 = trackB[0]; //hbt_object.track[trackNumber][0];
       float py2 = trackB[1]; //hbt_object.track[trackNumber][1];
       float dx = px1-px2;
       float xt = px1+px2;
       float dy = py1-py2;
       float yt = py1+py2;
       float k1 = sqrt(xt*xt+yt*yt);
       float k2 = (dx*xt+dy*yt);
       return (k2/k1); //qout
  };
  
  //THIS FUNCTION COMPUTES THE QSIDE HBT COMPONENT
  //  float getQside(float px1, float px2, float py1, float py2) {
  float hltBuilder::getQside(hlt_track *trackA, hlt_track *trackB) {
       float px1 = trackA->pt*cos(trackA->psi);
       float py1 = trackA->pt*sin(trackA->psi);
       float px2 = trackB->pt*cos(trackB->psi);
       float py2 = trackB->pt*sin(trackB->psi);
       float xt = px1+px2;
       float yt = py1+py2;
       float k1 = sqrt(xt*xt+yt*yt);
       return 2.0*(px1*py2-px2*py1)/k1; //qside
  };
  //float getQside(hlt_track *trackA, hbt_event_info hbt_object, int trackNumber) {
  float hltBuilder::getQside(hlt_track *trackA, float *trackB) {
       float px1 = trackA->pt*cos(trackA->psi);
       float py1 = trackA->pt*sin(trackA->psi);
       float px2 = trackB[0]; //hbt_object.track[trackNumber][0];
       float py2 = trackB[1]; //hbt_object.track[trackNumber][1];
       float xt = px1+px2;
       float yt = py1+py2;
       float k1 = sqrt(xt*xt+yt*yt);
       return 2.0*(px1*py2-px2*py1)/k1; //qside
  };
    
  //THIS FUNCTION COMPUTES THE QLONG HBT COMPONENT
  //  float getQlong(float pz1, float pz2, float E1, float E2) {
  float hltBuilder::getQlong(hlt_track *trackA, hlt_track *trackB) {
       float pz1 = trackA->pt*trackA->tanl;
       float pz2 = trackB->pt*trackB->tanl;
       float p1 = trackA->pt*sqrt(1.+pow(trackA->tanl,2));
       float p2 = trackB->pt*sqrt(1.+pow(trackB->tanl,2));
       float E1 = sqrt(p1*p1+0.13957018*0.13957018);
       float E2 = sqrt(p2*p2+0.13957018*0.13957018);
       float dz = pz1-pz2;
       float zz = pz1+pz2;
       float dt = E1-E2;
       float tt = E1+E2;
       float beta = zz/tt;
       float gamma = 1.0/(sqrt(1.0 - beta*beta));
       return gamma*(dz - beta*dt); //qlong
  };
  //float getQlong(hlt_track *trackA, hbt_event_info hbt_object, int trackNumber) {
  float  hltBuilder::getQlong(hlt_track *trackA, float *trackB) {
       float pz1 = trackA->pt*trackA->tanl;
       float pz2 = trackB[2]; //hbt_object.track[trackNumber][2];
       float p1 = trackA->pt*sqrt(1.+pow(trackA->tanl,2));
       float p2 = sqrt(pow(trackB[0],2)+pow(trackB[1],2)+pow(trackB[2],2));
       		      //sqrt(pow(hbt_object.track[trackNumber][0],2)
       		      //+pow(hbt_object.track[trackNumber][1],2)
		      //+pow(hbt_object.track[trackNumber][2],2));
       float E1 = sqrt(p1*p1+0.13957018*0.13957018);
       float E2 = sqrt(p2*p2+0.13957018*0.13957018);
       float dz = pz1-pz2;
       float zz = pz1+pz2;
       float dt = E1-E2;
       float tt = E1+E2;
       float beta = zz/tt;
       float gamma = 1.0/(sqrt(1.0 - beta*beta));
       return gamma*(dz - beta*dt); //qlong
  };

//HERES SOME NEW FUNCTIONS******************************
Bool_t hltBuilder::vertexEventCut(HLT_EVE *hlt_eve, eventCut_info *eventCuts) {
// printf("hello there. vertexEventCut function here\n");
  if( eventCuts->zvertexMax <= eventCuts->zvertexMin ) return kFALSE;
  if( (hlt_eve->vertexZ < eventCuts->zvertexMin) ||
      (hlt_eve->vertexZ > eventCuts->zvertexMax) ) {
      //printf("event didn't pass vertexZ cut\n");
      return kTRUE;
  }
  float vertR = sqrt(pow(hlt_eve->vertexX,2)+pow(hlt_eve->vertexY,2));
  if( eventCuts->rvertexMax <= eventCuts->rvertexMin ) return kFALSE;
  if( (vertR < eventCuts->rvertexMin) ||
      (vertR > eventCuts->rvertexMax) ) {
      //printf("event didn't pass vertexR cut\n");
      return kTRUE;
  }
// printf("hello there. vertexEventCut function here\n");

  return kFALSE;
};

Bool_t hltBuilder::multiplicityEventCut(int MULTIPLICITY, eventCut_info *eventCuts) {
  if( eventCuts->multMax <= eventCuts->multMin) return kFALSE;
  if( (MULTIPLICITY < eventCuts->multMin) ||
      (MULTIPLICITY > eventCuts->multMax) ) return kTRUE;
  return kFALSE;
};

Bool_t hltBuilder::BemcEventCut(float BemcEastEnergy, float BemcWestEnergy, eventCut_info *eventCuts) {
  if(eventCuts->BEMCeastEnergyMax < eventCuts->BEMCeastEnergyMin) return kFALSE;
  if(eventCuts->BEMCwestEnergyMax < eventCuts->BEMCwestEnergyMin) return kFALSE;
  if( (BemcEastEnergy < eventCuts->BEMCeastEnergyMin) && (BemcEastEnergy > eventCuts->BEMCeastEnergyMax) &&
      (BemcWestEnergy < eventCuts->BEMCwestEnergyMin) && (BemcWestEnergy > eventCuts->BEMCwestEnergyMax) ) return kTRUE;
  return kFALSE;
};

//testing with pointers instead of obects
Bool_t hltBuilder::trackCut(hlt_track *track, HLT_EVE *eve, trackCut_info *cut) {
  Bool_t Pass = kTRUE;
  if ( cut->nHitsMax <= cut->nHitsMin ) Pass = kTRUE;
  else if ( (track->nHits < cut->nHitsMin ) || (track->nHits > cut->nHitsMax) ) return kFALSE;
  
  if ( cut->dcaMax <= cut->dcaMin ) Pass = kTRUE;
  else if ( (getDCA(track, eve) < cut->dcaMin) || (getDCA(track, eve) > cut->dcaMax) ) return kFALSE;

  if ( cut->ptMax <= cut->ptMin ) Pass = kTRUE;
  else if ( (track->pt < cut->ptMin) || (track->pt > cut->ptMax) ) return kFALSE;
  
  if ( cut->etaMax <= cut->etaMin ) Pass = kTRUE;
  else if ( (getEta(track) < cut->etaMin) || (getEta(track) > cut->etaMax) ) return kFALSE;
  
  int species = getPID(track); //track->pt, track->dedx, track->q, track->tanl);
  float rap = getRap(track, species);
  
  if ( cut->rapMax <= cut->rapMin ) Pass = kTRUE;
  else if ( (rap < cut->rapMin) || (rap > cut->rapMax) ) return kFALSE;
  
  //NOTE:  Do PID cuts separately.
  return Pass;
};
/* Don't use this version ------ Deleted in next version
Bool_t trackCut(hlt_track track, HLT_EVE *eve, trackCut_info cut) {
  Bool_t Pass = kTRUE;
  if ( cut.nHitsMax <= cut.nHitsMin ) Pass = kTRUE;
  else if ( (track.nHits < cut.nHitsMin ) || (track.nHits > cut.nHitsMax) ) return kFALSE;
  
  if ( cut.dcaMax <= cut.dcaMin ) Pass = kTRUE;
  else if ( (getDCA(track, eve) < cut.dcaMin) || (getDCA(track, eve) > cut.dcaMax) ) return kFALSE;

  if ( cut.ptMax <= cut.ptMin ) Pass = kTRUE;
  else if ( (track.pt < cut.ptMin) || (track.pt > cut.ptMax) ) return kFALSE;
  
  if ( cut.etaMax <= cut.etaMin ) Pass = kTRUE;
  else if ( (getEta(track) < cut.etaMin) || (getEta(track) > cut.etaMax) ) return kFALSE;
  
  int species = getPID(&track); //track.pt, track.dedx, track.q, track.tanl);
  float rap = getRap(track, species);
  
  if ( cut.rapMax <= cut.rapMin ) Pass = kTRUE;
  else if ( (rap < cut.rapMin) || (rap > cut.rapMax) ) return kFALSE;
  
  //NOTE:  Do PID cuts separately.
  return Pass;
};
*/
Bool_t hltBuilder::dipAngleCut(hlt_track *trackA, hlt_track *trackB) {
  return kTRUE; //always return true unless we actually want a dip angle cut
  float pxA = trackA->pt*cos(trackA->psi);
  float pyA = trackA->pt*sin(trackA->psi);
  float pzA = trackA->pt*trackA->tanl;
  float pxB = trackB->pt*cos(trackB->psi);
  float pyB = trackB->pt*sin(trackB->psi);
  float pzB = trackB->pt*trackB->tanl;
  float pA = sqrt(pxA*pxA+pyA*pyA+pzA*pzA);
  float pB = sqrt(pxB*pxB+pyB*pyB+pzB*pzB);  
  float dipAngleMax = 0.04;
  return ((Bool_t)( acos( (sqrt(pxA*pxA+pyA*pyA)*sqrt(pxB*pxB+pyB*pyB) + pzA*pzB)
		   /fabs(pA*pB)) < dipAngleMax ));
};
Bool_t hltBuilder::dipAngleCut(hlt_track *trackA, float *trackB) {
  return kTRUE; //always return true unless we actually want a dip angle cut
  float pxA = trackA->pt*cos(trackA->psi);
  float pyA = trackA->pt*sin(trackA->psi);
  float pzA = trackA->pt*trackA->tanl;
  float pxB = trackB[0]; //trackB.pt*cos(trackB.psi);
  float pyB = trackB[1]; //trackB.pt*sin(trackB.psi);
  float pzB = trackB[2]; //trackB.pt*trackB.tanl;
  float pA = sqrt(pxA*pxA+pyA*pyA+pzA*pzA);
  float pB = sqrt(pxB*pxB+pyB*pyB+pzB*pzB);  
  float dipAngleMax = 0.04;
  return ((Bool_t)( acos( (sqrt(pxA*pxA+pyA*pyA)*sqrt(pxB*pxB+pyB*pyB) + pzA*pzB)
		   /fabs(pA*pB)) < dipAngleMax ));
};

Bool_t hltBuilder::antiTrackSplittingCut(hlt_track *trackA, hlt_track *trackB) {
  return kTRUE;
  int maxNhitsForPair = 48;
  return ( (Bool_t)( (trackA->nHits + trackB->nHits) < maxNhitsForPair) );
};
Bool_t hltBuilder::antiTrackSplittingCut(hlt_track *trackA, float *trackB) {
  return kTRUE;
  int maxNhitsForPair = 48;
  return ( (Bool_t)( (trackA->nHits + trackB[4]) < maxNhitsForPair) );
};

Bool_t hltBuilder::pairCut(hlt_track *trackA, hlt_track *trackB) {
  float ktMin = 0.15;
  float ktMax = 0.25;
  float pxA = trackA->pt*cos(trackA->psi);
  float pyA = trackA->pt*sin(trackA->psi);
  float pxB = trackB->pt*cos(trackB->psi);
  float pyB = trackB->pt*sin(trackB->psi);
  float kt = 0.5*sqrt(pow(pxA+pxB,2)+pow(pyA+pyB,2));
//  printf("hey from pairCut\n");
  return ( (Bool_t)( (kt > ktMin) && (kt < ktMax) ) );
};
Bool_t hltBuilder::pairCut(hlt_track *trackA, float *trackB) {
  float ktMin = 0.15;
  float ktMax = 0.25;
  float pxA = trackA->pt*cos(trackA->psi);
  float pyA = trackA->pt*sin(trackA->psi);
  float pxB = trackB[0]; //trackB.pt*cos(trackB.psi);
  float pyB = trackB[1]; //trackB.pt*sin(trackB.psi);
  float kt = 0.5*sqrt(pow(pxA+pxB,2)+pow(pyA+pyB,2));
  return ( (Bool_t)( (kt > ktMin) && (kt < ktMax) ) );
};

void hltBuilder::updateCurrentHbtEvent(hbt_event_info *hbt_current, int multiplicity, hlt_track *track, HLT_EVE *event, int trackindex) {
  hbt_current->mult = multiplicity;
  hbt_current->zvertex = event->vertexZ;
  hbt_current->track[trackindex][0] = track->pt*cos(track->psi);
  hbt_current->track[trackindex][1] = track->pt*sin(track->psi);
  hbt_current->track[trackindex][2] = track->pt*track->tanl;
  hbt_current->track[trackindex][3] = track->q;
  hbt_current->track[trackindex][4] = track->nHits;
  hbt_current->ntracks = trackindex;
};

void hltBuilder::updateHbtEventBuffer(int vmb,int mmb, hbt_event_info *hbt_current) {
  for(int i=Nhbtmixing-1;i>0;i--) {
    if((vmb > NvertexMixingBins) || (mmb > NmultMixingBins)) {
      printf("mbins out of range, mmb = %d,     vmb = %d\n",mmb,vmb);
    } else {
      hbt_buffer[i][vmb][mmb].mult = hbt_buffer[i-1][vmb][mmb].mult;
      hbt_buffer[i][vmb][mmb].zvertex = hbt_buffer[i-1][vmb][mmb].zvertex;
      hbt_buffer[i][vmb][mmb].ntracks = hbt_buffer[i-1][vmb][mmb].ntracks;
      for(int j=0;j<hbt_buffer[i][vmb][mmb].ntracks;j++) {
        for(int k=0;k<5;k++) {
          hbt_buffer[i][vmb][mmb].track[j][k] = hbt_buffer[i-1][vmb][mmb].track[j][k];
        }
      }
      for(int j=hbt_buffer[i][vmb][mmb].ntracks;j<10000;j++) {
        for(int k=0;k<5;k++) {
          hbt_buffer[i][vmb][mmb].track[j][k] = 0.;  //hbt_buffer[i-1][vmb][mmb].track[j][k];
        }
      }
    }
  }//end of loop over hbt_buffer[Nhbtmixing-1] down to hbt_buffer[1]
  //Now assign hbt_buffer[0] to be equal to the current buffer for the next event.
  hbt_buffer[0][vmb][mmb].mult = hbt_current->mult;  
  hbt_buffer[0][vmb][mmb].zvertex = hbt_current->zvertex;
  hbt_buffer[0][vmb][mmb].ntracks = hbt_current->ntracks;
  for(int j=0;j<hbt_current->ntracks;j++) {
    for(int k=0;k<5;k++) {
      hbt_buffer[0][vmb][mmb].track[j][k] = hbt_current->track[j][k];
    }
  }
  for(int j=hbt_buffer[0][vmb][mmb].ntracks;j<10000;j++) {
    for(int k=0;k<5;k++) {
      hbt_buffer[0][vmb][mmb].track[j][k] = 0.;
    }
  }
};
/* Don't use these ------ Delete these in next version
float getRap(hlt_track track, int species) {
  float m = 0.;
  if(species == 0) m = 0.000511; //mass electron 0.511 kev
  else if((species == 1) || (species == 2)) m = 0.13957018;		//mass pion in gev/c^2
  else if((species == 3) || (species == 4)) m = 0.493677;		//mass kaon in gev/c^2
  else if((species == 5) || (species == 6)) m = 0.93827203;		//mass proton in gev/c^2
  else if((species == 7)) m = 2*0.93827203;				//mass deuteron -- get more accurate value from PDG book
  else return 999999999; //nine nines.
  float p = track.pt*sqrt(1.+pow(track.tanl,2));
  float E = sqrt(p*p+m*m);
  float pz = track.pt*track.tanl;
  return (-0.5*log((E+pz)/(E-pz)));
};

float getEta(hlt_track track) {
  float p = fabs(track.pt*sqrt(1.+pow(track.tanl,2)));
  float pz = track.pt*track.tanl;
  return (0.5*log((p+pz)/(p-pz)));
};

float getDCA(hlt_track track, HLT_EVE *eve) {
  float dca = sqrt(pow(eve->vertexZ - track.z0,2) 
  	         + pow(eve->vertexX - track.r0*cos(track.phi0),2)
		 + pow(eve->vertexY - track.r0*sin(track.phi0),2));
  return dca;
};
*/
//Testing these with pointers instead of objects
float hltBuilder::getRap(hlt_track *track, int species) {
  float m = 0.;
  if(species == 0) m = 0.000511; //mass electron 0.511 kev
  else if((species == 1) || (species == 2)) m = 0.13957018;		//mass pion in gev/c^2
  else if((species == 3) || (species == 4)) m = 0.493677;		//mass kaon in gev/c^2
  else if((species == 5) || (species == 6)) m = 0.93827203;		//mass proton in gev/c^2
  else if((species == 7)) m = 2*0.93827203;				//mass deuteron -- get more accurate value from PDG book
  else return 999999999; //nine nines.
  float p = track->pt*sqrt(1.+pow(track->tanl,2));
  float E = sqrt(p*p+m*m);
  float pz = track->pt*track->tanl;
  return (-0.5*log((E+pz)/(E-pz)));
};

float hltBuilder::getEta(hlt_track *track) {
  float p = fabs(track->pt*sqrt(1.+pow(track->tanl,2)));
  float pz = track->pt*track->tanl;
  return (0.5*log((p+pz)/(p-pz)));
};

float hltBuilder::getDCA(hlt_track *track, HLT_EVE *eve) {
  float dca = sqrt(pow(eve->vertexZ - track->z0,2) 
  	         + pow(eve->vertexX - track->r0*cos(track->phi0),2)
		 + pow(eve->vertexY - track->r0*sin(track->phi0),2));
  return dca;
};

//    mPion = 0.13957018;  	//pion mass in gev/c^2
//    mKaon = 0.493677;    	//kaon mass in gev/c^2
//    mProton = 0.93827203;	//proton mass in gev/c^2
    
void hltBuilder::setUpdateSwitch(Bool_t a) { UPDATE_SWITCH = a; };
Bool_t hltBuilder::getUpdateSwitch() { return UPDATE_SWITCH; };

int hltBuilder::sameSignCheck(hlt_track *trkA, hlt_track *trkB) {
  if(  (trkA->q  == 1) && (trkB->q == 1) ) return 1; //both pos. charges
  if(  (trkA->q == -1) && (trkB->q == -1) ) return -1; //both neg. charges
  return 0; //different charges
};
int hltBuilder::sameSignCheck(hlt_track *trkA, float *trkB) {
  if(  (trkA->q  == 1) && (trkB[3] == 1) ) return 1; //both pos. charges
  if(  (trkA->q == -1) && (trkB[3] == -1) ) return -1; //both neg. charges
  return 0; //different charges
};

Bool_t hltBuilder::fullBuffer(int vb, int mb) {
  //for given vertex bin vb and multiplicity bin mb check that the 
  //last event out of Nhbtmixing events has an event with non-zero multiplicity
  //printf("Nhbtmixing = %d\n",Nhbtmixing);
  if( (vb < NvertexMixingBins) && (mb < NmultMixingBins) ) { //printf("vb = %d, mb = %d\n",vb,mb);
    //***printf("hbt_buffer[Nhbtmixing-1][vb][mb].mult = %d\n",hbt_buffer[Nhbtmixing-1][vb][mb].mult);
    //printf("hey from fullBuffer\n");
    return ((Bool_t)( (hbt_buffer[Nhbtmixing-1][vb][mb].mult > 0)  ));
  } else {
;//    printf("vb = %d, mb = %d\n",vb,mb);
  }
  return kFALSE;
};

int hltBuilder::getMultMixingBin(int mult) {
  for(int mc=0;mc < NmultMixingBins;mc++) {
    if( (mult > (multRange/NmultMixingBins*mc)) &&
        (mult <= (multRange/NmultMixingBins*(mc+1))) ) return mc;
  }
  return 999999999; //out of range
};
int hltBuilder::getVertexMixingBin(float vertZ) {
  float vertZmin = -75.; //cm
  //printf("vertZ = %f\n",vertZ);
  for(int vc=0;vc < NvertexMixingBins; vc++) {
    //printf("vertZ > %f && vertZ <= %f\n",(vertZmin+10*vertRange/NvertexMixingBins*vc),(vertZmin+10*vertRange/NvertexMixingBins*(vc+1)));
    if( (vertZ > (vertZmin+vertRange/NvertexMixingBins*vc)) &&
        (vertZ <=(vertZmin+vertRange/NvertexMixingBins*(vc+1))) ) return vc; 
  }
  return 999999999; //out of range
};

void hltBuilder::computeYieldsHistogram() {
  float piplusyield = piplus->Integral(1,20);
  float piminusyield = piminus->Integral(1,20);
  float kplusyield = kplus->Integral(1,20);
  float kminusyield = kminus->Integral(1,20);
  float pplusyield = pplus->Integral(1,20);
  float pminusyield = pminus->Integral(1,20);
  Yield->Reset();
  if( piplusyield > 0)  Yield->SetBinContent(1,piminusyield/piplusyield);
  if( kplusyield > 0)  Yield->SetBinContent(2,kminusyield/kplusyield);
  if( pplusyield > 0)  Yield->SetBinContent(3,pminusyield/pplusyield);
  if( piminusyield > 0)  Yield->SetBinContent(4,kminusyield/piminusyield);
  if( piplusyield > 0)  Yield->SetBinContent(5,kplusyield/piplusyield);
  if( piminusyield > 0)  Yield->SetBinContent(6,pminusyield/piminusyield);
  if( piplusyield > 0)  Yield->SetBinContent(7,pplusyield/piplusyield);
  float tot_piplusyield = tot_piplus->Integral(1,20);
  float tot_piminusyield = tot_piminus->Integral(1,20);
  float tot_kplusyield = tot_kplus->Integral(1,20);
  float tot_kminusyield = tot_kminus->Integral(1,20);
  float tot_pplusyield = tot_pplus->Integral(1,20);
  float tot_pminusyield = tot_pminus->Integral(1,20);
  tot_Yield->Reset();
  if( tot_piplusyield > 0)  tot_Yield->SetBinContent(1,tot_piminusyield/tot_piplusyield);
  if( tot_kplusyield > 0)  tot_Yield->SetBinContent(2,tot_kminusyield/tot_kplusyield);
  if( tot_pplusyield > 0)  tot_Yield->SetBinContent(3,tot_pminusyield/tot_pplusyield);
  if( tot_piminusyield > 0)  tot_Yield->SetBinContent(4,tot_kminusyield/tot_piminusyield);
  if( tot_piplusyield > 0)  tot_Yield->SetBinContent(5,tot_kplusyield/tot_piplusyield);
  if( tot_piminusyield > 0)  tot_Yield->SetBinContent(6,tot_pminusyield/tot_piminusyield);
  if( tot_piplusyield > 0)  tot_Yield->SetBinContent(7,tot_pplusyield/tot_piplusyield);
};

void hltBuilder::computeHbtCorrelationFunction() {
  hbtCF_qinv->Reset();
  hbtCF_qinv->Add(hbtnum);
  float normalization = 1;
  if(hbtnum->Integral(15,20) > 0) {
    normalization = hbtden->Integral(15,20)/hbtnum->Integral(15,20);
  }
  hbtCF_qinv->Divide(hbtden);
  hbtCF_qinv->Scale(normalization);

  tot_hbtCF_qinv->Reset();
  tot_hbtCF_qinv->Add(hbtnum);
  float tot_normalization = 1;
  if(tot_hbtnum->Integral(15,20) > 0) {
    tot_normalization = tot_hbtden->Integral(15,20)/tot_hbtnum->Integral(15,20);
  }
  tot_hbtCF_qinv->Divide(hbtden);
  tot_hbtCF_qinv->Scale(tot_normalization);
};

void hltBuilder::computeV2Corrected() {
  float res = resolution->GetBinContent(1);
  corrected_v2_pt->Scale(1./2./sqrt(res));   //(1./2./sqrt(res));
  float tot_res = tot_resolution->GetBinContent(1);
  tot_corrected_v2_pt->Scale(1./2./sqrt(tot_res));
};

void hltBuilder::main(int argc, char *argv[])
{
  hltBuilder me;
  
  me.Main(argc, argv);
};


 
