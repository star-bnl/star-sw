////////////////////////////////////////////////////////////
///// Liang Xue Run10 HLT(di-electron,Heavy Fragment, //////
/////       High Pt Triggered Evevt) online OA        //////
/////            using Jeff's new FramWork            //////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
/////                                                ///////  
/////        Beam Energy Scan Online plots           ///////  
/////                                                ///////
////////////////////////////////////////////////////////////
#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_L3/daq_l3.h>
#include <TStyle.h>
#include "TVector3.h"
#include <fstream>
#include <iostream>
#include <iomanip>
#include <TH1I.h>
#include <TH1D.h>
#include <TH2F.h>
#include <TH3F.h>
#include <TFile.h> 
#include <TProfile.h>
#include <math.h>
#include <DAQ_HLT/daq_hlt.h>
#include "RTS/include/HLT/HLTFormats.h"
//#include <time.h>
//#include <sys/time.h>

class hltBuilder : public JevpBuilder {
 public:
    
  ///////////// struct ////////////
  struct hbt_event_info {
    int mult;
    float zvertex;
    int ntracks; 
    float track[10000][5];
  };      
        
  struct eventCut_info {
    float zvertexMax;   float zvertexMin;
    float rvertexMax;  	float rvertexMin;
    float BEMCeastEnergyMax;	float BEMCeastEnergyMin;
    float BEMCwestEnergyMax;	float BEMCwestEnergyMin;
    float BEMC_EastWestDiffMax;	float BEMC_EastWestDiffMin;
    float multMax;      float multMin;
  };      

  struct trackCut_info {
    int nHitsMin;       int nHitsMax;
    float dcaMin;       float dcaMax;
    float ptMin;        float ptMax;
    float etaMin;       float etaMax;
    float rapMin;       float rapMax;
  };

  //Timing variables to reset periodic plots
  int last_time;
  float nHours;
  //timeval PeriodicStart; 
  //timeval PeriodicResetTest; 
  //timeval PeriodicResult;

  ////////// plots /////////
  JevpPlot *HltPlots[156]; //was 144 //was 132 //was 110 //was 54
        
  ////// histogram ////////
  //These are for the Vertex Method of counting good events
  TH1F* cVz1;  TH1F* cVz2;  TH1F* cVz3;  TH1F* cVz4;  //plot1-plot4
  TH1F* cVx1;  TH1F* cVx2;  TH1F* cVx3;		   //plot5-plot8
  TH1F* cVy1;  TH1F* cVy2;  TH1F* cVy3;		   //plot9-plot12
  TH1F* cVr1;  TH1F* cVr2;  TH1F* cVr3;  TH1F* cVr4;  //plot13-plot16
  TH2F* tot_Vxy1;  TH2F* tot_Vxy2;  TH2F* tot_Vxy3;  TH2F* tot_Vxy4;  //plot17-plot20
  TH1F* cM1;  TH1F* cM2;  TH1F* cM3;  TH1F* cM4;  //plot21-plot24
    
  //These are for the BEMC related backup method of counting good events  TH1F* Vz1;  TH1F* Vz2;  TH1F* Vz3;  TH1F* Vz4;  //plot1-plot4
  TH1F* Vz1;  TH1F* Vz2;  TH1F* Vz3;  TH1F* Vz4;  //plot1-plot4
  TH1F* Vx1;  TH1F* Vx2;  TH1F* Vx3;  TH1F* Vx4;  //plot5-plot8
  TH1F* Vy1;  TH1F* Vy2;  TH1F* Vy3;  TH1F* Vy4;  //plot9-plot12
  TH1F* Vr1;  TH1F* Vr2;  TH1F* Vr3;  TH1F* Vr4;  //plot13-plot16
  TH2F* Vxy1;  TH2F* Vxy2;  TH2F* Vxy3;  TH2F* Vxy4;  //plot17-plot20
  TH1F* Mult1;  TH1F* Mult2;  TH1F* Mult3;  TH1F* Mult4;  //plot21-plot24
  TH1F* Eta1;  TH1F* Eta2;  TH1F* Eta3;  TH1F* Eta4;    //plot26-plot28
  TH2F* Timevsmultiplicity;  //plot29
  TH2F* Ratevsmultiplicity;  //plot30
      
  TProfile* v2_pt;
  TH1F* v2ptCounter;
  //plot31
  TProfile* resolution;
  //plot32
  TProfile* corrected_v2_pt;
  TH1F* corrected_v2ptCounter; 
  //plot33
  TH2F* dedx;
  //plot34
  TH1F* piplus;
  TH1F* piminus;
  TH1F* kplus;
  TH1F* kminus;
  TH1F* pplus;
  TH1F* pminus;
  //plot35
  TH1F* Yield;
  TH1F* corrected_Yield;
  //plot36
  TH1F* hbtnum;
  TH1F* hbtden;
  //plot37
  TH1F* hbtCF_qinv;
  //plot38
  TH2F* BEMC_1a;  TH2F* BEMC_2a;  TH2F* BEMC_3a;  TH2F* BEMC_4a;
  TH2F* BEMC_1b;  TH2F* BEMC_2b;  TH2F* BEMC_3b;  TH2F* BEMC_4b;
  TH2F* BEMC_1c;  TH2F* BEMC_2c;  TH2F* BEMC_3c;  TH2F* BEMC_4c;
  TH2F* BEMC_1d;  TH2F* BEMC_2d;  TH2F* BEMC_3d;  TH2F* BEMC_4d;
  //plot39 - plot54

//****************** New cumulative histos for Vertex Method of counting
  TH1F* tot_cVz1;  TH1F* tot_cVz2;  TH1F* tot_cVz3;  TH1F* tot_cVz4;  //plot1-plot4
  TH1F* tot_cVx1;  TH1F* tot_cVx2;  TH1F* tot_cVx3;		   //plot5-plot8
  TH1F* tot_cVy1;  TH1F* tot_cVy2;  TH1F* tot_cVy3;		   //plot9-plot12
  TH1F* tot_cVr1;  TH1F* tot_cVr2;  TH1F* tot_cVr3;  TH1F* tot_cVr4;  //plot13-plot16
  //TH2F* tot_Vxy1;  TH2F* tot_Vxy2;  TH2F* tot_Vxy3;  TH2F* tot_Vxy4;  //plot17-plot20
  TH1F* tot_cM1;  TH1F* tot_cM2;  TH1F* tot_cM3;  TH1F* tot_cM4;  //plot21-plot24
//**************** End new cumulative histos for Vertex Method of counting
  
//****************** New cumulative analysis histos
  TH2F* tot_Timevsmultiplicity;  //plot29
  TH2F* tot_Ratevsmultiplicity;  //plot30
      
  TProfile* tot_v2_pt;
  TH1F* tot_v2ptCounter;
  //plot31
  TProfile* tot_resolution;
  //plot32
  TProfile* tot_corrected_v2_pt;
  TH1F* tot_corrected_v2ptCounter; 
  //plot33
  TH2F* tot_dedx;
  //plot34
  TH1F* tot_piplus;
  TH1F* tot_piminus;
  TH1F* tot_kplus;
  TH1F* tot_kminus;
  TH1F* tot_pplus;
  TH1F* tot_pminus;
  //plot35
  TH1F* tot_Yield;
  TH1F* tot_corrected_Yield;
  //plot36
  TH1F* tot_hbtnum;
  TH1F* tot_hbtden;
  //plot37
  TH1F* tot_hbtCF_qinv;
  //plot38
//****************** End new cumulative analysis histos

//****************** New Periodic histos for Vertex Method of counting - added Apr 27, 2010
  TH1F* periodic_cVz1;  TH1F* periodic_cVz2;  TH1F* periodic_cVz3;  TH1F* periodic_cVz4;  //plot1-plot4
  TH1F* periodic_cVx1;  TH1F* periodic_cVx2;  TH1F* periodic_cVx3;		   //plot5-plot8
  TH1F* periodic_cVy1;  TH1F* periodic_cVy2;  TH1F* periodic_cVy3;		   //plot9-plot12
  TH1F* periodic_cVr1;  TH1F* periodic_cVr2;  TH1F* periodic_cVr3;  TH1F* periodic_cVr4;  //plot13-plot16
  TH2F* periodic_cVxy1;  TH2F* periodic_cVxy2;  TH2F* periodic_cVxy3;  TH2F* periodic_cVxy4;  //plot17-plot20
  TH1F* periodic_cM1;  TH1F* periodic_cM2;  TH1F* periodic_cM3;  TH1F* periodic_cM4;  //plot21-plot24
//**************** End new Periodic histos for Vertex Method of counting

//******************* Set D - Vz histos, Vpd Triggered events single run, periodic, and accumulating for Vertex Method of counting
  TH1F* setD_singlerun_cVz1;  TH1F* setD_singlerun_cVz2;  TH1F* setD_singlerun_cVz3;  TH1F* setD_singlerun_cVz4;
  TH1F* setD_periodic_cVz1;  TH1F* setD_periodic_cVz2;  TH1F* setD_periodic_cVz3;  TH1F* setD_periodic_cVz4;
  TH1F* setD_accumulated_cVz1;  TH1F* setD_accumulated_cVz2;  TH1F* setD_accumulated_cVz3;  TH1F* setD_accumulated_cVz4;
  Bool_t FILL_VPD_HISTOS;
//******************* End Set D - Vz histos, Vpd Triggered events

//******************* Set E - Vz histos, Vpd Triggered events single run, periodic, and accumulating for Vertex Method of counting
  TH1F* setE_singlerun_cVz1;  TH1F* setE_singlerun_cVz2;  TH1F* setE_singlerun_cVz3;  TH1F* setE_singlerun_cVz4;
  TH1F* setE_periodic_cVz1;  TH1F* setE_periodic_cVz2;  TH1F* setE_periodic_cVz3;  TH1F* setE_periodic_cVz4;
  TH1F* setE_accumulated_cVz1;  TH1F* setE_accumulated_cVz2;  TH1F* setE_accumulated_cVz3;  TH1F* setE_accumulated_cVz4;
//******************* End Set E - Vz histos, Vpd Triggered events

  //////////////////////
  Bool_t NO_CUTS_VERTEX;
  Bool_t VzLT200_VxLT2_VyLT2_VERTEX;
  Bool_t VxLT2_VyLT2_VERTEX;
  Bool_t VzLT70_VxLT2_VyLT2_VERTEX;
  Bool_t VzLT70_VxGT2_VyGT2_VERTEX;
  Bool_t VrLT2_VERTEX;
  Bool_t VzLT70_VERTEX;
  Bool_t PRIM_TRACK_GT5_MULT;  Bool_t NO_CUTS_BEMC;
  Bool_t eGT20_wGT20_BEMC;
  Bool_t eGT20_wGT20_diffLT6_BEMC;
  Bool_t eGT1_wGT1_BEMC;
  Bool_t eGT20_wGT20_diffLT6_bbcGT100_BEMC;

  double mPion;
  double mKaon;
  double mProton;
  int Nhbtmixing;
  int NmultMixingBins;
  int NvertexMixingBins;
  int vmb; int mmb; int UPDATE_SWITCH; double multRange; double vertRange;
  hbt_event_info hbt_buffer[1][3][3]; // initialize Nhbtmixing to the size of the first array index, NvertexMixingBins to second index, NmultMixingBins to third index
  hbt_event_info hbt_current;

  //create cuts here (set them in initialize function.)   
  eventCut_info eventCuts;
  trackCut_info multTrackCuts;
  trackCut_info v2ptTrackCuts;
  trackCut_info ptSpectraTrackCuts;
  trackCut_info hbtTrackCuts;
  //  Bool_t  vertexEventCut(HLT_EVE *hlt_eve, eventCut_info eventCuts);
  //  Bool_t  multiplicityEventCut( int MULTIPLICITY, eventCut_info eventCuts);
  //  Bool_t  trackCut(hlt_track track, trackCut_info cut);
  int eventCounter;
  Bool_t HBTCALC;
  Bool_t V2CALC;

  //const char* plotsetname; //*********************************Chris Added this to get it to compile.
  ////////////////////// Function //////////////////// 

  hltBuilder(JevpServer *parent=NULL) : JevpBuilder(parent)
    { plotsetname = (char *)"hlt"; }
 
  void initialize(int argc, char *argv[]);
   
  void startrun(daqReader *rdr);

  void stoprun(daqReader *rdr); 

  void event(daqReader *rdr);

  int selectEvent(daqReader *rdr);

  int selectRun(daqReader *rdr); 

  int getPID(hlt_track *track);

  float getQinv(hlt_track *trackA, hlt_track *trackB);

  float getQinv(hlt_track *trackA, float *trackB);

  float getQout(hlt_track *trackA, hlt_track *trackB);

  float getQout(hlt_track *trackA, float *trackB); 

  float getQside(hlt_track *trackA, hlt_track *trackB);

  float getQside(hlt_track *trackA, float *trackB);

  float getQlong(hlt_track *trackA, hlt_track *trackB);

  float  getQlong(hlt_track *trackA, float *trackB);

  Bool_t vertexEventCut(HLT_EVE *hlt_eve, eventCut_info *eventCuts);

  Bool_t multiplicityEventCut(int MULTIPLICITY, eventCut_info *eventCuts);
  
  Bool_t BemcEventCut(float BemcEastEnergy, float BemcWestEnergy, eventCut_info *eventCuts);

  Bool_t trackCut(hlt_track *track, HLT_EVE *eve, trackCut_info *cut);

  Bool_t dipAngleCut(hlt_track *trackA, hlt_track *trackB) ;

  Bool_t dipAngleCut(hlt_track *trackA, float *trackB) ;

  Bool_t antiTrackSplittingCut(hlt_track *trackA, hlt_track *trackB);

  Bool_t antiTrackSplittingCut(hlt_track *trackA, float *trackB);

  Bool_t pairCut(hlt_track *trackA, hlt_track *trackB);

  Bool_t pairCut(hlt_track *trackA, float *trackB);

  void updateCurrentHbtEvent(hbt_event_info *hbt_current, int multiplicity, hlt_track *track, HLT_EVE *event, int trackindex);

  void updateHbtEventBuffer(int vmb,int mmb, hbt_event_info *hbt_current) ;

  float getRap(hlt_track *track, int species);

  float getEta(hlt_track *track) ;

  float getDCA(hlt_track *track, HLT_EVE *eve);

  void setUpdateSwitch(Bool_t a);

  Bool_t getUpdateSwitch();

  int sameSignCheck(hlt_track *trkA, hlt_track *trkB);

  int sameSignCheck(hlt_track *trkA, float *trkB) ;

  Bool_t fullBuffer(int vb, int mb);

  int getMultMixingBin(int mult);

  int getVertexMixingBin(float vertZ);

  void computeYieldsHistogram();

  void computeHbtCorrelationFunction();
  
  void computeV2Corrected();

  static void main(int argc, char *argv[]);

  ClassDef(hltBuilder, 1);
};
//////////////////////////////////////////////////







