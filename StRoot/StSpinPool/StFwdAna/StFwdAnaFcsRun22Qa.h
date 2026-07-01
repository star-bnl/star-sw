/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  To generate a ROOT file of histograms related to the FCS and event data in the produced MuDsts from RHIC Run 22 that can be used to do quality assurance (QA) on the data.

  DESCRIPTION
  This class inherits from StMaker and contains many histograms to be used for Quality Assurance (QA) of MuDst files that contain Forward Calorimeter System (FCS) data. It uses #LoadHists() together with the functions #AddH1F(), #AddH2F(), #AddH1FArr(), #AddH2FArr() to ease histogram creation and management. These functions should be used exclusively in this and inherited classes to automate histogram management. #FillEventInfo() is used (and can be re-implemented) to fill QA histograms related to event information. #FillFcsInfo() is used (and can be re-implemented) to fill QA histograms related to FCS information. The idea is that inherited classes don't need to implement the #Init() function only the #LoadHists() function. Also can be used to do some EPD Qa with the #FillEpdInfo() function.

  LOG
  @[May 24, 2024 .. June 4, 2024] > Implemented basic variables needed to read data from MuDst. QA histograms of event information: number of events, spin, vertex, trigger, and bunch crossing. Fcs hit information: ADC vs. TB, Energy, Multiplicity, NPeaks, Peak location, Fit Chi^2/NDF, Total Energy. EPD DEP Adc and time peak location to QT adc and time peak location. Fcs Cluster information: Multiplicity (Tower, Neighbor, Points), Energy, Location, SigmaMax and SigmaMin, theta,Chi^2/NDF for 1 and 2 Photon fits. Cluster Pi0 reconstruction with highest energy clusters: Invariant Mass, Angle, Energy Sum, dgg, zgg, High vs. Low Energy. Point information: Multiplicity, Energy, Location. Point Pi0 reconstructions with highest energy points: Invariant Mass, Angle, Energy Sum, dgg, zgg, High vs. Low Energy. Implemented functions #AddH1F(), #AddH2F(), #AddH1FArr(), #AddH2FArr() to ease histogram creation and management. #LoadHists() can be modified by inherited classes to create separate QA histograms. Internal #mAllHists will own and handle all the created histograms automatically when functions #AddH1F(), #AddH2F(), #AddH1FArr(), #AddH2FArr() are used. #FillEventInfo() is used to fill the event information histograms. #FillFcsInfo() fills the others. Also implemented #mSpinRndm to generate random spin patterns for testing, as long #mSpinDbMkr equals 0 it will generate random spins.

  @[June 7, 2024] > Modified the ranges of some histograms. Change #mH1F_Epd_NHits to store total epd hits, and added #mH1F_Epd_NHitsWest to hold information for just the EPD west hits. Added #SetOwner() which will set #mAllHists as owner of all the histograms also changed how histograms are created so that #TObjArray::SetOwner() is only called once as it should be since one call loops over the entire array. Added #mFileOutput for saving ROOT files and is created in #Init() before the histograms to comply with ROOT framework philosophy. In #Make() get the hit, cluster, and point arrays outside the idet loop since each idet call needs the same hit/cluster/point #TClonesArray. Fixed how to loop over hits, clusters, and points since the number of hits/clusters/points doesn't take into the account the first index so need to add the number of hits/clusters/points to the first index to get the correct maximum index of the loop. Added drawing functions for the histograms.

  @[June 25, 2024] > Added booleans #mFcsAdcTbOn, #mEpdAdcQaOn, #mEpdTacQaOn to control turning on and off the histograms for the Fcs ADC vs. TB histograms, the FCS Dep sums vs. Adc from Epd Qa, and the Fcs peak location vs. EPD TAC values respectively. These histograms are all object arrays and will take huge amounts of space and are not essential usually to assess data quality. Added #mH1F_BbcTimeDiff histogram to look at BBC time difference which is used for vertex. Added #mH1F_VertexZdc for checking z vertex from ZDC. Added #mH2F_Mult_tofVecal to check Fcs multiflicity against TOF multiplicity since the reference multiplicity was missing from data. Modified #DrawEventInfo() to plot the new histograms.

  @[July 10, 2024] > Checking if able to retrieve StEpdHits from Trig data. It seems StMuEvent does have trigger data but StEvent does not. The trigger data from MuDsts can be used to fill the EPD hits, which I discovered after running code similar to that in EPD hit maker. modified StEpdHitMaker to read trigger data from MuDsts. Implmented code that will either grab StMuEpdHitCollection from MuDst or from StEpdHitMaker whichever is available, respective of that order. Changed and added some more plot options.

  @[July 15, 2024] > Checking if MuDst contains a #TClonesArray of EPD hits always returns a non-zero value. This means to check if EPD data is in MuDsts it is better to check the size of the #TClonesArray, which is what the code does now.

  @[July 30, 2024] > Added more EPD QA histograms as well EPD vertex histogram based on the averaged TAC value. This is done in #FillEpdInfo() and modified #Make() to call this function if any of the EPD QA flags are on. As a result removed some of those EPD related histograms from #FillFcsInfo(). Also Added #mEpdTacAdcOn which is a flag for turning on the EPD hits TAC vs. ADC nMip for each channel. Also note that nmip returns the ADC normalized to the mip ADC not the ADC value where the MIP is. Modified and added plotting options as needed. Added histograms and code include checking early and average East and West EPD TAC values with and without an nMip cut.

  @[July 31, 2024] > Added and modified the functions related to drawing the EPD QA histograms. In particular, separated out the histograms into different categories with their own draw functions.
  @[August, 13, 2024] > Changed tacdiff histograms from 1d to 2d. They now store the tac difference taken from doing averages vs. the tac differences taken from the earliest (i.e. highest) TAC values and added a tac cut of 50 to the average histograms.

  @[August 14, 2024] > Renamed class to StMuFcsRun22QaMaker

  @[August 20, 2024] > Got rid of EPD related histgrams and draw functions. Uses new #HistManager class to manage histograms.
  
  @[September 5, 2024] > Made default vertex -999.

  @[September 6, 2024] > Made changes to add all vertex information to a TGraph but resulted in plots that were hard to read. After finding out only triggers with ID greater than 890000 are production triggers; modfied #mH1F_Triggers to be large enough to hold all these triggers. Each new production trigger increases by 40 from the original number.

  @[September 9, 2024] > Clean up extraneous code and added #getFileName() for #mHists

  @[September 16, 2024] > Added #StFcsRun22TriggerMap to manage looking up Fcs triggers and for generating histogram with named bin labels. Also fills trigger histogram by name not Id. There were 64 FCS triggers in Run 22 and I added a 65th bin called "NF" which is for trigger Ids that are not one of those 64. Also added #DrawTrigger() which just draws the trigger histogram since with the bin names the font is too small in the "Event" histogram

  @[November 22, 2024] > Added spin related QA histograms #mH1F_spin4Vbx7 and #mH1F_spin4Vbx48 which is not "filled" but populated with the spin4 bit vs. bx7 and bx48 ids respectively. Implemented code in #InitRun() to print information from spin database. Code now uses #StSpinDbMaker to properly read the spin database to get 4 bit spin value (spin4). Wrote #DrawBx7Bx48Ana() that draws the bx7Vbx48 histogram and its projections as well as the difference (Mostly copied from another macro HistoAna.cc). Wrote #DrawSpinInfo() to the draw the new spin histograms. Got rid of storing the spin pattern since that is not needed. Made comments more ROOT friendly

  @[January 8, 2025] > Added static function #MakeGraph() which can be used to add many graphs to a #TObjArray. This way I can easily manage alot of QA graphs across multiple #StMakers. Added many QA graphs and methods for processing and filling them. Fixed how histograms are loaded so it can work with multiple files. Addressed memory leak issues by properly deleting things now.

  @[January 9, 2024] > Added #PrintSpinBits() to help with printing the spin4 bits to cross check with the spin database.

  @[January 21, 2025] > Added more graphs for the run by run qa. These are related to the cluster and point energy, and multiplicity. Also, added #GraphAverage() that is used to check if the graphs for the various triggers are zero or not. This is used to avoid plotting triggers that had 0 for all runs. Added a histogram and a graph to check hit multiplicity with some cuts applied.

  @[February 1, 2025] > Small fix for including StEnumerations.h

  @[July 31, 2025] > Added #mBestMassOn which will turn on and off the histograms and code for finding the highest energy cluster or point and filling the relevant histograms. This can be used to speed up the QA code if you don't care about the best invariant mass in your QA and slightly reduces the memory impact

  @[January 21, 2026] > Copied from #StMuFcsRun22QaMaker and renamed and modified to work in "StMuFcsAna" structure
  @[May 29, 2026] > Commented out printing for when I needed to test why the MuDsts did not contain the point-cluster associations
  @[June 3, 2026] > Commented out some extraneous code from the MuDst Q&A testing
  @[July 1, 2026] > Changed name from StMuFcsAnaRun22Qa to StFwdAnaFcsRun22Qa
  
  Do DEP calib of EPD chs, bunch xing analysis for spin. Change some plots so they use logz and move/remove the stats box for some of hte 2d histograms when plotting. Show on the fly EPD MIP peak locations and valleys
 */

#ifndef STFWDANA_STFWDANAFCSRUN22QA_HH
#define STFWDANA_STFWDANAFCSRUN22QA_HH

//C/C++ Headers
#include <iostream>

//ROOT Headers
// #include "TRandom3.h"
// #include "TCanvas.h"
// #include "TObjArray.h"
// #include "TString.h"
// #include "TFile.h"
// #include "TTree.h"
// #include "TH1F.h"
// #include "TH2F.h"
// #include "TGraphErrors.h"

//STAR Headers
// #include "StEnumerations.h"
// #include "StMaker.h"
// #include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
// #include "StMuDSTMaker/COMMON/StMuDstMaker.h"
// #include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
// #include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
// #include "StEvent/StTriggerData.h"
// #include "StEvent/StTriggerId.h"
// #include "StMessMgr.h"
// #include "StMuDSTMaker/COMMON/StMuEvent.h"
// #include "StMuDSTMaker/COMMON/StMuTypes.hh"
// #include "Stypes.h"
// #include "StFcsDbMaker/StFcsDbMaker.h"
// #include "StFcsDbMaker/StFcsDb.h"
// #include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
// #include "StMuDSTMaker/COMMON/StMuFcsHit.h"
// #include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
// #include "StMuDSTMaker/COMMON/StMuFcsPoint.h"
// #include "StEpdDbMaker/StEpdDbMaker.h"
// #include "StEpdHitMaker/StEpdHitMaker.h"

//Custom headers in this folder
// #include "HistManager.h"
// #include "StFcsRun22TriggerMap.h"
#include "StFwdAnaVirtual.h"

class StFwdAnaFcsRun22Qa : public StFwdAnaVirtual
{
 public:
  StFwdAnaFcsRun22Qa();
  ~StFwdAnaFcsRun22Qa();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata);
  virtual Int_t DoMake(StFwdAnaData* anadata);
  
  void setFcsAdcTbOn(bool value=true)  { mFcsAdcTbOn = value; }
  void setEpdAdcQaOn(bool value=true)  { mEpdAdcQaOn = value; }
  void setEpdTacQaOn(bool value=true)  { mEpdTacQaOn = value; }
  void setBestMassOn(bool value=true)  { mBestMassOn = value; }

  //virtual void Paint(Option_t opt="");
  
  //void DrawEventInfo(TCanvas* canv, const char* savename);

  void DrawTrigger(TCanvas* canv, const char* savename);
  //void DrawVertex(TCanvas* canv, const char* savename);
  void DrawBxId(TCanvas* canv, const char* savename);
  void DrawBx7Bx48Ana(TCanvas* canv, const char* savename);
  void DrawSpinInfo(TCanvas* canv, const char* savename);
  void DrawFcsHitSingle(TCanvas* canv, unsigned int det, const char* savename);
  void DrawFcsTotalE(TCanvas* canv, const char* savename);
  void DrawFcsClusterSingle(TCanvas* canv, unsigned int det, const char* savename);
  void DrawFcsPointSingle(TCanvas* canv, unsigned int det, const char* savename);
  
  void DrawAdcVTb(TCanvas* canv, const char* savename);
  void DrawFcsHitQa(TCanvas* canv, const char* savename);

  void DrawEpdDepAdcQa(TCanvas* canv, const char* savename);
  void DrawEpdDepTacQa(TCanvas* canv, const char* savename);
    
  void DrawFcsClusterQa(TCanvas* canv, const char* savename);
  void DrawFcsClusterPi0(TCanvas* canv, const char* savename);
  void DrawFcsPointQa(TCanvas* canv, const char* savename);
  void DrawFcsPointPi0(TCanvas* canv, const char* savename);
  /*
  Int_t LoadGraphsFromFile(TFile* file, TObjArray* graphs, StMuFcsAnaData* anadata );
  void FillGraphs(Int_t irun);

  void DrawGraphs(TCanvas* canv, const char* savename="testGraphs.png" );
  void DrawGraphTrig(TCanvas* canv, const char* savename );
  void DrawGraphNhits(TCanvas* canv, const char* savename="testGraphNhits.png" );
  void DrawGraphNhitsCut(TCanvas* canv, const char* savename="testGraphNhitsCut.png" );
  void DrawGraphESum(TCanvas* canv, const char* savename="testGraphESum.png" );

  void DrawGraphNClusters(TCanvas* canv, const char* savename="testGraphNClusters.png");
  void DrawGraphNPoints(TCanvas* canv, const char* savename="testGraphNPoints.png");
  void DrawGraphCluEn(TCanvas* canv, const char* savename="testGraphCluEn.png");
  void DrawGraphPoiEn(TCanvas* canv, const char* savename="testGraphPoiEn.png");
  */
  void PrintSpinBits();        ///< Special funtion to cross check the spin bit dump with spin bits stored in the histograms #mH1F_spin4Vbx7 and #mH1F_spin4Vbx48.
  
protected:
  Int_t FillEventInfo(StFwdAnaData* anadata);
  Int_t FillFcsInfo(StFwdAnaData* anadata);

  TH1* mH1F_AllFcsTriggers = 0;       ///< Triggers in the events
  TH1* mH2F_BxId_7V48 = 0;            ///< Bunch crossing Id 7 bit vs. 48 bit
  TH1* mH2F_Mult_tofVref = 0;         ///< Tof multiplicty vs. Reference multiplicity
  TH1* mH2F_Mult_tofVecal = 0;        ///< Tof multiplicity vs. Fcs Ecal multiplicity
  TH1* mH1F_Spin = 0;                 ///< Spin info distribution
  //TH1* mH1F_BunchXing = 0;            ///< Spin state vs. Bunch crossing number with respect to blue clock
  //TH1* mH1F_spin4Vbx7 = 0;             ///< Spin4 state vs. bx7
  //TH1* mH1F_spin4Vbx48 = 0;            ///< Spin4 state vs. bx48
  
  TObjArray* mH2F_Hit_adcVtb[kFcsNDet];  ///< Adc vs. tb for all channels
  TH1* mH2F_Hit_enVid[kFcsNDet];         ///< Energy vs. channel Id
  TH1* mH2F_Hit_fitpeakVid[kFcsNDet];    ///< Timebin of peaks vs. channel id
  TH1* mH2F_Hit_chi2Vid[kFcsNDet];       ///< chi^2 of fitted peaks (npeaks>1 only) vs. channel id
  TH1* mH2F_Hit_npeaksVid[kFcsNDet];     ///< number peaks in fit vs. channel id
  TH1* mH1F_Hit_NHits[kFcsNDet];         ///< Hit multiplicity in FCS
  TH1* mH1F_Hit_NHitsCut[kFcsNDet];      ///< Hit multiplicity in FCS with 1GeV cut on Ecal and Hcal and 250 ADC for Pres
  TH1* mH1F_Hit_ESum[3];                 ///< Total energy sum in ecal[0], hcal[1], pres[2]
  //TH1* mH2F_Hit_colVrow[3];            ///< @[May 28, 2024] > Trying to emulate mHitMap in StFcsQaMaker

  TObjArray* mH2F_HitPres_depVqt[2];    ///< Special for checking EPD ADC Qt vs. DEP sum split by North[0], South[1] Fcs designation
  TObjArray* mH2F_HitPres_peakVtac[2];  ///< Special for checking EPD TAC values vs. found peak time from FCS split by North[0], South[1] Fcs designation
  //TObjArray* mH2F_HitEpd_tacVadcmip[2]; ///< Special for checking EPD TAC vs. ADC/ADC_1mip histograms which may help with slew corrections in the EPD
  
  TH1* mH1F_NClusters[kFcsNDet];              ///< Cluster multiplicity
  TH1* mH1F_Clu_NTowers[kFcsNDet];            ///< Number towers in a cluster
  TH1* mH1F_Clu_NNei[kFcsNDet];               ///< Number of neighbor clusters
  TH1* mH1F_Clu_NPoints[kFcsNDet];            ///< Number points in a cluster
  TH1* mH1F_Clu_En[kFcsNDet];                 ///< Cluster energy
  TH1* mH2F_Clu_yVx[kFcsNDet];                ///< Cluster reconstruction location in local x,y space (i.e. row,column space)
  TH1* mH2F_Clu_sigmaxVsigmin[kFcsNDet];      ///< Cluster sigma max vs. sigma min
  TH1* mH1F_Clu_theta[kFcsNDet];              ///< Cluster theta (angle in x-y plane that defines direction of least second sigma)
  TH1* mH2F_Clu_Chi2NdfPhoton_2V1[kFcsNDet];  ///< Chi^2/NDF for 2 photon fit vs. 1 photon fit
  TH1* mH2F_CluHigh_angleVesum = 0;           ///< opening angle in highest two clusters vs. energy sum of the two clusters
  TH1* mH2F_CluHighEn_lowVhigh = 0;           ///< Highet two cluster energies with highest energy cluster being x-axis and lower one being y-axis
  TH1* mH2F_CluHigh_dggVesum = 0;             ///< Highest two cluster energies, dgg vs. esum
  TH1* mH2F_CluHigh_invmassVesum = 0;         ///< Highest two cluster energies, invariant mass vs. energy sum of the two clusters
  TH1* mH2F_CluHigh_invmassVdgg = 0;          ///< highest 2 clusters dgg vs. invariant mass 
  TH1* mH2F_CluHigh_invmassVzgg = 0;          ///< highest 2 clusters zgg vs. invariant mass

  TH1* mH1F_NPoints[kFcsNDet];          ///< Point Multiplicity
  TH1* mH1F_Poi_En[kFcsNDet];           ///< Point Energy
  TH1* mH1F_Poi_NCluPhotons[kFcsNDet];  ///< number of photons in parent cluster
  TH1* mH2F_Poi_yVx[kFcsNDet];          ///< Point reconstruction location in local x,y space (i.e. row, column space)
  TH1* mH2F_PoiHigh_angleVesum = 0;     ///< point opening angle of two highest points
  TH1* mH2F_PoiHighEn_lowVhigh = 0;     ///< point energy of 2 highest energy points, higher energy point on x-axis, lower energy point on y-axis
  TH1* mH2F_PoiHigh_dggVesum = 0;       ///< 2 highest energy points, dgg vs. Sum of the energy of the 2 points
  TH1* mH2F_PoiHigh_invmassVesum = 0;   ///< Highest two point energies, invariant mass vs. energy sum of the two points
  TH1* mH2F_PoiHigh_invmassVdgg = 0;    ///< highest 2 points
  TH1* mH2F_PoiHigh_invmassVzgg = 0;    ///< highest 2 points

  //TGraph* EpdNmips;                 ///< At first/last event in a run fill with all nMIP values for west side epd channels

  bool mFcsAdcTbOn = true;             ///< For turning on/off Adc V tb histograms for the FCS
  bool mEpdAdcQaOn = true;             ///< For turning on/off Qt V Dep histograms from the EPD data
  bool mEpdTacQaOn = true;             ///< For turning on/off Tac V PeakX histograms from the EPD data
  bool mBestMassOn = true;             ///< For turning on/off finding the best cluster and point mass pair

  TGraph* mG_Entries = 0;                ///< Graph for number of entries vs. Run Index
  TGraph* mG_Triggers[65];               ///< Graph for number of events in a given trigger vs. run number
  TGraphErrors* mGE_VertexVpd = 0;       ///< Graph for Mean VPD vertex and Err as RMS vs. Run Index
  TGraphErrors* mGE_VertexBbc = 0;       ///< Graph for Mean BBC vertex and Err as RMS vs. Run Index
  TGraph* mG_UpSpin = 0;                 ///< Graph for Number of times Spin state was up (+1) vs. Run Index
  TGraph* mG_NoSpin = 0;                 ///< Graph for Number of times Spin state was nonexistent vs. Run Index
  TGraph* mG_DownSpin = 0;               ///< Graph for Number of times Spin state was down (-1) vs. Run Index
  TGraphErrors* mGE_NHits[kFcsNDet];     ///< Graph for Mean Number of hits for a given detectorId and Err as RMS vs. Run Index
  TGraphErrors* mGE_NHitsCut[kFcsNDet];  ///< Graph for Mean Number of hits with cuts for a given detectorId and Err as RMS vs. Run Index
  TGraphErrors* mGE_ESum_Ecal = 0;       ///< Graph for Mean Total Energy Sum for Ecal and Err as RMS vs. Run Index
  TGraphErrors* mGE_ESum_Hcal = 0;       ///< Graph for Mean Total Energy Sum for Hcal and Err as RMS vs. Run Index
  TGraphErrors* mGE_ESum_Pres = 0;       ///< Graph for Mean Total Energy Sum for Pres and Err as RMS vs. Run Index
  TGraphErrors* mGE_NClusters[kFcsNDet]; ///< Graphs for mean of nhit clusters and Err as RMS vs. Run Index for the different detector Ids
  TGraphErrors* mGE_NPoints[kFcsNDet];   ///< Graphs for mean of nhit  points and Err as RMS vs. Run Index for the different detector Ids
  TGraph* mG_Clu_En[kFcsNDet];           ///< Graphs for mean of cluster energy vs. Run Index for the different detector Ids
  TGraph* mG_Poi_En[kFcsNDet];           ///< Graphs for mean of point energy vs. Run Index for the different detector Ids

  double GraphAverage(TGraph* g);        ///< Helper function to check if a graph has zero average so I don't plot it

  ClassDef(StFwdAnaFcsRun22Qa,1)
};

#endif
