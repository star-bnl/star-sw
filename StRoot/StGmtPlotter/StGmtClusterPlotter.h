///Cluster Plotter for GMT from FgtClusterPlotter
//Maxence Vandenbroucke

#ifndef _ST_GMT_CLUSTER_PLOT_MAKER_
#define _ST_GMT_CLUSTER_PLOT_MAKER_

#include "StMaker.h"
#include <TH2D.h>
#include <TH3D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TPolyLine3D.h>
#include <TVector3.h>
#include <StPhysicalHelixD.hh>

class StGmtCollection;

class StGmtClusterPlotter : public StMaker {
 public:
  StGmtClusterPlotter(const Char_t* name="GmtClusterPlotter");
  
  virtual ~StGmtClusterPlotter();
  
  Int_t Init();
  Int_t Make();
  Int_t Finish();
  //   Bool_t checkPulse(StGmtHit* pClus);
  //virtual const char *GetCVS() const
  //{static const char cvs[]="Tag $Name:  $ $Id: StGmtClusterPlotter.h,v 1.1.1.1 2013/09/02 15:01:31 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  void DrawBoxes();
 protected:
  
  void ProjOnModule(const int module, const TVector3 &vloc, TVector3 &vdet); //convert tpc local coordinate into gmt detector coordinate
  // for accessing the data
  StGmtCollection *mGmtCollectionPtr;
  
  TH2D** hGmtXY;
  TH2D** hGmtHitXY;
  TH2D** hTpcXY;
  TH3D* h3Dhits;
  TH2D* hGmtXYGlob;
  TH2D* hGmtHitXYGlob;
  TH2D* hTpcXYGlob;
  TH1D** hResX;
  TH1D* hResXGlob;
  TH1D* hResYGlob;
  TH1D** hResY;
  TH1D* hNGMThits;
  TH1D* hNTPChits;
  TH2D* hGmtCorXX;
  TH2D* hGmtCorXY;
  TH2D* hGmtCorYY;
  TH2D* hGmtCorYX;

  TH1D *  hTrFitPts; 
  TH1D *  hGmtAmpX; 
  TH1D *  hGmtAmpY; 
  TH1D *  hGmtAmpRatio; 

  
  TPolyLine3D * pl;//cosmtics
  std::vector <TVector3> vhit;
  std::vector <TVector3> vhitTPC;
  
  int runningEvtNr;
  TFile * myRootFile;

  StPhysicalHelixD ** mHel;
  
 private:   
  ClassDef(StGmtClusterPlotter,1);
  
  
};

#endif
