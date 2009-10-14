// $Id: StFtpcLaser.hh,v 1.4 2009/10/14 15:59:55 jcs Exp $
//
// $Log: StFtpcLaser.hh,v $
// Revision 1.4  2009/10/14 15:59:55  jcs
// changes to be able to vary the gas temperature in addition to varying t0 and
// gas composition
//
// Revision 1.3  2006/04/04 10:57:05  jcs
// Fix memory leak
//
// Revision 1.2  2006/03/15 15:13:56  jcs
// add lines for listing CVS update info
//

#ifndef STAR_StFtpcLaser
#define STAR_StFtpcLaser

#include <Stiostream.h>
#include <fstream>
#include <cmath>
#include "TROOT.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TObjArray.h"
#include "TString.h"
#include "TPostScript.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TBranch.h"
#include "TLeaf.h"
#include "TLeafI.h"
#include "TCanvas.h"
#include "TLeaf.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TLegend.h"
#include "TLine.h"
#include "TLatex.h"
#include "TPaveLabel.h"
#include "TSpectrum.h"

//---------------------------------------------------------------

struct RUN
{
  Int_t run;
  Int_t date;
  Int_t time;
  Float_t micropertimebin;
  Float_t normalizedNowPressure, standardPressure;
  Float_t baseTemperature, gasTemperatureWest, gasTemperatureEast;
  Float_t deltapW,deltapE;
};

struct HIT 
{
  Float_t x,y,z;
  Float_t rad,phi;
  Float_t raderror,phierror;
};

struct CLUSTER
{
  Float_t timepos,padpos,timesigma,padsigma;
  Float_t peakheight, charge;
  Int_t timebin,pad;
  Int_t padlength,timelength;
  Int_t row,sec;
  Int_t flag;
  Int_t numpeaks;
};

struct EVENT
{
  Int_t run;
  Int_t nevent;
};

struct TEVENT
{
  Int_t run;
  Int_t nevent;
};


struct TCLUSTER
{
  Int_t row,sec, padlength, timelength;
  Float_t peakheight, charge;
  Int_t ntracks;
  Float_t padpos, timepos;
  Float_t padpossigma, timepossigma;
};

struct THIT
{
  Float_t x,y,z;
  Float_t ex,ey,ez;
};

struct TREVENT
{
  Int_t run;
  Int_t nevent;
};

struct TRACK
{
  Float_t px,py,pz;
  Float_t eta,p,pt;
  Int_t npoints;
  Int_t charge;
};

struct MVERTEX
{
  Float_t x,y,z;
};

class StFtpcLaser 
{

 private:
   
 protected:
   
 public: 

  TFile *f;

  RUN Run;
  CLUSTER cluster;
  HIT hit;
  TCLUSTER tcluster;
  THIT thit;
  TRACK track;
  EVENT event;
  TEVENT tevent;
  TREVENT trevent;
  MVERTEX mvertex;

  TBranch *bRun;
  TBranch *bhit, *bevent, *bcluster;
  TBranch *bthit, *btcluster, *btevent;
  TBranch *btrevent, *btrack,*btrvertex;

  TTree *drtree;
  TTree *dtree;
  TTree *dttree;
  TTree *dtrtree; 
  
  StFtpcLaser();
  virtual void Init(TString filename);
  virtual ~StFtpcLaser();
  virtual Float_t zyltrafo(Float_t x,Float_t y, Float_t z);
  virtual bool laser_sector(int whichftpc,int whichsec,int sec);
  virtual int laser_straight(float *rad,int max);
  virtual void readtree(TFile *f);
  virtual void GetTreeEntry(int k);
  virtual void GetClusterTreeEntry(int k);

};
#endif
