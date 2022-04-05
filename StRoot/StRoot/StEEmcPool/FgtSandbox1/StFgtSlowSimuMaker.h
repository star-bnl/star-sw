// $Id: StFgtSlowSimuMaker.h,v 1.3 2014/08/06 11:42:57 jeromel Exp $


/* \class StFgtSlowSimuMaker        
\author Jan Balewski, Wei-Ming Zang

*/
 
#ifndef STAR_StFgtSlowSimuMaker
#define STAR_StFgtSlowSimuMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StFgtGeom.h"
#include "StFgtContainers.h"

#include "TGeoManager.h" // for gGeoManager
#include "TROOT.h" // for gROOT

class St_g2t_fgt_hit;
class TH2F;
class TH1F;
class TRandom3;
class HexLatice;

class StFgtSlowSimuMaker : public StMaker {   
 private:
  enum {mxH=32,eLossDim=10000};
  TH1 *hA[mxH];
  TObjArray *HList;
  StFgtGeom *geom;
  HexLatice *hexLat;
  double *mRadStripRelativeGain;//indexed by radStripID, the same for all disks
  double *mPhiStripRelativeGain;//indexed by phiStripID, the same for all disks

  // working arrays
  TH2F *digXY; // 2D digitization response of one quart, local REF   
  TH1F *digPhi; // phi-strips response vector, one disk 
  TH1F *digRad; // rad-strips response vector, one disk 

  // same as working arrays, but not reset for for each quad and disk  WMZ
  TH2F *digXYAll;  // 2D digitization response of all quart, local REF   
  TH1F *digPhiAll; // phi-strips response vector, all disks
  TH1F *digRadAll; // rad-strips response vector, all disks

  int mInpEve;
  double par_2DpixAmplThres; // minimal content of considered digXY array
  double par_stripAmplThres; // a.u., drop strips below it
  double par_XYamplSigma; // signal smearing in X-Y
  double par_radStripGainMean,par_radStripGainSigma;  //relative gain variation: mean & sigma
  double par_phiStripGainMean,par_phiStripGainSigma;  //relative gain variation: mean & sigma

  bool   par_forcePerp; // for testing only, tracks wil be perp to GEM gas
  int    par_useOnlyDisk; // 0=all , for testing only, drop response from other  disks
  int    par_cutoffOfBichel; // cutoff channel of meLossTab[10000] built from BichselELossProbHighBG.dat used to reject very high and unrealistic loss value  
  double par_hexLaticePitch, par_hexLaticePhi1deg;
  double par_transDiffusionPerPath;
  TF1 *amplF;
  TRandom3* mRnd;
  void addHit(TVector3 rLocal, double ampl=1.) ;
  void responseLinearModel(TVector3 Rloc, TVector3 Dloc); // primitive model
  void responseFrankModel(TVector3 Rloc, TVector3 Dloc);// detailed, w/ fluct
  double meLossTab[eLossDim];
 
  void InitHisto1();
  void InitHisto2();
  void CloseHisto();
  void sort_g2t_hits(St_g2t_fgt_hit *);
  bool projectQuadrant( int iquad);
  void exportStripPlane(TH1F *h, vector<fgt_strip> &L);

 public:
  vector<fgt_g2t_auxil> mG2tHitList[kFgtMxDisk+1][kFgtMxQuad]; // tmp for clus eval
  vector<fgt_strip>  mRadAdcList[kFgtMxDisk]; // tmp for the cluster finder
  vector<fgt_strip>  mPhiAdcList[kFgtMxDisk]; // tmp for the cluster finder
  void setRadStripGain(double g, double s){ par_radStripGainMean=g; par_radStripGainSigma=s;}
  void setPhiStripGain(double g, double s){ par_phiStripGainMean=g; par_phiStripGainSigma=s;}
  void setTransDiffusion(double x) {par_transDiffusionPerPath=x;}

  StFgtSlowSimuMaker(const char *name="FgtSlowSimu");
  virtual       ~StFgtSlowSimuMaker();
  virtual Int_t  Init();
  virtual Int_t  Finish(); 
  virtual Int_t  Make();
  virtual void  Clear(Option_t *option="");
  void  setHList(TObjArray * x){HList=x;}
  void  saveHisto(TString fname);
  void  setStripThresh(double x) {par_stripAmplThres=x;}
  void  setHexGemLatice(double x, double y) {par_hexLaticePitch=x;  par_hexLaticePhi1deg=y;}
  void  initFrankModel(TString fname="xBichselELossProbHighBG.dat");
  void  forcePerpTracks(bool x=true) {par_forcePerp=x;}
  void  useOnlyDisk(int  x){par_useOnlyDisk=x;}
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtSlowSimuMaker.h,v 1.3 2014/08/06 11:42:57 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
 private:
  
  ClassDef(StFgtSlowSimuMaker,0)   
};
    
#endif


// $Log: StFgtSlowSimuMaker.h,v $
// Revision 1.3  2014/08/06 11:42:57  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2011/04/08 22:18:42  balewski
// added access to TGeo
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//
 
