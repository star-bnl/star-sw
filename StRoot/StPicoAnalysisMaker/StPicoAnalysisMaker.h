// $Id: StPicoAnalysisMaker.h,v 1.16 2014/08/06 11:43:53 jeromel Exp $
/*!
 * \class  StPicoAnalysisMaker
 * \author Maksym Zyzak
 * \date   2017/10/17
 * \brief  class for analysis of PicoDst
 */                                                                      
#ifndef STAR_StPicoAnalysisMaker
#define STAR_StPicoAnalysisMaker
//#define __DEVT__
#include <assert.h>
#include <map>
#include <utility>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "TAxis.h"
#include "TObject.h"
#include "SystemOfUnits.h"
#include "StarRoot/TPolynomial.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFParticle.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#include "StBichsel/Bichsel.h"
#ifndef StMaker_H
#include "StMaker.h"
#endif
class StKFParticleInterface;
class StKFParticlePerformanceInterface;
class StPicoDst;

class StPicoAnalysisMaker : public StMaker {
 private:
  Char_t                mBeg[1];        //!
  StPicoDst                        *fPicoDst;                          //!
  StKFParticleInterface            *mStKFParticleInterface;            //!
  StKFParticlePerformanceInterface *mStKFParticlePerformanceInterface; //!
  Char_t                mEnd[1];        //!
  int fdEdXMode;
 protected:
 public: 
  StPicoAnalysisMaker(const char *name="PicoAnalysis");
  virtual       ~StPicoAnalysisMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runumber);
  void           BookTrackPlots();
  void           BookVertexPlots();
  virtual Int_t  Make();
  void           RunAnalysis();
  Bool_t         Check();
  static void    PrintMem(const Char_t *opt = "");
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPicoAnalysisMaker.h,v 1.0 2017/10/07 11:43:53 mzyzak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  std::vector<int> GetTofPID(double m2, double p, int q);
  ClassDef(StPicoAnalysisMaker,0)   //
};
#endif
// $Log: StPicoAnalysisMaker.h,v $
