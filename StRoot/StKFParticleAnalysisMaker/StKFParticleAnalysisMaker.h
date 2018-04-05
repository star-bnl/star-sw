// $Id: StKFParticleAnalysisMaker.h,v 1.16 2014/08/06 11:43:53 jeromel Exp $
/*!
 * \class  StKFParticleAnalysisMaker
 * \author Maksym Zyzak
 * \date   2017/10/17
 * \brief  class for analysis of PicoDst
 */                                                                      
#ifndef STAR_StKFParticleAnalysisMaker
#define STAR_StKFParticleAnalysisMaker
//#define __DEVT__
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TMVA/Reader.h"

class StKFParticleInterface;
class StKFParticlePerformanceInterface;
class StPicoDst;
class StMuDst;
class TNtuple;
class TFile;

class StKFParticleAnalysisMaker : public StMaker {
 private:
  static const int fNNTuples = 4;
  Char_t                mBeg[1];        //!
  StMuDst                          *fMuDst;
  StPicoDst                        *fPicoDst;                          //!
  StKFParticleInterface            *fStKFParticleInterface;            //!
  StKFParticlePerformanceInterface *fStKFParticlePerformanceInterface; //!
  TNtuple* fCutsNTuple[fNNTuples];
  TFile* fNTupleFile[fNNTuples];
  int fNTuplePDG[fNNTuples];
  TString fNtupleNames[fNNTuples];
  TString fNtupleCutNames[fNNTuples];
  TString fTMVACutFile[fNNTuples];
  double fTMVACut[fNNTuples];
  TMVA::Reader* fTMVAReader[fNNTuples];
  Char_t                mEnd[1];        //!
  std::vector<float> fTMVAParticleParameters[fNNTuples];
  bool fIsPicoAnalysis;
  int fdEdXMode;
  Bool_t fStoreTmvaNTuples;
  Bool_t fProcessSignal;
  Bool_t fCollectPIDHistograms;
  Bool_t fTMVAselection;
  
 public: 
  StKFParticleAnalysisMaker(const char *name="KFParticleAnalysis");
  virtual       ~StKFParticleAnalysisMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runumber);
  void           BookVertexPlots();
  virtual Int_t  Make();
  Bool_t         Check();
  void AnalysePicoDst() { fIsPicoAnalysis = true;  }
  void AnalyseMuDst()   { fIsPicoAnalysis = false; }
  static void    PrintMem(const Char_t *opt = "");
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StKFParticleAnalysisMaker.h,v 1.0 2017/10/07 11:43:53 mzyzak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  void ProcessSignal() { fProcessSignal = true; }
  void StoreTMVANtuples() { fStoreTmvaNTuples = true; }
  void CollectPIDHistograms() { fCollectPIDHistograms = true; }
  void UseTMVA() { fTMVAselection = true; }
  void SetTMVAcutFileD0(TString file)    { fTMVACutFile[0] = file; }
  void SetTMVAcutFileDPlus(TString file) { fTMVACutFile[1] = file; }
  void SetTMVAcutFileDs(TString file)    { fTMVACutFile[2] = file; }
  void SetTMVAcutFileLc(TString file)    { fTMVACutFile[3] = file; }
  void SetTMVAcutD0(double cut)    { fTMVACut[0] = cut; }
  void SetTMVAcutDPlus(double cut) { fTMVACut[1] = cut; }
  void SetTMVAcutDs(double cut)    { fTMVACut[2] = cut; }
  void SetTMVAcutLc(double cut)    { fTMVACut[3] = cut; }
  
  ClassDef(StKFParticleAnalysisMaker,0)   //
};
#endif
// $Log: StKFParticleAnalysisMaker.h,v $
