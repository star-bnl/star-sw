// \class StFmsFpsMaker
// \author Akio Ogawa
//
//   This is analysis for FMS-FPS correlations.
// 
//  $Id: StFmsFpsMaker.h,v 1.1 2015/09/02 14:56:12 akio Exp $
//  $Log: StFmsFpsMaker.h,v $
//  Revision 1.1  2015/09/02 14:56:12  akio
//  Initial version of FMS-FPS correation analysis
//

#ifndef STAR_StFmsFpsMaker_HH
#define STAR_StFmsFpsMaker_HH

#include "StMaker.h"
#include "StEnumerations.h"

class StFmsDbMaker;
class StFmsCollection;
class TH2F;

class StFmsFpsMaker : public StMaker{
public: 
  StFmsFpsMaker(const Char_t* name="FmsFps");
  ~StFmsFpsMaker();
  Int_t Init();
  Int_t Make();
  Int_t Finish();

  void setReadMuDST(int v=1){mReadMuDST=v;}         //Read MuDST hits if available, and update FPS hits in StEvent using current DB values
                                                    //if this is set to 0 (default), use hits as is (new DB values will NOT be applied)
  void setQA(char* file){mQA=true; mFilename=file;} //enable alighment histograms for QA

  void print();

private:
  enum {NCUT=13};

  StFmsDbMaker* mFmsDbMaker;
  StFmsCollection* mFmsColl;

  bool mQA;
  char* mFilename;
  TFile* mFile;
  
  int mReadMuDST; 
  void readMuDST();

  //float mHit[NQ][kFpsNLayer][kFpsNSlat];
  //int   mNHit[NQ][kFpsNLayer];
  //int   mNFmsPoint[NQ][kFpsNLayer][kFpsNSlat];
  //int   mFmsPoint[kFpsNQuad][kFpsNLayer][kFpsNSlat][NMAXPOINT];
  //void makeHitArray(); 
  
  void corrFmsFps();
  void pid(int opt=0);

  void fmsFpsAlignment();
  TH2F* mH2[kFpsNQuad][kFpsNLayer][NCUT];
  TH1F* mHd[kFpsNQuad][kFpsNLayer][NCUT];
  TH2F* mHd2[kFpsNQuad][kFpsNLayer][NCUT];
  TH1F* mNp[NCUT];
  TH1F* mHene[NCUT];
  TH1F* mHpt[NCUT];
  TH2F* mHept[NCUT];
  TH1F* mHeta[NCUT];
  TH1F* mHphi[NCUT];
  TH1F* mHx[NCUT];
  TH1F* mHy[NCUT];
  TH2F* mHxy[NCUT];
  TH1F* mHm1[NCUT];
  TH1F* mHm2[NCUT];
  TH1F* mHpid[NCUT];
  TH1F* mHpid2[NCUT];

  ClassDef(StFmsFpsMaker,0);
};

#endif
