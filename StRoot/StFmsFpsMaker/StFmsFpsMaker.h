// \class StFmsFpsMaker
// \author Akio Ogawa
//
//   This is analysis for FMS-FPS correlations.
// 
//  $Id: StFmsFpsMaker.h,v 1.2 2015/09/18 18:50:06 akio Exp $
//  $Log: StFmsFpsMaker.h,v $
//  Revision 1.2  2015/09/18 18:50:06  akio
//  cleaning up and adding QA histos
//
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
  void setPrint(int v=1){mPrint=v;}
  void print();

private:
  StFmsDbMaker* mFmsDbMaker;
  StFmsCollection* mFmsColl;

  bool mQA;
  char* mFilename;
  TFile* mFile;
  int mPrint;

  int mReadMuDST; 
  void readMuDST();

  void corrFmsFps();
  void pid(int opt=0);
  void isolationCone();

  //QA & alignment related
  enum {NCUT1=6};  //0:all, 1:E>3GeV, 2:PID=gamma, 3:PID=hadron, 4:PID=electron, 5:others
  enum {NCUT2=6};  //0:all combo, 1:E1,E2>5GeV, 2:PID=gg, 3:PID=hh, 4:ee, 5:other PID combos

  void fmsFpsAlignment(); 
  TH2F* mH2[kFpsNQuad][kFpsNLayer][NCUT1];
  TH1F* mHd[kFpsNQuad][kFpsNLayer][NCUT1];
  TH2F* mHd2[kFpsNQuad][kFpsNLayer][NCUT1];

  TH1F* mHn[NCUT1];
  TH1F* mHene[NCUT1];
  TH1F* mHelo[NCUT1];
  TH1F* mHpt[NCUT1];
  TH2F* mHept[NCUT1];
  TH1F* mHeta[NCUT1];
  TH1F* mHphi[NCUT1];
  TH1F* mHx[NCUT1];
  TH1F* mHy[NCUT1];
  TH2F* mHxy[NCUT1];
  TH1F* mHpid[NCUT1];
  TH1F* mHpid2[NCUT1];


  TH1F* mPn[NCUT1];
  TH1F* mPene[NCUT2];
  TH1F* mPpt[NCUT2];
  TH2F* mPept[NCUT2];
  TH1F* mPeta[NCUT2];
  TH1F* mPphi[NCUT2];
  TH2F* mPpid[NCUT2];
  TH1F* mPm1[NCUT2];
  TH1F* mPm2[NCUT2];
  TH1F* mPzgg[NCUT2];
  TH1F* mPr30[NCUT2];
  TH1F* mPr100[NCUT2];

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsFpsMaker.h,v 1.2 2015/09/18 18:50:06 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StFmsFpsMaker,0);
};

#endif
