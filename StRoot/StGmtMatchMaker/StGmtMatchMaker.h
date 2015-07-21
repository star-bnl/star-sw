#ifndef StGmtMatchMaker_hh     
#define StGmtMatchMaker_hh

#include "StMaker.h"
#include "TString.h"
#include "TH2D.h"
#include "StGmtData.h"
#include "StPhysicalHelixD.hh"
#include "StHelixD.hh"
#include "StThreeVector.hh"
//
//
#define nModules 8
#define nTimebins 7

class StEvent;
class StTrack;

class StGmtMatchMaker : public StMaker {
public:
  
  StGmtMatchMaker(const Char_t *name="gmtMatchMaker");     // constructor
  ~StGmtMatchMaker() {}                               // destructor
  
  Int_t  Make();                      // invoked for every event
  Int_t  Finish();                    // called once at the end
  virtual Int_t Init();
  void setHistoFileName(const Char_t* filename){mOutputFile=filename;}
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StGmtMatchMaker.h,v 1.00 2014/07/08 17:16:26 $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
 private:
  void project2GMT();
  void projectTrack2GmtR(const StPhysicalHelixD helix, float bfield, StThreeVector<double> &pos);
  void projectTrack2GmtModule(const StPhysicalHelixD helix, float bfield, int id, StThreeVector <double> &mpos);
  int getModuleID(StThreeVector<double> global);
  void getModulePos(int id, StThreeVector<double> &cen, StThreeVector<double> &n);
  //void local2Global(StThreeVector<double> local, StThreeVector<double> & global);
  void local2Global(int mid, double coordNX, double coordNY, double &phi, double &z);
  void global2Local(StThreeVector<double> global, StThreeVector<double> & local, int id);
  void getCoord(StThreeVector<double> local, StThreeVector<float> & coord);

  //
  //
  Int_t  mEventCounter;  //!
  
  static float const tofRadius=210.968+3.6;
  StEvent* event;

  TH2D *hADCvsChannel[nModules*2];
  TH2D *hChannelCorr[nModules*2];
  TH2D *hChannelCorrAll;
  TH2D *hProjCorrX[nModules];
  TH2D *hProjCorrY[nModules];
  TH2D *hProjCorrXAll;
  TH2D *hProjCorrYAll;
  TH2D *hProjGlCorrPhiAll;
  TH2D *hProjGlCorrZAll;
  TH2D *hProjModuleCorr;
  TH2D *hProjCoordXCorr;
  TH2D *hProjCoordYCorr;
  
  TH2D *hdCoordXvsId;
  TH2D *hdCoordYvsId;
  
  TH2D *hdCoordXvsPt;
  TH2D *hdCoordYvsPt;
  
  Double_t  mPeds[nModules*2][128];
  Double_t  mPedDevs[nModules*2][128];
  TString   mOutputFile;
  StGmtData mGmtData;
  TTree     *mGmtTuple; //! Gmt ntuple
  TFile     *mGmtFile; //! Gmt ntuple

  ClassDef(StGmtMatchMaker,0)
};
#endif
