#ifndef StGmtMatchMaker_hh     
#define StGmtMatchMaker_hh
#include <string.h>
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
  
  StGmtMatchMaker(const Char_t *name="gmtMatchMaker") : StMaker(name), mEventCounter(0) {memset(mBeg,0,mEnd-mBeg+1);}     // constructor
  ~StGmtMatchMaker() {}                               // destructor
  Int_t  Make();                      // invoked for every event
  virtual Int_t InitRun(Int_t run);
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
  Char_t                mBeg[1];        //!
  Int_t  mEventCounter;  //!
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
  TTree     *mGmtTuple; //! Gmt ntuple
  Char_t                mEnd[1];        //!
  StGmtData mGmtData;
  static float tofRadius;

  ClassDef(StGmtMatchMaker,0)
};
#endif
