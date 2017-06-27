//StiVMCMaker.h

#ifndef StiVMCMaker_HH
#define StiVMCMaker_HH

#include "StMaker.h"
#include "StEvent/StEnumerations.h"

class TFile;
class TTree;
class StEvent;
class StiHit;
class StiKalmanTrack;
class StiStEventFiller;
class StiKalmanTrackContainer;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StiToolkit;
class StiVertexFinder;
class StiDetector;
class StarVMCDetector;

class StiVMCMaker : public StMaker {
 public:
  
  StiVMCMaker(const char* name = "StiVMC");
  virtual ~StiVMCMaker() {}
  virtual void  Clear(const char* opt="");
  virtual Int_t Init();
  virtual Int_t InitDetectors();
  virtual void  loadHits(StEvent* pEvent);
  virtual void  loadSsdHits(StEvent *pEvent);
  virtual void  loadSvtHits(StEvent *pEvent);
  virtual void  loadTpcHits(StEvent *pEvent);
  virtual void  loadTofHits(StEvent *pEvent);
  virtual void  loadCtbHits(StEvent *pEvent);
  virtual void  loadFgtHits(StEvent *pEvent);
  virtual void  loadIstHits(StEvent *pEvent);
  virtual void  loadPxlHits(StEvent *pEvent);
  virtual void  loadPhmdHits(StEvent *pEvent);
  virtual void  loadZdcHits(StEvent *pEvent);
  virtual void  loadFtpcHits(StEvent *pEvent);
  virtual void  loadBarrelEmcTowerHits(StEvent *pEvent);
  virtual void  loadEndcapEmcTowerHits(StEvent *pEvent);
  virtual void  loadBarrelEmcPreShowerHits(StEvent *pEvent);
  virtual void  loadEndcapEmcPreShowerHits(StEvent *pEvent);
  virtual Int_t InitRun(int);
  virtual Int_t Make();
  virtual Int_t Finish();

  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StiVMCMaker.h,v 2.3 2009/08/19 19:56:40 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}	


 private:
  void  MyClear();
  
  Bool_t                eventIsFinished;
  Bool_t               _Initialized;
  StiToolkit  *        _toolkit;
  StiStEventFiller *   _eventFiller;
  StiDetector          *fStiDetector;
  StarVMCDetector      *fVMCDetector;
  StiDetectorContainer *fDetectorContainer;
  TString               fPath;
  Int_t                 fIndx[25];
  StDetectorId          fId;
  TStopwatch           *mTimg[5];   //HitLoad,GloTrks,Vtx,PriTrks,StFill
  ClassDef(StiVMCMaker,0)
};
#endif
