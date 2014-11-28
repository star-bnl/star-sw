#ifndef STAR_StLuminosityMaker
#define STAR_StLuminosityMaker

#ifndef StMaker_H
#include "StMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#endif

#include "TClonesArray.h"
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StLuminosityHolder.h"

using namespace std;

class StLuminosityMaker : public StMaker {
 private:
  vector<unsigned int> mTriggers;
  vector<unsigned int> mNTotal;
  vector<unsigned int> mNVertex;
  vector<unsigned int> mNCuts;
  vector<unsigned int> mNSoftTrig;
  vector<float> mLumTotal;
  vector<float> mLumCuts;
  vector<float> mLumVertex;
  vector<float> mLumSoftTrig;
  vector<float> mPrescales;
  StMuDstMaker* muDstMaker;
  StTriggerSimuMaker* mTriggerSimuMaker;
  TClonesArray*  mLumHolder;

  const char* runMode;
  float mXsec;
  int runNumber;
  float mVertexCut;
  int mOverrideMode;
  unsigned int mFilterMode;

  void printOutput();
  void saveOutput();

 public: 
  StLuminosityMaker(const char *name="Luminosity");
  virtual       ~StLuminosityMaker() {}
  virtual Int_t Init();
  virtual Int_t InitRun(int run);
  virtual Int_t  Make();
  virtual Int_t Finish();

  void getTriggersFromFilterMaker(const char* filtername="triggerFilter");
  
  void setFilterMode(unsigned int flag=1) {mFilterMode = flag;}
  void addTrigger(unsigned int trigId);
  void setMode(const char* newMode);
  void setCrossSectionNB(float newXsec, int overRideMode = 1);
  void setVertexCutcm(float newcut){mVertexCut = newcut;}
  const float getCrossSectionNB() const {return mXsec;}
  const float getVertexCutcm() const {return mVertexCut;}
  const TClonesArray* getHolder() const {return mLumHolder;}
  const vector<float> getLumTotal() const {return mLumTotal;}
  const vector<float> getLumCuts() const {return mLumCuts;}
  const vector<float> getLumVertex() const {return mLumVertex;}
  const vector<float> getLumSoftTrig() const {return mLumSoftTrig;}
  const vector<float> getPrescales() const {return mPrescales;}
  const vector<unsigned int> getTriggers() const {return mTriggers;}
  const vector<unsigned int> getNTotal() const {return mNTotal;}
  const vector<unsigned int> getNCuts() const {return mNCuts;}
  const vector<unsigned int> getNVertex() const {return mNVertex;}
  const vector<unsigned int> getNSoftTrig() const {return mNSoftTrig;}

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StLuminosityMaker.h,v 1.4 2014/08/06 11:43:39 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StLuminosityMaker,0)   //StAF chain virtual base class for Makers
};

#endif
