#ifndef StTriggerStudyEvent_hh
#define StTriggerStudyEvent_hh
#include "StarClassLibrary/StThreeVectorF.hh"
#include "TObject.h"
#include <map>

using namespace std;

class StTriggerStudyEvent : public TObject {
 private:
  vector<unsigned int>mTriggers;
  vector<unsigned int>mPrescales;
  vector<unsigned int>mSimuTriggers;
  vector<float>mJPEt;
  StThreeVectorF mVertexPosition;
  int mBbcTimeBin;
  map<int,int>mTowerDSM;
  map<int,float>mTowerADC;//pedestal subtracted
  map<int,int>mTPatchDSM;
  map<int,int>mJPatchDSM;
  vector<unsigned int>mTowers;
  vector<unsigned int>mHTowers;
  vector<unsigned int>mTPatches;
  vector<unsigned int>mJPatches;


 public:
  StTriggerStudyEvent();
  virtual ~StTriggerStudyEvent() {}
  virtual void Clear(const Option_t* opt="");
  void setJPEt(vector<float> vec){mJPEt = vec;}
  void setTriggers(vector<unsigned int> triggers){mTriggers = triggers;}
  void setPrescales(vector<unsigned int> prescales){mPrescales = prescales;}
  void setVertexPosition(StThreeVectorF vert){mVertexPosition = vert;}
  void setBbcTimeBin(int bbctimebin){mBbcTimeBin = bbctimebin;}
  void setSimuTriggers(vector<unsigned int> simtrig){mSimuTriggers = simtrig;}
  void addTowerDSM(int softID, int dsm){mTowerDSM[softID] = dsm;mHTowers.push_back(softID);}
  void addTowerADC(int softID, float adc){mTowerADC[softID] = adc;mTowers.push_back(softID);}
  void addTPatchDSM(int patchID,int dsm){mTPatchDSM[patchID] = dsm;mTowers.push_back(patchID);}
  void addJPatchDSM(int patchID,int dsm){mJPatchDSM[patchID] = dsm;mTowers.push_back(patchID);}

  const int getBbcTimeBin() const {return mBbcTimeBin;}
  const StThreeVectorF getVertexPosition() const {return mVertexPosition;}
  const vector<unsigned int> getTriggers() const {return mTriggers;}
  const vector<unsigned int> getPrescales() const {return mPrescales;}
  const vector<unsigned int> getSimuTriggers() const {return mSimuTriggers;}
  const vector<unsigned int> getTowers() const {return mTowers;}
  const vector<unsigned int> getHTowers() const {return mHTowers;}
  const vector<unsigned int> getTPatches() const {return mTPatches;}
  const vector<unsigned int> getJPatches() const {return mJPatches;}
  int getTowerDSM(int softID) {return mTowerDSM[softID];}
  float getTowerADC(int softID) {return mTowerADC[softID];}
  int getTPatchDSM(int softID) {return mTPatchDSM[softID];}
  int getJPatchDSM(int softID) {return mJPatchDSM[softID];}
  const vector<float> getJPEt() const {return mJPEt;}


  ClassDef(StTriggerStudyEvent, 1)
};
#endif
