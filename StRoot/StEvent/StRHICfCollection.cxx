#include "StRHICfCollection.h"

ClassImp(StRHICfCollection)

StRHICfCollection::StRHICfCollection()
{
  clear();
}

StRHICfCollection::~StRHICfCollection()
{
  if(mRHICfRawHitColl){
    delete mRHICfRawHitColl;
    mRHICfRawHitColl = nullptr;
  }
  if(mRHICfHitColl){
    delete mRHICfHitColl;
    mRHICfHitColl = nullptr;
  }
  if(mRHICfPointColl.size() != 0){
    for(unsigned int i=0; i<mRHICfPointColl.size(); i++){
      delete mRHICfPointColl[i];
      mRHICfPointColl[i] = nullptr;
    }
  }
}

void StRHICfCollection::clear()
{
  mRHICfRawHitColl = new StRHICfRawHit();
  mRHICfHitColl = 0;

  mBunchNumber = 0;
  mRHICfTrigger = 0;
  mRunTime[0] = 0;
  mRunTime[1] = 0;
  mRunTRGM = 0;
  mRunType = 999;
}

StRHICfRawHit* StRHICfCollection::rawHitCollection(){return mRHICfRawHitColl;}
StRHICfHit* StRHICfCollection::hitCollection(){return mRHICfHitColl;}

void StRHICfCollection::addPoint(StRHICfPoint* pointColl){mRHICfPointColl.push_back(pointColl);}
void StRHICfCollection::addPointCollection(std::vector<StRHICfPoint*> coll){mRHICfPointColl = coll;}

std::vector<StRHICfPoint*>& StRHICfCollection::pointCollection(){return mRHICfPointColl;}
const std::vector<StRHICfPoint*>& StRHICfCollection::pointCollection() const {return mRHICfPointColl;}

void StRHICfCollection::isAllSave(){mRHICfHitColl = new StRHICfHit();}

//=========== Set ===========//
void StRHICfCollection::setBunchNumber(UInt_t bunch){mBunchNumber = bunch;}
void StRHICfCollection::setRunType(UInt_t type){mRunType = type;}
void StRHICfCollection::setTriggerNumber(UInt_t trigger){mRHICfTrigger = trigger;}
void StRHICfCollection::setRunTime(Int_t idx, UInt_t time){mRunTime[idx] = time;}
void StRHICfCollection::setRunTRGM(UInt_t trgm){mRunTRGM = trgm;}

//=========== Get ===========//
UInt_t StRHICfCollection::numberOfPoints() const {return mRHICfPointColl.size();}
UInt_t StRHICfCollection::getBunchNumber(){return mBunchNumber;}
UInt_t StRHICfCollection::getRunType(){return mRunType;}
UInt_t StRHICfCollection::getTriggerNumber(){return mRHICfTrigger;}
UInt_t StRHICfCollection::getRunTime(Int_t idx){return mRunTime[idx];}
UInt_t StRHICfCollection::getRunTRGM(){return mRunTRGM;}
