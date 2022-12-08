#include "StRHICfCollection.h"

ClassImp(StRHICfCollection)

StRHICfCollection::StRHICfCollection()
{
	clear();
}

StRHICfCollection::~StRHICfCollection()
{
}

void StRHICfCollection::clear()
{
	mRHICfRawHitColl = new StRHICfRawHit();
	mRHICfHitColl = 0;

	mRunNumber = 0;
	mEventNumber = 0;
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
std::vector<StRHICfPoint*>& StRHICfCollection::pointCollection(){return mRHICfPointColl;}
const std::vector<StRHICfPoint*>& StRHICfCollection::pointCollection() const {return mRHICfPointColl;}

void StRHICfCollection::isAllSave(){mRHICfHitColl = new StRHICfHit();}

//=========== Set ===========//
void StRHICfCollection::setRunNumber(unsigned int run){mRunNumber = run;}
void StRHICfCollection::setEventNumber(unsigned int event){mEventNumber = event;}
void StRHICfCollection::setBunchNumber(unsigned int bunch){mBunchNumber = bunch;}
void StRHICfCollection::setRunType(unsigned int type){mRunType = type;}
void StRHICfCollection::setTriggerNumber(unsigned int trigger){mRHICfTrigger = trigger;}
void StRHICfCollection::setRunTime(Int_t idx, unsigned int time){mRunTime[idx] = time;}
void StRHICfCollection::setRunTRGM(unsigned int trgm){mRunTRGM = trgm;}

//=========== Get ===========//
unsigned int StRHICfCollection::numberOfPoints() const {return mRHICfPointColl.size();}
unsigned int StRHICfCollection::getRunNumber(){return mRunNumber;}
unsigned int StRHICfCollection::getEventNumber(){return mEventNumber;}
unsigned int StRHICfCollection::getBunchNumber(){return mBunchNumber;}
unsigned int StRHICfCollection::getRunType(){return mRunType;}
unsigned int StRHICfCollection::getTriggerNumber(){return mRHICfTrigger;}
unsigned int StRHICfCollection::getRunTime(Int_t idx){return mRunTime[idx];}
unsigned int StRHICfCollection::getRunTRGM(){return mRunTRGM;}
