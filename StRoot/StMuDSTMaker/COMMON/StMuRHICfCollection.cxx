#include "StMuRHICfCollection.h"
#include "StMuRHICfRawHit.h"
#include "StMuRHICfHit.h"
#include "StMuRHICfPoint.h"

ClassImp(StMuRHICfCollection)

StMuRHICfCollection::StMuRHICfCollection()
{
  mRawHit = 0;
  mHit = 0;
  mPoint = 0;
}

StMuRHICfCollection::~StMuRHICfCollection()
{
  if(mRawHit){delete mRawHit;}
  if(mHit){delete mHit;}
  if(mPoint){delete mPoint;}
  mRawHit = NULL;
  mHit = NULL;
  mPoint = NULL;
}

void StMuRHICfCollection::init()
{
  mRawHit = new TClonesArray("StMuRHICfRawHit", 1);
  mHit = new TClonesArray("StMuRHICfHit", 1);
  mPoint = new TClonesArray("StMuRHICfPoint", 0);
}

unsigned int StMuRHICfCollection::numberOfPoints() const 
{
  if (!mPoint) return 0;
  return mPoint->GetEntriesFast();
}

StMuRHICfRawHit* StMuRHICfCollection::addRawHit()
{
  if(!mRawHit){init();}
  StMuRHICfRawHit* muRHICfRawHit = (StMuRHICfRawHit*) mRawHit -> ConstructedAt(0);
  return muRHICfRawHit;
}

StMuRHICfHit* StMuRHICfCollection::addHit()
{
  if(!mHit){init();}
  StMuRHICfHit* muRHICfHit = (StMuRHICfHit*) mHit -> ConstructedAt(0);
  return muRHICfHit;
}

StMuRHICfPoint* StMuRHICfCollection::addPoint()
{
  if(!mPoint){init();}
  int counter = mPoint->GetEntriesFast();
  return new ((*mPoint)[counter]) StMuRHICfPoint;
}

StMuRHICfRawHit* StMuRHICfCollection::getRawHit()
{
  if(!mRawHit){return NULL;}
  return (StMuRHICfRawHit*) mRawHit -> At(0);
}

StMuRHICfHit* StMuRHICfCollection::getHit()
{
  if(!mHit){return NULL;}
  return (StMuRHICfHit*) mHit -> At(0);
}

StMuRHICfPoint* StMuRHICfCollection::getPoint(Int_t index)
{
  if(!mPoint){return NULL;}
  return static_cast<StMuRHICfPoint*>(mPoint->At(index));
}

TClonesArray* StMuRHICfCollection::getPointArray(){return mPoint;}

void StMuRHICfCollection::setRHICfRawHitArray(TClonesArray *array){mRawHit = array;}
void StMuRHICfCollection::setRHICfHitArray(TClonesArray *array){mHit = array;}
void StMuRHICfCollection::setRHICfPointArray(TClonesArray* array){mPoint = array;}