/**************************************************************
 *
 * $Id: StSsdWaferCollection.cc,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * Author: cr
 **************************************************************/
#include <Stiostream.h>
#include "StObject.h"
#include "StSsdWaferCollection.hh"
#include "StSsdConfig.hh"
#include "StMessMgr.h"

#include "TString.h"

ClassImp(StSsdWaferCollection)

/*!
  Since several configurations will be defined for the SSD,  
  the constructor of this class has one parameter that will correspond to 
  the configuration name. So far, there is only one configuration defined:
  "LADDER": corresponds to ladder 0 (16 wafers or 32 hybrids).
*/
StSsdWaferCollection::StSsdWaferCollection() 
{

  mConfig = TString();
  mSsdConfig = NULL;
}

StSsdWaferCollection::StSsdWaferCollection(const char* config) 
{
  setConfiguration(config);
}

StSsdWaferCollection::StSsdWaferCollection(StSsdConfig* config) 
{
  setConfiguration(config);
}

StSsdWaferCollection::~StSsdWaferCollection()
{
  //delete mSsdConfig;
}

void StSsdWaferCollection::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);
  mSsdConfig = new StSsdConfig();
  mSsdConfig->setConfiguration(config);

  clear();
  resize(mSsdConfig->getTotalNumberOfHybrids()/2);
}

void StSsdWaferCollection::setConfiguration(StSsdConfig* config)
{
  // set the Collection configuration

  mSsdConfig = config;
  mConfig = TString(mSsdConfig->getConfiguration());

  clear();
  resize(mSsdConfig->getTotalNumberOfHybrids()/2);
}

/*! 
  returns an internal index for the specified wafer. 
  This index should be used to store/retrieve a specific wafer in/from the collection.
  Or one can use the getObject method which parameters are the barrel, ladder, wafer 
  and hybrid numbers.
*/
int StSsdWaferCollection::getWaferIndex(int barrelID, int ladderID, int waferID)
{
  int index;

  if (mSsdConfig) {
    index = (int)mSsdConfig->getHybridIndex(barrelID, ladderID, waferID, 1)/2;
    return index;
  }

  return -1;
}

/*
StSsdHybridObject* StSsdWaferCollection::At(int index)
{
  return (StSsdHybridObject*)at(index);
}

void StSsdWaferCollection::AddAt(StSsdHybridObject* object, int index)
{
  put_at((TObject*)object,index);
}
*/

/// Method to retrieve an object (StSsdHybridObject) of the collection using the barrel, ladder, wafer numbers.
StSsdHybridObject* StSsdWaferCollection::getObject(int barrelID, int ladderID, int waferID)
{
  int index = getWaferIndex(barrelID, ladderID, waferID);
  if (index<0) return 0;
  return (StSsdHybridObject*)at(index);
}

int StSsdWaferCollection::getNumberOfBarrels() {return mSsdConfig->getNumberOfBarrels();}
int StSsdWaferCollection::getNumberOfLadders(int barrel) {return mSsdConfig->getNumberOfLadders(barrel);}
int StSsdWaferCollection::getNumberOfWafers(int barrel)  {return mSsdConfig->getNumberOfWafers(barrel);}
int StSsdWaferCollection::getTotalNumberOfWafers() {return (int)mSsdConfig->getTotalNumberOfHybrids()/2;}
const char* StSsdWaferCollection::getConfiguration(){return mConfig.Data();}

