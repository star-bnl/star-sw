/**************************************************************
 * Author: christelle roy
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


int StSsdWaferCollection::getNumberOfLadders() {return mSsdConfig->getNumberOfLadders();}
int StSsdWaferCollection::getNumberOfWafers()  {return mSsdConfig->getNumberOfWafers();}
int StSsdWaferCollection::getTotalNumberOfWafers() {return (int)mSsdConfig->getTotalNumberOfHybrids()/2;}
const char* StSsdWaferCollection::getConfiguration(){return mConfig.Data();}

