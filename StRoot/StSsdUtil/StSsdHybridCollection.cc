/*!
 * Description: SSD Hybrid Array BASE class
 * christelle roy
 */

#include <Stiostream.h>
#include "StObject.h"
#include "StSsdHybridCollection.hh"
#include "StSsdConfig.hh"
#include "StMessMgr.h"

#include "TString.h"

ClassImp(StSsdHybridCollection)

/*!
  As the SSD can present various configurations, 
  the constructor of this class has one parameter that corresponds 
  to the configuration name. So far, there is only one configuration 
  defined: "LADDER": corresponds to Ladder 0 (16 wafers or 32 hybrids).
*/
StSsdHybridCollection::StSsdHybridCollection() 
{

  mConfig = TString();
  mSsdConfig = NULL;
}

StSsdHybridCollection::StSsdHybridCollection(const char* config) 
{
  setConfiguration(config);
}

StSsdHybridCollection::StSsdHybridCollection(StSsdConfig* config) 
{
  setConfiguration(config);
}

StSsdHybridCollection::~StSsdHybridCollection()
{
  if( mSsdConfig){
    delete mSsdConfig;
  }
}

/// set the Collection configuration
void StSsdHybridCollection::setConfiguration(const char* config)
{
  mConfig = TString(config);
  mSsdConfig = new StSsdConfig();
  mSsdConfig->setConfiguration(config);

  resize(mSsdConfig->getTotalNumberOfHybrids());
  clear();
}

/// set the Collection configuration
void StSsdHybridCollection::setConfiguration(StSsdConfig* config)
{
   mConfig = TString(config->getConfiguration());
   mSsdConfig = new StSsdConfig();
   mSsdConfig->setConfiguration(mConfig);
  resize(mSsdConfig->getTotalNumberOfHybrids());
  clear();
}

int StSsdHybridCollection::getNumberOfLadders() {return mSsdConfig->getNumberOfLadders();}
int StSsdHybridCollection::getNumberOfWafers()  {return mSsdConfig->getNumberOfWafers();}
int StSsdHybridCollection::getNumberOfHybrids() {return mSsdConfig->getNumberOfHybrids();}
int StSsdHybridCollection::getTotalNumberOfHybrids() {return mSsdConfig->getTotalNumberOfHybrids();}
const char* StSsdHybridCollection::getConfiguration(){return mConfig.Data();}




