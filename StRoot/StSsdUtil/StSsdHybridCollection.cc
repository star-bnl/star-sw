/*!
 * Description: SSD Hybrid Array BASE class
 *
 *
 *
 * This class corresponds to a collection of hybrid objects(for instance, StSsdHybridObject).
 * Basically, it represents the entire SSD, since the detector is a "collection" 
 * of hybrids (basic unit).
 * This should be the base class for any "SSD object".
 * For instance, the SSD pedestals (that inherits from StSsdHybridCollection)
 * is a collection of hybrid pedestals (that inherits from StSsdHybridObject).
 *
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

/*!
  Returns an internal index for the specified hybrid. 
  This index should be used to store/retrieve a specific hybrid in/from the collection.
  Or one can use the getObject method which parameters are the barrel, ladder, wafer 
  and hybrid numbers.
*/
int StSsdHybridCollection::getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  if (mSsdConfig)
    return mSsdConfig->getHybridIndex(barrelID, ladderID, waferID, hybridID);

  return -1;
}

/// returns an proper index for the specified hybrid if there was no swapping
int StSsdHybridCollection::getProperHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  if (mSsdConfig)
    return mSsdConfig->getProperHybridIndex(barrelID, ladderID, waferID, hybridID);

  return -1;
}

/// Method to retrieve an object (StSsdHybridObject) of the collection using 
/// the barrel, ladder, wafer and hybrid numbers.
StSsdHybridObject* StSsdHybridCollection::getObject(int barrelID, int ladderID, int waferID, int hybridID)
{


  int index = getHybridIndex(barrelID, ladderID, waferID, hybridID);

  if (index<0) return 0;

  return (StSsdHybridObject*)at(index);
}

int StSsdHybridCollection::getNumberOfBarrels() {return mSsdConfig->getNumberOfBarrels();}
int StSsdHybridCollection::getNumberOfLadders(int barrel) {return mSsdConfig->getNumberOfLadders(barrel);}
int StSsdHybridCollection::getNumberOfWafers(int barrel)  {return mSsdConfig->getNumberOfWafers(barrel);}
int StSsdHybridCollection::getNumberOfHybrids() {return mSsdConfig->getNumberOfHybrids();}
int StSsdHybridCollection::getTotalNumberOfHybrids() {return mSsdConfig->getTotalNumberOfHybrids();}
const char* StSsdHybridCollection::getConfiguration(){return mConfig.Data();}




/***************************************************************************
 *
 * $Id: StSsdHybridCollection.cc,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * Author: cr
 ***************************************************************************/
