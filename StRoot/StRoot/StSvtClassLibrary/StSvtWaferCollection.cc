/***************************************************************************
 *
 * $Id: StSvtWaferCollection.cc,v 1.3 2004/01/27 02:29:29 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Wafer Array BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferCollection.cc,v $
 * Revision 1.3  2004/01/27 02:29:29  perev
 * LeakOff
 *
 * Revision 1.2  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/08/16 21:02:04  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/
///////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                       //
// This class corresponds to a collection of hybrid objects(for instance, StSvtHybridObject).            // 
// Basically, it represents the entire SVT, since the detector is a "collection" of hybrids (basic unit).// 
// This should be the base class for any "SVT object".                                                   //
// For instance, the SVT pedestals (that inherits from StSvtHybridCollection)                            //
// is a collection of hybrid pedestals (that inherits from StSvtHybridObject).                           //
//                                                                                                       //
///////////////////////////////////////////////////////////////////////////////////////////////////////////


#include <Stiostream.h>
#include "StObject.h"
#include "StSvtWaferCollection.hh"
#include "StSvtConfig.hh"
#include "StMessMgr.h"

#include "TString.h"

ClassImp(StSvtWaferCollection)

StSvtWaferCollection::StSvtWaferCollection() 
{
  // As the SVT can present various configurations, 
  // the constructor of this class has one parameter that corresponds to the configuration name.
  // So far, there are three configurations defined:
  //   "FULL": corresponds to the full detector (216 wafers or 432 hybrids);
  //   "Y1L": corresponds to the year 1 ladder (7 wafers or 14 hybrids). 
  //          They are located in barrel 3 ladder 1. The index goes from 1 to 14;
  //   "SYST": corresponds to the system test configuration. 
  //           The configuration of the system test correspond to 9 wafers (or 18 hybrids) 
  //           that corresponds to the maximum number of detectors that a Receiver board can read. 
  //           There is no meaningful barrel, ladder, wafer or hybrid number information. 
  //           The hybrid index depends on the order of the read out in the RB.

  mConfig = TString();
  mSvtConfig = NULL;
}

StSvtWaferCollection::StSvtWaferCollection(const char* config) 
{
  setConfiguration(config);
}

StSvtWaferCollection::StSvtWaferCollection(StSvtConfig* config) 
{
  setConfiguration(config);
}

StSvtWaferCollection::~StSvtWaferCollection()
{
  //delete mSvtConfig;
}

void StSvtWaferCollection::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);
  mSvtConfig = new StSvtConfig();
  mSvtConfig->setConfiguration(config);

  clear();
  resize(mSvtConfig->getTotalNumberOfHybrids()/2);
}

void StSvtWaferCollection::setConfiguration(StSvtConfig* config)
{
  // set the Collection configuration

  mSvtConfig = config;
  mConfig = TString(mSvtConfig->getConfiguration());

  clear();
  resize(mSvtConfig->getTotalNumberOfHybrids()/2);
}

int StSvtWaferCollection::getWaferIndex(int barrelID, int ladderID, int waferID)
{
  // returns an internal index for the specified wafer. 
  // This index should be used to store/retrieve a specific wafer in/from the collection.
  // Or one can use the getObject method which parameters are the barrel, ladder, wafer and hybrid numbers.

  int index;

  if (mSvtConfig) {
    index = (int)mSvtConfig->getHybridIndex(barrelID, ladderID, waferID, 1)/2;
    return index;
  }

  return -1;
}

/*
StSvtHybridObject* StSvtWaferCollection::At(int index)
{
  return (StSvtHybridObject*)at(index);
}

void StSvtWaferCollection::AddAt(StSvtHybridObject* object, int index)
{
  put_at((TObject*)object,index);
}
*/

StSvtHybridObject* StSvtWaferCollection::getObject(int barrelID, int ladderID, int waferID)
{
  // Method to retrieve an object (StSvtHybridObject) of the collection using the barrel, ladder, wafer numbers.

  int index = getWaferIndex(barrelID, ladderID, waferID);

  if (index<0) return 0;

  //return (StSvtHybridObject*)At(index);
  return (StSvtHybridObject*)at(index);
}

int StSvtWaferCollection::getNumberOfBarrels() {return mSvtConfig->getNumberOfBarrels();}
int StSvtWaferCollection::getNumberOfLadders(int barrel) {return mSvtConfig->getNumberOfLadders(barrel);}
int StSvtWaferCollection::getNumberOfWafers(int barrel)  {return mSvtConfig->getNumberOfWafers(barrel);}
int StSvtWaferCollection::getTotalNumberOfWafers() {return (int)mSvtConfig->getTotalNumberOfHybrids()/2;}
const char* StSvtWaferCollection::getConfiguration(){return mConfig.Data();}

