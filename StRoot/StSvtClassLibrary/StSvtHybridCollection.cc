/***************************************************************************
 *
 * $Id: StSvtHybridCollection.cc,v 1.12 2004/01/27 02:27:56 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridCollection.cc,v $
 * Revision 1.12  2004/01/27 02:27:56  perev
 * LeakOff
 *
 * Revision 1.11  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.10  2001/11/12 22:55:22  caines
 * Delete mConfig when delete called to fix memory leak
 *
 * Revision 1.9  2001/11/06 18:29:34  caines
 * Add back in delete of mSvtConfig
 *
 * Revision 1.8  2001/10/04 02:56:26  caines
 * Fix some of the hybrid swapping indexing
 *
 * Revision 1.7  2001/08/16 21:02:03  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 * Revision 1.6  2001/02/18 00:10:28  caines
 * Improve and use StSvtConifg
 *
 * Revision 1.5  2000/11/30 20:39:12  caines
 * Changed to allow us of database
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
#include "StSvtHybridCollection.hh"
#include "StSvtConfig.hh"
#include "StMessMgr.h"

#include "TString.h"

ClassImp(StSvtHybridCollection)

StSvtHybridCollection::StSvtHybridCollection() 
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

StSvtHybridCollection::StSvtHybridCollection(const char* config) 
{
  setConfiguration(config);
}

StSvtHybridCollection::StSvtHybridCollection(StSvtConfig* config) 
{
  setConfiguration(config);
}

StSvtHybridCollection::~StSvtHybridCollection()
{
  if( mSvtConfig){
    delete mSvtConfig;
  }
}

void StSvtHybridCollection::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);
  mSvtConfig = new StSvtConfig();
  mSvtConfig->setConfiguration(config);
  clear();
  resize(mSvtConfig->getTotalNumberOfHybrids());
}

void StSvtHybridCollection::setConfiguration(StSvtConfig* config)
{
  // set the Collection configuration

   mConfig = TString(config->getConfiguration());
   mSvtConfig = new StSvtConfig();
   mSvtConfig->setConfiguration(mConfig);
   clear();
   resize(mSvtConfig->getTotalNumberOfHybrids());
}

int StSvtHybridCollection::getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  // returns an internal index for the specified hybrid. 
  // This index should be used to store/retrieve a specific hybrid in/from the collection.
  // Or one can use the getObject method which parameters are the barrel, ladder, wafer and hybrid numbers.

  if (mSvtConfig)
    return mSvtConfig->getHybridIndex(barrelID, ladderID, waferID, hybridID);

  return -1;
}
int StSvtHybridCollection::getProperHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
 // returns an proper index for the specified hybrid if there was no swapping

  if (mSvtConfig)
    return mSvtConfig->getProperHybridIndex(barrelID, ladderID, waferID, hybridID);

  return -1;
}
/*
StSvtHybridObject* StSvtHybridCollection::At(int index)
{
  return (StSvtHybridObject*)at(index);
}

void StSvtHybridCollection::AddAt(StSvtHybridObject* object, int index)
{
  put_at((TObject*)object,index);
}
*/
StSvtHybridObject* StSvtHybridCollection::getObject(int barrelID, int ladderID, int waferID, int hybridID)
{
  // Method to retrieve an object (StSvtHybridObject) of the collection using the barrel, ladder, wafer and hybrid numbers.

  int index = getHybridIndex(barrelID, ladderID, waferID, hybridID);

  if (index<0) return 0;

  //return (StSvtHybridObject*)At(index);
  return (StSvtHybridObject*)at(index);
}

int StSvtHybridCollection::getNumberOfBarrels() {return mSvtConfig->getNumberOfBarrels();}
int StSvtHybridCollection::getNumberOfLadders(int barrel) {return mSvtConfig->getNumberOfLadders(barrel);}
int StSvtHybridCollection::getNumberOfWafers(int barrel)  {return mSvtConfig->getNumberOfWafers(barrel);}
int StSvtHybridCollection::getNumberOfHybrids() {return mSvtConfig->getNumberOfHybrids();}
int StSvtHybridCollection::getTotalNumberOfHybrids() {return mSvtConfig->getTotalNumberOfHybrids();}
const char* StSvtHybridCollection::getConfiguration(){return mConfig.Data();}

