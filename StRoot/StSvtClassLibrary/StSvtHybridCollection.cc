/***************************************************************************
 *
 * $Id: StSvtHybridCollection.cc,v 1.6 2001/02/18 00:10:28 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridCollection.cc,v $
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


#include <iostream.h>
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
  delete mSvtConfig;
}

void StSvtHybridCollection::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);
  mSvtConfig = new StSvtConfig();
  mSvtConfig->setConfiguration(config);

  resize(mSvtConfig->getTotalNumberOfHybrids());
  clear();
}

void StSvtHybridCollection::setConfiguration(StSvtConfig* config)
{
  // set the Collection configuration

  mSvtConfig = config;
  mConfig = TString(mSvtConfig->getConfiguration());

  resize(mSvtConfig->getTotalNumberOfHybrids());
  clear();
}

int StSvtHybridCollection::getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  // returns an internal index for the specified hybrid. 
  // This index should be used to store/retrieve a specific hybrid in/from the collection.
  // Or one can use the getObject method which parameters are the barrel, ladder, wafer and hybrid numbers.

  //if (mSvtConfig)
  //  return mSvtConfig->getHybridIndex(barrelID, ladderID, waferID, hybridID);

  int index;
  int mNumberOfBarrels;                        // Number of Barrels
  int mNumberOfLadders[MAX_NUMBER_OF_BARRELS]; // Number of Ladders of each Barrel
  int mNumberOfWafers[MAX_NUMBER_OF_BARRELS];  // Number of Wafers of each Ladder (Barrel dependent)
  int mNumberOfHybrids;                        // Number of Hybrids of each Wafer ( = 2)

  mNumberOfBarrels = mSvtConfig->getNumberOfBarrels();

  for (int i = 0;i < mNumberOfBarrels;i++) {
    mNumberOfLadders[i] = mSvtConfig->getNumberOfLadders(i+1);
    mNumberOfWafers[i] = mSvtConfig->getNumberOfWafers(i+1);
  }
  mNumberOfHybrids = mSvtConfig->getNumberOfHybrids();


  switch  (barrelID) {
    
  case 1:
    index = (ladderID-1)*mNumberOfWafers[barrelID-1]*mNumberOfHybrids + (waferID-1)*mNumberOfHybrids + (hybridID-1);
    break;
    
  case 2:
    index = mNumberOfLadders[barrelID-2]*mNumberOfWafers[barrelID-2]*mNumberOfHybrids +
      (ladderID-1)*mNumberOfWafers[barrelID-1]*mNumberOfHybrids + (waferID-1)*mNumberOfHybrids + (hybridID-1);
    break;
    
  case 3:
    index = mNumberOfLadders[barrelID-2]*mNumberOfWafers[barrelID-2]*mNumberOfHybrids + 
      mNumberOfLadders[barrelID-3]*mNumberOfWafers[barrelID-3]*mNumberOfHybrids +
      (ladderID-1)*mNumberOfWafers[barrelID-1]*mNumberOfHybrids + (waferID-1)*mNumberOfHybrids + (hybridID-1);
    break;
    
  default:
    gMessMgr->Error() << "There is NO barrel number " << barrelID << " !!!";
    gMessMgr->Print();
    index = -1;
    break;
  }

  if ( !strncmp(mConfig, "SYST", strlen("SYST")) ) {
    if      ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 1)) index = 0;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 2)) index = 1;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 1)) index = 2;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 2)) index = 3;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 1)) index = 4;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 2)) index = 5;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 1)) index = 6;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 2)) index = 7;    
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 4) && (hybridID == 1)) index = 4;
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 4) && (hybridID == 2)) index = 5;
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 3) && (hybridID == 1)) index = 6;
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 3) && (hybridID == 2)) index = 7;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 7) && (hybridID == 1)) index = 8;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 7) && (hybridID == 2)) index = 9;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 6) && (hybridID == 1)) index = 10;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 6) && (hybridID == 2)) index = 11;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 5) && (hybridID == 1)) index = 16;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 5) && (hybridID == 2)) index = 17;
    
    else index = -1;
  }
  else if ( !strncmp(mConfig, "Y1L", strlen("Y1L")) ) {
    if      ((barrelID == 3) && (ladderID == 2) && (waferID == 1) && (hybridID == 1)) index = 0;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 1) && (hybridID == 2)) index = 1;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 2) && (hybridID == 1)) index = 2;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 2) && (hybridID == 2)) index = 3;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 3) && (hybridID == 1)) index = 4;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 3) && (hybridID == 2)) index = 5;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 1)) index = 6;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 2)) index = 7;    
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 1)) index = 8;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 2)) index = 9;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 1)) index = 10;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 2)) index = 11;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 1)) index = 12;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 2)) index = 13;

    else index = -1;
  }

  return index;
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

