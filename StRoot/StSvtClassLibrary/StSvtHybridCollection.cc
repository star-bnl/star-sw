/***************************************************************************
 *
 * $Id: StSvtHybridCollection.cc,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridCollection.cc,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
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
#include "StMessMgr.h"

ClassImp(StSvtHybridCollection)

StSvtHybridCollection::StSvtHybridCollection(char* config) : 
  StObjArray(1)
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

  if (config)  
    setConfiguration(config);
  else {
    mNumberOfBarrels = 0;
    for (int barrel = 0;barrel<MAX_NUMBER_OF_BARRELS;barrel++) {
      mNumberOfLadders[barrel] = 0;
      mNumberOfWafers[barrel] = 0;
    }
    mNumberOfHybrids = 0;

    mTotalNumberOfHybrids = 0;
  }
}

StSvtHybridCollection::~StSvtHybridCollection()
{}


void StSvtHybridCollection::setConfiguration(char* config)
{
  // set the Collection configuration

  fConfig = config;
 
  if ( !strncmp(config, "ASCII", strlen("ASCII")) ) {
    mNumberOfBarrels = 1;
    mNumberOfLadders[0] = 1;
    mNumberOfWafers[0] = 1;
    mNumberOfHybrids = 2;
    mTotalNumberOfHybrids =2;    
   }

  else if ( !strncmp(config, "SYST", strlen("SYST")) ) {
    mNumberOfBarrels = 3;
    mNumberOfLadders[0] = 8;
    mNumberOfLadders[1] = 12;
    mNumberOfLadders[2] = 16;
    mNumberOfWafers[0] = 4;
    mNumberOfWafers[1] = 6;
    mNumberOfWafers[2] = 7;
    mNumberOfHybrids = 2;
    mTotalNumberOfHybrids = 18;
  }
  else if ( !strncmp(config, "Y1L", strlen("Y1L")) ) {
    mNumberOfBarrels = 3;
    mNumberOfLadders[0] = 0;
    mNumberOfLadders[1] = 0;
    mNumberOfLadders[2] = 1;
    mNumberOfWafers[0] = 0;
    mNumberOfWafers[1] = 0;
    mNumberOfWafers[2] = 7;
    mNumberOfHybrids = 2;
    mTotalNumberOfHybrids = 14;
  }
  else if ( !strncmp(config, "FULL", strlen("FULL")) ) {
    mNumberOfBarrels = 3;
    mNumberOfLadders[0] = 8;
    mNumberOfLadders[1] = 12;
    mNumberOfLadders[2] = 16;
    mNumberOfWafers[0] = 4;
    mNumberOfWafers[1] = 6;
    mNumberOfWafers[2] = 7;
    mNumberOfHybrids = 2;
    mTotalNumberOfHybrids = 432;
  }
  else
    gMessMgr->Message("Configuration of SVT not defined! It must be SYST, Y1L or FULL","E");

  Expand(mTotalNumberOfHybrids);
}

int StSvtHybridCollection::getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  // returns an internal index for the specified hybrid. 
  // This index should be used to store/retrieve a specific hybrid in/from the collection.
  // Or one can use the getObject method which parameters are the barrel, ladder, wafer and hybrid numbers.

  int index;


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

  if ( !strncmp(fConfig, "SYST", strlen("SYST")) ) {
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
  else if ( !strncmp(fConfig, "Y1L", strlen("Y1L")) ) {
    if      ((barrelID == 3) && (ladderID == 1) && (waferID == 1) && (hybridID == 1)) index = 0;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 1) && (hybridID == 2)) index = 1;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 2) && (hybridID == 1)) index = 2;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 2) && (hybridID == 2)) index = 3;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 3) && (hybridID == 1)) index = 4;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 3) && (hybridID == 2)) index = 5;
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

StHybridObject* StSvtHybridCollection::getObject(int barrelID, int ladderID, int waferID, int hybridID)
{
  // Method to retrieve an object (StHybridObject) of the collection using the barrel, ladder, wafer and hybrid numbers.

  int index = getHybridIndex(barrelID, ladderID, waferID, hybridID);

  return (StHybridObject*)At(index);
}
