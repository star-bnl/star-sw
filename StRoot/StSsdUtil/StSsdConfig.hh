/*!
 * \file StSsdConfig.hh
 */
/***************************************************************************
 *
 * $Id: StSsdConfig.hh,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * Author: christelle Roy
 ***************************************************************************
 *
 * Description: SSD Geometry object. It makes the link with the Data Base
 *
 **************************************************************************/

#ifndef STSSDCONFIG_HH
#define STSSDCONFIG_HH

#define MAX_NUMBER_OF_BARRELS 1                                        /*! \def MAX_NUMBER_OF_BARRELS */

#include "StObject.h"
#include "TString.h"

class StSsdConfig: public StObject
{
public:
  StSsdConfig();
  virtual ~StSsdConfig();

  StSsdConfig(const StSsdConfig&);
  StSsdConfig& operator = (const StSsdConfig&);

  int getNumberOfBarrels() {return mNumberOfBarrels;}
  int getNumberOfLadders(int barrel) {return mNumberOfLadders[barrel-1];}
  int getNumberOfWafers(int barrel)  {return mNumberOfWafers[barrel-1];}
  int getNumberOfHybrids() {return mNumberOfHybrids;}
  int getTotalNumberOfHybrids() {return mTotalNumberOfHybrids;}

  int getNumberOfStrips() {return mNumberOfStrips;}
  int getNumberOfTimeBins() {return mNumberOfTimeBins;}

  void setNumberOfBarrels(int barrels) {mNumberOfBarrels = barrels;}
  void setNumberOfLadders(int barrel, int ladders) {mNumberOfLadders[barrel-1] = ladders;}
  void setNumberOfWafers(int barrel, int wafers)  {mNumberOfWafers[barrel-1] = wafers;}
  void setNumberOfHybrids(int hybrids) {mNumberOfHybrids = hybrids;}
  void setTotalNumberOfHybrids(int hybrids) {mTotalNumberOfHybrids = hybrids;}

  void setNumberOfStrips(int strips) {mNumberOfStrips = strips;}
  void setNumberOfTimeBins(int timeBins) {mNumberOfTimeBins = timeBins;}

  void setConfiguration(); // set the SSD configuration
  void setConfiguration(const char* config); // set the SSD configuration

  int getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID);
  int getProperHybridIndex(int barrelID, int ladderID, int waferID, int hybridID);
  int getBarrel(int index);
  int getLayer(int index);
  int getLadder(int index);
  int getWafer(int index);
  int getHybrid(int index);
  
  const char* getConfiguration(); // Returns the SSD configuration

protected:
  int mNumberOfBarrels;                        // Number of Barrels
  int mNumberOfLadders[MAX_NUMBER_OF_BARRELS]; // Number of Ladders of each Barrel
  int mNumberOfWafers[MAX_NUMBER_OF_BARRELS];  // Number of Wafers of each Ladder (Barrel dependent)
  int mNumberOfHybrids;        // Number of Hybrids of each Wafer 
  
  int mTotalNumberOfHybrids;   // Total Number of Hybrids (entire SSD)
  
  int mNumberOfStrips;   // Number of Strips in one hybrid
  int mNumberOfTimeBins; // Number of Time Bins in one hybrid

  TString mConfig;       // SSD Configuration 

  ClassDef(StSsdConfig,1)
};

#endif
