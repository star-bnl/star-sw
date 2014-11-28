/*!
 * \file StSsdConfig.hh
 */
/***************************************************************************
 * Author: christelle Roy
 * Description: SSD Geometry object. It makes the link with the Data Base
 **************************************************************************/

#ifndef STSSDCONFIG_HH
#define STSSDCONFIG_HH

#define MaxNumberOfLadders 20

#include "StObject.h"
#include "TString.h"

class ssdConfiguration_st;

class StSsdConfig: public StObject
{
 private:
   StSsdConfig(const StSsdConfig&);
   StSsdConfig& operator = (const StSsdConfig&);
public:
  StSsdConfig();
  virtual ~StSsdConfig();


  int getNumberOfLadders() {return mNumberOfLadders;}
  int getNumberOfWafers()  {return mNumberOfWafers;}
  int getNumberOfHybrids() {return mNumberOfHybrids;}
  int getTotalNumberOfLadders() {return mTotalNumberOfLadders;}
  int getTotalNumberOfHybrids() {return mTotalNumberOfHybrids;}
  int getNumberOfStrips() {return mNumberOfStrips;}

  void setNumberOfLadders(int ladders){mNumberOfLadders = ladders;}
  void setNumberOfWafers(int wafers)  {mNumberOfWafers = wafers;}
  void setNumberOfHybrids(int hybrids) {mNumberOfHybrids = hybrids;}
  void setTotalNumberOfHybrids(int hybrids) {mTotalNumberOfHybrids = hybrids;}
  void setTotalNumberOfLadders(int totladders) {mTotalNumberOfLadders = totladders;}
  void setNumberOfStrips(int strips) {mNumberOfStrips = strips;}

  void setConfiguration(); // set the SSD configuration
  void setConfiguration(const char* config); // set the SSD configuration

  void setLadderIsActive(int ladder, int status){mStatus[ladder-1] = status;} 
  int  getLadderIsActive(int ladder){return mStatus[ladder-1];}

  const char* getConfiguration(); // Returns the SSD configuration

protected:
  int mStatus[MaxNumberOfLadders]; 
  int totLadderPresent;        // Number of Present Ladders //////////// A MODIFIER PLUS TARD
  int mNumberOfLadders;        // Number of Present Ladders 
  int mNumberOfWafers;         // Number of Wafers of each Ladder (Barrel dependent)
  int mNumberOfHybrids;        // Number of Hybrids of each Wafer 
  int mTotalNumberOfLadders;   // Total Number of Ladders (entire SSD)  
  int mTotalNumberOfHybrids;   // Total Number of Hybrids (entire SSD)
  
  int mNumberOfStrips;   // Number of Strips in one hybrid

  TString mConfig;       // SSD Configuration 

  ClassDef(StSsdConfig,1)
};

#endif
