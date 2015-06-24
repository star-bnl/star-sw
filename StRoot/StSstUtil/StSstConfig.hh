//$Id: StSstConfig.hh,v 1.2 2015/06/24 17:37:21 smirnovd Exp $
//
//$Log: StSstConfig.hh,v $
//Revision 1.2  2015/06/24 17:37:21  smirnovd
//StSstUtil: Prepend included headers with path to submodule
//
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTCONFIG_HH
#define STSSTCONFIG_HH

#define MaxNumberOfLadders 20

#include "St_base/StObject.h"
#include "TString.h"

class ssdConfiguration_st;

class StSstConfig: public StObject
{
 private:
   StSstConfig(const StSstConfig&);
   StSstConfig& operator = (const StSstConfig&);
public:
  StSstConfig();
  virtual ~StSstConfig();


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

  ClassDef(StSstConfig,1)
};

#endif
