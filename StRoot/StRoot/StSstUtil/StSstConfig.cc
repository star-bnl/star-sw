//$Id: StSstConfig.cc,v 1.2 2016/06/06 18:48:12 bouchet Exp $
//
//$Log: StSstConfig.cc,v $
//Revision 1.2  2016/06/06 18:48:12  bouchet
//coverity : UNINIT_CTOR
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include "StSstConfig.hh"
#include "StMessMgr.h"

#include "tables/St_ssdConfiguration_Table.h"

ClassImp(StSstConfig)

StSstConfig::StSstConfig()
  :totLadderPresent(-1956)      // Number of Present Ladders 
  ,mNumberOfLadders(-1956)      // Number of Present Ladders 
  ,mNumberOfWafers(-1956)       // Number of Wafers of each Ladder (Barrel dependent)
  ,mNumberOfHybrids(-1956)      // Number of Hybrids of each Wafer 
  ,mTotalNumberOfLadders(-1956) // Total Number of Ladders (entire SSD)  
  ,mTotalNumberOfHybrids(-1956) // Total Number of Hybrids (entire SSD)
  ,mNumberOfStrips(-1956)      // Number of Strips in one hybrid

{ memset(mStatus,-1,sizeof(mStatus));   }

StSstConfig::~StSstConfig()
{}

StSstConfig::StSstConfig(const StSstConfig& geom){
  totLadderPresent =  geom.mNumberOfLadders;
  mNumberOfLadders = geom.mNumberOfLadders;
  mNumberOfWafers = geom.mNumberOfWafers;
  mNumberOfHybrids = geom.mNumberOfHybrids;
  mNumberOfStrips = geom.mNumberOfStrips;
  mTotalNumberOfLadders = geom.mTotalNumberOfLadders;
  mTotalNumberOfHybrids = geom.mTotalNumberOfHybrids;
}

StSstConfig& StSstConfig::operator = (const StSstConfig& geom)
{
  return *this;
}

void StSstConfig::setConfiguration()
{
  if  ((totLadderPresent == 1))  // LADDER 0
    mConfig = TString("LAD0");
  else if (totLadderPresent == 10) // HALF SSD
    mConfig = TString("HALF");
  else if (totLadderPresent == 20) // FULL SSD
    mConfig = TString("FULL");
  else
    mConfig = TString("NULL");
}

void StSstConfig::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);

  if ( !strncmp(config, "HALF", strlen("HALF")) ) {
     setNumberOfLadders(10);
     setNumberOfWafers(160);
     setNumberOfHybrids(2);
     setTotalNumberOfHybrids(320);
  } else if ( !strncmp(config, "FULL", strlen("FULL")) ) {
     setNumberOfLadders(20);
     setNumberOfWafers(320);
     setNumberOfHybrids(2);
     setTotalNumberOfHybrids(640);
  } else {
     LOG_ERROR << "Configuration of SSD not defined! It must be LAD0 or HALF or FULL "<< endm;
  }
}

const char* StSstConfig::getConfiguration()
{
  return mConfig.Data();
}


