/***************************************************************************
 * author: christelle Roy
 * Description: SSD Config
 **************************************************************************/

#include "StSsdConfig.hh"
#include "StMessMgr.h"

#include "tables/St_ssdConfiguration_Table.h"

ClassImp(StSsdConfig)

StSsdConfig::StSsdConfig()
{}

StSsdConfig::~StSsdConfig()
{}

StSsdConfig::StSsdConfig(const StSsdConfig& geom)
{}

StSsdConfig& StSsdConfig::operator = (const StSsdConfig& geom)
{
  return *this;
}

void StSsdConfig::setConfiguration()
{
  if  ((mTotalNumberOfHybrids == 32))  // LADDER 0
    mConfig = TString("LAD0");
  else if (mTotalNumberOfHybrids == 320) // HALF SSD
    mConfig = TString("HALF");
  else if (mTotalNumberOfHybrids == 640) // FULL SSD
    mConfig = TString("SSD");
  else
    mConfig = TString("NULL");
}

void StSsdConfig::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);

  if ( !strncmp(config, "HALF", strlen("HALF")) )
    {
      setNumberOfLadders(10);
      setNumberOfWafers(160);
      setNumberOfHybrids(2);
      setTotalNumberOfHybrids(320);
    }
  else
    gMessMgr->Message("Configuration of SSD not defined! It must be LAD0 or HALF or SSD ","E");
  
}

const char* StSsdConfig::getConfiguration()
{
  return mConfig.Data();
}


