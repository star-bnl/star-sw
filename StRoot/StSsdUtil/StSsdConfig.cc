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
  if  ((totLadderPresent == 1))  // LADDER 0
    mConfig = TString("LAD0");
  else if (totLadderPresent == 10) // HALF SSD
    mConfig = TString("HALF");
  else if (totLadderPresent == 20) // FULL SSD
    mConfig = TString("FULL");
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
    if ( !strncmp(config, "FULL", strlen("FULL")) )
      {
	setNumberOfLadders(20);
	setNumberOfWafers(320);
	setNumberOfHybrids(2);
	setTotalNumberOfHybrids(640);
      }
    else
      gMessMgr->Message("Configuration of SSD not defined! It must be LAD0 or HALF or FULL ","E");
  
}

const char* StSsdConfig::getConfiguration()
{
  return mConfig.Data();
}


