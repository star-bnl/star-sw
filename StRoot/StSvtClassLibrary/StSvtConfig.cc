/***************************************************************************
 *
 * $Id: StSvtConfig.cc,v 1.1 2000/11/30 20:38:50 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Config
 *
 ***************************************************************************
 *
 * $Log: StSvtConfig.cc,v $
 * Revision 1.1  2000/11/30 20:38:50  caines
 * Svt configuration files
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT Config object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////


#include "StSvtConfig.hh"
#include "StMessMgr.h"

ClassImp(StSvtConfig)

StSvtConfig::StSvtConfig()
{}

StSvtConfig::~StSvtConfig()
{}

StSvtConfig::StSvtConfig(const StSvtConfig& geom)
{}

StSvtConfig& StSvtConfig::operator = (const StSvtConfig& geom)
{
  return *this;
}

void StSvtConfig::setConfiguration()
{
  if (mTotalNumberOfHybrids == 18 )  // SYSTEM TEST  
    mConfig = TString("SYST");
  else if (mTotalNumberOfHybrids == 14 )  // YEAR 1 LADDER  
    mConfig = TString("Y1L");
  else if (mTotalNumberOfHybrids == 432 )  // FULL SVT  
    mConfig = TString("FULL");
}

void StSvtConfig::setConfiguration(const char* config)
{
  mConfig = TString(config);
}

const char* StSvtConfig::getConfiguration()
{
  return mConfig.Data();
}

int StSvtConfig::getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
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

  if ( mTotalNumberOfHybrids == 18 ) {  // SYSTEM TEST
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
  else if ( mTotalNumberOfHybrids == 14 ) {  // YEAR 1 LADDER
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
