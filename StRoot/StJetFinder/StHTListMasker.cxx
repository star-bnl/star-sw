/***************************************************************************
 *
 * $Id: StHTListMasker.cxx,v 1.1 2004/03/25 19:03:50 thenry Exp $
 * 
 * Author: Thomas Henry March 2004
 ***************************************************************************
 *
 * Description:  Masker (Maker) which masks out hot towers.
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/02/20 thenry
 *
 **************************************************************************/
#include <string>
#include <iostream>
#include <math.h>
#include <sys/times.h>

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StSpinMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StEmcTpcFourPMaker.h"
#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"

#include "StHTListMasker.h"

ClassImp(StHTListMasker)
  
StHTListMasker::StHTListMasker(const char* name,
				   const char* listName) : 
  SafetyArray(name), HTListName(listName) 
{
  ifstream HotTowerFile(HTListName.c_str());
  string line;
  while(HotTowerFile.good())
    {
      getline(HotTowerFile, line);
      if(line.size() != 0)
        {
           string::size_type colpos = line.find(':');
           if(colpos == string::npos)
             {
               badTowers.insert(static_cast<unsigned int>(atol(line.c_str())));
             } 
           else
             {
               string::size_type starpos = line.find('*');
               string::size_type rstarpos = line.rfind('*');
               line[colpos] = 0;
               unsigned int towerId = 0;
               if((starpos > colpos) || (starpos == string::npos)) 
		 towerId = static_cast<unsigned int>(atol(line.c_str()));
               string runNumberStr = line.substr(colpos+1);
               long runNumber = 0;
               if((rstarpos < colpos) || (rstarpos == string::npos))
                 {
                   runNumber = atol(runNumberStr.c_str());
                   badRunTowers[runNumber].insert(towerId);
                 }
               else
                 {
                   if(towerId != 0)
                     badTowers.insert(towerId);
                 }
             }
        }
    }
    HotTowerFile.close();
    cout << "Hot Towers Found: " << endl;
    for(towerSet::iterator it = badTowers.begin(); it != badTowers.end(); ++it)
      cout << *it << endl;
    cout << "Run Number Non-Trivial: " << endl;
    for(runSetMap::iterator it = badRunTowers.begin(); it != badRunTowers.end(); ++it)
      cout << (*it).first << endl;
}

Int_t StHTListMasker::Make()
{
  return kStOK;
}

Int_t StHTListMasker::Init()
{
  return kStOK;
}

Int_t StHTListMasker::Finish()
{
  return kStOK;
}

bool StHTListMasker::isGood(unsigned int runNumber, long index)
{
  if(badTowers.find(index) != badTowers.end())
    return false;
  runSetMap::iterator run = badRunTowers.find(runNumber);
  if(run != badRunTowers.end())
    {
      towerSet &runSet = (*run).second;
      if(runSet.find(0) != runSet.end())
        return false;
      if(runSet.find(index) != runSet.end())
        return false;
    }
  return true;
}







