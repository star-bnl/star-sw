/***************************************************************************
 *
 * $Id: StDevRow.cc,v 1.4 1999/10/18 16:11:52 calderon Exp $
 * $Log: StDevRow.cc,v $
 * Revision 1.4  1999/10/18 16:11:52  calderon
 * Frank found 2 leaks that these changes will correct:
 * -Delete the TrackPairInfos in the Clear() method
 * -Correct the sub detector destructors to delete all
 *  instances to StLocalHit.
 *
 * Revision 1.3  1999/09/23 21:25:19  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/

#include "StDevRow.hh"

#include "StTpcLocalHit_recon.hh"
#include "StTpcLocalHit_mc.hh"



//________________________
StDevRow::StDevRow(){
  /* noop */
}

//________________________
StDevRow::~StDevRow(){
    for (unsigned int i=0; i<localHits.size(); i++) delete localHits[i];
    localHits.clear();
}

//________________________
void StDevRow::addHit(const StTpcHit* hit, float xLocal, float zGlobal){
  StTpcLocalHit_recon* h = new StTpcLocalHit_recon(hit,xLocal,zGlobal);
  localHits.push_back(h);
  
}

//________________________
void StDevRow::addHit(const StMcTpcHit* hit, float xLocal, float zGlobal){
  StTpcLocalHit_mc* h = new StTpcLocalHit_mc(hit,xLocal,zGlobal);
  localHits.push_back(h);
}

//________________________
