
#include "StDevRow.hh"

#include "StTpcLocalHit_recon.hh"
#include "StTpcLocalHit_mc.hh"



//________________________
StDevRow::StDevRow(){
  /* noop */
}

//________________________
StDevRow::~StDevRow(){
  /* noop */
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
