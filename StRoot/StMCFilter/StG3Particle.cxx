// @(#)root/eg:$Id: StG3Particle.cxx,v 1.6 2009/08/25 20:49:16 fine Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include <stdlib.h>
#include <math.h>
#include <vector>
#include <cassert>

#include "StG3Particle.h"
#include "StGENParticle.h"

StG3ParticleMaster *StG3ParticleMaster::mgInst =0;
GFKINE_t StG3ParticleMaster::mgFK=0; //pointer to Geant routine GFKINE
GFVERT_t StG3ParticleMaster::mgFV=0; //pointer to Geant routine GFVERT

static std::vector<StGENParticle*> myVec;

//______________________________________________________________________________
StG3ParticleMaster::StG3ParticleMaster(GFKINE_t fk,GFVERT_t fv)
{
  assert(!mgInst && "Never,Jamais");
  mgInst = this;
  mgFK = fk;
  mgFV = fv;
}
//______________________________________________________________________________
StG3ParticleMaster::~StG3ParticleMaster()
{
  mgInst = 0;
  for (int i=0;i<(int)myVec.size();i++) { delete myVec[i]; }
  myVec.resize(0);
}
//______________________________________________________________________________
const StG3ParticleMaster *StG3ParticleMaster::Instance() 
{
  assert(mgInst && "Never,Jamais");
  return mgInst;
}  
//______________________________________________________________________________
void StG3ParticleMaster::Update() 
{
//      SUBROUTINE GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NWBUF)
//      SUBROUTINE GFVERT(NVTX,V,NTBEAM,NTTARG,TOFG,UBUF,NWBUF)
  const double kC =2.99792458000000000e+10;

  int ITRA,IPART,NVERT,NWBUF,UBUF[100];
  float VERT[4],PVERT[5];
  int NVTX,NTBEAM,NTTARG;
  float V[3],TOFG;
  mNTk = 0;
  int size0 =  myVec.size();

  for (ITRA = 1; 2009; ITRA++) 
  {
    (mgFK)(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NWBUF);
    if (!IPART) break;
    mNTk = ITRA;
    NVTX = NVERT;
    TOFG = 0;NTBEAM=0,NTTARG=0;
    if (NVTX) {
      (mgFV)(NVTX,V,NTBEAM,NTTARG,TOFG,UBUF,NWBUF);
    }
    if (ITRA > size0) {myVec.push_back(new StGENParticle(ITRA-1));}
    else              {myVec[ITRA-1]->Clear()			 ;}
    StGENParticle *tk = myVec[ITRA-1];
    tk->Clear();
    VERT[3] = TOFG*kC;
    PVERT[4] = Gea2Mas(IPART);
    tk->SetGea(IPART);
    tk->SetPdg(Gea2Pdg(IPART));
    tk->SetVert(VERT);
    tk->SetMom(PVERT);
    if (!NVTX) continue;
    if (NTBEAM) {
      assert(NTBEAM<ITRA && "Never,Jamais");
      tk->SetMother(0,myVec[NTBEAM-1]);
      myVec[NTBEAM-1]->AddDaughter(tk);
    }
    if (NTTARG>0 && NTTARG<ITRA) tk->SetMother(1,myVec[NTTARG-1]);
  } 

}
//______________________________________________________________________________
const StGenParticle *StG3ParticleMaster::operator()(int idx) const
{
  assert(idx>=0 && "Never,Jamais");
  if (idx >= mNTk) 			return 0;
  if (!myVec[idx]->GetStatusCode()) 	return 0;
  return myVec[idx];
}

