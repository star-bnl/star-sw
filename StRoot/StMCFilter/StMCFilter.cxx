// @(#)root/eg:$Id: StMCFilter.cxx,v 1.1 2009/04/10 19:59:20 perev Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include "stdlib.h"
#include "ctype.h"
#include "math.h"

#include "StMCFilter.h"
#include <map>
#include "StHepParticle.h"
#include "StG3Particle.h"

typedef std::map<std::string, StMCFilter *> myMap_t;
static  myMap_t myMap;
StMCFilter      *StMCFilter::fgSelected   =0;
StHepParticles *StMCFilter::fgHepParticle =0;
StG3Particles  *StMCFilter::fgG3Particle  =0;

extern void *gStarFiltAction;

//______________________________________________________________________________
StMCFilter::StMCFilter(const char *name)
{
  memset(fBeg,0,fEnd-fBeg+1);
  fName= name;
  std::string myName(fName);
  for (int i=0;i<(int)myName.size();i++) { myName[i]=tolower(myName[i]);}


  myMap_t::iterator it = myMap.find(myName);
  assert (it == myMap.end());
  myMap[myName] = this;
  gStarFiltAction=(void*)&StMCFilter::Action;
}
//______________________________________________________________________________
StMCFilter::~StMCFilter()
{
  myMap.clear(); 
  delete fgHepParticle; fgHepParticle=0;
  delete fgG3Particle ; fgG3Particle =0;
  
}  
//______________________________________________________________________________
int StMCFilter::Select(const char *name)
{
  assert(!fgSelected);
  std::string myName(name);
  for (int i=0;i<(int)myName.size();i++) { myName[i]=tolower(myName[i]);}
  myMap_t::iterator it = myMap.find(std::string(myName));
  assert (it != myMap.end());
  if (it == myMap.end()) return 1;
  fgSelected = (*it).second;
  return 0;
}
//______________________________________________________________________________
void StMCFilter::SetEG(void *hepEvt)
{
  assert(!fgHepParticle);
  fgHepParticle = new StHepParticles(hepEvt);
}
//______________________________________________________________________________
void StMCFilter::SetG3(void *gfKine,void *gfVert)
{
  assert(!fgG3Particle);
  fgG3Particle = new StG3Particles((GFKINE_t)gfKine,(GFVERT_t)gfVert);
}
//______________________________________________________________________________
int StMCFilter::REJECTEG()
{
  if (!fgSelected || !fgHepParticle) return 0;
  fgSelected->fCnt[0][0]++;
  fgHepParticle->Update();
  int ans =fgSelected->RejectEG(*fgHepParticle);
  if (ans) fgSelected->fCnt[0][1]++;
  return ans;
}
//______________________________________________________________________________
int StMCFilter::REJECTGT()
{
  if (!fgSelected || !fgG3Particle) return 0;
  fgSelected->fCnt[1][0]++;
  fgG3Particle->Update();
  int ans = fgSelected->RejectGT(*fgG3Particle);
  if (ans) fgSelected->fCnt[1][1]++;
  return ans;
}
//______________________________________________________________________________
int StMCFilter::REJECTGE()
{
  
  if (!fgSelected || !fgG3Particle) return 0;
  fgG3Particle->Update();
  fgSelected->fCnt[2][0]++;
  int ans = fgSelected->RejectGE(*fgG3Particle);
  if (ans) fgSelected->fCnt[2][1]++;
  return ans;
}
//______________________________________________________________________________
int  StMCFilter::Action(int kase, void *par1,void *par2)
{
  enum { kSelect=0,kEGInit=1, kEGReje=2
                  ,kGTInit=3, kGTReje=4
		  ,kGEInit=5, kGEReje=6,kFinish=7};

  switch (kase) {
  
    case kSelect: return StMCFilter::Select((char*)par1);

    case kEGInit: StMCFilter::SetEG(par1     ); 	return 0;
    case kGTInit: StMCFilter::SetG3(par1,par2); 	return 0;
    case kGEInit: 					return 0;

    case kEGReje: return StMCFilter::REJECTEG();
    case kGTReje: return StMCFilter::REJECTGT();
    case kGEReje: return StMCFilter::REJECTGE();
    case kFinish: StMCFilter::FINISH(); return 0;
    default: assert(0 && "StMCFilter::Action Wrong case");
  }
  return 0;
}    
//______________________________________________________________________________
void StMCFilter::FINISH()
{
static const char *filtName[] = {"RejectEG","RejectGT","RejectGE"};
  if (!fgSelected) return;
  printf("*** Filter Finish(%s) ***\n",fgSelected->GetName().c_str());
  for (int i=0; i<3; i++) 
  {
    if (!fgSelected->fCnt[i][0]) continue;
    printf ("*** Filter %s nTot=%d \tnRej=%d\n",filtName[i]
           ,fgSelected->fCnt[i][0],fgSelected->fCnt[i][1]);
  }
  fgSelected->Finish();
}


  
