#include <string.h>
#include <assert.h>
#include "StvTrackFitter.h"
ClassImp(StvTrackFitter)
StvTrackFitter *StvTrackFitter::fgInst=0;
//______________________________________________________________________________
StvTrackFitter::StvTrackFitter(const char *name):TNamed(name,"")
{
  assert(!fgInst);
  fgInst=this;
  Clear();
}
//______________________________________________________________________________
StvTrackFitter::~StvTrackFitter()
{assert(this==fgInst); fgInst=0;}

//______________________________________________________________________________
void StvTrackFitter::Clear(const char *)
{
  memset(mBeg,0,mEnd-mBeg+1);
  mXi2=3e33;
}
