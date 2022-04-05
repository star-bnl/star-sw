#include <string.h>
#include <assert.h>
#include "StvTrackFitter.h"
ClassImp(StvTrackFitter)
//______________________________________________________________________________
StvTrackFitter::StvTrackFitter(const char *name):TNamed(name,"")
{
  Clear();
}
//______________________________________________________________________________
StvTrackFitter::~StvTrackFitter()
{;}

//______________________________________________________________________________
void StvTrackFitter::Clear(const char *)
{
  memset(mBeg,0,mEnd-mBeg+1);
  mXi2=3e33;
}
