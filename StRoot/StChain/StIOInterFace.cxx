#include "StIOInterFace.h"
//_____________________________________________________________________________
ClassImp(StIOInterFace)

//_____________________________________________________________________________
StIOInterFace::StIOInterFace(const char *name="",const char *iomode="r")
:StMaker(name)
{
  if (iomode) SetIOMode(iomode);
}  
//_____________________________________________________________________________
void StIOInterFace::SetBranch(const Char_t *brName,const Char_t *file,const Char_t *mode)
{
  TString ts("SetBranch:");

  if (file) { ts += " file="; ts += file;}
  if (mode) { ts += " mode="; ts += mode;}
  IntoBranch(brName,ts);  
}
//_____________________________________________________________________________
void StIOInterFace::IntoBranch(const Char_t *brName,const Char_t *logNames)
{
 AddAlias(brName,logNames,".branches");  
}
//_____________________________________________________________________________
//void StIOInterFace::Streamer(TBuffer &b) {};
