#include <string.h>
#include "G3Bridge.h"

G3Bridge *G3Bridge::fBridge = 0;

//______________________________________________________________________________
G3Bridge::G3Bridge()
{
  memset(fBeg,0,fEnd-fBeg);
  fBridge = this;
}
//______________________________________________________________________________
G3Bridge *G3Bridge::Instance()
{
   return fBridge;
}
