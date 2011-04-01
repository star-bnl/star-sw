#if 0
// $Id: StMCHits.cxx,v 1.1 2011/04/01 18:55:17 perev Exp $
//
//
// Class StMCHits
// ------------------

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "TError.h"
#include "TObjArray.h"
#include "StMCHits.h"
#include "StTGeant3.h"

ClassImp(StMCHit)
//_____________________________________________________________________________
StMCHit::StMCHit(ULong64_t U)
{
   fID = U;
   memset(fXYZ,0,(char*)&fELoss-(char*)fXYZ+sizeof(fELoss));
}
//_____________________________________________________________________________
static ULong64_t Reverse(ULong64_t in)
{
   ULong64_t ot=0;
   while (in) {ot = (ot<<1) | (in&1); in>>=1;}
   return ot;
}

//_____________________________________________________________________________
Int_t StMCHit::Compare(const TObject *obj) const
{
   StMCHit *that = (StMCHit*)obj; 
   if (fID == that->fID) return 0;
   ULong64_t me = Reverse(fID);
   ULong64_t he = Reverse(that->fID);
   return (me<he) ? -1:1;
}
//_____________________________________________________________________________
void StMCHit::GetXYZ (Double_t *xyz) const
{
   memcpy(xyz,fXYZ,sizeof(fXYZ));
}
//_____________________________________________________________________________
void StMCHit::GetDif (Double_t *dif) const
{
   for (int i;i<3;i++) dif[i]=fDif[i];
}
//_____________________________________________________________________________
Double_t  StMCHit::GetStep() const
{
   return 2*sqrt(fDif[0]*fDif[0]+fDif[1]*fDif[1]+fDif[2]*fDif[2]);
}
//_____________________________________________________________________________
void StMCHit::GetDir(Double_t *dir) const
{
   double stp = GetStep();
   for (int i;i<3;i++) dir[i]=fDif[i]/stp;
}
//_____________________________________________________________________________
void StMCHit::GetBeg (Double_t *beg) const
{
   for (int i;i<3;i++) beg[i]=fXYZ[i]-fDif[i];
}
//_____________________________________________________________________________
void StMCHit::GetEnd (Double_t *end) const
{
   for (int i;i<3;i++) end[i]=fXYZ[i]+fDif[i];
}


//_____________________________________________________________________________
void StMCHit::SetBegEnd (Double_t *beg,Double_t *end)
{
   for (int i=0;i<3;i++) {
      fXYZ[i] =         0.5*(beg[i]+end[i]);
      fDif[i] = (float) 0.5*(end[i]-beg[i]);
   }
}




ClassImp(StMCHits)

//_____________________________________________________________________________
StMCHits::StMCHits(const char *name,const char *tit)
  : TNamed(name,tit)
{
   fHits = new TObjArray(100);
}

#endif
