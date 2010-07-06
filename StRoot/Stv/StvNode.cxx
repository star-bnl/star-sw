//StvKalmanTrack.cxx
/*
 * $Id: StvNode.cxx,v 1.1 2010/07/06 20:27:43 perev Exp $
 *
 * /author Victor Perev
 */

#include <Stiostream.h>
#include <math.h>
#include <stdio.h>

#include "Stv/StvNode.h"
#include "Stv/StvHit.h"
#include "TString.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"


//______________________________________________________________________________
void StvNode::reset()
{ 
static int myCount=0;
  memset(mBeg,0,mEnd-mBeg+1);
  mId = ++myCount; 
  mXi2 = 3e33;
}
//______________________________________________________________________________
void StvNode::SetPre(StvNodePars &par,StvFitErrs &err,int dir) 	
{
  mPP[dir]=par;mPE[dir]=err;
  if (mIsFit[2]) return;
  mFP[2]=par;mPE[2]=err;mIsFit[2]=1;
}
//______________________________________________________________________________
void StvNode::SetFit(StvNodePars &par,StvFitErrs &err,int dir) 	
{
  mFP[dir]=par;mFE[dir]=err;  mIsFit[dir]=2;
  if (dir==2 || mIsFit[2]==2) return;
  mFP[2]=par;mFE[2]=err;mIsFit[2]=2;
}
//______________________________________________________________________________
StDetectorId StvNode::GetDetId() const
{
  const StHitPlane *hp = GetHitPlane(); if (!hp) return kUnknownId;
  return hp->GetDetId();
} 
//________________________________________________________________________________
double StvNode::GetTime() const
{
  return 0;
} 
//________________________________________________________________________________
void StvNode::Print(const char *opt) const
{
static const char *txt = "XYZEPTCHR";
static const char *hhh = "UVWR";
static const char *HHH = "XYZR";
  if (!opt || !opt[0]) opt = "2RZ";
  double val;
  const StvNodePars &fp= GetFP();
  StvHit *hit = GetHit();
  TString ts;
  if (hit) {ts+=(GetXi2()>1e3)? "h":"H";}
  printf("%p(%s)",(void*)this,ts.Data());
  if (strchr(opt,'2')) printf("\t%s=%g","Xi2",GetXi2());

  for (int i=0;txt[i];i++) {
    if (!strchr(opt,txt[i])) continue;
    if (txt[i]=='R') 		{val = fp.getRxy();}
    else if (txt[i]=='P')	{val = fp.getPt() ;}
    else 			{val = fp[i]      ;}
    printf("\t%c=%g",txt[i],val);
//    if (err) printf("(%6.1g)",err);
  }//end for i

  if (hit) {
    for (int i=0;hit && hhh[i];i++) {
      if (!strchr(opt,hhh[i])) continue;
      if (HHH[i]=='R') 		{val = hit->getRxy();}
      else 			{val = hit->x_g()[i];} 
      printf("\th%c=%g",HHH[i],val);
  } }
  printf("\n");
  return;
}    
 
