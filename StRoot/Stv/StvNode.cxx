//StvKalmanTrack.cxx
/*
 * $Id: StvNode.cxx,v 1.3 2010/08/01 00:10:48 perev Exp $
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
  mFP[2]=par;mFE[2]=err;mIsFit[2]=1;
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
static const char *txt = "XYZAPTCHRE";
static const char *hhh = "xyzre";
  if (!opt || !opt[0]) opt = "2RZErze";
  double val,err[2];
  const StvNodePars &fp= GetFP();
  const StvFitErrs  &fe= GetFE();
  StvHit *hit = GetHit();
  TString ts;
  if (hit) {ts+=(GetXi2()>1e3)? "h":"H";}
  printf("%p(%s)",(void*)this,ts.Data());
  if (strchr(opt,'2')) printf("\t%s=%g","Xi2",GetXi2());
  for (int i=0;txt[i];i++) {
    err[0]=-999;val=-999;
    if (!strchr(opt,txt[i])) continue;
    if (txt[i]=='R') 		{val = fp.getRxy();}
    else if (txt[i]=='P')	{val = fp.getPt() ;}
    else if (txt[i]=='E')	{err[0] = sqrt(fe.mHH); err[1] = sqrt(fe.mZZ);}
    else 			{val = fp[i]      ;}
    if (abs(val+999)>1e-6) 	{ printf("\t%c=%g",txt[i],val);}
    if (err[0]>-999)  		{ printf("\tHH=%7.2g ZZ=%7.2g",err[0],err[1]);}
  }//end for i

  if (hit) {
    for (int i=0;hit && hhh[i];i++) {
       err[0]=-999;val=-999;
      if (!strchr(opt,hhh[i])) continue;
      if (hhh[i]=='r') 		{val = hit->getRxy();}
      else if (hhh[i]=='e')	{err[0] = sqrt(mHrr[0]); err[1] = sqrt(mHrr[2]);}
      else 			{val = hit->x_g()[i];} 
      if (abs(val+999)>1e-6) 	{printf("\th%c=%g",hhh[i],val);}
      if (err[0]>-999)  	{ printf("\thh=%7.2g zz=%7.2g",err[0],err[1]);}
    } 
    
  
  
  
  
  }
  printf("\n");
  return;
}    
 
