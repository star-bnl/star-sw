//StvKalmanTrack.cxx
/*
 * $Id: StvNode.cxx,v 1.15 2013/05/20 18:56:57 perev Exp $
 *
 * /author Victor Perev
 */

#include <Stiostream.h>
#include <math.h>
#include <stdio.h>
#include "TString.h"

#include "Stv/StvNode.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvELossTrak.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"


//______________________________________________________________________________
void StvNode::reset()
{ 
static int myCount=0;
  memset(mBeg,0,mEnd-mBeg+1);
  mId = ++myCount; 
  StvDebug::Break(mId);
  mXi2[0] = 3e33;mXi2[1] = 3e33;mXi2[2] = 3e33;
}
//______________________________________________________________________________
void StvNode::SetPre(StvNodePars &par,StvFitErrs &err,int dir) 	
{
  mPP[dir]=par;mPE[dir]=err;
  mFP[dir]=par;mFE[dir]=err;
  mFP[  2]=par;mFE[  2]=err;
}
//______________________________________________________________________________
void StvNode::SetFit(StvNodePars &par,StvFitErrs &err,int dir) 	
{
  mFP[dir]=par;mFE[dir]=err;
  if (dir==2) return;
  mFP[2]=par;mFE[2]=err;
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
static const char *txt = "X Y Z A P T C H R E L K U Xi P[ E[ ";
static const char *hhh = "x y z r e ";
  if (!opt || !opt[0]) opt = "_";
  TString myOpt(opt);myOpt+=" ";
  int dir = myOpt.Index("=");
  dir = (dir<0)? 2:myOpt[dir+1]-'0';
  int djr = dir; if (djr>3) djr-=4;
  double val,err[2];
  const StvNodePars &fp= mFP[dir];
  const StvFitErrs  &fe= mFE[dir];
  StvHit *hit = GetHit();
  TString ts; if (hit) {ts = (mXi2[0]>1e3 && mXi2[1]>1e3)? "h":"H";}
  if (GetType()==kDcaNode ) ts='D';
  if (GetType()==kPrimNode) ts='P';
  printf("%p(%s)",(void*)this,ts.Data());
  printf("\t%s=%g","Xi2",GetXi2(djr));
  int iopt=0;
  const char *myopt = myOpt.Data(); char*e;
  for (int i=0;txt[i];i++) {
    if (txt[i]==' ') continue;
    err[0]=-999;val=-999;
    if ((iopt=myOpt.Index(TString(txt+i,2)))<0) continue;
    if (txt[i+1]==' ') {//Single letter request 
           if (txt[i]=='R') 	{val = fp.getRxy();}
      else if (txt[i]=='P')	{val = fp.getPt() ;}
      else if (txt[i]=='E')	{err[0] = sqrt(fe.mHH); err[1] = sqrt(fe.mZZ);}
      else if (txt[i]=='L')	{val = GetLen();}
      else if (txt[i]=='K')	{val = fe.MaxCorr();}
      else if (txt[i]=='U')	{val = fe.mPP;}
      else if (txt[i]=='H')	{val = fp._hz;}
      else                      {val = fp[i];}
      if (abs(val+999)>1e-6) 	{ printf("\t%c=%g",txt[i],val);}
      if (err[0]>-999)  	{ printf("\tHH=%7.2g ZZ=%7.2g",err[0],err[1]);}
      } else if (txt[i+1]=='[') {// now print by index
      int idx = strtol(myopt+iopt+2,&e,10);
      TString tnam(myopt+iopt,e-(myopt+iopt)+1);
      if      (txt[i]=='P') 	{val = fp[idx];}
      else if (txt[i]=='E') 	{val = fe[idx];}
      else 			continue;
      printf("\t%s=%g",tnam.Data(),val);
    } 
  }//end for i

  if (hit) {
    for (int i=0;hit && hhh[i];i++) {
       err[0]=-999;val=-999;
      if (hhh[i]==' ') continue;
      if ((iopt=myOpt.Index(TString(hhh+i,2)))<0) continue;
      if (hhh[i+1]==' ')  	{//Single letter request 
        if (hhh[i]=='r')  	{ val = hit->getRxy();}
        else if (hhh[i]=='e')	{err[0] = sqrt(mHrr[0]); err[1] = sqrt(mHrr[2]);}
      else 			{val = hit->x()[i];} 
      if (abs(val+999)>1e-6) 	{printf("\th%c=%g",hhh[i],val);}
      if (err[0]>-999)  	{printf("\thh=%7.2g zz=%7.2g",err[0],err[1]);}
      } else if (txt[i+1]=='[') {// now print by index

      int idx = strtol(myopt+i+2,&e,10);
      TString tnam(myopt+i,e-(myopt+i)+1);
      if      (txt[i]=='e') 	{val = mHrr[idx];}
      printf("\t%s=%g",tnam.Data(),val);
      } 
    }
  }

  printf("\n");
  return;
}    
//________________________________________________________________________________
void StvNode::SetDer(const StvFitDers &der, int dir)
{
   mDer[  dir]=der;
   mDer[1-dir]=der;
   mDer[1-dir].Reverse();
} 
//________________________________________________________________________________
void StvNode::SetHit(StvHit *hit)
{
   if (mHit == hit) 	return;
   if (mHit) mHit->addTimesUsed(-1);
   mHit = hit;
   if (!mHit) 		return;
   assert(!mHit->timesUsed());
   mHit->addTimesUsed(1);
} 
//________________________________________________________________________________
void StvNode::UpdateDca()
{ 
  const StvNodePars &P = mFP[2];
  double dL = -( P._x*P._cosCA+P._y*P._sinCA)
              /(1+(-P._x*P._sinCA+P._y*P._cosCA)*P._curv);
    if (fabs(dL)<1e-6) return;
    THelixTrack hlx;		
    mFP[2].get(&hlx);
    mFE[2].Get(&hlx);
    dL = hlx.Path(0.,0.);
    hlx.Move(dL);
    mFP[2].set(&hlx,mFP[2]._hz);
    mFE[2].Set(&hlx,mFP[2]._hz);
}
//________________________________________________________________________________
int StvNode::Check(const char *tit, int dirs) const
{
  if (!tit) tit="";
  int nerr=0,ans;
  for (int k=0;k<5;k++) {
    TString ts;
    if (tit[0]) {ts=tit; ts+="/par["; ts+=k;ts+="]"; }
      int itst = (((1&k)+1)&dirs);
      if (k==4) itst = 1;
      if (!itst) continue;
      ans = mPP[k].check(ts); if (ans) nerr++;
      ans = mPE[k].Check(ts); if (ans) nerr++;
  }
  return nerr;
}
//________________________________________________________________________________
int StvNode::ResetELoss(double s,const StvNodePars &pars)
{
static StvELossTrak *el = new StvELossTrak();

  double p2 = pars.getP2();  
  if (fabs(mELossData.mP*mELossData.mP-p2) <0.01*p2) return 0;
  if (!mELossData.mMate) return 1;
  el->Reset();
  double p = sqrt(p2);
  el->Set(mELossData.mMate,p); el->Add(fabs(s));
  mELossData.mTheta2 = el->GetTheta2();
  mELossData.mOrt2   = el->GetOrt2();
  mELossData.mdPP    = el->dPP();
  mELossData.mELoss  = el->ELoss();
  mELossData.mdPPErr2= el->dPPErr2();
  mELossData.mTotLen = el->TotLen();
  mELossData.mP      = el->P();
  return 0;
}
 
