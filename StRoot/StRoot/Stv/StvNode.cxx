//StvKalmanTrack.cxx
/*
 * $Id: StvNode.cxx,v 1.38 2016/12/09 21:21:41 perev Exp $
 *
 * /author Victor Perev
 */

#include <Stiostream.h>
#include <math.h>
#include <stdio.h>
#include "TString.h"

#include "Stv/StvToolkit.h"
#include "Stv/StvNode.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvELossTrak.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"


//______________________________________________________________________________
void StvNode::reset()
{ 
static int myCount=0;
  assert(mBeg[0]=='@');
  memset(mBeg,0,mEnd-mBeg+1);
  mId = ++myCount; 
  StvDebug::Break(mId);
  mXi2[0] = 3e33;mXi2[1] = 3e33;mXi2[2] = 3e33;
}
//______________________________________________________________________________
void StvNode::unset()
{ 
static StvToolkit *kit = StvToolkit::Inst();
  if (mELoss) kit->FreeELossTrak(mELoss); 
  mELoss = 0;
  assert(mBeg[0]!='@');
  memset(mBeg,'@',mEnd-mBeg+1);
}
//______________________________________________________________________________
StvNode &StvNode::operator=(const StvNode &from)
{ 
static StvToolkit* kit=StvToolkit::Inst();
  memcpy(mBeg,from.mBeg,mEnd-mBeg+1); 
  if (mELoss) { //Recreate ELossTrak
    mELoss = kit->GetELossTrak();
   *mELoss = *from.mELoss;
  }
  return *this;
}

//______________________________________________________________________________
void StvNode::SetPre(StvNodePars &par,StvFitErrs &err,int dir)  
{
  assert(err.mHH>0);
  mPP[dir]=par;mPE[dir]=err;
  mFP[dir]=par;mFE[dir]=err;
  mFP[  2]=par;mFE[  2]=err;
}
//______________________________________________________________________________
void StvNode::SetFit(StvNodePars &par,StvFitErrs &err,int dir)  
{
  assert(err.mHH>0);
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
static const char *txt = "X Y Z Pt Cu H R E Ep L Ps Tl P[ E[ Pa ";
static const char *hhh = "x y z r e ";
  if (!opt || !opt[0]) opt = "_";
  TString myOpt(opt);myOpt+=" ";
  int dir = myOpt.Index("=");
  dir = (dir<0)? 2:myOpt[dir+1]-'0';
  int djr = dir; if (djr>3) djr-=4;
  double val,err[2];
  const StvNodePars &fp= mFP[dir];
  const StvFitErrs  &fe= mFE[dir];
  int dkr = (dir<2)? dir:0;
  const StvFitErrs  &pe= mPE[dkr];
//const StvNodePars &pp= mPP[dkr];
  StvHit *hit = GetHit();
  TString ts; 
  if (mHitPlane) ts = "h"; 
  if (hit)       ts = "H"; 
  if (GetType()==kDcaNode ) ts='D';
  if (GetType()==kPrimNode) ts='P';

  printf("%p(%s)",(void*)this,ts.Data());
  printf("\t%s=%g","Xi2",GetXi2(djr));
  int iopt=0;
  const char *myopt = myOpt.Data(); char*e;
  for (int i=0;txt[i];i++) {
    if (txt[i]==' ') continue;
    int nc = 2; if (txt[i+1]=='[') nc = 4;
    TString ts(txt+i,nc);
    err[0]=-999;val=-999;
    const char *cal = 0;
    if ((iopt=myOpt.Index(ts))<0) continue;
    int idx =(txt[i+1]=='[') ? strtol(myopt+iopt+2,&e,10):0;
    
    {//Single letter request 
           if (ts=="X ")        {val = fp._x;}
      else if (ts=="Y ")        {val = fp._y;}
      else if (ts=="Z ")        {val = fp._z;}
      else if (ts=="R ")        {val = fp.getRxy();}
      else if (ts=="Ps")        {val = fp._psi ;}
      else if (ts=="Tl")        {val = fp._tanl ;}
      else if (ts=="Pt")        {val = fp.getPt() ;}
      else if (ts=="Cu")        {val = fp._curv;}
      else if (ts=="E ")        {err[0] = sqrt(fe.mHH); err[1] = sqrt(fe.mZZ);}
      else if (ts=="Ep")        {err[0] = sqrt(pe.mHH); err[1] = sqrt(pe.mZZ);}
      else if (ts=="L ")        {val = GetLen();}
      else if (ts=="H ")        {val = fp._hz;}
      else if (ts=="P[")        {val = fp[idx];}
      else if (ts=="E[")        {val = fe[idx];}
      else if (ts=="Pa")        {cal = (mHitPlane)? mHitPlane->GetPath():"";}
      if (!cal && val==-999 && err[0]==-999) continue;
      printf("\t%s=",ts.Data());
      if (cal)                  { printf("%s ",cal);}
      if (fabs(val+999)>1e-6)   { printf("%g",val);}
      if (err[0]>-999)          { printf("HH(%7.2g) ZZ(%7.2g)",err[0],err[1]);}
    } 
  }//end for i

  if (hit) {
    printf(" hit(%p) ",(void*)hit);
    for (int i=0; hhh[i];i++) {
       err[0]=-999;val=-999;
      if (hhh[i]==' ') continue;
      if ((iopt=myOpt.Index(TString(hhh+i,2)))<0) continue;
      if (hhh[i+1]==' ')        {//Single letter request 
        if (hhh[i]=='r')        { val = hit->getRxy();}
        else if (hhh[i]=='e')   {err[0] = sqrt(mHrr[0]); err[1] = sqrt(mHrr[2]);}
      else                      {val = hit->x()[i/2];} 
      if (fabs(val+999)>1e-6)   {printf("\th%c=%g",hhh[i],val);}
      if (err[0]>-999)          {printf("\thh=%7.2g zz=%7.2g",err[0],err[1]);}
      } else if (txt[i+1]=='[') {// now print by index

      int idx = strtol(myopt+i+2,&e,10);
      TString tnam(myopt+i,e-(myopt+i)+1);
      if      (txt[i]=='e')     {val = mHrr[idx];}
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
   mHit = hit;
   if (!mHit)           return;
   assert(!mHit->isUsed());
} 
//________________________________________________________________________________
void StvNode::SetMem(StvHit *hit[2],double xi2[2])
{
  memcpy(memHit,hit,sizeof(memHit));
  memXi2[0]=xi2[0];memXi2[1]=xi2[1];
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
int StvNode::ResetELoss(const StvNodePars &pars,int dir)
{
static const double kSmaP      =0.01;
static const double kBigP      =3     	,kSmaDiff=1e-2;

  if (!mELoss) return 0;
  double p = pars.getP(); 
  if (p>kBigP) p=kBigP;
  if (p<kSmaP) p=kSmaP;
  double myP = mELoss->P();
  if (fabs(myP-p)<kSmaDiff*p) 		return 0;
  mELoss->Update(dir,p);	
  return 0;
}
 
