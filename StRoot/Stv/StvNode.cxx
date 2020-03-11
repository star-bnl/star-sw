//StvKalmanTrack.cxx
/*
 * $Id: StvNode.cxx,v 1.38.2.3 2020/03/11 20:57:21 perev Exp $
 *
 * /author Victor Perev
 */

#include <Stiostream.h>
#include <math.h>
#include <stdio.h>
#include "TString.h"
#include "TVector3.h"

#include "Stv/StvToolkit.h"
#include "Stv/StvNode.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvELossTrak.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
 const StvNode* StvNode::gPreNode = 0;

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
  assert(err[0]>0);
  mPP[dir]=par;mPE[dir]=err;
  mFP[dir]=par;mFE[dir]=err;
  mFP[  2]=par;mFE[  2]=err;
}
//______________________________________________________________________________
void StvNode::SetFit(StvNodePars &par,StvFitErrs &err,int dir)  
{
  assert(err[0]>0);
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
static const char *txt = "X Y Z Pt Cu H R E Ep L Ps Tl P[ E[ Pa Sg Al";
static const char *hhh = "x y z r e re";
// Cu - Curvature
// R  - Rxy
// Ps - Psi or Phi angle
// Tl - tangens lambda
// H  - Hz field
// Pa - Path
// Sg - Sign of track
// Al - Along, Dot(dX,Mom) normalized

  if (!opt || !opt[0]) opt = "_";
  TString myOpt(opt);myOpt+=" ";
  int dir = myOpt.Index("=");
  dir = (dir<0)? 2:myOpt[dir+1]-'0';
  double val,err[2];
  const StvNodePars &fp= mFP[dir];
  const StvFitErrs  &fe= mFE[dir];
  assert(fe[0]<100. && fe[2]<100);
  int dkr = (dir<2)? dir:0;
  const StvFitErrs  &pe= mPE[dkr];
//const StvNodePars &pp= mPP[dkr];
  StvHit *hit = GetHit();
  TString ts; 
  if (mHitPlane) ts = "h"; 		//Not used hit
  if (hit)       ts = "H"; 		//Used hit
  if (GetType()==kDcaNode ) ts='D';	// It is Dca node
  if (GetType()==kPrimNode) ts='P';	// it is Orimary

  printf("%p(%s)",(void*)this,ts.Data());
  printf("\t%s=%g","Xi2",GetXi2(dir));
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
           if (ts=="X ")        {val = fp[0];}
      else if (ts=="Y ")        {val = fp[1];}
      else if (ts=="Z ")        {val = fp[2];}
      else if (ts=="R ")        {val = fp.getRxy();}
      else if (ts=="Ps")        {val = fp.getPsi();}
      else if (ts=="Tl")        {val = fp.getTanL();}
      else if (ts=="Pt")        {val = fp.getPt() ;}
      else if (ts=="Cu")        {val = fp.getCurv();}
      else if (ts=="E ")        {err[0] = sqrt(fe[0]); err[1] = sqrt(fe[2]);}
      else if (ts=="Ep")        {err[0] = sqrt(pe[0]); err[1] = sqrt(pe[2]);}
      else if (ts=="L ")        {val = GetLen();}
      else if (ts=="H ")        {val = fp._h[3];}
      else if (ts=="P[")        {val = fp[idx];}
      else if (ts=="E[")        {val = fe[idx];}
      else if (ts=="Pa")        {cal = (mHitPlane)? mHitPlane->GetPath():"None";}
      else if (ts=="Sg")        {val = fp.getSign();}
      else if (ts=="Al")        {
        val = 0;
	if  (gPreNode) {
          auto dx = (TVector3(GetFP()._x)-TVector3(gPreNode->GetFP()._x)).Unit();
          auto dp =  TVector3(GetFP()._d).Unit();
          val = dx.Dot(dp);
      } }
      if (!cal && val==-999 && err[0]==-999) continue;
      printf("\t%s=",ts.Data());
      if (cal)                  { printf("%s ",cal);}
      if (fabs(val+999)>1e-6)   { printf("%g",val);}
      if (err[0]>-999)          { printf("UU(%7.2g) VV(%7.2g)",err[0],err[1]);}
    } 
  }//end for i

  if (hit) {
    printf(" hit(%p) ",(void*)hit);
    for (int i=0; hhh[i];i++) {
       err[0]=-999;val=-999;
      if (hhh[i]==' ') continue;
      ts = hhh[i]; ts+=hhh[i+1];i++;
      if (myOpt.Index(ts)<0) continue;
      iopt=i/2;
      switch(iopt) {
	case 0:;case 1:; case 2:; 
          val = hit->x()[iopt]; 		break;
	case 3: val = hit->getRxy(); 	break;
	case 4: {err[0] = sqrt(mHrr[0]); err[1] = sqrt(mHrr[2]); break;}
	case 5: { //residual
          TVector3 vhit(hit->x());
          TVector3 vdir(fp._d); 
          TVector3 vpos0(fp._x); 
          val = (vpos0-vhit).Mag2()- pow((vpos0-vhit).Dot(vdir),2);
          val = sqrt(val); break;
	}
      }
      if (fabs(val+999)>1e-6)   {printf("\th%s=%g",ts.Data(),val);}
      if (err[0]>-999)          {printf("\th%s=%7.2g %7.2g",ts.Data(),err[0],err[1]);}
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
   mDer[1-dir].Backward();
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
  double dL = -( P.pos()[0]*P.dir()[0]+P.pos()[1]*P.dir()[1]);
  if (fabs(dL)<1e-6) return;
  THelix3d hlx;            
  mFP[2].get(&hlx);
  mFE[2].Get(&hlx);
  dL = hlx.Path(0.,0.);
  hlx.Move(dL);
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
StvELossTrak *StvNode::ResetELoss(const StvNodePars &pars,int dir)
{
  if (!mELoss) return 0;
  double p = pars.getP(); 
  mELoss->Set(0,p);	
  return mELoss;
}
//________________________________________________________________________________
//________________________________________________________________________________

