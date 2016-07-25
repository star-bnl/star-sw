// $Id: StXTrak.cxx,v 1.3 2016/07/22 19:00:20 perev Exp $
/// \File StXTrak.cxx
/// \author V.Perev 2016
//
/*!

\class StXTrak

A class StXTrak is a auxiliary for Sti/StiCA/Stv packages.
<br>
Main tasks:
<ul>
<li> Xtend/prolong StTrack to far detector;
<li> Save produced data into StEvent.
</ul>
*/
#include <math.h>
#include <string.h>
#include <assert.h>
#include "TCernLib.h"
#include "TVector3.h"
#include "StXTrak.h"
#include "TGeoManager.h"
#include "TGeoSwim.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoMaterial.h"
#include "StThreeVectorD.hh"
#include "THelixTrack.h"
#include "StiUtilities/StiDebug.h"

//#include "Sti/StiElossCalculator.h"
#include "StvUtil/StvELossTrak.h"
#include "StiUtilities/StiDebug.h"

//_____________________________________________________________________________
StXTrak::StXTrak(MyMag *myMag,MyLoss* eLoss,TGeoSwimEnd* myEnd) 
{
  mMyLoss= eLoss;		//ELoss calculator
  if (!mMyLoss) mMyLoss = new MyLoss();

  mMyMag = myMag;		//Mag field calculator
  if (!mMyMag) mMyMag = new MyMag();


  mMyEnd = myEnd; 	//End condition

  static const char* farDets[]={
  "BTOF","BBCA","CALB","ECAL","ETTV",
  "FGTM","FBOX","FSCE","HCMO","MUTD",
  "PHMD","VPDD","ZCAL",
  0};

  if (!mMyEnd ) mMyEnd = new TGeoSwimDefaultEnd(TGeoSwimEnd::kNameVolu,farDets);
                                                
  mSwim = new TGeoSwim();
  mSwim->Set(mMyMag,mMyLoss,mMyEnd);
  mSwim->Set(500.,-400.,400.,5.);
}

//_____________________________________________________________________________
void  StXTrak::Clear() 
{
 memset(mBeg,0,mEnd-mBeg+1);
}

//_____________________________________________________________________________
void StXTrak::Set1stPoint(int charge1st,double pos1st[3],double mom1st[3])
{
static const double kHftRad = 3;
enum {kUndef, kPrimary, kDca,k1stPoint};

  memcpy(m1stTk.mPos,pos1st,sizeof(m1stTk.mPos));
  memcpy(m1stTk.mMom,mom1st,sizeof(m1stTk.mMom));
  m1stTk.mCosLam = sin(TVector3(m1stTk.mMom).Theta());
  m1stTk.mCharge=charge1st;
  TVector3 vMom(m1stTk.mMom),vPos(m1stTk.mPos);
  double B[3];
  (*mMyMag)(m1stTk.mPos,B);
  m1stTk.mCurv=B[2]*m1stTk.mCharge/vMom.Perp();
  m1stTk.mP = vMom.Mag();;
  m1stTk.mPti = m1stTk.mCharge/vMom.Perp();
  m1stTk.mPt  = 1./(fabs(m1stTk.mPti)+1e-10);
  m1stTk.mLen= 0;
  double dis = vPos.Perp();
  mFlag1st=k1stPoint; 		//0=undefined, 1=primary, 2=dca,3=1st point
  if (dis<kHftRad) mFlag1st=kPrimary;
  if (vMom.Dot(vPos)<1e-3)  mFlag1st=kDca;

  if (mFlag1st==k1stPoint) {
    double sinAlf = dis*m1stTk.mCurv/2;
    m1stTk.mLen = (fabs(sinAlf)>1e-3)? asin(sinAlf)*2./m1stTk.mCurv:dis;
    m1stTk.mLen/= m1stTk.mCosLam;
  }
}



//_____________________________________________________________________________
void StXTrak::Set2ndPoint(int charge2nd,double pos2nd[3],double mom2nd[3])
{
  memcpy(m2ndTk.mPos,pos2nd,sizeof(m2ndTk.mPos));
  memcpy(m2ndTk.mMom,mom2nd,sizeof(m2ndTk.mMom));
  m2ndTk.mCosLam = sin(TVector3(m2ndTk.mMom).Theta());
  m2ndTk.mCharge=charge2nd;
  TVector3 vPos(m2ndTk.mPos);
  TVector3 vMom(m2ndTk.mMom);
  TVector3 dPos = (vPos - TVector3(m1stTk.mPos));
  TVector3 aMom = (vMom + TVector3(m1stTk.mMom))*0.5;

  
  double B[3];
  (*mMyMag)(m2ndTk.mPos,B);
  m2ndTk.mCurv=B[2]*m2ndTk.mCharge/vMom.Perp();
  m2ndTk.mP = vMom.Mag();;
  m2ndTk.mPti = m2ndTk.mCharge/vMom.Perp();
  m2ndTk.mPt  = 1./(fabs(m2ndTk.mPti)+1e-10);
  m2ndTk.mLen= 0;
  double curv = (m1stTk.mCurv+m2ndTk.mCurv)*0.5;
  double dis = dPos.Perp();
  double sinAlf = dis*curv/2;
  m2ndTk.mLen = (fabs(sinAlf)>1e-3)? asin(sinAlf)*2./curv:dis;
  m2ndTk.mLen/= (m1stTk.mCosLam+m2ndTk.mCosLam)*0.5;
  m2ndTk.mLen+=m1stTk.mLen;

  mCurTk = m2ndTk;
}

//_____________________________________________________________________________
int StXTrak::Next()
{

  mSwim->Set(mCurTk.mPos,mCurTk.mMom,mCurTk.mCurv);
  auto ans = mSwim->Swim(500);

  memcpy(mCurTk.mPos,mSwim->GetPos(),sizeof(mCurTk.mPos));
  memcpy(mCurTk.mMom,mSwim->GetDir(),sizeof(mCurTk.mMom));
  mCurTk.mCurv = mSwim->GetCurv();
  mCurTk.mP   = mSwim->GetP();
  mCurTk.mPt  = mSwim->GetPt();
  mCurTk.mPti = mSwim->GetPti();
  mCurTk.mLen+= mSwim->GetLen();
  mCurTk.mPLoss   = mSwim->GetPLoss();
  double d = mSwim->GetLen(1)-mSwim->GetLen(0);
  StiDebug::Count("Thick",d);
  return ans;
}
#include "StarMagField/StarMagField.h"
//______________________________________________________________________________
/*! Calculates mag field 
  Field is calcualated via StarMagField class 
*/
void MyMag::operator()(const double x[3],double b[3]) 
{
  
static const double EC = 2.99792458e-4;
static StarMagField *magf = StarMagField::Instance();
     magf->BField(x,b);
     b[0]*=EC;
     b[1]*=EC;
     b[2]*=EC;
}

MyLoss::MyLoss() 
{
 mELoss = new StvELossTrak;
 mELoss->Reset(1);
}


double MyLoss::operator()(const TGeoMaterial* mate,double P,double len
                       ,double *theta2)
{
  mELoss->Reset(1);
  mELoss->Set(mate,P);
  mELoss->Add(len);
  double dP = mELoss->PLoss(P);
  if (theta2){};
  return dP;
}
