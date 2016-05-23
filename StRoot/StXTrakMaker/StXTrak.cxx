// $Id: StXTrak.cxx,v 1.1 2016/05/20 18:40:41 perev Exp $
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

//#include "Sti/StiElossCalculator.h"
#include "StvUtil/StvELossTrak.h"
#include "StiUtilities/StiDebug.h"

//_____________________________________________________________________________
StXTrak::StXTrak(MyMag *myMag,StvELossTrak* eLoss) 
{
mELoss=eLoss;		//ELoss calculator
mMyMag=myMag;		//Mag field calculator
}



//_____________________________________________________________________________
double StXTrak::Path(double posp[3],double momp[3], int charge)
{
static const double kRxyMax=300;
static const char*  farDets[]={"TOF","MTD","MUTD",0};
    double otLen,inLen,dP,lenTot=0,rxy;
    int iend=0;

    memcpy(mPos,posp,sizeof(mPos));
    memcpy(mMom,momp,sizeof(mMom));
    mCharge = charge;
    TGeoSwim swim;


    double dPtot = 0;

    int CHARGE = mCharge;
    double CURV = mCurv; 
//??    if (fabs(CURV )>1./120) continue;    
    StThreeVectorD STPOS(mPos);
    StThreeVectorD STMOM(mMom);
    StThreeVectorD STDIR(STMOM); STDIR = STDIR.unit();
    double cosTh = STDIR.perp();
    STDIR*=1./cosTh;
    StThreeVectorD stdir(STDIR);
    THelixTrack hlxBeg(STPOS.xyz(),STMOM.xyz(),CURV);
    StThreeVectorD stpos(STPOS),stmom(STMOM);
    mP  = STMOM.mag(); 
    double p = mP, Pt = STMOM.perp(), pt = Pt;
    double Eta = stmom.pseudoRapidity();
    double Phi = stmom.phi()*57.;
    double rxyPre = 0;
    int found = 0;

//		Loop over volumes
    for (int iVol=0; iVol<1000;iVol++) {
      double B[3];
      (*mMyMag)(stpos.xyz(),B);
      double curv = -B[2]*CHARGE/pt;
//      if (fabs(B[2])<0.01*fabs(magSumm)) break;
      if (!iVol) {
	if (curv*CURV<0) {curv = -curv; CHARGE=-CHARGE;}
	assert(iVol || curv*CURV>0);
      }

      const TGeoMaterial *gmate=0;
      TString path;
      double maxLen = 50,myLen = maxLen;

//	Loop inside one volume with big ELoss

      for (int it=0;it<10;it++) {
        swim.Set(stpos.xyz(),stdir.xyz(),curv);
        path = swim.GetPath();
        gmate = swim.GetMate();
        iend = swim.Swim(myLen);
	if (iend>1) {printf("End=%d\n",iend); break; }
	inLen = swim.GetLen(0);
	otLen = swim.GetLen(1);
        mELoss->Reset(1);
        mELoss->Set(gmate,p);
        mELoss->Add(otLen);
        dP = mELoss->PLoss(p);
	if (dP<0.1*p) break;
        myLen *= 0.1*p/dP * 0.9; 
    }//end it

//		We got new position
    memcpy(stpos.xyz(),swim.GetPos(1),sizeof(double)*3);
    memcpy(stdir.xyz(),swim.GetDir(1),sizeof(double)*3);
    double dca = hlxBeg.Dca(stpos.xyz());
    if (dca>20) {
  	printf("BOT OHO\n");
    }
    cosTh = stdir.perp();
    stdir*=1./cosTh;
    double dpt = pt/p*dP;
    pt -= dpt;
    if (pt<1e-2) { iend = 1313; break; }
    dPtot += dP;
    lenTot += otLen;
    double lenxy = cosTh*otLen;
    double dc   = dP/p*curv;
    double dphi = dc*lenxy /2.;
    double dh   = dc*lenxy*lenxy/6.;

    curv +=dc; p -= dP;
    assert(p>0);
    stpos[0]+=-stdir[1]*dh;
    stpos[1]+= stdir[0]*dh;
    double cosPhi = stdir[0]; if (cosPhi){}
    stdir[0] += -stdir[1]*dphi;
    stdir[1] +=  stdir[0]*dphi;

    rxy = stpos.perp();
static int pri=0;
if (pri)
      printf("%6d - Rxy = %g(%g %g) \tName=%s/%s\n",iVol,rxy,inLen,dP/p
            ,swim.GetNode(1)->GetName()
            ,path.Data());

      if (rxy>kRxyMax) 	break;
      if (rxy<rxyPre )  break;
      for (int j=0;farDets[j];j++) {
        if (path.Index(farDets[j])<0) continue;
        found = j+1; break;
      }
      if (found)     break;
      rxyPre = rxy;
  }//End iVol

  if (iend)		return -99;
  if (!found)		return -99;

  THelixTrack hlx(STPOS.xyz(),STMOM.xyz(),CURV);
  double dca = hlx.Dca(stpos.xyz());
  if (fabs(stpos[2])<200 && dca > 0.15) {
  	printf("BOT OHO BOT OHO BOT OHO \n");
  }
  
  
  double lenTof = hlx.Path(stpos.xyz());
//  if (dca<0.05) continue;
//  if (fabs(pos[2])>200) continue;
    StiDebug::Count("PriDca",dca);
    StiDebug::Count("PriDca:1/Pt",1./pt, dca);
    StiDebug::Count("PriDca:Rxy",rxy, dca);
    StiDebug::Count("PriDca:Z",stpos[2], dca);
    StiDebug::Count("PriDca:dPtot",dPtot, dca);
    StiDebug::Count("PridP",dPtot);
    StiDebug::Count("PridP:1/Pt",1./pt, dPtot);
    StiDebug::Count("PriRxy",rxy);
    StiDebug::Count("lenResid:Z ",stpos[2],lenTof-lenTot);
  return lenTot;
}
