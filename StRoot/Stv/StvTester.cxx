#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TMath.h"
#include "TMatrixD.h"
#include "TVectorD.h"
#include "TCernLib.h"
#include "TGeoManager.h"
#include "StvTester.h"
#include "StvTrack.h"
#include "StvNode.h"
#include "StvUtil/StvNodePars.h"
#include "StvHit.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvELossTrak.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

StvTester *StvTester::mgTester=0;
static const double piMass=0.13956995;
static const double kAccu=1e-3;
//______________________________________________________________________________
StvTester::StvTester(const char *name):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
  mEl = new StvELossTrak;
  assert(!mgTester);
  mgTester = this;
}
//______________________________________________________________________________
StvTester *StvTester::Inst()
{	
  if (!mgTester) mgTester = new StvTester();
  return mgTester;
}
//______________________________________________________________________________
int StvTester::TestIt(const char* tit,const StvTrack *tk)
{
  int iErr=0;
#if 0
static int nCall = 0; nCall++;
  int n=0,pora=0;
  const StvNode *curNode,*preNode=0;
  double pCur,pPre=0,pBeg[2],pErr,lenTot=0,eAcc[2]={0};
  double len=0,pLos,pDlt,pct,sample=0;
  
  for (StvNodeConstIter it=tk->begin();it!=tk->end();++it) {
    curNode = (*it); n++;
//  const StvHit *hit = curNode->GetHit();
    const StvNodePars &par = curNode->GetFP(); 
    const StvFitErrs  &err = curNode->GetFE(); 
    pCur = sqrt(par.getP2());
    pErr = sqrt(err.mPP/par.getCos2L());
    do {		//Pseudo loop
      if (!preNode){pBeg[0] = pCur;pBeg[1]=pCur; break;}
      { //test 1`
	pLos = PLoss(curNode,preNode,&len);
	sample += len;
	eAcc[0]+=pLos; eAcc[1]+=pLos;
	pora = sample>555;
	pDlt = pBeg[1] - pCur;

	if ( pora && (eAcc[2] > pErr || fabs(pDlt) > pErr)) {
          pct = fabs(eAcc[1]-pDlt)/(pCur)*100;
	  if (pct>1) 
            {iErr|=1;Error(tit,"P=%g Len=%4.1f dP=%g Loss=%g dif=%4.1f%%"
	                   ,pCur,sample,pDlt,eAcc[1],pct);}
          sample=0;eAcc[1]=0;pBeg[1]=pCur; pora = 0;
	}
      }//end test1


      {			//Now TEST2
        
	const StvELossData &dat = preNode->GetELoss();
	double pLos2 = (dat.mdPP) *len*pPre;
        if (1 || fabs(pLos2-pLos) > pErr) {
          pct = fabs(pLos2-pLos)/(pLos2+pLos)*200;
	  printf("===== Rxy=%g Z=%g StvPLoss = %g InDive = %g dif=%1.1f%%\n"
	        ,par.getRxy(),par._z,pLos,pLos2,pct); }

      }
    } while(0);

    assert(fabs(len)<400);
    lenTot += len;
    assert(fabs(lenTot)<400);
    preNode = curNode;
    pPre  = pCur;
  }
  pDlt = pBeg[0]-pCur;
  if ( eAcc[0] > pErr || fabs(pDlt) > pErr) {
    pct = fabs(pDlt-eAcc[0])/(pCur)*100;
    if (pct>1) 
          {iErr+=1000;Error(tit,"P=%g Len=%4.1f dP=%g Loss=%g dif=%4.1f%% ****"
	                 ,pCur,lenTot,pDlt,eAcc[0],pct);}

  }
#endif
  return iErr;
}
//______________________________________________________________________________
double StvTester::DeltaLen(const StvNode* curNode,const StvNode* preNode,double *lenXY) const
{
   const StvNodePars &parA = preNode->GetFP();
   const StvNodePars &parB = curNode->GetFP();

   const double *x1 = &parA._x;
   const double *x2 = &parB._x;
   double dlen = sqrt(pow(x1[0]-x2[0],2) + pow(x1[1]-x2[1],2));
   double curv = 0.5*fabs(parA._curv+parB._curv);
   double dsin = (0.5*dlen*curv);
   if (dsin>0.9) dsin=0.9;
   dlen = (dsin<0.01)? dlen*(1.+dsin*dsin/6) : 2*asin(dsin)/curv; 
   if (lenXY) *lenXY = dlen;
   dlen =sqrt(dlen*dlen + pow(x1[2]-x2[2],2));

   return dlen;
}
//______________________________________________________________________________
double StvTester::PLoss(const StvNode* curNode,const StvNode* preNode, double *len) const
{
#if 0
static int nCall = 0; nCall++;
  const StvNodePars &parA = preNode->GetFP();
  const StvNodePars &parB = curNode->GetFP();
  THelixTrack curHlx,preHlx;
  double momA = sqrt(parA.getP2());
  double momB = sqrt(parB.getP2());

  parA.get(&preHlx);
  parB.get(&curHlx);
  double lenA = preHlx.Path(curHlx.Pos());
  double lenB = curHlx.Path(preHlx.Pos());
  double totLen = (fabs(lenA)+fabs(lenB))/2;
  double step = 0.1;
  if (parA.getRxy()<60 || parB.getRxy()<60) step = 0.01;
  int nSteps = int(totLen/step+1);
  double stepA = fabs(lenA/nSteps);
  double stepB = fabs(lenB/nSteps);
  curHlx.Move(lenB);
  mEl->Reset();
  for (int iStep=0;iStep<nSteps;iStep++) {
    double wt = double(iStep)/nSteps;
    double myX[3];
    TCL::vlinco(curHlx.Pos(),wt,preHlx.Pos(),1.-wt,myX,3);
    TGeoNode *geoNode = gGeoManager->FindNode(myX[0],myX[1],myX[2]); 
    assert(geoNode);
    TGeoVolume *geoVolu = geoNode->GetVolume();
    TGeoMaterial *geoMate = geoVolu->GetMaterial();
    double a  = geoMate->GetA();
    double z  = geoMate->GetZ();
    double d  = geoMate->GetDensity();
    double x0 = geoMate->GetRadLen();
    double p = momA*(1-wt)+momB*wt;
    mEl->Set (a, z, d, x0, p, piMass, +1.0);
    double s = stepA*(1-wt)+stepB*wt;
    mEl->Add(s); 
    curHlx.Move(stepB);  
    preHlx.Move(stepA);  
  }
  assert(fabs(totLen)<400);
  if (len) *len = totLen;
  double dP0 = totLen*momB*mEl->dPovPLen();
  double dP1 = mEl->dEdX()*totLen*sqrt(momA*momA+piMass*piMass)/momA;
  if ( fabs(dP0-dP1)/(dP0+dP1)*200>1) {
    printf ("PLoss dP0=%g dP1=%g\n",dP0,dP1);
  }
  return dP0;
#endif
return 0;
}







