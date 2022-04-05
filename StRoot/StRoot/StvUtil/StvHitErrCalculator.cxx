#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "TCernLib.h"
#include "StvHitErrCalculator.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvHit.h"
#include <map>
#include <string>

static std::map<std::string,StvHitErrCalculator *> calcMap;
enum {kMaxLam = 85,kMaxPsi=85};
static const double kMinCosLam = cos(M_PI/180*kMaxLam),k2MinCosLam=kMinCosLam*kMinCosLam;
static const double kMinCosPsi = cos(M_PI/180*kMaxPsi),k2MinCosPsi=kMinCosPsi*kMinCosPsi;
static const double kMinCpCl = 0.1;


// //______________________________________________________________________________
// double Det33(double T[3][3])
// {
// //  	00 01 02
// //  	10 11 12
// //  	20 21 22
//   T[0][0]*T[1][1]*T[2][2]+T[0][1]*T[1][2]*T[2][0]+T[1][0]*T[2][2]*T[0][2]
//  -T[2][0]*T[1][1]*T[0][2]

ClassImp(StvHitErrCalculator)
ClassImp(StvTpcHitErrCalculator)
//______________________________________________________________________________
StvHitErrCalculator::StvHitErrCalculator(const char *name,int nPar):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
  mNPar = nPar;
  if (!*GetName()) return;
  StvHitErrCalculator *&calc = calcMap[GetName()];
  assert(!calc && "Name clash");
  calc = this;
}
//______________________________________________________________________________
StvHitErrCalculator *StvHitErrCalculator::Inst(const char *name)
{
  StvHitErrCalculator *calc = calcMap[name];
  assert(calc);
  return calc;
}
//______________________________________________________________________________
void StvHitErrCalculator::SetPars(const double *par)
{
  memcpy(mPar,par, GetNPars()*sizeof(*mPar));
}
//______________________________________________________________________________
void StvHitErrCalculator::SetTrack(const float tkDir[3])
{  
  double d[3]={tkDir[0],tkDir[1],tkDir[2]};
  SetTrack(d);
}
//______________________________________________________________________________
void StvHitErrCalculator::SetTrack(const double tkDir[3])
{  
  double nor = (tkDir[0]*tkDir[0]+tkDir[1]*tkDir[1]+tkDir[2]*tkDir[2]);
  nor = (fabs(nor-1)< 1e-2)? (nor+1)*0.5 : sqrt(nor);
  TCL::vscale(tkDir,1./nor,mTG[0],3);
  
  nor = (1.-mTG[0][2])*(1+mTG[0][2]);
  if (nor <1e-6) { 
    mTG[1][0]=1; mTG[1][1]=0; mTG[1][2]=0;
    mTG[2][0]=0; mTG[2][1]=1; mTG[2][2]=0;
  } else {
    nor = sqrt(nor);
    mTG[1][0] = -mTG[0][1]/nor; mTG[1][1] = mTG[0][0]/nor;mTG[1][2] = 0;

    mTG[2][0] = /*mTG[0][1]*mTG[1][2]*/-mTG[1][1]*mTG[0][2]  ;
    mTG[2][1] =   mTG[0][2]*mTG[1][0]/*-mTG[1][2]*mTG[0][0]*/;
    mTG[2][2] =   mTG[0][0]*mTG[1][1]  -mTG[1][0]*mTG[0][1]  ;
  }

}
//______________________________________________________________________________
int StvHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 
//		Nt = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  memset(mDD[0],0,mNPar*sizeof(mDD[0]));
  memset(mDRr,0,sizeof(mDRr));
  mDD[kYErr][kYY] = 1;
  mDD[kZErr][kZZ] = 1;
  mDRr[kXX] =  0.;
  mDRr[kYY] =  mDD[kYErr][kYY]*mPar[kYErr];
  mDRr[kZZ] =  mDD[kZErr][kZZ]*mPar[kZErr];
  mDRr[kZY] = 0;
  if (!hRr) return 0;
  hRr[kXX] = mDRr[kYY];
  hRr[kYX] = mDRr[kZY];
  hRr[kYY] = mDRr[kZZ];
  return 0;
}  
//______________________________________________________________________________
int StvHitErrCalculator::CalcLocals(const float hiDir[3][3])
{
/// Calculate hit error matrix in DCA  system. In this system
/// track is along x axis, Y axis comes thru hit point 
static const double s15 = sin(3.14/180*15);
static const double c15 = cos(3.14/180*15);
static const double s45 = sin(3.14/180*45);
static const double c45 = cos(3.14/180*45);

  if (!hiDir) {
    mSp  = s15    ; mCp  = c15    ; mSl  = s45    ; mCl  = c45    ;
    mSp2 = s15*s15; mCp2 = c15*c15; mSl2 = s45*s45; mCl2 = c45*c45;
    mCpCl = mCp*mCl;
    return 0;
  }
  for (int j=0;j<3;j++) {
    mTL[j] = (hiDir[j][0]*mTG[0][0]+hiDir[j][1]*mTG[0][1]+hiDir[j][2]*mTG[0][2]);}


//		mTL = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  mSl = mTL[2],mCl2 = ((1-mSl)*(1+mSl));
  if (mCl2<k2MinCosLam) 				return 1; //Lambda too big
  mCl=sqrt(mCl2); mSp = mTL[1]/mCl; mCp = mTL[0]/mCl; 

  if (fabs(mCp) < kMinCosPsi) 				return 2; //Phi (psi) too big
  mSp2 = mSp*mSp; mCp2 = mCp*mCp; mSl2=mSl*mSl;
  mCpCl = fabs(mCp*mCl); if (mCpCl < kMinCpCl) mCpCl = kMinCpCl;

  for (int i=0;i<3;i++) { 
  for (int j=0;j<3;j++) {
    double s = 0;
    for (int k=0;k<3;k++) {s+=mTG[i][k]*hiDir[j][k];}
    mTT[i][j] = s;}};
  return 0;
}
//______________________________________________________________________________
int StvHitErrCalculator::CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
static int nCall = 0; nCall++;
/// Calculate hit error matrix in DCA  system. In this system
/// track is along x axis, Y axis comes thru hit point 
   mCp2 = -1; 			//To test of calling CalcLocals
   int ans = CalcDetErrs(hiPos,hiDir,0);
   if (mCp2 <= -1) {ans = CalcLocals(hiDir);}
   if (ans) return ans;
   TCL::trasat(mTT[0],mDRr,mTRr,3,3); 
//   TCL::tratsa(mTT[0],mDRr,mTRr,3,3); 
   if (!hRr) return 0;
   hRr[kXX] = mTRr[kYY];
   hRr[kYX] = mTRr[kZY];
   hRr[kYY] = mTRr[kZZ];
   assert(hRr[kXX]>0);
   assert(hRr[kYY]>0);
   assert(hRr[kYY]*hRr[kXX]>hRr[kYX]*hRr[kYX]);
   return 0;
} 
//______________________________________________________________________________
void StvHitErrCalculator::CalcDcaDers(double dRr[kMaxPars][3])
{
// Calculate deriavatives of err matrix.
// must be called after CalcDcaErrs(...)
  double myDRr[6];
  for (int iPar=0;iPar<mNPar;iPar++) {
    TCL::trasat(mTT[0],mDD[iPar],myDRr,3,3); 
    dRr[iPar][kXX] = myDRr[kYY];
    dRr[iPar][kYX] = myDRr[kZY];
    dRr[iPar][kYY] = myDRr[kZZ];
  }
}
//______________________________________________________________________________
void StvHitErrCalculator::CalcDetDers(double dRr[kMaxPars][3])
{
// Calculate deriavatives of err matrix.
// must be called after CalcDetErrs(...)
  for (int iPar=0;iPar<mNPar;iPar++) {
    dRr[iPar][kXX] = mDD[iPar][kYY];
    dRr[iPar][kYX] = mDD[iPar][kZY];
    dRr[iPar][kYY] = mDD[iPar][kZZ];
  }
}
//______________________________________________________________________________
double StvHitErrCalculator::Trace(const float hiPos[3]) 
{
  double hiErr[3];
  CalcDetErrs(hiPos,0,hiErr);
  return hiErr[0]+hiErr[2];
}
//______________________________________________________________________________
int StvHitRrCalculator::CalcDcaErrs(const StvHit *stvHit,double hRr[3]) 
{
   TCL::ucopy(stvHit->errMtx(),mDRr,6);
   TCL::trasat(mTG[0],mDRr,mTRr,3,3); 
   hRr[kXX] = mTRr[kYY];
   hRr[kXY] = mTRr[kYZ];
   hRr[kYY] = mTRr[kZZ];
   return 0;
}
//______________________________________________________________________________
int StvHitRrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
   assert(hiPos && hiDir && hRr);
   assert(0 && "Not implemented");
   return 0;
}

//______________________________________________________________________________
int StvTpcHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 
// <dX*dX>  = DD/12
// 
// <dY*dX>	 = tP* DD/12
// <dY*dY>  = tP2*DD/12 +WWy/cP2
// 
// <dZ*dX>  = tL/cP*(DD/12) 
// <dZ*dY> =  (tP*tL)/cP*((DD/12 + WWy))
// <dZ*dZ> =  tL2/cP2*(DD/12 + WWy) +WWz

  double Rxy = sqrt(hiPos[0]*hiPos[0]+hiPos[1]*hiPos[1]);
  if ( Rxy < 100 ) {assert( strstr(GetName(),"Inner"));}

  int ans = CalcLocals(hiDir);
  if (ans) return ans;

  memset(mDD[0],0,mNPar*sizeof(mDD[0]));
  memset(mDRr,0,sizeof(mDRr));

  double myTp = mSp/mCp, myTp2 = myTp*myTp;
  double myTl = mSl/mCl, myTl2 = myTl*myTl;
  mZSpan = fabs(fabs(hiPos[2])-210)/100;
  double WWyy = mPar[kYYDiff]*mZSpan;
  double WWzz = mPar[kZZDiff]*mZSpan;
  double DD   = mPar[kThkDet]/12;

  double qCpCl = sqrt(mCpCl);
  double yF   = mPar[kYFact ]*qCpCl; 
  double zF   = mPar[kZFact ]*qCpCl; 

// <dX*dX>  = DD/12
// 
// <dY*dX>  = tP   * DD/12 *sqrt(yFact)
// <dY*dY>  = tP2*(DD/12) +WWy/cP2
// 
// <dZ*dX>  = tL/cP*(DD/12) 
// <dZ*dY> =  (tP*tL)/cP*((DD/12 + WWy))
// <dZ*dZ> =  tL2/cP2*(DD/12 + WWy) +WWz +AB/cP2

//   mDRr[kXX] = DD;
// 
//   mDRr[kYX] = myTp*DD*yF;
//   mDRr[kZX] = myTl/mCp*DD*zF;
// 
  mDRr[kYY] =(myTp2*DD + WWyy/mCp2)*yF*yF + mPar[kYErr];
  mDRr[kZY] = myTp*myTl/mCp*(DD + WWyy)*yF*zF;
  mDRr[kZZ] = (myTl2/mCp2*(DD +WWyy) + WWzz + mPar[kZAB2]/180/mCp2)*zF*zF + mPar[kZErr];

//		NOW DERIVATIVEs

// 			<dX*dX>  = DD/12
// 

//  			mDRr[kYX] = myTp*DD*yF;
//  mDD[kYFact ][kYX] = myTp*DD*qCpCl;


//  			mDRr[kZX] = myTl/mCp*DD*zF;
//  mDD[kZFact ][kZX] = myTl/mCp*DD*qCpCl;

//  			mDRr[kYY] =(myTp2*DD + WWyy/mCp2)*yF*yF + mPar[kYErr];
  mDD[kYErr  ][kYY] = 1;;
  mDD[kYFact ][kYY] = (myTp2*DD + WWyy/mCp2)*yF*2*qCpCl;
  mDD[kYYDiff][kYY] = mZSpan/mCp2*yF*yF;

//		  	mDRr[kZY] = myTp*myTl/mCp*(DD + WWyy)*yF*zF;
  mDD[kYFact ][kZY] =  myTp*myTl/mCp*(DD + WWyy)*zF*qCpCl;
  mDD[kZFact ][kZY] =  myTp*myTl/mCp*(DD + WWyy)*yF*qCpCl;
  mDD[kYYDiff][kZY] =  mZSpan*myTp*myTl/mCp*yF*zF;

//  		mDRr[kZZ] = (myTl2/mCp2*(DD +WWyy) + WWzz + mPar[kZAB2]/180/mCp2)*zF*zF + mPar[kZErr];
  mDD[kZErr  ][kZZ] = 1;;
  mDD[kZFact ][kZZ] = (myTl2/mCp2*(DD +WWyy) + WWzz + mPar[kZAB2]/180/mCp2)*zF*2*qCpCl;
  mDD[kYYDiff][kZZ] = mZSpan*myTl2/mCp2*zF*zF;
  mDD[kZZDiff][kZZ] = mZSpan*zF*zF;
  mDD[kZAB2  ][kZZ] = 1./180/mCp2*zF*zF;


  assert(mDRr[kYY]>0);
  assert(mDRr[kZZ]>0);
  assert(mDRr[kYY]*mDRr[kZZ]>mDRr[kZY]*mDRr[kZY]);

  if (!hRr) return 0;
  hRr[kXX] = mDRr[kYY];
  hRr[kYY] = mDRr[kZZ];
  hRr[kYX] = mDRr[kZY];

  return 0;

}  
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
int StvTpcGeoErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
// <dX*dX>  = DD/12
// 
// <dY*dX>  = tP*DD/12
// <dY*dY>  = tP2*(DD/12) +WWy/cP2
// 
// <dZ*dX>  = tL/cP*(DD/12) 
// <dZ*dY> =  (tP*tL)/cP*((DD/12 + WWy))
// <dZ*dZ> =  tL2/cP2*(DD/12 + WWy) +WWz

/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 

  int ans =CalcLocals(hiDir);
  if (ans) return ans;

  memset(mDD[0],0,mNPar*sizeof(mDD[0]));
  double myTp = mSp/mCp, myTp2 = myTp*myTp;
  double myTl = mSl/mCl, myTl2 = myTl*myTl;

  mZSpan = fabs(fabs(hiPos[2])-210)/100;
  mDD[kYThkDet][kXX] = 0.5/12;
  mDD[kZThkDet][kXX] = 0.5/12;

  mDD[kYThkDet][kYX] = myTp /12;

  mDD[kYThkDet][kYY] = myTp2/12;
  mDD[kYDiff  ][kYY] = mZSpan/mCp2;

  mDD[kZThkDet][kZX] = myTl/mCp/12;

  mDD[kYThkDet][kZY] = myTl*myTp/mCp/12 *0.5;
  mDD[kZThkDet][kZY] = myTl*myTp/mCp/12 *0.5;
  mDD[kYDiff  ][kZY] = mZSpan*myTl*myTp/mCp;

  mDD[kZThkDet][kZZ] = myTl2/mCp2/12;
  mDD[kYDiff  ][kZZ] = mZSpan*myTl2/mCp2;
  mDD[kZDiff  ][kZZ] = mZSpan;
  TCL::vscale(mDD[0],mCpCl,mDD[0],6*mNPar);

  for (int ig=0;ig<6;ig++) {
    double s = 0;
    for (int ip=0;ip<mNPar;ip++) {s+=mDD[ip][ig]*mPar[ip];}
    mDRr[ig]=s;
  } 
  if (!hRr) return 0;
  hRr[kXX] = mDRr[kYY];
  hRr[kYY] = mDRr[kZZ];
  hRr[kYX] = mDRr[kZY];
  return 0;
}  

//______________________________________________________________________________
int StvTpcStiErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 
// <dX*dX>  = DD/12
// 
  int ans = CalcLocals(hiDir);
  if (ans) return ans;
  mZSpan = fabs(fabs(hiPos[2])-210)/100;
  mCpCl = 1;
  double myTp = mSp/mCp, myTp2 = myTp*myTp;
  double myTl = mSl/mCl, myTl2 = myTl*myTl;

  double DDy = mPar[kYThkDet];
  double DDz = mPar[kZThkDet];
  double WWy = mPar[kYDiff]*mZSpan;
  double WWz = mPar[kZDiff]*mZSpan;

  memset(mDD[0],0,mNPar*sizeof(mDD[0]));
//	Sti code
//  ecross=Coeff[0]+Coeff[1]*dz/(cosCA*cosCA) +Coeff[2]*tanCA* tanCA;
//  edip  =Coeff[3]+Coeff[4]*dz*cosDipInv2    +Coeff[5]*tanDip*tanDip;


  mDRr[kXX] = 1e-6;
  mDRr[kYY] = myTp2*DDy +WWy/mCp2+mPar[kYErr];
  mDRr[kZZ] = myTl2*DDz +WWz/mCl2+mPar[kZErr];

  if (!hRr) return 0;
  hRr[kXX] = mDRr[kYY];
  hRr[kYY] = mDRr[kZZ];
  hRr[kYX] = mDRr[kZY];

  return 0;

}  


#include "TRandom.h"
#include "TVector3.h"
//______________________________________________________________________________
void StvTpcHitErrCalculator::Dest(double phiG,double lamG)
{
  double par[10]={0};
static const char *titPa[9]={"YErr  ","ZErr ","ThkDet","YYDiff","ZZDiff"
                            ,"YZDiff","YFact","ZFact ","ZAB2  "};

static const char *titYZ [3] = {"YY","ZY","ZZ"};
static const char *titXYZ[6] = {"XX","YX","YY","ZX","ZY","XX"};

// kYErr  	=0, 	/* Intrinsic resolution, padrow or Y direction		*/
// kZErr  	=1, 	/* Intrinsic resolution, z direction			*/
// kThkDet	=2,	/* detector thickness**2 , not fitted			*/
// kYYDiff	=3,  	/* Diffusion in XY direction *yFactor			*/
// kZZDiff	=4,  	/* Diffusion in Z direction  *ZFactor			*/
// kYFact 	=5, 	/*	Error factor in Y-direction 			*/
// kZFact 	=6, 	/*	Error factor in Z-direction 			*/
// kZAB2  	=7};	/* Constant member in Z direction (a*b)**2		*/

  par[kYErr]=0.03*0.03;
  par[kZErr]=0.07*0.07;
  par[kThkDet]=1.;
  par[kYYDiff]=0.11;
  par[kZZDiff]=0.12;
  par[kYFact ]=0.9;
  par[kZFact ]=0.8;
  par[kZAB2  ]=1;

  double Lam = lamG/180*M_PI;
  double Phi = phiG/180*M_PI;
  double cL = cos(Lam);
  double sL = sin(Lam);
  double cP = cos(Phi);
  double sP = sin(Phi);
  double Nt[3]={cL*cP,cL*sP,sL};
  float  hiPos[3]   = {100,0,55};
  float  hiDir[3][3]={{1,0,0},{0,1,0},{0,0,1}};

//		Randomize orientation
  double LamH = (gRandom->Rndm()-0.5);
  double PhiH = (gRandom->Rndm()-0.5);
//		copy all info from arrays to TVector3
  TVector3 myV[4];
  for (int i=0;i<3;i++) { myV[i] = TVector3(hiDir[i]);}
  myV[3] = TVector3(Nt);
//		Rotate it
  for (int i=0;i<4;i++) { myV[i].RotateZ(PhiH);myV[i].RotateX(LamH);}

//		copy all info back into arrays
  for (int i=0;i<3;i++) { Nt[i] = myV[3][i];
  for (int j=0;j<3;j++) { hiDir[i][j] = myV[i][j]; }}

  StvTpcHitErrCalculator calc("TpcInnerHitErrs");
  int nPars = calc.GetNPars();
  calc.SetPars(par);
  calc.SetTrack(Nt);
  double hRR[6],dRR[10][3],dRRx[10][6];

  StvTpcHitErrCalculator calk("TpcInnerHitErrs.tmp");
  for (int detDca = 0; detDca<3; detDca++) {
static const char *tit[3] = {"Test CalcDetDers()","Test CalcDcaDers()","Test internals" };   
    printf("\n\n\n StvTpcHitErrCalculator::Dest(%s)\n\n",tit[detDca]);
    switch(detDca) {
      case 0:  {calc.CalcDetErrs(hiPos,hiDir,hRR); calc.CalcDetDers(dRR); break;}
      case 1:  {calc.CalcDcaErrs(hiPos,hiDir,hRR); calc.CalcDcaDers(dRR); break;}
      case 2:  {calc.CalcDetErrs(hiPos,hiDir,hRR); calc.CalcDetDers(dRR);
                memcpy(hRR    ,calc.mDRr  ,sizeof(hRR ));
		memcpy(dRRx[0],calc.mDD[0],sizeof(dRRx));                 break;}
    };
    for (int j=0;j<3;j++) {
      printf("hRR[%d]=%g  Der = %g %g %g %g\n",j,hRR[j]
            ,dRR[0][j],dRR[1][j],dRR[2][j],dRR[3][j]);
    }

    for (int ider=0;ider<nPars;ider++) {
      if (ider ==kThkDet) 	continue;
      double myPar[10],delta;
      memcpy(myPar,par,nPars*sizeof(par[0]));
      delta = myPar[ider]*1e-2;
      if (delta<1e-6) delta=1e-6;
      myPar[ider]+=delta;
      calk.SetPars(myPar);
      calk.SetTrack(Nt);
      double myRR[6];
      switch(detDca) {
        case 0: calk.CalcDetErrs(hiPos,hiDir,myRR);break;
        case 1: calk.CalcDcaErrs(hiPos,hiDir,myRR);break;
        case 2: calk.CalcDetErrs(hiPos,hiDir,myRR);
                memcpy(myRR,calk.mDRr,sizeof(myRR));
      };
      
      switch(detDca) {
      case 0:; case 1:;
      for (int j=0;j<3;j++) {
	double est = (myRR[j]-hRR[j])/delta;
	double eps = (dRR[ider][j]-est)/(fabs(dRR[ider][j])+fabs(est)+1e-10);
	printf("Der[%s][%s]=%g \tnum=%g \teps=%g\n",titPa[ider],titYZ[j],dRR[ider][j],est,eps);
      }
      break;
      case 2:
      for (int j=0;j<6;j++) {
	double est = (myRR[j]-hRR[j])/delta;
	double eps = (dRRx[ider][j]-est)/(fabs(dRRx[ider][j])+fabs(est)+1e-10);
	printf("Der[%s][%s]=%g \tnum=%g \teps=%g\n",titPa[ider],titXYZ[j],dRRx[ider][j],est,eps);
      } };//end switch
    }
  }
}

#if 1
//______________________________________________________________________________
void StvHitErrCalculator::Test(double phiG,double lamG)
{
  int nPar = 2;
  double par[6]={0};
  par[kYErr]=0.1*0.1;
  par[kZErr]=0.2*0.2;

//   double Lam = 3.14*(gRandom->Rndm()-0.5)/2;
//   double Phi = 3.14*(gRandom->Rndm()-0.5);
  double Lam = lamG/180*M_PI;
  double Phi = phiG/180*M_PI;
  double W = 0; 	//Width of the track
  double D = 0;  	//Thickness of detector plane 
  double cL = cos(Lam);
  double sL = sin(Lam);
  double tL = tan(Lam);
  double cP = cos(Phi);
  double sP = sin(Phi);
  TVector3 Nt(cL*cP,cL*sP,sL);
  TVector3 Np(-sP, cP, 0);
  TVector3 Nl(-sL*cP,-sL*sP,cL);

  TVector3 V;
  double YZ[3]={0},BG[3]={0};
  int nEl=100000,iEl=0;
  while (1) {
    double alfa = D/(Nt[0])*(gRandom->Rndm()-0.5)*10;
    double beta = gRandom->Gaus()*W;
    double gama = gRandom->Gaus()*W;
    V = Nt*alfa + Np*beta + Nl*gama;
    if (fabs((V[0]))>0.5*D) continue;
    V[1]+=  gRandom->Gaus()*sqrt(par[kYErr]);
    V[2]+=  gRandom->Gaus()*sqrt(par[kZErr]);

    if(++iEl>=nEl) break;

//    Project along X to X=0
    alfa = (V[0]); V[0] =0;
    beta = (Np*V);
    gama = (Nl*V);
    BG[0]+=beta*beta; BG[1]+=beta*gama;BG[2]+=gama*gama;

    beta = (V[1]);
    gama = (V[2]);
    YZ[0] += beta*beta; YZ[1] += beta*gama;YZ[2] += gama*gama;
  }
  for (int j=0;j<3;j++){YZ[j]/=nEl; BG[j]/=nEl;} 
  
  printf("Phi=%d Lam=%d: \tYY=%g \tYZ=%g \tZZ=%g \tTrace=%g\n"
        , int(Phi/3.1415*180),int(Lam/3.1415*180),YZ[0],YZ[1],YZ[2],YZ[0]+YZ[2]);

  StvHitErrCalculator calc("",nPar);
  calc.SetPars(par);
  double np[3]={ cP,  sP, tL};
  double hitErr[3];
  calc.SetTrack(np);
  float hiDir[3][3]={{1,0,0}
                    ,{0,1,0}
		    ,{0,0,1}};
  calc.CalcDetErrs(0,hiDir,hitErr);
  printf("Det Calc:  \tYY=%g \tYZ=%g \tZZ=%g \tTrace=%g\n"
        ,hitErr[0],hitErr[1],hitErr[2],hitErr[0]+hitErr[2]);

  printf("DCA       : \tBB=%g \tBG=%g \tGG=%g \tTrace=%g\n",BG[0],BG[1],BG[2],BG[0]+BG[2]);
  calc.CalcDcaErrs(0,hiDir,hitErr);
  printf("DCA Calc  : \tBB=%g \tBG=%g \tGG=%g \tTrace=%g\n"
        ,hitErr[0],hitErr[1],hitErr[2],hitErr[0]+hitErr[2]);

   double LamH = (gRandom->Rndm()-0.5);
   double PhiH = (gRandom->Rndm()-0.5);
//		copy all info from arrays to TVector3
   TVector3 myV[4];
   for (int i=0;i<3;i++) { myV[i] = TVector3(hiDir[i]);}
   myV[3] = TVector3(np);
//		Rotate it
   for (int i=0;i<4;i++) { myV[i].RotateZ(PhiH);myV[i].RotateX(LamH);}

//		copy all info back into arrays
   for (int i=0;i<3;i++) { np[i] = myV[3][i];
   for (int j=0;j<3;j++) { hiDir[i][j] = myV[i][j]; }}
//		Now try how life is here
   calc.SetTrack(np);
   calc.CalcDetErrs(0,hiDir,hitErr);
   printf("Det Rot   :  \tYY=%g \tYZ=%g \tZZ=%g \tTrace=%g\n"
        ,hitErr[0],hitErr[1],hitErr[2],hitErr[0]+hitErr[2]);

   double myHitErr[3];
   double myT[2][2]= {{cP,0},{-sL*sP,cL}};
   TCL::trasat(myT[0],hitErr,myHitErr,2,2); 
   printf("DCAtest   : \tBB=%g \tBG=%g \tGG=%g \tTrace=%g\n"
        ,myHitErr[0],myHitErr[1],myHitErr[2],myHitErr[0]+myHitErr[2]);


   calc.CalcDcaErrs(0,hiDir,hitErr);
   printf("DCA Rot   : \tBB=%g \tBG=%g \tGG=%g \tTrace=%g\n"
        ,hitErr[0],hitErr[1],hitErr[2],hitErr[0]+hitErr[2]);



}
//______________________________________________________________________________
void StvHitErrCalculator::Dest(double phiG,double lamG)
{
  int nPars = 2;
  double par[6]={0};
  par[kYErr]=0.1*0.1;
  par[kZErr]=0.2*0.2;

  double Lam = lamG/180*M_PI;
  double Phi = phiG/180*M_PI;
  double cL = cos(Lam);
  double sL = sin(Lam);
  double cP = cos(Phi);
  double sP = sin(Phi);
  double Nt[3]={cL*cP,cL*sP,sL};
  float  hiPos[3]   = {100,0,0};
  float  hiDir[3][3]={{1,0,0},{0,1,0},{0,0,1}};

   double LamH = (gRandom->Rndm()-0.5);
   double PhiH = (gRandom->Rndm()-0.5);
//		copy all info from arrays to TVector3
   TVector3 myV[4];
   for (int i=0;i<3;i++) { myV[i] = TVector3(hiDir[i]);}
   myV[3] = TVector3(Nt);
//		Rotate it
   for (int i=0;i<4;i++) { myV[i].RotateZ(PhiH);myV[i].RotateX(LamH);}

//		copy all info back into arrays
   for (int i=0;i<3;i++) { Nt[i] = myV[3][i];
   for (int j=0;j<3;j++) { hiDir[i][j] = myV[i][j]; }}



  StvHitErrCalculator calc("",nPars);
  calc.SetPars(par);
  calc.SetTrack(Nt);
  double hRR[3],dRR[10][3];
  calc.CalcDcaErrs(hiPos,hiDir,hRR);
  calc.CalcDcaDers(dRR);
  for (int j=0;j<nPars;j++) {
    printf("hRR[%d]=%g  Der = %g %g %g %g\n",j,hRR[j]
          ,dRR[0][j],dRR[1][j],dRR[2][j],dRR[3][j]);
  }

  StvHitErrCalculator calk("",nPars);
  for (int ider=0;ider<nPars;ider++) {
    double myPar[4],delta;
    memcpy(myPar,par,sizeof(myPar));
    delta = myPar[ider]*1e-1;
    myPar[ider]+=delta;
    calk.SetPars(myPar);
    myPar[ider]=par[ider];
    calk.SetTrack(Nt);
    double myRR[3];
    calk.CalcDcaErrs(hiPos,hiDir,myRR);
    for (int j=0;j<3;j++) {
      double est = (myRR[j]-hRR[j])/delta;
      double eps = (dRR[ider][j]-est)/(fabs(dRR[ider][j])+fabs(est)+1e-10);
      printf("Der[%d][%d]=%g \tnum=%g \teps=%g\n",ider,j,dRR[ider][j],est,eps);
    }
  }
}

#include "Stv/StvHit.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
//______________________________________________________________________________
int StvHitErrCalculator::CalcDcaErrs(const StvHit *hit,double hRr[3])
{
   const StHitPlane *hp = hit->detector(); 
   const Mtx33F_t &hD = hp->GetDir(hit->x());
   return CalcDcaErrs(hit->x(),hD,hRr);
}

#endif //0
