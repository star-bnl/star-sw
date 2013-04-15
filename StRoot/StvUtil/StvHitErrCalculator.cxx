#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "TCernLib.h"
#include "StvHitErrCalculator.h"
#include "StvUtil/StvDebug.h"
#include <map>
#include <string>

static std::map<std::string,StvHitErrCalculator *> calcMap;
static const double kMinCos = 0.01, kMinCpCl=0.1;
static const double kMaxSin  = (1-kMinCos)*(1+kMinCos);



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
  
  nor = (1.-mTG[0][2]*mTG[0][2]);
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
void StvHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 
//		Nt = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  memset(mDD[0],0,mNPar*3*sizeof(mDD[0][0]));
  mDD[0][0] = 1;
  mDD[1][2] = 1;
  hRr[0] =  mDD[kYErr][0]*mPar[kYErr];
  hRr[2] =  mDD[kZErr][2]*mPar[kZErr];
  hRr[1] = 0;

}  
//______________________________________________________________________________
void StvHitErrCalculator::CalcLocals(const float hiDir[3][3])
{
/// Calculate hit error matrix in DCA  system. In this system
/// track is along x axis, Y axis comes thru hit point 
static const double s15 = sin(3.14/180*15);
static const double c15 = cos(3.14/180*15);
static const double s45 = sin(3.14/180*45);
static const double c45 = cos(3.14/180*45);

  mFailed = 0;
  if (!hiDir) {
    mSp  = s15    ; mCp  = c15    ; mSl  = s45    ; mCl  = c45    ;
    mSp2 = s15*s15; mCp2 = c15*c15; mSl2 = s45*s45; mCl2 = c45*c45;
    mCpCl = mCp*mCl;
    return;
  }
  for (int j=0;j<3;j++) {
    mTL[j] = (hiDir[j][0]*mTG[0][0]+hiDir[j][1]*mTG[0][1]+hiDir[j][2]*mTG[0][2]);}

//		mTL = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  mSl = mTL[2],mCl2 = ((1-mSl)*(1+mSl));
  if (mCl2<kMinCos*kMinCos) 	{	//Lambda too big
    mCl=kMinCos; mCl2=mCl*mCl; mSl = (mSl<0)? -kMaxSin:kMaxSin;
    mSp = 0; mCp = 1; mFailed = 1;
  } else           		{	//Normal case
    mCl=sqrt(mCl2); mSp = mTL[1]/mCl; mCp = mTL[0]/mCl; 
  }
  if (fabs(mCp) < kMinCos) { 		//Phi (psi) too big
    mCp = (mCp<0)? -kMinCos:kMinCos; 
    mSp = (mSp<0)? -kMaxSin:kMaxSin; mFailed = 2;
  }
  mSp2 = mSp*mSp; mCp2 = mCp*mCp; mSl2=mSl*mSl;
  mCpCl = fabs(mCp*mCl); if (mCpCl < kMinCpCl) mCpCl = kMinCpCl;
  for (int i=0;i<2;i++) {for (int j=0;j<2;j++) { double s = 0;
    for (int k=0;k<3;k++) {s+=mTG[i+1][k]*hiDir[j+1][k];}
    mTT[i][j] = s;}};
  if (fabs(mCp)<0.1) StvDebug::Break(-1);

}
//______________________________________________________________________________
void StvHitErrCalculator::CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in DCA  system. In this system
/// track is along x axis, Y axis comes thru hit point 
   double detHitErr[3];
   mCp2 = -1; 			//To test of calling CalcLocals
   CalcDetErrs(hiPos,hiDir,detHitErr);
   if (mCp2> -1) ;CalcLocals(hiDir);

   double Phi = atan2(hiPos[1],hiPos[0]);
   Phi =  (int(fmod((Phi/M_PI*180)+15,30)))*30./180.*M_PI;
   

   
   double rxy  = cos(Phi)*hiPos[0]+sin(Phi)*hiPos[1];
   double psid = atan2(mSp,mCp)/M_PI*180;
   double lamd = atan2(mSl,mCl)/M_PI*180;
   double cros = acos(mCp*mCl)/M_PI*180;
   if (fabs(psid) <90 ) { //ignore seed hit errs calculation
   if (rxy<123) {//inner
     StvDebug::Count("InnYYdet",          sqrt(detHitErr[0]));
     StvDebug::Count("InnYYdet:Psi",psid, sqrt(detHitErr[0]));

     StvDebug::Count("InnZZdet",          sqrt(detHitErr[2]));
     StvDebug::Count("InnZZdet:Lam",lamd, sqrt(detHitErr[2]));

     StvDebug::Count("InnZZdet:Z"  ,hiPos[2], sqrt(detHitErr[2]));
     StvDebug::Count("InnYYdet:Z"  ,hiPos[2], sqrt(detHitErr[0]));
   } else {
     StvDebug::Count("OutYYdet",          sqrt(detHitErr[0]));
     StvDebug::Count("OutYYdet:Psi",psid, sqrt(detHitErr[0]));

     StvDebug::Count("OutZZdet",          sqrt(detHitErr[2]));
     StvDebug::Count("OutZZdet:Lam",lamd, sqrt(detHitErr[2]));

     StvDebug::Count("OutZZdet:Z"  ,hiPos[2], sqrt(detHitErr[2]));
     StvDebug::Count("OutYYdet:Z"  ,hiPos[2], sqrt(detHitErr[0]));
   } }
   TCL::trasat(mTT[0],detHitErr,hRr,2,2); 
   if (fabs(psid) <90 ) { //ignore seed hit errs calculation
   if (rxy<123) {//inner
     StvDebug::Count("InnYYdca",          sqrt(hRr[0]));
     StvDebug::Count("InnYYdca:Psi",psid, sqrt(hRr[0]));

     StvDebug::Count("InnZZdca",          sqrt(hRr[2]));
     StvDebug::Count("InnZZdca:Lam",lamd, sqrt(hRr[2]));

     StvDebug::Count("InnZZdca:Z",hiPos[2], sqrt(hRr[2]));
     StvDebug::Count("InnYYdca:Z",hiPos[2], sqrt(hRr[0]));

     StvDebug::Count("InnYZdca"           ,hRr[1]/sqrt(hRr[0]*hRr[2]));
     StvDebug::Count("InnYZdca:Cross",cros,hRr[1]/sqrt(hRr[0]*hRr[2]));
   } else {
     StvDebug::Count("OutYYdca",          sqrt(hRr[0]));
     StvDebug::Count("OutYYdca:Psi",psid, sqrt(hRr[0]));

     StvDebug::Count("OutZZdca",          sqrt(hRr[2]));
     StvDebug::Count("OutZZdca:Lam",lamd, sqrt(hRr[2]));

     StvDebug::Count("OutZZdca:Z",hiPos[2], sqrt(hRr[2]));
     StvDebug::Count("OutYYdca:Z",hiPos[2], sqrt(hRr[0]));

     StvDebug::Count("OutYZdca"           ,hRr[1]/sqrt(hRr[0]*hRr[2]));
     StvDebug::Count("OutYZdca:Cross",cros,hRr[1]/sqrt(hRr[0]*hRr[2]));
   } }
} 
//______________________________________________________________________________
double StvHitErrCalculator::Trace(const float*) 
{
  return mPar[kYErr]+mPar[kZErr];
}
//______________________________________________________________________________
void StvHitErrCalculator::CalcDcaDers(double dRR[kMaxPars][3])
{
// Calculate deriavatives of err matrix.
// must be called after CalcDcaErrs(...)
  for (int iPar=0;iPar<mNPar;iPar++) {
    TCL::trasat(mTT[0],mDD[iPar],dRR[iPar],2,2); 
  }

}
//______________________________________________________________________________
//______________________________________________________________________________
void StvTpcHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 

  CalcLocals(hiDir);
  memset(mDD[0],0,mNPar*3*sizeof(mDD[0][0]));
  mZSpan = fabs(fabs(hiPos[2])-210)/100;
  mDD[kYErr   ][0] = 1;
  mDD[kYDiff  ][0] = mZSpan/mCp2	*mCpCl;
  mDD[kYThkDet][0] = mSp2/mCp2/12	*mCpCl;

  mDD[kZErr   ][2] = 1;
  mDD[kZDiff  ][2] = mZSpan		*mCpCl;
  mDD[kYDiff  ][2] = mZSpan*mSl2/mCl2	*mCpCl;
  mDD[kZThkDet][2] = mSl2/mCl2/12	*mCpCl;
  mDD[kZAB2   ][2] = 1./180		*mCpCl;

  for (int ig=0;ig<3;ig++) {
    hRr[ig]=0;
    for (int ip=0;ip<mNPar;ip++) {
      if(!mDD[ip][ig]) continue;
      hRr[ig]+=mDD[ip][ig]*mPar[ip];
  } }

  if (hRr[0]>100) hRr[0]=100;
  if (hRr[2]>100) hRr[1]=100;

}  
//______________________________________________________________________________
double StvTpcHitErrCalculator::Trace(const float hiPos[3]) 
{
  double hiErr[3];
  CalcDetErrs(hiPos,0,hiErr);
  return hiErr[0]+hiErr[2];
}



#include "TRandom.h"
#include "TVector3.h"
//______________________________________________________________________________
void StvTpcHitErrCalculator::Dest(double phiG,double lamG)
{
  double par[10]={0};
  par[kYErr]=0.03*0.03;
  par[kZErr]=0.07*0.07;
  par[kYThkDet]=1.;
  par[kZThkDet]=1.5;
  par[kYDiff]=par[kYErr]*1;
  par[kZDiff]=par[kZErr]*2;
  par[kZAB2 ]=1;

  double Lam = lamG/180*M_PI;
  double Phi = phiG/180*M_PI;
  double cL = cos(Lam);
  double sL = sin(Lam);
  double cP = cos(Phi);
  double sP = sin(Phi);
  double Nt[3]={cL*cP,cL*sP,sL};
  float  hiPos[3]   = {100,0,155};
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

  StvTpcHitErrCalculator calc("UUUUUUUUU");
  int nPars = calc.GetNPars();
  calc.SetPars(par);
  calc.SetTrack(Nt);
  double hRR[3],dRR[10][3];
  calc.CalcDcaErrs(hiPos,hiDir,hRR);
  calc.CalcDcaDers(dRR);
  for (int j=0;j<3;j++) {
    printf("hRR[%d]=%g  Der = %g %g %g %g\n",j,hRR[j]
          ,dRR[0][j],dRR[1][j],dRR[2][j],dRR[3][j]);
  }

  StvTpcHitErrCalculator calk("");
  for (int ider=0;ider<nPars;ider++) {
    double myPar[10],delta;
    memcpy(myPar,par,nPars*sizeof(par[0]));
    delta = myPar[ider]*1e-1;
    if (delta<1e-6) delta=1e-6;
    myPar[ider]+=delta;
    calk.SetPars(myPar);
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
#if 0
//______________________________________________________________________________
void StvHitErrCalculator::Test(double phiG,double lamG)
{
  double par[6]={0};
  par[kYErr]=0.1*0.1;
  par[kZErr]=0.2*0.2;
  par[kThkDet]=9*9/12;
  par[kWidTrk]=0.1*0.1;

//______________________________________________________________________________
void StvHitErrCalculator::Test(double phiG,double lamG)
{
  double par[6]={0};
  par[kYErr]=0.1*0.1;
  par[kZErr]=0.2*0.2;
  par[kThkDet]=9*9/12;
  par[kWidTrk]=0.1*0.1;

//   double Lam = 3.14*(gRandom->Rndm()-0.5)/2;
//   double Phi = 3.14*(gRandom->Rndm()-0.5);
  double Lam = lamG/180*M_PI;
  double Phi = phiG/180*M_PI;
  double W = sqrt(par[kWidTrk]); 	//Width of the track
  double D = sqrt(par[kThkDet]*12);  	//Thickness of detector plane 
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

  StvHitErrCalculator calc("",4);
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
  double par[6]={0};
  par[kYErr]=0.03*0.03;
  par[kZErr]=0.07*0.07;
  par[kThkDet]=3*3/12.;
  par[kWidTrk]=0.1*0.1;

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



  StvHitErrCalculator calc("",4);
  calc.SetPars(par);
  calc.SetTrack(Nt);
  double hRR[3],dRR[10][3];
  calc.CalcDcaErrs(hiPos,hiDir,hRR);
  calc.CalcDcaDers(dRR);
  for (int j=0;j<3;j++) {
    printf("hRR[%d]=%g  Der = %g %g %g %g\n",j,hRR[j]
          ,dRR[0][j],dRR[1][j],dRR[2][j],dRR[3][j]);
  }

  StvHitErrCalculator calk("",4);
  for (int ider=0;ider<4;ider++) {
    double myPar[4],delta;
    memcpy(myPar,par,sizeof(myPar));
    delta = myPar[ider]*1e-1;
    myPar[ider]+=delta;
    calk.SetPars(myPar);
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
#endif //0
