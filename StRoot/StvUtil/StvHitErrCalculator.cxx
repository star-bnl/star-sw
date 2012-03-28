#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "TCernLib.h"
#include "StvHitErrCalculator.h"
#include <map>
#include <string>

static std::map<std::string,StvHitErrCalculator *> calcMap;



// //______________________________________________________________________________
// double Det33(double T[3][3])
// {
// //  	00 01 02
// //  	10 11 12
// //  	20 21 22
//   T[0][0]*T[1][1]*T[2][2]+T[0][1]*T[1][2]*T[2][0]+T[1][0]*T[2][2]*T[0][2]
//  -T[2][0]*T[1][1]*T[0][2]

ClassImp(StvHitErrCalculator)
//______________________________________________________________________________
StvHitErrCalculator::StvHitErrCalculator(const char *name):TNamed(name,"")
{
  memset(mPar,0,sizeof(mPar));
  if (!*GetName()) return;
  StvHitErrCalculator *&calc = calcMap[GetName()];
  assert(!calc);
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
  double nor = sqrt(tkDir[0]*tkDir[0]+tkDir[1]*tkDir[1]+tkDir[2]*tkDir[2]);
  double sinL = tkDir[2]/nor;
  double cosL=((1-sinL)*(1+sinL)); 
  if (cosL<1e-6) {cosL=0;sinL=1;} else {cosL = sqrt(cosL);}
  double cosP=1,sinP=0;
  if (cosL) { cosP = tkDir[0]/cosL/nor; sinP = tkDir[1]/cosL/nor;}

  mNG[0][0]= cosL*cosP; mNG[0][1]= cosL*sinP; mNG[0][2]= sinL;
  mNG[1][0]=-sinP;      mNG[1][1]= cosP;      mNG[1][2]= 0;
  mNG[2][0]=-sinL*cosP; mNG[2][1]=-sinL*sinP; mNG[2][2]= cosL;
}
//______________________________________________________________________________
void StvHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 

  float Nt[3];
//		Nt == track direction in Hit frame
  for (int j=0;j<3;j++) {
    Nt[j] = hiDir[j][0]*mNG[0][0]+hiDir[j][1]*mNG[0][1]+hiDir[j][2]*mNG[0][2];
  }
//		Nt = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  mSl = Nt[2],mCl = sqrt((1-mSl)*(1+mSl));
  mSp = Nt[1]/mCl, mCp = Nt[0]/mCl;
  hRr[0] = (mPar[kThkDet]*mSp*mSp   + mPar[kWidTrk])
           / (mCp*mCp) + mPar[kYErr];
  hRr[2] = (mPar[kThkDet]*(mSl*mSl) + mPar[kWidTrk]*((mSp*mSl)*(mSp*mSl)+mCp*mCp))
           / ((mCp*mCl)*(mCp*mCl)) + mPar[kZErr];
  hRr[1] = (mPar[kThkDet]           + mPar[kWidTrk])*(mSp*mSl)/(mCp*mCp*mCl);

}  
//______________________________________________________________________________
void StvHitErrCalculator::CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
/// Calculate hit error matrix in DCA  system. In this system
/// track is along x axis, Y axis comes thru hit point 

  float *NtG = mNG[0];		   
  float  NL[3][3], *Nt=NL[0],*Np=NL[1],*Nl=NL[2];
  for (int j=0;j<3;j++) {
    Nt[j] = (hiDir[j][0]*NtG[0]+hiDir[j][1]*NtG[1]+hiDir[j][2]*NtG[2]);}

//		Nt = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  mSl = Nt[2],mCl = ((1-mSl)*(1+mSl));
  if (mCl<1e-6) { mCl=0;         mSp = 0;         mCp = 1;        }
  else          { mCl=sqrt(mCl); mSp = Nt[1]/mCl, mCp = Nt[0]/mCl;}
  Np[0]=-mSp;     Np[1]=mCp;     Np[2]=0;
  Nl[0]=-mSl*mCp; Nl[1]=-mSl*mSp;Nl[2]=mCl;

  float tmp[3][3],T[3][3];
  TCL::mxmpy3(hiDir[0],NL[0] ,tmp[0],3,3,3);
  TCL::mxmpy (mNG[0]   ,tmp[0],T[0]  ,3,3,3);
  mTT[0][0]=T[1][1]; mTT[0][1]=T[1][2];mTT[1][0]=T[2][1];mTT[1][1]=T[2][2];
  float myDet = mTT[0][0]*mTT[1][1]-mTT[0][1]*mTT[1][0];
  assert(fabs(fabs(myDet)-1)<0.001);


  float g[3],G[3];
  memset(mDD[0],0,sizeof(mDD));
////  g[0] = mPar[kThkDet]*mSp*mSp 	   + mPar[kWidTrk] + mPar[kYErr]*mCp*mCp;
////  g[2] = mPar[kThkDet]*mCp*mCp*mSl*mSl + mPar[kWidTrk] + mPar[kYErr]*(mSl*mSp)*(mSl*mSp)+ mPar[kZErr]*mCl*mCl;
////  g[1] = mPar[kThkDet]*mCp*mSp*mSl;
  double amp = fabs(mCp*mCl);
  amp = 1; //??????????????????????????????????????????????????????
  mDD[kThkDet][0] = mSp*mSp		*amp;	      
  mDD[kWidTrk][0] = 1			*amp;
  mDD[kYErr  ][0] = mCp*mCp		*amp;

  mDD[kThkDet][1] = mCp*mSp*mSl		*amp;

  mDD[kThkDet][2] = mCp*mCp*mSl*mSl	*amp;
  mDD[kWidTrk][2] = 1			*amp;
  mDD[kYErr  ][2] = (mSl*mSp)*(mSl*mSp)	    ;
  mDD[kZErr  ][2] = mCl*mCl		    ;

  g[0] = mDD[kThkDet][0]*mPar[kThkDet]+mDD[kWidTrk][0]*mPar[kWidTrk]+mDD[kYErr][0]*mPar[kYErr];
  g[1] = mDD[kThkDet][1]*mPar[kThkDet];
  g[2] = mDD[kThkDet][2]*mPar[kThkDet]+mDD[kWidTrk][2]*mPar[kWidTrk]+mDD[kYErr][2]*mPar[kYErr]+mDD[kZErr][2]*mPar[kZErr];

  TCL::trasat(mTT[0], g, G, 2,2);
  hRr[0]=G[0]; hRr[1]=G[1]; hRr[2]=G[2];
} 
//______________________________________________________________________________
double StvHitErrCalculator::Trace(const float*) {
  return mPar[kWidTrk]+mPar[kYErr]+mPar[kWidTrk]+mPar[kZErr];
}
//______________________________________________________________________________
void StvHitErrCalculator::CalcDcaDers(double dRR[kMaxPars][3])
{
// Calculate deriavatives of err matrix.
// must be called after CalcDcaErrs(...)
  memset(dRR[0],0,sizeof(double)*3*kMaxPars);
  float G[3];
  for (int ip=0;ip<4;ip++) {
    TCL::trasat(mTT[0], mDD[ip], G, 2,2);
    for (int j=0;j<3;j++) {dRR[ip][j]=G[j];};
  }
}
  
//______________________________________________________________________________
//______________________________________________________________________________
void StvTpcHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3])
{
    float zSpan = fabs(hiPos[2]-210)/100;
    double save1 = mPar[kYErr],save2 = mPar[kZErr];
    mPar[kYErr]+=mPar[kYDiff]*zSpan;
    mPar[kZErr]+=mPar[kZDiff]*zSpan;
    StvHitErrCalculator::CalcDetErrs(hiPos,hiDir,hRR);
    mPar[kYErr]=save1;mPar[kZErr]=save2;
}
//______________________________________________________________________________
void StvTpcHitErrCalculator::CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3])
{
    mZSpan = fabs(hiPos[2]-210)/100;
    double save1 = mPar[kYErr],save2 = mPar[kZErr];
    mPar[kYErr]+=mPar[kYDiff]*mZSpan;
    mPar[kZErr]+=mPar[kZDiff]*mZSpan;
    StvHitErrCalculator::CalcDcaErrs(hiPos,hiDir,hRR);
    mPar[kYErr]=save1;mPar[kZErr]=save2;
}
//______________________________________________________________________________
void StvTpcHitErrCalculator::CalcDcaDers(double dRR[kMaxPars][3])
{
   StvHitErrCalculator::CalcDcaDers(dRR);
   for (int j=0;j<3;j++) {
     dRR[kYDiff][j]=dRR[kYErr][j]*mZSpan;
     dRR[kZDiff][j]=dRR[kZErr][j]*mZSpan;
   }
}
//______________________________________________________________________________
double StvTpcHitErrCalculator::Trace(const float hiPos[3]) 
{
    mZSpan = fabs(hiPos[2]-210)/100;
    double save1 = mPar[kYErr],save2 = mPar[kZErr];
    mPar[kYErr]+=mPar[kYDiff]*mZSpan;
    mPar[kZErr]+=mPar[kZDiff]*mZSpan;
    double err2 = StvHitErrCalculator::Trace(hiPos);
    mPar[kYErr]=save1;mPar[kZErr]=save2;
    return err2;
}




#if 1
#include "TRandom.h"
#include "TVector3.h"
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
//double tP = tan(Phi);
  TVector3 Nt(cL*cP,cL*sP,sL);
  TVector3 Np(-sP, cP, 0);
  TVector3 Nl(-sL*cP,-sL*sP,cL);



  TVector3 V;
//   printf("Nt="); Nt.Print();
//   printf("Np="); Np.Print();
//   printf("Nl="); Nl.Print();
  double YZ[3]={0},BG[3]={0};
  int nEl=10000,iEl=0;
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
  
  printf("Phi=%d Lam=%d: YY=%g YZ=%g ZZ=%g\n"
        , int(Phi/3.1415*180),int(Lam/3.1415*180),YZ[0],YZ[1],YZ[2]);

  StvHitErrCalculator calc;
  calc.SetPars(par);
  double np[3]={ cP,  sP, tL};
  double hitErr[3];
  calc.SetTrack(np);
  float hiDir[3][3]={{1,0,0}
                    ,{0,1,0}
		    ,{0,0,1}};
  calc.CalcDetErrs(0,hiDir,hitErr);
  printf("Calculator:  YY=%g YZ=%g ZZ=%g\n"
        ,hitErr[0],hitErr[1],hitErr[2]);

  printf("DCA       : BB=%g BG=%g GG=%g\n",BG[0],BG[1],BG[2]);
  calc.CalcDcaErrs(0,hiDir,hitErr);
  printf("DCA Calc  : BB=%g BG=%g GG=%g\n"
        ,hitErr[0],hitErr[1],hitErr[2]);


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

  StvHitErrCalculator calc;
  calc.SetPars(par);
  calc.SetTrack(Nt);
  double hRR[3],dRR[10][3];
  calc.CalcDcaErrs(hiPos,hiDir,hRR);
  calc.CalcDcaDers(dRR);
  for (int j=0;j<3;j++) {
    printf("hRR[%d]=%g  Der = %g %g %g %g\n",j,hRR[j]
          ,dRR[0][j],dRR[1][j],dRR[2][j],dRR[3][j]);
  }

  StvHitErrCalculator calk;
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
      printf("Der[%d][%d]=%g num=%g eps=%g\n",ider,j,dRR[ider][j],est,eps);
    }
  }
}
//______________________________________________________________________________
void StvTpcHitErrCalculator::Dest(double phiG,double lamG)
{
  double par[6]={0};
  par[kYErr]=0.03*0.03;
  par[kZErr]=0.07*0.07;
  par[kThkDet]=3*3/12.;
  par[kWidTrk]=0.1*0.1;
  par[kWidTrk]=0.1*0.1;
  par[kYDiff]=par[kYErr]*0.3;
  par[kZDiff]=par[kZErr]*0.5;

  double Lam = lamG/180*M_PI;
  double Phi = phiG/180*M_PI;
  double cL = cos(Lam);
  double sL = sin(Lam);
  double cP = cos(Phi);
  double sP = sin(Phi);
  double Nt[3]={cL*cP,cL*sP,sL};
  float  hiPos[3]   = {100,0,155};
  float  hiDir[3][3]={{1,0,0},{0,1,0},{0,0,1}};

  StvTpcHitErrCalculator calc;
  calc.SetPars(par);
  calc.SetTrack(Nt);
  double hRR[3],dRR[10][3];
  calc.CalcDcaErrs(hiPos,hiDir,hRR);
  calc.CalcDcaDers(dRR);
  for (int j=0;j<3;j++) {
    printf("hRR[%d]=%g  Der = %g %g %g %g\n",j,hRR[j]
          ,dRR[0][j],dRR[1][j],dRR[2][j],dRR[3][j]);
  }

  StvTpcHitErrCalculator calk;
  for (int ider=0;ider<6;ider++) {
    double myPar[6],delta;
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
      printf("Der[%d][%d]=%g num=%g eps=%g\n",ider,j,dRR[ider][j],est,eps);
    }
  }
}
#endif //0
