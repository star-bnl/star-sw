#include "xTCL.h"
#include "TArrayI.h"
#include <cassert>

//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
double xTCL::vmaxa(const double *a,int na)
{ 
  double r=0;
  for (int i=0;i<na;i++){if (r < TMath::Abs(a[i])) r = TMath::Abs(a[i]);}
  return r;
}
//______________________________________________________________________________
double xTCL::vmaxa(const TVectorD &a)
{ 
  return vmaxa(a.GetMatrixArray(),a.GetNrows());
}
//______________________________________________________________________________
int xTCL::lvmaxa(const double *a,int na)
{ 
  double r=0;
  int l=0;
  for (int i=0;i<na;i++){if (r < TMath::Abs(a[i])) {r = TMath::Abs(a[i]);l=i;}}
  return l;
}
//______________________________________________________________________________
int xTCL::lvmina(const double *a,int na)
{ 
  double r=TMath::Abs(a[0]);
  int l=0;
  for (int i=1;i<na;i++){if (r > TMath::Abs(a[i])) {r = TMath::Abs(a[i]);l=i;}}
  return l;
}
//______________________________________________________________________________
void xTCL::vfill(double *a,double f,int na) 
{
  for (int i=0;i<na;i++) {a[i]=f;}
}
//______________________________________________________________________________
/*
* $Id: xTCL.cxx,v 1.5 2014/02/18 19:45:49 perev Exp $
*
* $Log: xTCL.cxx,v $
* Revision 1.5  2014/02/18 19:45:49  perev
* Eigen2 copy of THelix one
*
* Revision 1.4  2009/08/28 16:38:55  fine
* fix the compilation issues under SL5_64_bits  gcc 4.3.2
*
* Revision 1.3  2008/10/29 19:38:06  perev
* method toEuler added
*
* Revision 1.2  2007/07/12 20:38:41  fisyak
* Add includes for ROOT 5.16
*
* Revision 1.1  2007/04/26 04:22:31  perev
* eXtended TCL class xTCL
*
* Revision 1.1.1.1  1996/04/01 15:02:13  mclareni
* Mathlib gen
*/
//_____________________________________________________________________________
static void eigen2(double G[3], double lam[2], double eig[2])
{
  double spur = G[0]+G[2];
//double det  = G[0]*G[2]-G[1]*G[1];
  double dis  = (G[0]-G[2])*(G[0]-G[2])+4*G[1]*G[1];
  dis = sqrt(dis);
  if (lam) {
    lam[0] = 0.5*(spur+dis);
    lam[1] = 0.5*(spur-dis);
  }
  if (!eig) return;
  double g[3]={G[0]-G[2]-dis,2*G[1],G[2]-G[0]-dis};
  int kase =0;
  for (int i=1;i<3;i++) { if (fabs(g[i])>fabs(g[kase])) kase = i;}
  switch(kase) {
    case 0: eig[0] = g[1]/g[0]; eig[1]=-1; break;
    case 1: eig[1] = g[0]/g[1]; eig[0]=-1; break;
    case 2: eig[1] = g[1]/g[2]; eig[0]=-1; break;
  }
  double nor = sqrt(eig[0]*eig[0]+eig[1]*eig[1]);
  if (nor>1e-11) {
    int j=(fabs(eig[0])>fabs(eig[1]))? 0:1;
    if(eig[j]<0) nor = -nor;
    eig[0]/=nor;eig[1]/=nor;}
  else {
    eig[0]=1;eig[1]=0;
  }

}
//_____________________________________________________________________________
double  xTCL::simpson(const double *F,double A,double B,int NP)
{
  int N2=NP-1;
  assert(N2>0 && !(N2&1));
  double S1=F[N2-1];
  double S2=0;

  for (int N = 1;N<=N2-3;N+=2) {S1+=F[N];S2+=F[N+1];}
  S1=S1+S1+S2;
  double H=(F[0]+F[N2]+S1+S1)*(B-A)/(3*N2);
  return H;
}
//_____________________________________________________________________________
double **xTCL::makeMatrixD(int m,int n)
{
  int szp = (m*sizeof(double*)+sizeof(double))/sizeof(double);
  int szd =m*n;
  double *d = new double[szp+szd];
  memset(d,0,(szp+szd)*sizeof(double));
  double **p = (double**)d;
  d+=szp;
  for (int i=0;i<m;i++) { p[i]=d; d+=n;};
  return p;
}
//______________________________________________________________________________
void xTCL::mxmlrt(const TMatrixD &A,const TMatrixD &B,TMatrixD &X)  
{
  int nRowA = A.GetNrows();
  int nColA = A.GetNcols();
  int nRowB = B.GetNrows();
  int nColB = B.GetNcols(); if(nColB){}
  assert(nColA ==nRowB);
  X.ResizeTo(nRowA,nRowA);
  TCL::mxmlrt(A.GetMatrixArray(),B.GetMatrixArray()
	     ,X.GetMatrixArray(),nRowA,nColA);

}
//______________________________________________________________________________
void xTCL::mxmlrtS(const double *A,const double *B,double *X,int nra,int nca)  
{
   TCL::vzero(X,nra*nra);
   for (int i=0,ii=0;i<nra;i++,ii+=nca) 	{
     for (int j=0,jj=0;j<nca;j++,jj+=nca)  	{
       if(!A[ii+j]) 	continue;
       for (int k=0,kk=0;k<=i;k++,kk+=nca)	{
         double &Xik =X[i*nra+k];
         for (int l=0;l<nca;l++)  		{
           if(!A[kk+l]) continue;
           Xik +=A[ii+j]*A[kk+l]*B[jj+l];
   } } } }
   for (int i=0;i<nra;i++){
   for (int k=0;k<i  ;k++){X[k*nra+i] = X[i*nra+k];}}
}       
//______________________________________________________________________________
void xTCL::mxmlrtS(const TMatrixD &A,const TMatrixD &B,TMatrixD &X)  
{
  int nRowA = A.GetNrows();
  int nColA = A.GetNcols();
  int nRowB = B.GetNrows();
  int nColB = B.GetNcols(); if(nColB){}
  assert(nColA ==nRowB);
  X.ResizeTo(nRowA,nRowA);
  xTCL::mxmlrtS(A.GetMatrixArray(),B.GetMatrixArray()
	       ,X.GetMatrixArray(),nRowA,nColA);

}
//______________________________________________________________________________
TMatrixD xTCL::T(const TMatrixD &mx)
{
  TMatrixD tmp(mx);
  return tmp.T();
}  
//______________________________________________________________________________
double xTCL::vasum(const double *a, int na)
{
  double sum = 0;
  for (int i=0;i<na;i++) { sum += TMath::Abs(a[i]);}
  return sum;
}
//______________________________________________________________________________
double xTCL::vasum(const TVectorD &a)
{
  return vasum(a.GetMatrixArray(),a.GetNrows());
}
//______________________________________________________________________________
int xTCL::SqProgSimple(      TVectorD &x
                      ,const TVectorD &g,const TMatrixD &G 
		      ,const TVectorD &Min		   
		      ,const TVectorD &Max, int iAktp)
{
static const double kSMALL = 1e-9;
static int nCall=0; nCall++;
  enum {kINIT=0,kADDCONS,kFREECONS};
  int kase = kINIT;
  int nPars = g.GetNrows();
  TVectorD xx(x),gg(g),add(nPars);
  TArrayI Side(nPars);
  int nCons=0,addCons = -1,freCons=-1,freSide=0,addSide=0,con=0;
  double maxGra=3e33;
  int iAkt = iAktp;
  while(1946) {
// 	Eliminate outdated constrains
    freCons=-1; freSide=0;
    if (nCons && kase==kFREECONS ) {
      double tryGra=kSMALL; freCons=-1;
      for (int ix=0;ix<nPars;ix++) {
        if(!Side[ix])		continue;
        double gra = gg[ix]*Side[ix];
	if (gra< tryGra)	continue;
	if (gra>=maxGra)	continue;
        freCons=ix; tryGra=gra;
      }
      if (freCons>=0) {
        maxGra = tryGra;
        freSide = Side[freCons];
        Side[freCons]=0;
        nCons--;
      }
    }

    if(kase==kFREECONS && freCons<0) 	{  break;} //DONE
    
//	Make new matrix, etc...
    TMatrixD S(G);
    TVectorD B(gg);
    if (nCons) {
      for (int ix=0;ix<nPars;ix++) {
	if (Side[ix]==0) continue;
        for (int jx=0;jx<nPars;jx++) {S[ix][jx]=0; S[jx][ix]=0;}
        S[ix][ix]=1; B[ix]=0;
    } } 
    if (iAkt==0 ) {

      double det=12345; S.Invert(&det);
//      if (det<0) iAkt=1;
//      else       add = (-1.)*(S*B);
      add = (-1.)*(S*B);
      double along = B*add;
      if (along>0) add*=(-1.);
    }

    if (iAkt==1 ) {
      double bb    = (B*B);
      double bSb   = (B*(S*B));
      double tau = -bb/(TMath::Abs(bSb)+3e-33);
      add = tau*B;

    }
    if(kase==kFREECONS && freSide) { //Free constrain case
      if (add[freCons]*freSide > -kSMALL) {
        Side[freCons]=freSide; nCons++; continue;}
    } 

//		Do we need new constrain?
    double fak=1;
    addCons = -1; addSide = 0;
    con = 0;
    for (int ix=0;ix<nPars;ix++) {
      if (Side[ix]) {add[ix]=0; con = 100*con+ix+1;continue;}
      double xi = xx[ix]+fak*add[ix];
      if (xi < Min[ix]){fak = (Min[ix]-xx[ix])/add[ix]; addCons=ix;addSide=-1;}
      if (xi > Max[ix]){fak = (Max[ix]-xx[ix])/add[ix]; addCons=ix;addSide= 1;}
      assert(fak<=1. && fak>=0.);
    }
    add*=fak;
    xx+= add;
    gg += G*add;
    maxGra=3e33;
    kase = kFREECONS; if (!addSide) continue;
    kase = kADDCONS;
    xx[addCons] = (addSide<0)? Min[addCons]:Max[addCons];
// 	Add new constrain;    
    Side[addCons] = addSide ;nCons++;

  } 				//end of while(1)
  x = xx;
  return abs(con);
}  	
//______________________________________________________________________________
void xTCL::toEuler(const double TT[3][3],double PhiThePsi[6])
{

//  TT[0][0] =  cPsi*cPhi - sPsi*cThe*sPhi
//  TT[1][0] = -sPsi*cPhi - cPsi*cThe*sPhi
//  TT[2][0] =  sThe*sPhi
//  TT[0][1] =  cPsi*sPhi + sPsi*cThe*cPhi
//  TT[1][1] = -sPsi*sPhi + cPsi*cThe*cPhi
//  TT[2][1] = -sThe*cPhi
//  TT[0][2] =  sPsi*sThe
//  TT[1][2] =  cPsi*sThe
//  TT[2][2] =  cThe

  double cThe = TT[2][2]; if (cThe>1) cThe=1; if (cThe<-1) cThe=-1;
  double sThe = (TT[0][2]*TT[0][2]+TT[1][2]*TT[1][2])
              + (TT[2][0]*TT[2][0]+TT[2][1]*TT[2][1]);
  sThe = sqrt(0.5*sThe);

  double N = 0.5*(cThe*cThe+sThe*sThe+1);
  cThe/=N; sThe/=N;

  double sPsi = 0,cPsi=1; 
  if (sThe>1e-6) { 
    sPsi = TT[0][2]/sThe; cPsi = TT[1][2]/sThe;
    N = 0.5*(cPsi*cPsi+sPsi*sPsi+1);
    cPsi/=N; sPsi/=N;
  }
  double cPhi = cPsi*TT[0][0]-sPsi*TT[1][0];
  double sPhi = cPsi*TT[0][1]-sPsi*TT[1][1];

  PhiThePsi[0] = cPhi ;  PhiThePsi[1] = sPhi;  
  PhiThePsi[2] = cThe ;  PhiThePsi[3] = sThe;  
  PhiThePsi[4] = cPsi ;  PhiThePsi[5] = sPsi;  


  double test 	= fabs(TT[0][0] -( cPsi*cPhi - sPsi*cThe*sPhi))
              	+ fabs(TT[1][0] -(-sPsi*cPhi - cPsi*cThe*sPhi))
 			+ fabs(TT[2][0] -( sThe*sPhi))
 			+ fabs(TT[0][1] -( cPsi*sPhi + sPsi*cThe*cPhi))
 			+ fabs(TT[1][1] -(-sPsi*sPhi + cPsi*cThe*cPhi))
 			+ fabs(TT[2][1] -(-sThe*cPhi))
 			+ fabs(TT[0][2] -( sPsi*sThe))
 			+ fabs(TT[1][2] -( cPsi*sThe))
 			+ fabs(TT[2][2] -( cThe));
  printf("EPS=%g\n",test);
//
}  
