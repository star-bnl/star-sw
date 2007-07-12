#include "xTCL.h"
#include "TArrayI.h"

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
//_____________________________________________________________________________
void xTCL::eigen2(const double err[3], double lam[2], double eig[2][2])
{

  double spur = err[0]+err[2];
  double det  = err[0]*err[2]-err[1]*err[1];
  double dis  = spur*spur-4*det;
  if (dis<0) dis = 0;
  dis = TMath::Sqrt(dis);
  lam[0] = 0.5*(spur+dis);
  lam[1] = 0.5*(spur-dis);
  eig[0][0] = 1; eig[0][1]=0;
  if (dis>1e-6*spur) {// eigenvalues are different
    if (TMath::Abs(err[0]-lam[0])>TMath::Abs(err[2]-lam[0])) {
     eig[0][1] = 1; eig[0][0]= -err[1]/(err[0]-lam[0]);
    } else {
     eig[0][0] = 1; eig[0][1]= -err[1]/(err[2]-lam[0]);
    }
    double tmp = TMath::Sqrt(eig[0][0]*eig[0][0]+eig[0][1]*eig[0][1]);
    eig[0][0]/=tmp; eig[0][1]/=tmp;
  }
  eig[1][0]=-eig[0][1];  eig[1][1]= eig[0][0];
}
//______________________________________________________________________________
/*
* $Id: xTCL.cxx,v 1.2 2007/07/12 20:38:41 fisyak Exp $
*
* $Log: xTCL.cxx,v $
* Revision 1.2  2007/07/12 20:38:41  fisyak
* Add includes for ROOT 5.16
*
* Revision 1.1  2007/04/26 04:22:31  perev
* eXtended TCL class xTCL
*
* Revision 1.1.1.1  1996/04/01 15:02:13  mclareni
* Mathlib gen
*/
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
