#include <stdio.h>
#include <stdlib.h>
#include "StiTrackNode.h"
//______________________________________________________________________________
void StiTrackNode::errPropag6( double G[21],const double F[6][6],int nF )
{
  enum {NP=6,NE=21};
  static const int idx66[6][6] =
  {{ 0, 1, 3, 6,10,15},{ 1, 2, 4, 7,11,16},{ 3, 4, 5, 8,12,17}
  ,{ 6, 7, 8, 9,13,18},{10,11,12,13,14,19},{15,16,17,18,19,20}};

  double g[NE];      memcpy(g,    G,sizeof( g));
  double fg[NP][NP]; memset(fg[0],0,sizeof(fg));

  for (int i=0;i<nF;i++) {
  for (int j=0;j<nF;j++) {
//    if (!F[i][j]) 	continue;
    if (fabs(F[i][j])<1.e-10) 	continue;
    for (int k=0;k<NP;k++) {
      int jk = idx66[j][k];
//      if (!g[jk])	continue;
      if (fabs(g[jk])<1.e-10)	continue;
      fg[i][k] += F[i][j]*g[jk]; 
  }}}

  for (int i=0;i<NP;i++) {        
  for (int k=i;k<NP;k++) {        
    int ik = idx66[i][k];
    double s = 0; 
    for (int j=0;j<NP;j++) {
      if (fabs(F[k][j])<1.e-10)	continue;
      s += fg[i][j]*F[k][j];
    }
    G[ik] += (s + fg[i][k] + fg[k][i]);
  }}
}       

//______________________________________________________________________________
void StiHitContino::reset()
{
   memset(this,0,sizeof(StiHitContino));
   mChi2[0]=1e61;
}
//______________________________________________________________________________
void StiHitContino::add(StiHit *hit,double chi2)
{
   int i=0;
   for (;i<kMaxSize;i++) {
     if (!mHits[i]) 		break;
     if (chi2 > mChi2[i]) 	continue;
     for (int jr = kMaxSize-2;jr>=i;jr--) 
       {mHits[jr+1]=mHits[jr]; mChi2[jr+1]=mChi2[jr];}
     				break;
   }
   if (i>=kMaxSize) 		return;
   mHits[i]=hit; mChi2[i]=chi2; 
}
//______________________________________________________________________________
void StiHitContino::print(const char* tit) const
{
  if (!tit || !*tit) tit ="print()";
  int n=getNHits();	
  printf(" ****	StiHitContino::%s nHits=%d\n",tit,n);
  for (int i=0;i<n;i++) {
    printf("%3d - hit=%p chi2 = %g\n",i,(void*)mHits[i],mChi2[i]);}
  printf("\n");
}
//______________________________________________________________________________
int StiHitContino::getNHits() const
{ int n=0; for(int i=0;i<kMaxSize;i++) {if (mHits[i]) n++;}; return n;}	
//______________________________________________________________________________

#include <complex>
typedef std::complex<double > Complex;
const Complex Im(0,1);

//______________________________________________________________________________
int StiTrackNode::cylCross(double r, const double dx[4],double Rho,double out[4])
{

 Complex d(dx[0],dx[1]),x(dx[2],dx[3]);
 Complex res[2];
 Complex xd = Im*x/d;
 double axd = std::abs(xd);
 if (fabs((axd-r)*Rho)< 0.01) {// Low curvature approx
   double a = (1.-xd.real()*Rho);
   double b = xd.imag();
   double c = (axd-r)*(axd+r);
   double dis = b*b - a*c;
   if (dis<0.) return 1;
   dis = sqrt(dis);
   double L[2];
   if (b<0) {a=-a;b=-b;c=-c;}
   L[0] = -c/(dis+b);
   L[1] = -(b+(dis))/a;
   res[0] = x+L[0]*(1.+Im*(0.5*L[0]*Rho))*d;
   res[1] = x+L[1]*(1.+Im*(0.5*L[1]*Rho))*d;

 } else {	//General case

   Complex xrd = xd*Rho;
   Complex xrd1 = xrd-1.;
   double axrd1 = std::abs(xrd1);
   double alpha = std::log(xrd1).imag();	
   double mycos = 0.5*(pow(r*Rho,2)-axrd1*axrd1-1.)/axrd1;
   if (fabs(mycos)>1.) return 1;
   double ang = acos(mycos);
   res[0] = x+d*(std::exp(Im*( ang+alpha))-1.)/(Im*Rho);
   res[1] = x+d*(std::exp(Im*(-ang+alpha))-1.)/(Im*Rho);
 }
 if (res[0].real() < res[1].real()) {//swap
   xd = res[0]; res[0]=res[1]; res[1]=xd;}
 memcpy(out,res,sizeof(res));
 return 0;
 }
 
 
  
  
