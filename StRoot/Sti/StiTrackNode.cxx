#include <stdio.h>
#include <stdlib.h>
#include "StiTrackNode.h"
int StiTrackNode::mgFlag=0;
  static const int idx66[6][6] =
  {{ 0, 1, 3, 6,10,15},{ 1, 2, 4, 7,11,16},{ 3, 4, 5, 8,12,17}
  ,{ 6, 7, 8, 9,13,18},{10,11,12,13,14,19},{15,16,17,18,19,20}};

//______________________________________________________________________________
void StiTrackNode::errPropag6( double G[21],const double F[6][6],int nF )
{
  enum {NP=6,NE=21};

  double g[NE];      memcpy(g,    G,sizeof( g));
  double fg[NP][NP]; memset(fg[0],0,sizeof(fg));

  for (int i=0;i<nF;i++) {
  for (int j=0;j<nF;j++) {
//    if (!F[i][j]) 	continue;
    if (fabs(F[i][j])<1.e-20) 	continue;
    for (int k=0;k<NP;k++) {
      int jk = idx66[j][k];
//      if (!g[jk])	continue;
      if (fabs(g[jk])<1.e-20)	continue;
      fg[i][k] += F[i][j]*g[jk]; 
  }}}

  for (int i=0;i<NP;i++) {        
  for (int k=i;k<NP;k++) {        
    int ik = idx66[i][k];
    double s = 0; 
    for (int j=0;j<NP;j++) {
      if (fabs(F[k][j])<1.e-20)	continue;
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
void StiHitContino::add(StiHit *hit,double chi2,double detr)
{
   int i=0;
   for (;i<kMaxSize;i++) {
     if (!mHits[i]) 		break;
     if (chi2 > mChi2[i]) 	continue;
     for (int jr = kMaxSize-2;jr>=i;jr--) 
       {mHits[jr+1]=mHits[jr]; mChi2[jr+1]=mChi2[jr];mDetr[jr+1]=mDetr[jr];}
     				break;
   }
   if (i>=kMaxSize) 		return;
   mHits[i]=hit; mChi2[i]=chi2;mDetr[i]=detr;
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
//______________________________________________________________________________
 
double StiTrackNode::sinX(double x)
{
  double x2 = x*x;
  if (x2>0.5) return (sin(x)-x)/x2/x;
  double nom = -1./6;
  double sum = nom;
  for (int it=4;1;it+=2) {
    nom = -nom*x2/(it*(it+1));
    sum +=nom;
    if (fabs(nom) <= 1e-10*fabs(sum)) break;
  }
  return sum;
} 
//______________________________________________________________________________
void StiTrackNode::mult6(double Rot[kNPars][kNPars],const double Pro[kNPars][kNPars]) 
{
  double T[kNPars][kNPars];

  if (!Rot[0][0]) {memcpy(Rot[0],Pro[0],sizeof(T)); return;}

  memcpy(T[0],Pro[0],sizeof(T));

  for (int i=0;i<kNPars;i++) {
  for (int j=0;j<kNPars;j++) {
    if(!Rot[i][j]) continue;
    for (int k=0;k<kNPars;k++) {
      if (!Pro[k][i]) continue;
      T[k][j] += Pro[k][i]*Rot[i][j];
  }}} 
  for (int i=0;i<kNPars;i++) {
  for (int k=0;k<kNPars;k++) {
    Rot[i][k] += T[i][k];
}}
}     
//______________________________________________________________________________
double StiTrackNode::getRefPosition() const
{
  if(!_detector) {
    assert(_hit);
    return _hit->x();
  }
  StiPlacement * place = _detector->getPlacement();
  assert(place);
  return place->getLayerRadius();
}
//______________________________________________________________________________
  double StiTrackNode::getLayerAngle()  const
{
  assert(_detector);
  StiPlacement * place = _detector->getPlacement();
  assert(place);
  return place->getLayerAngle();
}

//______________________________________________________________________________
double StiNodeErrs::operator()(int i,int j) const
{
  return A[idx66[i][j]];
}
//______________________________________________________________________________
StiNodeErrs &StiNodeErrs::merge(double wt,StiNodeErrs &other)
{
   double wt0 = 1.-wt;
   for (int i=0;i<kNErrs;i++) {A[i] = wt0*A[i] + wt*other.A[i];}
   return *this;
}
//______________________________________________________________________________
void StiNodeErrs::recov(const double *maxerr) 
{
static const double CORRMAX  = 0.999;
static const double CORRMAX2 = CORRMAX*CORRMAX;

  if (maxerr) {
    for (int i=0;i<kNPars;i++) {
      double maxDia = maxerr[i]*maxerr[i];
      int ld = idx66[i][i];
      if (A[ld]<0) {
	 printf("StiNodeErrs::cut. Negative igonal %g(%d)\n",A[ld],i);
	 print();
	 A[ld] = maxDia;
      }
      if (A[ld]<maxDia) continue;
      double fak = sqrt(maxDia/A[ld]);
      for (int j=0;j<kNPars;j++) {A[idx66[i][j]]*=fak;}
    }
  }
    for (int i=0;i<kNPars;i++) {
      double aii = A[idx66[i][i]];
      for (int j=0;j<kNPars;j++) {
        if (i==j) continue;
        double ajj = A[idx66[j][j]];
        double &aij = A[idx66[i][j]];
        if (aij*aij <= aii*ajj*CORRMAX2) continue;
        if (aij*aij > aii*ajj){
          printf("StiNodeErrs::recov : Correlation too big %g[%d][%d]>%g\n"
                ,aij,i,j,sqrt(aii*ajj));}	  
        double tmp = sqrt(aii*ajj)*CORRMAX;
        aij = (aij>0)? tmp:-tmp;
      }//end j
    }//end i

}
//______________________________________________________________________________
void StiNodeErrs::print() const
{
   const double *d = A;
   for (int n=1;n<=6;n++) {
     printf("%d - ",n);
     for (int i=0;i<n;i++){printf("%g\t",*(d++));}; printf("\n");
   }  
}     

//______________________________________________________________________________
int StiNodeErrs::check(const char *pri) const
{
  int i,j,kase=0;
  double aii,ajj,aij;
  for (i=0;i<kNPars;i++) {
    aii = A[idx66[i][i]];
    if (aii<0) {kase = 1; break;}
    if (aii<=0) continue;
    for (j=0;j<kNPars ;j++) {
      if (i==j) continue;
      ajj = A[idx66[j][j]];
      if (ajj<=0) continue;
      aij = A[idx66[i][j]];
      if ((aij*aij)> 0.9999*aii*ajj) {kase = 2; break;}
    }
    if (kase) break;
  }  
  if (!kase || !pri) return kase;
  switch(kase) {
  
    case 1: printf("StiNodeErrs::check(%s) FAILED: Negative diagonal %g[%d][%d]\n",pri,aii,i,i);  
          break;
    case 2: printf("StiNodeErrs::check(%s) FAILED: Correlation too big %g[%d][%d]>%g\n"
                ,pri,aij,i,j,sqrt(aii*ajj)); 	  
  }    
  return kase;
}  
//______________________________________________________________________________
int StiNodePars::check(const char *pri) const
{
static double MAXPARS[]={200,200,210,1.5,0.2,100};
  int ierr=0;
  for (int i=0;i<kNPars;i++) {if (fabs(P[i]) > MAXPARS[i]) {ierr = i+1 ; break;}} 
  if(ierr) goto FAILED;
  for (int i=-2;i<0;i++)     {if (fabs(P[i]) > 1.)         {ierr = i+12; break;}} 
FAILED: 
  if (!ierr || !pri) return ierr;
  printf("StiNodePars::check(%s) == FAILED(%d)\n",pri,ierr);
  print();
  return ierr;
} 
//______________________________________________________________________________
StiNodePars &StiNodePars::merge(double wt,StiNodePars &other)
{
   double wt0 = 1.-wt;
   for (int i=0;i<kNPars;i++) {P[i] = wt0*P[i] + wt*other.P[i];}
   ready();
   return *this;
}
//______________________________________________________________________________
void StiNodePars::print() const
{
static const char* tit[]={"cosCA","sinCA","X","Y","Z","Eta","Curv","TanL",0};
  for (int i=-2;i<kNPars;i++) {printf("%s = %g, ",tit[i+2],P[i]);}
  printf("\n");
}   
