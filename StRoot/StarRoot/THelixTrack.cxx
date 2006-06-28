#include <stdlib.h>
#include <math.h>
#include "TError.h"
#include "TArrayD.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TSystem.h"
#include "TCL.h"
#include "TRandom.h"
#include "TRandom2.h"
#include "THelixTrack.h"
#include "StMatrixD.hh"
#include <complex>

// Complex numbers
typedef std::complex<double > Complex;
const Complex Im(0,1);
//_____________________________________________________________________________
inline static double dot(const Complex &a,const Complex &b)
{return a.real()*b.real()+a.imag()*b.imag();}
//_____________________________________________________________________________
Complex expOne(Complex x)
{
  double a = std::abs(x);
  if (a<0.01) {
    return 1.+x*((1/2.) + x*((1/6.)+ x*(1/24.)));
  } else {
    return (std::exp(x)-1.)/x;
  }
}
//_____________________________________________________________________________
Complex expOneD(Complex x)
{
  double a = std::abs(x);
  if (a<0.01) {
    return (1/2. + x*((1/3.)+ x*((1/8.)+x*(1/30.))));
  } else {
    return (std::exp(x)*(x-1.)+1.)/(x*x);
  }
}






const double Zero = 1.e-6;
static Complex sgCX1,sgCX2,sgCD1,sgCD2,sgImTet,sgCOne,sgCf1;
#if 0
//_____________________________________________________________________________
static int myEqu(double *s, int na, double *b,int nb)
{
  StMatrixD mtx(na,na);
  double *m = &mtx(1,1);
  TCL::trupck(s,m,na);
  size_t ierr=0;
  mtx.invert(ierr);
  if (ierr) return ierr;
  for (int ib=0;ib<nb;ib++) {
    TCL::vmatl(m,b+ib*na,s,na,na);
    memcpy(b+ib*na,s,na*sizeof(*b));
  }
  TCL::trpck(m,s,na);
  return 0;  
}
//_____________________________________________________________________________
static void Eigen2(const double err[3], double lam[2], double eig[2][2])
{

  double spur = err[0]+err[2];
  double det  = err[0]*err[2]-err[1]*err[1];
  double dis  = spur*spur-4*det;
  if (dis<0) dis = 0;
  dis = sqrt(dis);
  lam[0] = 0.5*(spur+dis);
  lam[1] = 0.5*(spur-dis);
  eig[0][0] = 1; eig[0][1]=0;
  if (dis>1e-6*spur) {// eigenvalues are different
    if (fabs(err[0]-lam[0])>fabs(err[2]-lam[0])) {
     eig[0][1] = 1; eig[0][0]= -err[1]/(err[0]-lam[0]);
    } else {
     eig[0][0] = 1; eig[0][1]= -err[1]/(err[2]-lam[0]);
    }
    double tmp = sqrt(eig[0][0]*eig[0][0]+eig[0][1]*eig[0][1]);
    eig[0][0]/=tmp; eig[0][1]/=tmp;
  }
  eig[1][0]=-eig[0][1];  eig[1][1]= eig[0][0];
}
#endif //0
//_____________________________________________________________________________
static Complex MyFactor(double rho,double drho,double s)
{
// Integral exp(i*Phi)*dL where Phi = rho*L + 0.5*drho*L**2
// Let it is equal  exp(i*Phi)*A(L) + const
// then dA/dL + i*(rho+drho*L)*A = 1
// Solve this equation for Taylor representation of A(L)
// static int Iter=0;
  Complex arr[3],add;
  Complex Sum; //
  Sum = 0.0;
  arr[0] = 1.; arr[1] = 0.;
  drho = drho/rho;
  double ss = s;
  for (int j=2;1;j++) {
    arr[2] = -Im*rho*(arr[1]+drho*arr[0])/double(j);
    ss *=s; add = ss*arr[2]; Sum += add;
    if (std::norm(Sum)*1.e-12>std::norm(add)) break;
//    printf(" Iter=%d %d %g\n",Iter++,j-1,std::abs(add));
    arr[0]=arr[1]; arr[1]=arr[2]; 
  }
  return Sum;
}

ClassImp(THelixTrack)
//_____________________________________________________________________________
THelixTrack::THelixTrack(const double *xyz,const double *dir,double rho
		        ,double drho)
{
//	Made from GEANT3 ghelix by V.Perevoztchikov
//
//    ******************************************************************
//    *                                                                *
//    *  Performs the tracking of one step in a magnetic field         *
//    *  The trajectory is assumed to be a helix in a constant field   *
//    *  taken at the mid point of the step.                           *
//    *  Parameters:                                                   *
//    *   input                                                        *
//    *     STEP =arc length of the step asked                         *
//    *     VECT =input vector (position,direction cos and momentum)   *
//    *     CHARGE=  electric charge of the particle                   *
//    *   output                                                       *
//    *     VOUT = same as VECT after completion of the step           *
//    *                                                                *
//    *    ==>Called by : <USER>, GUSWIM                               *
//    *       Author    M.Hansroul  *********                          *
//    *       Modified  S.Egli, S.V.Levonian                           *
//    *       Modified  V.Perevoztchikov
//    *                                                                *
//    ******************************************************************
//
  memset(fBeg,0,fEnd-fBeg);
  fEmxXY[0]=-2006; fEmxSZ[0]=-2006;
  Set(xyz,dir,rho,drho);
}
//_____________________________________________________________________________
THelixTrack::THelixTrack(const THelixTrack &from)
{
  memcpy(fBeg,from.fBeg,fEnd-fBeg);
}
//_____________________________________________________________________________
THelixTrack::THelixTrack()
{
  memset(fBeg,0,fEnd-fBeg);
  fEmxXY[0]=-2006; fEmxSZ[0]=-2006;
}
//_____________________________________________________________________________
THelixTrack::THelixTrack(const double *pnts,int npnts, int rowsize)
//
//    ******************************************************************
//    *                                                                *
//    *  Create helix as fit of array of points                        *
//    *                                                                *
//    ******************************************************************
//
{
  Fit(pnts,npnts,rowsize);
}

//_____________________________________________________________________________
double THelixTrack::Fit(const double *pnts,int npnts, int rowsize)
//
//    ******************************************************************
//    *                                                                *
//    *  Create helix as fit of array of points                        *
//    *                                                                *
//    ******************************************************************
//
{
   double xm,ym,xxm,yym,xym,rrm,rrxm,rrym;
   double x,y,z,x1st=0,y1st=0,s,Xcd,Ycd,Rhoc,Rhoc2,dR,Rho,xyz[3],res,resmax;
   int ip,np,lv,maxres;


   int    *ign = new int   [npnts]; 
   double *stp = new double[npnts];

   memset (ign,0,npnts*sizeof(int));

   for (int iter=0; iter <3; iter++) {
     np = 0;
     xm=0,ym=0,xxm=0,yym=0,xym=0,rrm=0,rrxm=0,rrym=0;
     for (lv =0,ip=0; ip < npnts; ip++,lv+=rowsize) 
     {
       if (ign[ip]) 	continue;
       np++;
       x = pnts[lv+0]-pnts[0];  
       y = pnts[lv+1]-pnts[1];  
       if (np==1) {x1st=x; y1st=y;}
       xm   += x;   ym  += y;
       xxm  += x*x; yym += y*y; xym += x*y;
       rrm  += 0.5*(x*x+y*y);
       rrxm += 0.5*(x*x+y*y)*x;
       rrym += 0.5*(x*x+y*y)*y;
     }
     xm  /=np; 	ym   /=np;
     xxm /=np;	yym  /=np;	xym  /=np;
     rrm /=np; 	rrxm /=np; 	rrym /=np;
     xxm -= xm*xm; 	yym -= ym*ym; 	xym -= xm*ym;
     rrxm -= rrm*xm;	rrym -= rrm*ym;

     double det = (xxm*yym-xym*xym);
     double a   = (rrxm*yym-rrym*xym);
     double b   = (rrym*xxm-rrxm*xym);
     double ab2  = (a*a+b*b);
     double ab  = ::sqrt(ab2);

     if (fabs(det) < ab ) { 	//small Rhoc
       Xcd = a/ab; if (det<0) Xcd = -Xcd;  
       Ycd = b/ab; if (det<0) Ycd = -Ycd;  
       Rhoc2 = det*det/(ab2);   
       Rhoc = ::sqrt(Rhoc2);   
       double c = 2.*((xm*Xcd+ym*Ycd)-rrm*Rhoc);
       dR = -c/(1.+::sqrt(1.-Rhoc*c));
       Rho = Rhoc/(1.+dR*Rhoc);
     } else {			//big Rhoc
       Xcd = a/det;   
       Ycd = b/det;   
       double Rc2 = (ab2)/(det*det);
       double Rc  = ::sqrt(Rc2);
       double c = 2.*((xm*Xcd+ym*Ycd)-rrm);
       dR = -c/(Rc+::sqrt(Rc*Rc-c));
       Rho = 1./(Rc+dR);
       ab = ::sqrt(Xcd*Xcd+Ycd*Ycd);
       double r1st = ::sqrt(x1st*x1st + y1st*y1st);
       if (ab < 0.001*r1st) {
         Xcd = -x1st/r1st;
         Ycd = -y1st/r1st;
       } else {
         Xcd = Xcd/ab;
         Ycd = Ycd/ab;
       }  
     }

     fX[0] = -dR*Xcd;
     fX[1] = -dR*Ycd;
     fX[2] = 0;
     fP[0] = +Ycd;
     fP[1] = -Xcd;
     fP[2] = 0;
     if (xm*fP[0]+ym*fP[1] < 0) {fP[0] = -fP[0];fP[1] = -fP[1]; Rho = -Rho;}
     fX[0] += pnts[0];
     fX[1] += pnts[1];

     Set(fX,fP,Rho,0);
     THelixTrack temp(*this);

     double zm=0,sm=0,zs=0,ss=0,s0=0;
     double fullen = 1.e+10;
     if (fabs(Rho)>1./fullen) fullen = 2.*M_PI/fabs(Rho);
     for (lv =0,ip=0; ip < npnts; ip++,lv+=rowsize) 
     {
       if (ign[ip]) 	continue;
       z = pnts[lv+2];  
       s = temp.Step(pnts+lv); 
       if (fabs(s*fCosL) > fabs(s*fCosL-fullen)) s = s-fullen/fCosL;
       temp.Move(s); s0 += s; stp[ip] = s0;
       zm += z; sm += s0; ss += s0*s0; zs += z*s0; 
     }
     zm /= np; sm /= np; ss /= np; zs /= np; 
     ss -= sm*sm; zs -= zm*sm; 
     double Z1 = zs/ss; 
     double Z0 = zm - Z1*sm;
     fX[2] = Z0;
     double p[3] = {fP[0],fP[1],Z1};

     Set(fX,p,Rho,0);
     maxres = 0;resmax = 0; res=0;
     for (lv =0,ip=0; ip < npnts; ip++,lv+=rowsize) 
     {
       if (ign[ip]) 	continue;
       Step(stp[ip]/fCosL,xyz);
       double rs = 0;
       for(int i=0;i<3;i++) {rs += ::pow(pnts[lv+i]-xyz[i],2);}
       if (rs>resmax) {resmax = rs; maxres = ip;} 
       res += rs;
     }
     res /= np;
     if (resmax < 1.e-6) 	break;
     if (resmax < 9*res) 	break;
     if (np <= 3)		break;
     ign[maxres] = 1;
                                break;//VP
   }
  delete [] ign;
  delete [] stp;
  
  return ::sqrt(resmax);
}
//_____________________________________________________________________________
void THelixTrack::Set(const double *xyz,const double *dir,double rho
		     ,double drho)
{
  fX[0] = xyz[0]; fX[1] = xyz[1]; fX[2] = xyz[2];
  fP[0] = dir[0]; fP[1] = dir[1]; fP[2] = dir[2];
  fRho = rho; fDRho=drho;
  Build();
}
//_____________________________________________________________________________
void THelixTrack::SetEmx(const double*  err2xy,const double*  err2sz)
{
  memcpy(fEmxXY,err2xy,sizeof(fEmxXY));
  memcpy(fEmxSZ,err2sz,sizeof(fEmxSZ));
}
//_____________________________________________________________________________
void THelixTrack::GetEmx(double err2xy[6],double err2sz[3]) const
{
   if(err2xy) memcpy(err2xy,fEmxXY,sizeof(fEmxXY));
   if(err2sz) memcpy(err2sz,fEmxSZ,sizeof(fEmxSZ));
}
//_____________________________________________________________________________
void THelixTrack::Set(double rho,double drho)
{
   fRho = rho; fDRho=drho; fMax = 1./(fabs(fRho*fCosL)+1.e-10);
}
//_____________________________________________________________________________
void THelixTrack::Backward()
{

  double x[3],d[3],rho;
  for (int i=0;i<3;i++) { x[i]=fX[i]; d[i]=-fP[i];}
  rho = -fRho;
  Set(x,d,rho,fDRho); 
  if(fEmxXY[0]>-1) {fEmxXY[3]=-fEmxXY[3];fEmxXY[4]=-fEmxXY[4];
                    fEmxSZ[1]=-fEmxSZ[1];}
}
//_____________________________________________________________________________
void THelixTrack::GetSpot(const double axis[3][3],double emx[3]) const
{
/// THelixTrack::GetSpot(double axis[3][3],emx[3]) const
/// axis[0,1]  - vectors in plane. 
/// axis[2]    - normal vector of plane
/// emx[3] error matrix of coordinates according vectors in plane.

//   transformation matrix from "helix" coordinate to global
   double my[3][3] = {{-fP[1]/fCosL, 0,fP[0]}
                     ,{ fP[0]/fCosL, 0,fP[1]}
                     ,{           0, 1,fP[2]}};

   double T[3][3],tmp[3][3],g[6],t[2][2];
   TCL::mxmpy (axis[0],my[0],T[0],3,3,3);
//   	now account that matrix axis may be non orthogonal
   TCL::traat(axis[0],g,3,3);
   if (fabs(g[0]-1)+fabs(g[1])+fabs(g[2]-1)
      +fabs(g[3])+fabs(g[4])+fabs(g[5]-1)>1e-10) {//non orthogonal case
     TCL::trsinv(g,g,3);
     memcpy(tmp[0],T[0],sizeof(T));
     TCL::trsa  (g,tmp[0],T[0],3,3);
   }
   TCL::vlinco(T[0],1.,T[2],-T[0][2]/T[2][2],t[0],2);
   TCL::vlinco(T[1],1.,T[2],-T[1][2]/T[2][2],t[1],2);
   double myerr[3]={fEmxXY[0],0,fEmxSZ[0]};
   TCL::trasat(t[0],myerr,emx,2,2);
   return;
}
//_____________________________________________________________________________
void THelixTrack::Build()
{

  double tmp;
    
  tmp = fP[0]*fP[0]+ fP[1]*fP[1]+ fP[2]*fP[2];
  if (fabs(tmp-1.) > 1.e-12) {
    tmp = ::sqrt(tmp); fP[0] /=tmp; fP[1] /=tmp; fP[2] /=tmp; }
    
  fCosL = ::sqrt(fP[0]*fP[0]+fP[1]*fP[1]);

  fMax = 1./(fabs(fRho*fCosL)+1.e-10);
}

//_____________________________________________________________________________
double THelixTrack::Move(double step) 
{
  if (fEmxXY[0]<0) {
    double xyz[3],dir[3],rho;
    Step(step,xyz,dir,rho);
    Set(xyz,dir,rho,fDRho);
  } else {
    double s = step*fCosL;
    TCircle circ;
    Fill(circ);
    circ.Move(s);
    fX[0] = circ.fX[0];       fX[1] = circ.fX[1];
    fP[0] = circ.fD[0]*fCosL; fP[1] = circ.fD[1]*fCosL;
    memcpy(fEmxXY,circ.fEmx,sizeof(fEmxXY));
    fX[2]+=fP[2]*step;
    fEmxSZ[0]+=s*(2*fEmxSZ[1]+s*fEmxSZ[2]);
    fEmxSZ[1]+=s*fEmxSZ[2];
  } 
  return step;
}

//_____________________________________________________________________________
double THelixTrack::Step(double stmax,const  double *surf, int nsurf,
                         double *xyz, double *dir, int nearest) const
{
  int i;
  double s[10]={0,0,0,0,0,0,0,0,0,0},tmp=0;
  memcpy(s,surf,nsurf*sizeof(surf[0]));
  
  for(i=1;i<nsurf;i++) if (fabs(s[i])>tmp) tmp = fabs(s[i]);
  if(fabs(tmp-1.)>0.1) {for(i=0;i<nsurf;i++) s[i]/=tmp;}
  double stmin = (nearest)? -stmax:0;
  if (!s[3] && !s[6] && !s[8] && !s[9] && fabs(s[4]-s[5])<1.e-12) 
        return StepHZ(s,nsurf,xyz,dir,nearest);
  else  return Step(stmin,stmax,s,nsurf,xyz,dir,nearest);
}


//_____________________________________________________________________________
double THelixTrack::Step(double stmin,double stmax, const double *s, int nsurf,
                         double *xyz, double *dir, int nearest) const
{
  int ix,jx,nx,ip,jp;
  double poly[4][3],tri[3],sol[2],cos1t,f1,f2,step,ss;
  const double *sp[4][4] = {{s+0,s+1,s+2,s+3}, {s+1,s+4,s+7,s+9}, 
                            {s+2,s+7,s+5,s+8}, {s+3,s+9,s+8,s+6}}; 

  THelixTrack th(fX,fP,fRho);
  cos1t = 0.5*fRho*fCosL;
  double totStep=0;
  while (2005) {
    double hXp[3]={-th.fP[1],th.fP[0],0};
    poly[0][0]=1.;poly[0][1]=0.;poly[0][2]=0.;
    tri[0]=tri[1]=tri[2]=0;
    for(ix=1;ix<4;ix++) {
      poly[ix][0] =th.fX  [ix-1]; 
      poly[ix][1] =th.fP  [ix-1]; 
      poly[ix][2] =hXp[ix-1]*cos1t;
    }

    nx = (nsurf<=4) ? 1:4;
    for(ix=0;ix<nx;ix++) {
      for(jx=ix;jx<4;jx++) {  
	ss = *sp[ix][jx]; if(!ss) 	continue;
	for (ip=0;ip<3;ip++) {
          f1 = poly[ix][ip]; if(!f1) 	continue;
          f1 *=ss;
          for (jp=0;jp+ip<3;jp++) {
            f2 = poly[jx][jp]; if(!f2) 	continue;
            tri[ip+jp] += f1*f2;
    } } } }

    int nsol = SqEqu(tri,sol);
    step = 1.e+12;
    if (nsol<0) 	return step;

    if (nearest && nsol>1) {
      if(fabs(sol[0])>fabs(sol[1])) sol[0]=sol[1];
      nsol = 1;
    }
    if (nsol) step = sol[0];
    if (step < stmin && nsol > 1) step = sol[1];
    if (step < stmin || step > stmax) 	{
      nsol = 0; 
      if (step>0) {step = stmax; stmin+=fMax/2;}
      else        {step = stmin; stmax-=fMax/2;}}

    if (!nsol && fabs(step) < 0.1*fMax) return 1.e+12;
    if (fabs(step)>fMax) {step = (step<0)? -fMax:fMax; nsol=0;}

    double x[3],d[3];
    th.Step(step,x,d);
    if (nsol) {//test it
      ss = s[0]+s[1]*x[0]+s[2]*x[1]+s[3]*x[2];
      if (nsurf > 4) ss += s[4]*x[0]*x[0]+s[5]*x[1]*x[1]+s[6]*x[2]*x[2];
      if (nsurf > 7) ss += s[7]*x[0]*x[1]+s[8]*x[1]*x[2]+s[9]*x[2]*x[0];
      if (fabs(ss)<1.e-7) {
	if (xyz) memcpy(xyz,x,sizeof(*xyz)*3);
	if (dir) memcpy(dir,d,sizeof(*dir)*3);
	return totStep+step;
    } }

    stmax -=step; stmin -=step;
    if (stmin>=stmax) return 1.e+12;
    totStep+=step;
    th.Move(step);
  }

}

//_____________________________________________________________________________
double THelixTrack::StepHZ(const double *su, int nsurf, 
                           double *xyz, double *dir,int nearest) const
{
   double tri[3] = {0,0,0};
   double f0,fc,fs,R,tet,tet0,tet1,tet2,costet,su45=0,fcs;
   

   R = 1./fRho/fCosL;
//		X
   f0 = fX[0] - fP[1]*R;
   fc = fP[1]*R;
   fs = fP[0]*R;

   tri[0] = su[0] + su[1]*f0;
   tri[1] = su[1]*fc;
   tri[2] = su[1]*fs;
   if (nsurf >4) {
     su45 = 0.5*(su[4]+su[5]);
     fcs  = fc*fc + fs*fs;
     tri[0] += su45*f0*f0 + su45*fcs; 
     tri[1] += su45*2*f0*fc;
     tri[2] += su45*2*f0*fs;
   }
//		Y
   f0 =  fX[1] + fP[0]*R;
   fc = -fP[0]*R;
   fs =  fP[1]*R;

   tri[0] += su[2]*f0;
   tri[1] += su[2]*fc;
   tri[2] += su[2]*fs;

   if (nsurf >4) {
     tri[1] += su45*2*f0*fc;
     tri[2] += su45*2*f0*fs;
   }
   costet = -tri[0]/::sqrt(tri[1]*tri[1]+tri[2]*tri[2]);
   if(fabs(costet)>1.) return 1.e+12;
   tet0 = atan2(tri[2],tri[1]);
   tet  = acos(costet);
   tet1 =  tet + tet0;
   tet2 = -tet + tet0;

   if (tet1 > 2*M_PI) tet1 -= 2*M_PI;
   if (tet2 > 2*M_PI) tet2 -= 2*M_PI;
   if (nearest) { 	//Select the neares solution
     if (fabs(tet1)>fabs(tet1-2*M_PI)) tet1 -=2*M_PI;
     if (fabs(tet1)>fabs(tet1+2*M_PI)) tet1 +=2*M_PI;
     if (fabs(tet2)>fabs(tet2-2*M_PI)) tet2 -=2*M_PI;
     if (fabs(tet2)>fabs(tet2+2*M_PI)) tet2 +=2*M_PI;
     if (fabs(tet1)>fabs(tet2)       ) tet1  =tet2;
     return Step(tet1*R,xyz,dir);
   } else {		//forward seqrch 
     double s1 = tet1*R;
     if (s1<=0) s1 += 2*M_PI*fabs(R);
     double s2 = tet2*R;
     if (s2<=0) s2 += 2*M_PI*fabs(R);
     if (s1>s2) s1=s2;
     return Step(s1,xyz,dir);
   }

}

//_____________________________________________________________________________
double THelixTrack::Path(double x,double y) const
{
   double ar[6]={fX[0],fX[1],0,fP[0]/fCosL,fP[1]/fCosL,0};
   THelixTrack ht(ar,ar+3,fRho);
   ar[0]=x;ar[1]=y;
   double s= ht.Path(ar)/fCosL;
   return s;
}
//_____________________________________________________________________________
double THelixTrack::Step(const double *point,double *xyz, double *dir) const
{

    static int nCount=0; nCount++;
    Complex cpnt(point[0]-fX[0],point[1]-fX[1]);
    Complex cdir(fP[0],fP[1]); cdir /=std::abs(cdir);
    double step[3];
//		Z estimated step 

    int zStep=0;
    step[1] = 0;
    if (fabs(fP[2]) > 0.01){ //Z approximation
      zStep = 1;
      step[1] = (point[2]-fX[2])/fP[2];
    }
//angle approximation
//		R estimated step
    {
      cpnt /= cdir;
      if (fabs(cpnt.real()*fRho) < 0.01) {
        step[2]=cpnt.real();
      } else {
        double rho = fRho;
        for (int i=0;i<2;i++) {
          Complex ctst = (1.+Im*rho*cpnt);
	  ctst /=std::abs(ctst);
	  ctst = std::log(ctst);
	  step[2]= ctst.imag()/rho;
          if (!fDRho) break;
	  rho = fRho+ 0.5*fDRho*step[2];
        }
      }
      step[2]/=fCosL;
    }

    if (zStep && fabs(step[1]-step[2]) > GetPeriod()) {
      int nperd = (int)((step[1]-step[2])/GetPeriod());
      step[2] += nperd*GetPeriod();
    }
    step[0] = step[2];

    double ds = step[1]-step[2];
    if (zStep && fabs(ds)>1.e-5) {
      double dz = ds*fP[2];
      step[0] += dz*dz/ds;
    }


    double xnear[6],ss=0;  double* pnear=xnear+3;
//		iterations
    double dstep = 1.e+10,dztep;
    double lMax = step[0]+0.25*GetPeriod();
    double lMin = step[0]-0.25*GetPeriod();

    if (zStep) {
      lMax = (step[1]>step[2])? step[1]:step[2];
      lMin = (step[1]>step[2])? step[2]:step[1];}
    int iter=40,icut=1;
    THelixTrack local(*this);
    local.Move(step[0]);
    lMax-=step[0];lMin-=step[0];
    local.Step(0.,xnear,pnear);
    for (; iter; iter--)
    { 
      double diff = (icut)? lMax-lMin: fabs(dstep);
      if (diff < 0.1) {
        if (diff < 1.e-5) 	break;
        double tmp = 0;
        for (int i=0;i<3;i++) {tmp += fabs(point[i]-xnear[i]);}
        if (diff < tmp*1.e-4) 	break;
        if (tmp < 1.e-5) 	break;
      }
      
      local.Step(ss,xnear,pnear);
      dstep = 0; icut = 0;
      for (int i=0;i<3;i++) {dstep += pnear[i]*(point[i]-xnear[i]);}
      if(dstep<0) {
        lMax = ss; dztep = -0.5*(lMax-lMin);
	if (dstep<dztep) {icut=1;dstep = dztep;}
      } else {
        lMin = ss; dztep =  0.5*(lMax-lMin);
	if (dstep>dztep) {icut=1;dstep = dztep;}
      }
      ss += dstep; 
    }
    Assert(iter);
    step[0]+=ss;
    fDCA[0] = ((point[0]-xnear[0])*(-pnear[1]) +(point[1]-xnear[1])*(pnear[0]))/fCosL;
    if (fRho<0) fDCA[0] = - fDCA[0];
    fDCA[1] = point[2]-xnear[2];
    return (xyz) ? Step(step[0],xyz,dir) : step[0];
}
//_____________________________________________________________________________
double THelixTrack::Dca(const double *point,double *dcaErr) const
{
   double x[3],T[3][3],emx[3];
   double s = Path(point,x,T[2]);
   for (int i=0;i<3;i++) {T[0][i]=point[i]-x[i];}
   double dca = sqrt(T[0][0]*T[0][0]+T[0][1]*T[0][1]+T[0][2]*T[0][2]);
   if (!dcaErr) return dca;

   for (int i=0;i<3;i++) {T[0][i]/=dca;}
   T[1][0]=T[0][1]*T[2][2]-T[2][1]*T[0][2];
   T[1][1]=T[0][2]*T[2][0]-T[2][2]*T[0][0];
   T[1][2]=T[0][0]*T[2][1]-T[2][0]*T[0][1];
   
   THelixTrack th(*this);
   th.Move(s);
   th.GetSpot(T,emx);
   *dcaErr=emx[0];
   return dca;
}
//_____________________________________________________________________________
double THelixTrack::Dca(const double point[3]
                       ,double &dcaXY,double &dcaZ,double dcaEmx[3]) const
/// Full 3d dca evaluation
/// point[3] - x,y,z of vertex
/// dcaXY - dca in xy plane
/// dcaZ  - dca in Z direction
/// dcaEmx[3] - err(dcaXY*dcaXY),err(dcaXY*dcaZ),err(dcaZ*dcaZ)
/// return distance to dca point
{
   double dif[3];
   double s = Path(point);
   THelixTrack th(*this);
   th.Move(s);
   const double *x=th.Pos();
   const double *d=th.Dir();

   for (int i=0;i<3;i++) {dif[i]=x[i]-point[i];}
   double nor = th.GetCos();
   double T[3][3]={{-d[1]/nor, d[0]/nor,    0}
                  ,{        0,        0,    1}
		  ,{ d[0]/nor, d[1]/nor,    0}};

   dcaXY = T[0][0]*dif[0]+T[0][1]*dif[1];
   dcaZ  = dif[2];
   double emxXY[6],emxSZ[3];
   th.GetEmx(emxXY,emxSZ);
   dcaEmx[0] = emxXY[0];
   dcaEmx[1] = 0;
//	cos(Lambda) **4 to account that we are in the nearest point
   dcaEmx[2] = emxSZ[0]*pow(th.GetCos(),4);
   return s;
}


//_____________________________________________________________________________
double THelixTrack::GetDCAxy() const
{
  return fDCA[0];
}
//_____________________________________________________________________________
double THelixTrack::GetDCAz() const
{
  return fDCA[1];
}
//_____________________________________________________________________________
double THelixTrack::GetDCA() const
{
  double tmp = sqrt(fDCA[0]*fDCA[0]+fDCA[1]*fDCA[1]);
  if (fDCA[0]<0) tmp = -tmp;
  return tmp;
}
//_____________________________________________________________________________
double THelixTrack::GetDCA(double xx,double yy) const
{
  double xd[9]; 
  memcpy(xd+0,fX,6*sizeof(fX[0]));
  xd[6]=xx; xd[7]=yy;xd[8]=0;
  xd[2]=0; xd[5]=0;	
  THelixTrack myHlx(xd,xd+3,fRho);	
  myHlx.Step(xd+6);
  return myHlx.GetDCAxy();
}	
	
//_____________________________________________________________________________
double THelixTrack::GetPeriod() const
{
   double per = (fabs(fRho) > 1.e-10) ? fabs(2.*M_PI/fRho):1.e+10;
   return per/fCosL;
}
//______________________________________________________________________________
void THelixTrack::Rot(double angle)
{
  Rot(cos(angle),sin(angle));
}
//______________________________________________________________________________
void THelixTrack::Rot(double cosa,double sina)
{
  Complex CX(fX[0],fX[1]),CP(fP[0],fP[1]);
  Complex A (cosa,sina);
  CX *=A; fX[0] = CX.real(); fX[1]=CX.imag();
  CP *=A;
  fP[0] = CP.real(); fP[1]=CP.imag();
}
//_____________________________________________________________________________
void THelixTrack::Streamer(TBuffer &){}
//_____________________________________________________________________________
void THelixTrack::Print(Option_t *) const
{
  printf("\n THelixTrack::this = %p\n",(void*)this);
  printf(" THelixTrack::fX[3] = { %f , %f ,%f }\n",fX[0],fX[1],fX[2]);
  printf(" THelixTrack::fP[3] = { %f , %f ,%f }\n",fP[0],fP[1],fP[2]);
  printf(" THelixTrack::fRho  =   %f \n\n",fRho);

  printf("double xyz[3] = {%g,%g,%g};\n" ,fX[0],fX[1],fX[2]); 
  printf("double dir[3] = {%g,%g,%g};\n" ,fP[0],fP[1],fP[2]); 
  printf("double Rho = %g;\n" ,fRho); 
  printf("THelixTrack *ht = new THelixTrack(xyz,dir,Rho);\n");
  
}
//_____________________________________________________________________________
int THelixTrack::SqEqu(double *cba, double *sol)
{
//	
//	made from fortran routine GVPSQR (Geant320)
/*
************************************************************************
*                                                                      *
*     SUBROUTINE GVPSQR (CBA,SOL,NSOL)             870924  VP          *
*                                                                      *
*       SOLVE  QUADRATIC  EQUATION                                     *
*                                                                      *
*   ARGUMENTS:                                                         *
*       CBA     Array of coeff's A0 + A1*x + A2*x**2                   *
*       SOL     Solutions                                              *
*       NSOL    Number of solutions :                                  *
*               if zero - SOL[0]= extremum                             *
*               if -ve  - No solution at all                           *
*                                                                      *
************************************************************************
*/
  const double zero2=1.e-12;
  double swap,a,b,c,amx,dis,bdis;
  int nsol;
/*--------------------------------------------------------------------*/

  a = cba[2]; b = cba[1]*0.5; c = cba[0];
  if (b < 0.) {a = -a; b = -b; c = -c;}
  amx = fabs(a); if (amx<b) amx = b; if (amx<fabs(c)) amx = fabs(c);
  if (amx <= 0.) return -1;
  a = a/amx; b = b/amx; c = c/amx;

  dis = b*b - a*c;
  nsol = 1;
  if (fabs(dis) <= zero2)  dis = 0;
  if (dis < 0.) { nsol = 0; dis  = 0.;}

  dis = ::sqrt(dis); bdis = b + dis;
  if (fabs(c) > 1.e+10*bdis)	return -1;
  sol[0] = 0.;
  if (fabs(bdis) <= 0.)      	return nsol;
  sol[0] = (-c/bdis);		
  if (dis <= 0.)            	return nsol;
  if (bdis >= 1.e+10*fabs(a))   return nsol;    
  nsol   = 2; sol[1] = (-bdis/a);
  if (sol[0] > sol[1]) { swap = sol[0]; sol[0] = sol[1]; sol[1] = swap;}
  return nsol;
}
//_____________________________________________________________________________
double THelixTrack::Eval(double step, double *xyz, double *dir,double &rho) const
{
   Eval(step,xyz,dir);
   rho = fRho +(step*fCosL)*fDRho;
   return step;
}
//_____________________________________________________________________________
double THelixTrack::Eval(double step, double *xyz, double *dir) const
{
  if (!step) {
    if (xyz) memcpy(xyz,fX,sizeof(fX));
    if (dir) memcpy(dir,fP,sizeof(fP));
    return 0.;
  }

  double ztep = step*fCosL;
  double teta = ztep*(fRho+0.5*ztep*fDRho);

  sgCX1=Complex(fX[0]  ,fX[1]  );
  sgCD1=Complex(fP[0],fP[1]);
  sgCD1 /=std::abs(sgCD1);
  sgImTet = Complex(0,teta);
  Complex ImRho(0,fRho);
  if (fabs(sgImTet.imag()) > 0.01)	{
//    Complex sgCf1 = std::exp(sgImTet)-1.;
    Complex hlf = std::exp(0.5*sgImTet);
    sgCf1 = 2.*Im*hlf*hlf.imag();
    sgCD2 = sgCD1 + sgCD1*sgCf1;
    sgCX2 = sgCD1*sgCf1/(ImRho);
    if (fDRho) {//
      sgCX2 += sgCD1*(sgCf1+1.)/(ImRho)*MyFactor(fRho,fDRho,ztep);
    }

  } else { 
    sgCOne = (1.+sgImTet*(0.5 +sgImTet*(1./6+sgImTet*(1./24+sgImTet/120.))));
    sgCf1 = sgImTet*sgCOne;
    sgCD2 = sgCD1 + sgCD1*sgCf1; 
    sgCX2 = sgCD1*ztep*sgCOne;
  }
  if (xyz) {
    xyz[0] = sgCX1.real()+sgCX2.real();
    xyz[1] = sgCX1.imag()+sgCX2.imag();
    xyz[2] = fX[2]+fP[2]*step;
  } 
  if (dir) {
    dir[0] = sgCD2.real()*fCosL;
    dir[1] = sgCD2.imag()*fCosL;
    dir[2] = fP[2];
  }

  return step;
}
//_____________________________________________________________________________
void THelixTrack::Fill(TCircle &circ) const
{
  circ.fX[0]=fX[0];
  circ.fX[1]=fX[1];
  circ.fD[0]=fP[0]/fCosL;
  circ.fD[1]=fP[1]/fCosL;
  circ.fRho=fRho;
  memcpy(circ.fEmx,fEmxXY,sizeof(fEmxXY));
}
//_____________________________________________________________________________
void THelixTrack::Test1()
{
double surf[4] = {-11.32212856152224, 0.50109792630239824, -0.86539108263698283, 0.00078459561521909921};
double xyz[3] = {-0.0206564,-0.0153429,0.0285582};
double dir[3] = {0.450295,-0.596426,-0.664463};
double Rho = 0.00678696;
THelixTrack TH(xyz,dir,Rho);

double s = TH.Step(100000,surf,4);
printf("s=%g = 15.3589\n",s);
}
//_____________________________________________________________________________
void THelixTrack::Test2()
{
double diff[3];

double xyz[3] = {-60.0301,1.51445,-1.57283};
double dir[3] = {-0.849461,0.526419,0.0360391};
double Rho = 0.00363571;
THelixTrack ht(xyz,dir,Rho);

double MyHit[3]= { -177.673, 41.305, 2.90798};
double MyClo[3];

printf("%s= %g %g %g\n","MyHit",MyHit[0],MyHit[1],MyHit[2]);
double s = ht.Step(MyHit,MyClo);
ht.Step(s,MyClo);
TCL::vsub(MyClo,MyHit,diff,3);
double MyDist = sqrt(TCL::vdot(diff,diff,3));
printf("%s= %g %g %g\n","MyClo ",MyClo[0],MyClo[1],MyClo[2]);
printf("MustBe= -177.661 41.4145 2.94559\n");

printf("%s= %g %g %g\n","MyDif ",diff[0],diff[1],diff[2]);
printf("MustBe= 0.0122709 0.109539 0.0376077\n");
printf("%s=%g\n","MyS   ",s);
printf("MustBe=125.375\n");
printf("%s= %g\n","MyDist",MyDist);
printf("MustBe= 0.116463\n");
}
//_____________________________________________________________________________
void THelixTrack::Test3()
{
double xyz[3] = {100,200,300};
double dir[3] = {-0.224845,-0.491295,-0.841471};
double Rho = 0.02;
double sur[8]={-120,1,0,0,0,0,0};
THelixTrack *ht = new THelixTrack(xyz,dir,Rho);
double newX[3],newD[3];
ht->Backward();
double s = ht->Step(1000.,sur,4,newX,newD);
printf("Result: s=%g newX=(%g %g %g) newD=(%g %g %g)\n"
      ,s,newX[0],newX[1],newX[2],newD[0],newD[1],newD[2]);
      
printf("MustBe: s=56.1931 newX=(120 222.222 347.285) newD=(0.464979 0.275174 0.841471)\n\n");

sur[6]=1e-6;
       s = ht->Step(1000.,sur,7,newX,newD);
printf("Result: s=%g newX=(%g %g %g) newD=(%g %g %g)\n"
      ,s,newX[0],newX[1],newX[2],newD[0],newD[1],newD[2]);
printf("MustBe: s=55.9338 newX=(119.88 222.151 347.067) newD=(0.464206 0.276476 0.841471)\n\n");
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
ClassImp(TCircle)
//______________________________________________________________________________
TCircle::TCircle(double *x,double *d,double rho)
{
  fX[0]=0; fX[1]=0; fD[0]=0; fD[1]=0;
  if (x) {fX[0]=x[0];fX[1]=x[1];}
  if (d) {
    fD[0]=d[0];fD[1]=d[1];
    double n = sqrt(fD[0]*fD[0]+fD[1]*fD[1]);
    fD[0]/=n; fD[1]/=n;
  }
  fRho = rho;
  memset(fEmx,0,sizeof(fEmx)); fEmx[0] = -2006;
}
//______________________________________________________________________________
void TCircle::Clear(const char *)   
{
 memset(fX,0,(char*)(fEmx+6)-(char*)fX);fEmx[0] = -2006;
}


//______________________________________________________________________________
void TCircle::SetEmx(const double *err)   
{ memcpy(fEmx,err,sizeof(fEmx));}

//______________________________________________________________________________
void TCircle::Nor(double *norVec) const  
{
// direction from center of circle

  norVec[0] =  fD[1];    norVec[1] = -fD[0];
  if (fRho>=0) return;
  norVec[0] = -norVec[0];norVec[1] = -norVec[1];
}
//______________________________________________________________________________
void TCircle::Print(const char* txt) const
{
  if (!txt) txt="";
  printf("TCircle(%s): x,y=%g %g dir=%g %g curv=%g\n",txt,fX[0],fX[1],fD[0],fD[1],fRho);
  if (fEmx[0]<-1) return;
  printf("Errs: %g\n"          ,fEmx[0]); 
  printf("    : %g \t%g\n"     ,fEmx[1],fEmx[2]); 
  printf("    : %g \t%g \t%g\n",fEmx[3],fEmx[4],fEmx[5]); 
}
//______________________________________________________________________________
double TCircle::Path(const double *pnt) const
{
  Complex CX1(pnt[0]-fX[0],pnt[1]-fX[1]);
  Complex CP(fD[0],fD[1]);
  Complex CXP = Im*CX1/CP;
  Complex CXPRho = CXP*fRho;
  double s;
  if (std::abs(CXPRho)>0.001) {
    s = std::log(1.+CXPRho).imag()/fRho;
  } else {
    s = (CXP*(1.-CXPRho*(0.5-CXPRho*(1/3.-CXPRho*0.25)))).imag();
  }
//   Check
  double x[2],d[2];
  Eval(s,x,d) ;
  assert(fabs((pnt[0]-x[0])*d[0]+(pnt[1]-x[1])*d[1])<1e-6);
  return s;
}
//______________________________________________________________________________
double TCircle::Eval(double step,double *X, double *D) const
{
  
  sgCX1		=Complex(fX[0],fX[1]);
  sgCD1		=Complex(fD[0],fD[1]);		//  exp(I*Fi0)
  sgImTet	=Complex(0,step*fRho);		//  I*Rho*L
  sgCOne        =expOne(sgImTet);
  sgCf1 	=sgImTet*sgCOne;
  
  sgCD2 = sgCD1*sgCf1+sgCD1; 			// exp(I*Fi0+I*Rho*L)
  sgCX2 = sgCD1*sgCOne*step;			// exp(I*Fi0)*(exp(I*Rho*L)-1)/(I*Rho)
  X[0] = sgCX2.real()+sgCX1.real();
  X[1] = sgCX2.imag()+sgCX1.imag();
  if (D) {
    sgCD2/= std::abs(sgCD2);
    D[0] = sgCD2.real();
    D[1] = sgCD2.imag();
  }
  return step;
}
//______________________________________________________________________________
double TCircle::Move(double step)
{
  Eval(step,fX,fD);
  if (fEmx[0]>-1) MoveErrs(step);
  if (fabs(fD[0])>1) fD[0]=(fD[0]<0)? -1:1;
  if (fabs(fD[1])>1) fD[1]=(fD[1]<0)? -1:1;
  return step;
}
//______________________________________________________________________________
void TCircle::MoveErrs(double l)
{
  double F[3][3],oErr[6];
  memset(F[0],0,sizeof(F));
  F[0][0] = sgCf1.real()+1.;
  F[0][1] = l*sgCOne.real();
  F[0][2] = l*l*expOneD(-sgImTet).real();
  
  F[1][1] = 1;
  F[1][2] = l;
  F[2][2] = 1;
  memcpy(oErr,fEmx,sizeof(fEmx));
  TCL::trasat(F[0],oErr,fEmx,3,3); 
}
//______________________________________________________________________________
void TCircle::Rot(double angle)
{
  Rot(cos(angle),sin(angle));
}
//______________________________________________________________________________
void TCircle::Rot(double cosa,double sina)
{
  Complex CX(fX[0],fX[1]),CP(fD[0],fD[1]);
  Complex A (cosa,sina);
  CX *=A; fX[0] = CX.real(); fX[1]=CX.imag();
  CP *=A; CP/=std::abs(CP);
  fD[0] = CP.real(); fD[1]=CP.imag();
}
//______________________________________________________________________________
void TCircle::Backward()
{
  fRho=-fRho;fD[0]=-fD[0];fD[1]=-fD[1];
  if(fEmx[0]>-1) {fEmx[3]=-fEmx[3];fEmx[4]=-fEmx[4];}
}
//______________________________________________________________________________
double TCircle::Approx(int nPts,const double *Pts,int istep)
{
	
  double xmed[12];memset(xmed,0,sizeof(xmed));
  double &xx  =xmed[2],&yy  =xmed[ 3],&xy  =xmed[4 ]
        ,&xxx =xmed[5],&yyy =xmed[ 6],&xxy =xmed[7 ],&xyy=xmed[8]
        ,&xxxx=xmed[9],&yyyy=xmed[10],&xxyy=xmed[11];

  double x, y,tmp;int i;
  double const *p;
  for (i=0,p=Pts;i<nPts;i++,p+=istep) {xmed[0]+=p[0]; xmed[1]+=p[1];}
  for (i=0;i<2;i++)      {xmed[i]/=nPts;}  

  for (i=0,p=Pts;i<nPts;i++,p+=istep){
    x = p[0]-xmed[0], y = p[1]-xmed[1];
    xx  += x*x;   yy += y*y;   xy +=x*y;   }
  for (i=2;i<5;i++)      {xmed[i]/=nPts;}  

  double Ei[2][2]={{1,0},{0,1}};
  double lMin = 2*(xx*yy- xy*xy)/((xx+yy)+sqrt((xx-yy)*(xx-yy) + 4*xy*xy));
  double lMax = xx+yy-lMin;
  if (lMax-lMin > 0.1*lMax) {
    if (fabs(xx-lMax)>fabs(yy-lMax)) {Ei[0][0]=- xy      ;Ei[0][1]=(xx-lMax);}
    else                             {Ei[0][0]=-(yy-lMax);Ei[0][1]= xy      ;}
    tmp = sqrt(Ei[0][0]*Ei[0][0]+Ei[0][1]*Ei[0][1]);
    Ei[0][0]/=tmp       ;Ei[0][1]/=tmp;
    Ei[1][0] = -Ei[0][1];Ei[1][1] = Ei[0][0];
    xx = lMax; yy = lMin; xy = 0;
  }

  for (i=0,p=Pts;i<nPts;i++,p+=istep){
    double xt = p[0]-xmed[0], yt = p[1]-xmed[1];
    x = Ei[0][0]*xt+Ei[0][1]*yt;
    y = Ei[1][0]*xt+Ei[1][1]*yt;
    xxx  += (x*x-xx)*x;       yyy  += (y*y-yy)*y; 
    xxy  += (x*x-xx)*y;       xyy  += (y*y-yy)*x;   
    xxxx += (x*x-xx)*(x*x-xx);yyyy += (y*y-yy)*(y*y-yy);
    xxyy += (x*x-xx)*(y*y-yy);             }
  for (i=5;i<12;i++)      {xmed[i]/=nPts;}  


  double det = xx*yy-xy*xy;
  double Rho,a,b,c,r,r2 = 0;
  double pars[2][6]; //contains qality,x0,y0,cos,sin,curv

  pars[0][0] = 3e33;pars[1][0] = 3e33;
  if (IsStrait()) det = 0;
  do {
    if (det <= (xx+yy)*1e-6) break;
  
    a =  0.5*( (xxx + xyy)*yy - (xxy + yyy)*xy);
    b =  0.5*(-(xxx + xyy)*xy + (xxy + yyy)*xx);
    r2 = (a*a + b*b +det*det*(xx+yy));
    r  = sqrt(r2);
    Rho = det/r;
    double ab = sqrt(a*a+b*b)+3e-33;
    double aDir = (a+3e-33)/(ab);
    double bDir = b/(ab);
//  double xc = xmed[0]+a/det;
//  double yc = xmed[1]+b/det;
//  printf ("R=%g %g %g\n",1./Rho,xc,yc);
//	Evaluate quality
    pars[0][0]  =(xxxx+yyyy)+2*(xxyy)
     + (-4*((xxx+xyy)*a+(xxy+yyy)*b))/det
     + ( 4*(xx*a*a +yy*b*b +2*xy*a*b))/det/det;
    pars[0][0] *=(Rho*Rho)/4;

    pars[0][1] = -det*(xx+yy)/(ab+r)*(aDir); 
    pars[0][2] = -det*(xx+yy)/(ab+r)*(bDir);  
    pars[0][3] = bDir; pars[0][4]=-aDir; pars[0][5]=Rho;
//  printf ("UUUUUUUUUUUUUUUUUU qa=%g rad=%g\n",pars[0][0],pars[0][5]);
  }while(0);// end of do 

//	Stright track approx
  {
    c = (xxy*xx-xy*xxx)/(xxxx*xx-xxx*xxx);
    b = (xy-c*xxx)/xx; a = -c*xx;
    pars[1][0] = a*a+b*b*xx+c*c*(xxxx+xx*xx)+yy
               +2*(a*c*xx+b*c*xxx-b*xy-c*xxy);
    pars[1][1] = 0;pars[1][2] = a;
    tmp = sqrt(1+b*b);
    pars[1][3] = 1/tmp;pars[1][4] = b/tmp;
    pars[1][5] = 2*c/((1+b*b)*tmp);
/// printf ("LLLLLLLLLLLLLLLLLL qa=%g rad=%g\n",pars[1][0],pars[1][5]);
  }

  i = (pars[0][0]<pars[1][0])? 0:1;
  memcpy(fX,pars[i]+1,5*sizeof(fX[0]));
  tmp = sqrt(fD[0]*fD[0]+fD[1]*fD[1]);
  fD[0]/=tmp; fD[1]/=tmp;
  Rot( atan2(Ei[0][1],Ei[0][0]));
  fX[0]+=xmed[0]; fX[1]+=xmed[1];

  double s = Path(Pts);
  Move(s);
  if (s>0) Backward();
  return pars[i][0];
}

//______________________________________________________________________________
double TCircle::Resid(int nPts,const double *Pts,int pstep
                              ,const double *Ers,int estep)
{
  if (!nPts) return 0;
  TCircle M = *this;
  const double *p = Pts,*e = Ers;
  double sum = 0,wtot=0,wt;
  for (int i=0;i<nPts;i++,p+=pstep,e+=estep) {
    double s = M.Path(p);
    M.Move(s);
    wt = (Ers)? 1./e[0]:1;
    sum+= (pow(M.fX[0]-p[0],2)+pow(M.fX[1]-p[1],2))*wt;
    wtot +=wt;
  }
  return sqrt(sum/wtot);
}

//______________________________________________________________________________
double TCircle::FitZ(double *Z0Tan,int nPts
                    ,const double *Pts,int pstep
		    ,const double *Zts,int zstep
		    ,const double *Ers,int estep)
{
  enum {kS,kZ,kSS,kSZ,kZZ,kWT,kZ0=0,kTan=1};
  double aver[6] = {0,0,0,0,0,0};
  TCircle MC = *this;
  double s = 0;
  const double *p = Pts;
  const double *z = Zts;
  const double *e = Ers;
  int n = 0;
  for (int i=0;i<nPts;i++) {
    double ds = MC.Path(p);
    MC.Move(ds);
    s+=ds;
    double wt = (Ers)? 1./(*e):1;
    aver[kWT]+=wt; 
    aver[kS ]+=s	*wt; 
    aver[kZ ]+=z[0]	*wt; 
    aver[kSS]+=s*s	*wt; 
    aver[kSZ]+=z[0]*s	*wt; 
    aver[kZZ]+=z[0]*z[0]*wt; 
    p+=pstep; z+=zstep;e+=estep,n++;
  }
  for (int j=kS;j<kWT;j++) {aver[j]/=aver[kWT];}
  aver[kSS]-=aver[kS]*aver[kS];
  aver[kSZ]-=aver[kS]*aver[kZ];
  aver[kZZ]-=aver[kZ]*aver[kZ];
  Z0Tan[kTan] = aver[kSZ]/aver[kSS];
  Z0Tan[kZ0 ] = aver[kZ]-Z0Tan[kTan]*aver[kS];
  double res = pow(Z0Tan[kZ0]+Z0Tan[kTan]*aver[kS] - aver[kZ],2)
             + Z0Tan[kTan]*Z0Tan[kTan]*aver[kSS] +aver[kZZ]
             - 2*Z0Tan[kTan]*aver[kSZ];
  return res;
}
//______________________________________________________________________________
double TCircle::Fit(int nPts,const double *Pts,int pstep
                            ,const double *Err,int estep)
{
//  C,S direction to the center
//  Dx,Dy direction of moving
//  C = -Dy, S=Dx  for positve curvature
  enum {kC=0,kS,kR,kCC,kSS,kCS,kCR,kSR,kRR,kXX=0,kXY,kYY};

  TArrayD ta(nPts*4); double *ar = ta.GetArray();
  double *C = ar,*S = (ar+=nPts),*R = (ar+=nPts),*W = (ar+=nPts);
  double av[9];
  double mySize = sqrt(pow(Pts[0]-Pts[0+(nPts-1)*pstep],2)+
                     pow(Pts[1]-Pts[1+(nPts-1)*pstep],2));


//  calc average
  TCircle TCbeg = *this;
  double dl = TCbeg.Path(Pts);
  TCbeg.Move(dl);
  double begNor[2],curNor[2];
  for (int iter=0;iter<20;iter++) {
    int cutStep = 0;
    double wtot=0,wt;
    begNor[0] =  TCbeg.fD[1]; begNor[1] = - TCbeg.fD[0];
    double rho = TCbeg.fRho;
    TCircle TCcur = TCbeg;
    memset(av,0,sizeof(av));
    double l = 0,dc,ds,res;
    const double *p = Pts; const double *e = Err;
    for (int iPt=0;iPt<nPts;iPt++,p+=pstep,e+=estep) {
      dl = TCcur.Path(p);
      TCcur.Move(dl);
      l += dl;
      curNor[0] =  TCcur.fD[1]; curNor[1] = - TCcur.fD[0];

      if (fabs(rho*l)<1e-3) {
	dc = -0.5*(curNor[1]+begNor[1])*l; 
	ds =  0.5*(curNor[0]+begNor[0])*l;
      } else {
	dc = (curNor[0]-begNor[0])/(rho);
	ds = (curNor[1]-begNor[1])/(rho);
      }
      res = 0;
      for (int j=0;j<2;j++) {res+=(p[j]-TCcur.fX[j])*curNor[j];}
      wt = 1;
//		Account errors
      if (Err) {
        double err = e[kXX]*curNor[0]*curNor[0]
                   + e[kYY]*curNor[1]*curNor[1]
                   + e[kXY]*curNor[0]*curNor[1]*2;
        if (err < 1e-8) err = 1e-8;
	wt = 1./err; W[iPt] = wt;
      }
	

      C[iPt]= dc; S[iPt]=ds; R[iPt]=res;
      av[kC]+=dc	*wt;
      av[kS]+=ds	*wt;
      av[kR]+=res	*wt;
      wtot  +=wt;
    }
    for (int j=kC;j<=kR;j++){ av[j]/=wtot;}

    for (int iPt=0;iPt<nPts;iPt++) {
      dc  = C[iPt]-av[kC];
      ds  = S[iPt]-av[kS];
      res = R[iPt]-av[kR];
      wt = (Err)? W[iPt]:1;
      av[kCC]+= dc*dc	*wt;
      av[kSS]+= ds*ds	*wt;
      av[kCS]+= dc*ds	*wt;
      av[kCR]+= dc*res	*wt;
      av[kSR]+= ds*res	*wt;
      av[kRR]+= res*res	*wt;
    }
    for (int j=kCC;j<=kRR;j++){ av[j]/=wtot;}
    double det =  av[kCC]*av[kSS]-av[kCS]*av[kCS];
    double dx  = 100, dy  = 100;
    if (det > 1e-5 *av[kCC]*av[kSS]) {
      dx  = ( av[kSS]*av[kCR]-av[kCS]*av[kSR])/det;
      dy  = (-av[kCS]*av[kCR]+av[kCC]*av[kSR])/det;
    }  else {
      dx = av[kCC]; dy = av[kCS];
      double alfa = av[kCR]/(dx*dx+dy*dy);
      dx *= alfa;   dy*=alfa; 
    }
    double tmp = fabs(dx); if (tmp<fabs(dy)) tmp=fabs(dy);
    if (tmp > 0.3) {
       cutStep = 1; tmp = 0.1/tmp;
       dx *= tmp; dy *= tmp; av[kR]*=tmp;
    }
    
    double h = (av[kR]-av[kC]*dx-av[kS]*dy);
    double dRR   = h*rho - (begNor[0]*dx+begNor[1]*dy);
    TCbeg.fRho  -= dRR*rho/(1+dRR);
    double sinD  = (TCbeg.fD[0]*dx+TCbeg.fD[1]*dy);
    double cosD  = sqrt(1-sinD)*(1+sinD);
    double newDx =  TCbeg.fD[0]*cosD + TCbeg.fD[1]*sinD;
    double newDy = -TCbeg.fD[0]*sinD + TCbeg.fD[1]*cosD;
    tmp = sqrt(newDx*newDx+newDy*newDy);
    TCbeg.fD[0]  = newDx/tmp;
    TCbeg.fD[1]  = newDy/tmp;
    TCbeg.fX[0] += h*begNor[0];
    TCbeg.fX[1] += h*begNor[1];
    if (fabs(h/mySize) + fabs(dRR) + fabs(sinD) <1e-5 && !cutStep) break;

  }
  *this = TCbeg;
  if (!Err) return av[kRR];
  



  


  return av[kRR];
}    
  
//______________________________________________________________________________
void TCircle::Test2() 
{
double xyz[4][3]= {{-39.530250549316406, -165.19537353515625, 184.05630493164062}
                  ,{-37.718906402587891, -167.19537353515625, 186.41175842285156}
		  ,{-35.468486785888672, -169.19537353515625, 189.05546569824219}
                  ,{-33.657142639160156, -171.19537353515625, 191.347900390625}};
double x[4],y[4];
for (int i=0;i<4;i++) { x[i]=xyz[i][0];  y[i]=xyz[i][1]; }



TCircle TC;
double qa0 = TC.Approx(4,xyz[0],3);
double qa1 = TC.Resid (4,xyz[0],3);
printf("Approx qa0 = %g qa1=%g\n",qa0,qa1);
TC.Print();


}
//______________________________________________________________________________
void TCircle::Test3() 
{
enum {nPnts=4};
double xyz[nPnts][3] = 
{{80.815544128417969, 159.77731323242188, 129.11553955078125}
,{82.239913940429688, 161.25840759277344, 131.24034118652344}
,{84.462181091308594, 162.28025817871094, 133.59538269042969}
,{86.321846008300781, 163.51133728027344, 135.19621276855469}};

double err[nPnts][4] = 
{{0.0010703595155359307, 0.00061836299089800776, 0.00035723771589107141,0.0032088035791992191}
,{0.0010505530116463389, 0.00060692047199979574, 0.00035062719848397145,0.0031350950603759769}
,{0.0010286003088986414, 0.00059423806134026682, 0.00034330037672605356,0.0030533996126220703}
,{0.0010136781863030494, 0.00058561716272119912, 0.00033831985920934062,0.0029978674575439454}};


double res;
TCircle circ;
res=circ.Approx(nPnts,xyz[0],3);
printf("res = %g \n",res);
circ.Print();
res=circ.Resid (nPnts,xyz[0],3);
printf("res = %g \n",res);
circ.Print();

circ.Show(nPnts,xyz[0],3);
res = circ.Fit(nPnts,xyz[0],3,err[0],4);
printf("res = %g \n",res);
circ.Print();
circ.Show(nPnts,xyz[0],3);

}
//______________________________________________________________________________
//______________________________________________________________________________
TCircleFitter::TCircleFitter()
{
  Clear();
}
//______________________________________________________________________________
void TCircleFitter::Clear(const char*)
{
   fArr.Reset();
   memset(fBeg,0,fEnd-fBeg+1);
   TCircle::Clear();
}
//______________________________________________________________________________
TCircleFitterAux* TCircleFitter::GetAux(int i) const
{
  return (TCircleFitterAux*)(fArr.GetArray()+i*TCircleFitterAux::dSize());
}
//______________________________________________________________________________
const double* TCircleFitter::GetX(int i) const
{
  return &(fAux[i].x);
}
//______________________________________________________________________________
double* TCircleFitter::GetX(int i) 
{
  return &(fAux[i].x);
}
//______________________________________________________________________________
void  TCircleFitter::Add(double x,double y,const double *errs) 
{
  fNuse =++fN;
  int n = fN*TCircleFitterAux::dSize();
  if (fArr.GetSize()<n) {fArr.Set(n*2);fAux=0;}
  if (!fAux) fAux = GetAux(0);
  TCircleFitterAux *aux = fAux+fN-1;
  aux->x = x; aux->y=y; aux->exy[0]=0; aux->exy[2]=0; aux->wt=0;
  if (errs) AddErr(errs);
}
//______________________________________________________________________________
void  TCircleFitter::Add(double x,double y,double z) 
{
  fNuse =++fN;
  int n = fN*TCircleFitterAux::dSize();
  if (fArr.GetSize()<n) {fArr.Set(n*2);fAux=0;}
  if (!fAux) fAux = GetAux(0);
  TCircleFitterAux *aux = fAux+fN-1;
  aux->x = x; aux->y=y; aux->z=z;aux->exy[0]=-1; aux->exy[2]=0;aux->wt=0;
}
//______________________________________________________________________________
void  TCircleFitter::AddErr(const double *errs,double ezz) 
{
  TCircleFitterAux *aux = fAux+fN-1;
  assert(errs[0]>=0);
  assert(errs[2]>=0);
  double spur = errs[0]+errs[2];
  assert(spur>0);
  double *e = aux->exy;
  memcpy(e,errs,sizeof(aux->exy));

  if (e[0]<0 && e[0]>-1e-5*spur) {e[0]=0;e[1]=0;}
  if (e[2]<0 && e[2]>-1e-5*spur) {e[2]=0;e[1]=0;}
  assert(e[1]*e[1]<=1.01*e[0]*e[2]);


  aux->ezz = ezz;
}
//______________________________________________________________________________
void  TCircleFitter::Skip(int idx) 
{
   fWtot-=fAux[idx].wt;
   fAux[idx].wt = -1;
   if (fNdf) fNdf--;
   fNuse--;
}
//______________________________________________________________________________
void  TCircleFitter::AddZ(double z,double ez) 
{
// Must be called immediatelly after Add(...)
  fAux[fN-1].z  =z;
  fAux[fN-1].ezz=ez;
}
//______________________________________________________________________________
/***************************************************************************
 *
 * $Id: THelixTrack.cxx,v 1.23 2006/06/28 18:39:07 perev Exp $
 *
 * Author: Victor Perev, Mar 2006
 * Rewritten Thomas version. Error hangling added
 * Author: Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 * Fast fitting routine using a iterational linear regression 
 * method (ILRM). Reference: N.Chernov, G.A.Ososkov, Computer  
 * Physics Communication 33 (1984) 329-333.                   
 *
 ***************************************************************************
 *
 * $Log: THelixTrack.cxx,v $
 * Revision 1.23  2006/06/28 18:39:07  perev
 * cos(dip)**4 added to Dca(...) to account z err in the nearest point
 *
 * Revision 1.22  2006/06/26 19:09:21  perev
 * DcaXY & DcaZ with errors added
 *
 * Revision 1.21  2006/06/09 19:53:51  perev
 * double Dca(double x,double y,double *dcaErr=0) added
 *
 * Revision 1.2  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/12/21 16:28:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
double TCircleFitter::Fit() 
{
static const int nAVERs = &fRr-&fXx;
static int nCall=0; nCall++;
    int i;
    double xx, yy, xx2, yy2;
    double f, g, h, p, q, t, g0, g02, a, b, c, d;
    double xroot, ff, fp;
    double dx, dy, nx,ny, xnom,wt,hord,tmp,radius2,radiuc2;
    fKase = fCase;
    if (fNuse < 3) return 3e33;
    TCircleFitterAux *aux = GetAux(0);
    dx = aux[fN-1].x - aux[0].x;
    dy = aux[fN-1].y - aux[0].y;
    hord = sqrt(dx*dx+dy*dy);
    fCos = dx/hord;
    fSin = dy/hord;
    int withErr = aux[0].exy[0]+aux[0].exy[2]>0;
    fNor[0] = -fSin,fNor[1] = fCos;
    int nter= (withErr)? 2:1;
    for (int iter=0;iter<nter;iter++) {
      fWtot = 0;
      memset(&fXgravity,0,sizeof(double)*(nAVERs+2));
      for (i=0; i<fN; i++) {
        if (aux[i].wt<0) continue;
        wt = 1;
	if (withErr) {
          if (iter) {
            fNor[0] = fXCenter - aux[i].x;
            fNor[1] = fYCenter - aux[i].y;
            tmp = sqrt(fNor[0]*fNor[0]+fNor[1]*fNor[1]);
            fNor[0]/=tmp; fNor[1]/=tmp; 
	  } 
//	  wt = Weight(i);
          wt = fNor[0]*fNor[0]*aux[i].exy[0]
             + fNor[0]*fNor[1]*aux[i].exy[1]*2
             + fNor[1]*fNor[1]*aux[i].exy[2];
          wt = 1/wt;
        }
        aux[i].wt = wt;
        fWtot += wt;
	fXgravity += aux[i].x *wt;
	fYgravity += aux[i].y *wt;
      }
      fXgravity /= fWtot;
      fYgravity /= fWtot;

      for (i=0; i<fN; i++) {
	  dx  = aux[i].x-fXgravity;
	  dy  = aux[i].y-fYgravity;
	  xx  =  dx*fCos + dy*fSin;
	  yy  = -dx*fSin + dy*fCos;
	  xx2 = xx*xx;
	  yy2 = yy*yy;
          wt  = aux[i].wt;
	  fXx    += xx2 		*wt;
	  fYy    += yy2 		*wt;
	  fXy    += xx*yy 		*wt;
	  fXrr   += xx*(xx2+yy2) 	*wt;
	  fYrr   += yy*(xx2+yy2) 	*wt;
	  fRrrr += (xx2+yy2)*(xx2+yy2) 	*wt;
      }
      double *dd = &fXx;
      for (i=0;i<nAVERs;i++) {dd[i]/=fWtot;}
      fRr = fXx+fYy;

      if (fNuse <= 3) fKase=1;
      if (!fKase) fKase =(fYy < fXx *(0.5)*(0.5)/210)? 1:2;
      switch(fKase) {
        case 1:	{	//Try 1st method

//  		Variables v0=1, v1=2*x, v2 =-rr == -(x*x+y*y)
//  		Orthogonal functions of these variables:
//  		Nor0 = fPol[0]
//  		Nor1 = fPol[1]+ v1*fPol[2]
//  		Nor2 = fPol[3]+ v1*fPol[4]+ v2*fPol[5] 
    
	  double myCof[3];    
	  fPol[0] = 1;
	  fPol[1] = 0;    fPol[2] = 1./(2*sqrt(fXx));
	  fPol[3] = fRr;  fPol[4] = fXrr/(2*fXx);   fPol[5] = 1.;
	  double tmp = sqrt(fPol[3]*fPol[3]
                	   +fPol[4]*fPol[4]*(4*fXx  )
	        	   +fPol[5]*fPol[5]*(fRrrr  )
                	   +fPol[3]*fPol[5]*(-fRr   ) *2
                	   +fPol[4]*fPol[5]*(-2*fXrr) *2);
	  fPol[3]/=tmp;fPol[4]/=tmp;fPol[5]/=tmp;
	  myCof[0] =   0;
	  myCof[1] = - (fPol[2]*(4*fXy));
	  myCof[2] = - (fPol[4]*(4*fXy) + fPol[5]*(-2*fYrr));
	  c = myCof[0]*fPol[0]+myCof[1]*fPol[1]+myCof[2]*fPol[3];
	  a =                  myCof[1]*fPol[2]+myCof[2]*fPol[4];
	  b =                                   myCof[2]*fPol[5];
          fYd = (fabs(b)>1e-6) ? 1./b : 1e6;
          fXd = a*fYd;
        }// end case 1
        break;

        case 2:	{	//Try 2nd method(Ososkov/Chernov)

	  f = (3.*fXx+fYy);
	  g = (fXx+3.*fYy);
	  h = 2*fXy;
	  p = fXrr;
	  q = fYrr;
	  t = fRrrr;
	  g0 = (fXx+fYy);
	  g02 = g0*g0;
	  a = -4.0;
	  b = (f*g-t-h*h)/g02;
	  c = (t*(f+g)-2.*(p*p+q*q))/(g02*g0);
	  d = (t*(h*h-f*g)+2.*(p*p*g+q*q*f)-4.*p*q*h)/(g02*g02);
	  xroot = 1.0;
	  for (i=0; i<5; i++) {
	      ff = (((xroot+a)*xroot+b)*xroot+c)*xroot+d;
	      fp = ((4.*xroot+3.*a)*xroot+2.*b)*xroot+c;
	      xroot -= ff/fp;
	  }
	  fG1 = xroot*g0;
	  xnom = (g-fG1)*(f-fG1)-h*h;
	  assert(xnom>1e-15);
	  fXd = ( p*(g-fG1)-q*h      )/xnom;
	  fYd = (-p*h      +q*(f-fG1))/xnom;
        }//end case 2
        break;
        
	default: assert(0);
      } //end switch
      fXCenter = fXd*fCos-fYd*fSin + fXgravity;
      fYCenter = fXd*fSin+fYd*fCos + fYgravity;
    }// end iters
    
//	Update TCircle
    switch (fKase) {    
      case 1:  {//Big R approx
	fCorrR = sqrt(1+a*a+c*b );
	fCorrB = sqrt(1+a*a     );
	fRho = fabs(b)/fCorrR;
	int sgB = (b<0)? -1:1;
	ny = sgB/sqrt(1+a*a);
	nx = a*ny;
	fH = -c*sgB/(fCorrR+fCorrB);
	fChi2 = (4*a*fXy +4*fYy- 2*b*fYrr)/4;
	fChi2 /= (fCorrR*fCorrR);
      } 
      break;
      case 2:  {//Ososkov
	radiuc2  = fXd*fXd+fYd*fYd;
	radius2  = radiuc2+fG1;
	double radius = ::sqrt(radius2);
	double radiuc = ::sqrt(radiuc2);
	fRho  = 1./radius;
	fH = -fG1/(radius+radiuc);
	nx = fXd/radiuc;
	ny = fYd/radiuc;
	fChi2 = (fG1-fRr)/2;
      }
      break;
      default: assert(0);
    }
    fNdf = fNuse-3;
    if (fNdf>0) fChi2 *= fWtot/fNdf;
    fA=a;fB=b;fC=c;
    fX[0] = nx*fH; fX[1] = ny*fH;
// 	let we are moving left --> right
//    	remember to change sign of correlation related to H if fRho<0
    fD[0] = ny; fD[1] =-nx;  
//
    Rot(fCos,fSin);
    fX[0] +=  fXgravity;
    fX[1] +=  fYgravity;
    tmp = fD[0]*(aux[0].x-fX[0])+fD[1]*(aux[0].y-fX[1]);
//	remember to change corrs related to rho and h
    fBack = 0;
    if (tmp>0) {fD[0]*=-1;fD[1]*=-1;fRho*=-1;fBack=1;}
    return fChi2;
}
//______________________________________________________________________________
void TCircleFitter::MakeErrs() 
{
   memset(fEmx,0,sizeof(fEmx));
   double F[3][3]; memset(F[0],0,sizeof(F));
   double myFact = 1.;
   switch (fKase) {
     case 1: { //For BigYC  fit
       fCov[0] = fPol[2]*fPol[2]+ fPol[4]*fPol[4];
       fCov[1] = fPol[4]*fPol[5];
       fCov[2] = fPol[5]*fPol[5];
       fCov[3] = fPol[1]*fPol[2]+ fPol[3]*fPol[4];
       fCov[4] = fPol[3]*fPol[5];
       fCov[5] = fPol[0]*fPol[0]+ fPol[1]*fPol[1]+fPol[3]*fPol[3];
       for (int i=0;i<6;i++) {fCov[i]*=4;}
       int sgB = (fB<0)? -1:1;
       double corrRB = fCorrR+fCorrB;
       double corrR3 = fCorrR*fCorrR*fCorrR;
 //      fH = -c*sgB/(fCorrR+fCorrB);

       F[0][0] =      sgB*fA*fC/(corrRB*fCorrB*fCorrR);		//dH/da
       F[0][1] =  0.5*sgB*fC*fC/(corrRB*corrRB*fCorrR);		//dH/db
       F[0][2] =  0.5*sgB*fC*fB/(corrRB*corrRB*fCorrR)		//dH/dc
               -      sgB      /(corrRB              );
       F[1][0] =  -1/(fCorrB*fCorrB);				//dFi/da
       F[2][0] =  -   sgB*fA*fB/(corrR3);			//d(aRho)/da
       F[2][1] =  -0.5*sgB*fC*fB/(corrR3)+sgB/fCorrR;		//d(aRho)/db
       F[2][2] =  -0.5*sgB*fB*fB/(corrR3);			//d(aRho)/dc
       myFact  = (fCorrR*fCorrR);
       break;
     }
      case 2:    { //For Ososkov/Chernov fit
// <F> = (C-<(x**2 +y**2)>)/(2*R)
// <dF/dA*dF/dA> = (<x*x>*R - <F>*A*A)/R**3
// <dF/dB*dF/dA> = (<x*y>*R - <F>*A*B)/R**3
// <dF/dB*dF/dB> = (<y*y>*R - <F>*B*B)/R**3
// <dF/dC*dF/dA> = -<F>*A  /(2*R**3)
// <dF/dC*dF/dB> = -<F>*B  /(2*R**3)
// <dF/dC*dF/dC> =  (R-<F>)/(4*R**3)

	double aRho = fabs(fRho);
	double aRho2 = aRho*aRho;
	double Fm   = (fG1 - (fXx+fYy))*aRho/2;
	fCov[0]  = (fXx - Fm*aRho*fXd*fXd)*aRho2;	//  <dF/dA * dF/dA>
	fCov[1]  = (fXy - Fm*aRho*fXd*fYd)*aRho2;  	//  <dF/dA * dF/dB>
	fCov[2]  = (fYy - Fm*aRho*fYd*fYd)*aRho2;	//  <dF/dB * dF/dB>
	double aRho3 = aRho*aRho2;
	fCov[3]  = -0.5*(Fm*fXd    )*aRho3;		//  <dF/dC * dF/dA>
	fCov[4]  = -0.5*(Fm*fYd    )*aRho3;		//  <dF/dC * dF/dB>
	fCov[5]  =  0.25*(1-Fm*aRho)*aRho2;		//  <dF/dC * dF/dC>

//   h = Rx - R
	double xyRho2= 1./(fXd*fXd+fYd*fYd);
	double xyRho = sqrt(xyRho2);
	double tmp = fG1*aRho2*xyRho2/(aRho+xyRho);

	F[0][0] =  fXd*tmp;		// dH /dXd
	F[0][1] =  fYd*tmp;		// dH /dYd
	F[0][2] = -0.5*aRho;		// dH /dG1
	F[1][0] = -fYd*xyRho2;		// dFi/dXd
	F[1][1] =  fXd*xyRho2;		// dFi/dYd
	F[1][2] = 0;			// dFi/dG1
	F[2][0] = -fXd*aRho3;		// dRho/dXd 
	F[2][1] = -fYd*aRho3;		// dRho/dYd 
	F[2][2] = -0.5*aRho3;		// dRho/dG1 

	TCL::trsinv(fCov,fCov       ,3);
        break;
      }
      default: assert(0);
   } // end switch
   TCL::trasat(F[0],fCov,fEmx,3,3); 
   TCL::vscale(fEmx,myFact/fWtot,fEmx,6);
   if (fBack) {fEmx[1]*=-1;fEmx[4]*=-1;}
}
//______________________________________________________________________________
double TCircleFitter::EvalChi2() 
{
  if (!fNuse) return 0;
  TCircle M = *this;
  double sum = 0,wtot=0,wt;
  TCircleFitterAux *aux = GetAux(0);
  const double *p = M.Pos();
  for (int i=0;i<fN;i++) {
    if (aux[i].wt<0) continue;
    double s = M.Path(&(aux[i].x));
    M.Move(s);
    wt = aux[i].wt;
    sum+= (pow(p[0]-aux[i].x,2)+pow(p[1]-aux[i].y,2))*wt;
    wtot +=wt;
  }
  if (fNdf) sum /= fNdf;
  fChi2 = sum;
  return sum;
}
//_____________________________________________________________________________
void TCircleFitter::FixAt(const double vals[5],int flag) 
{
///  void TCircleFitter::FixAt(const double vals[4],double curv,int flag) 
///  fix circle at specific x,y;Psi;Curv
///  vals[0,1]	- x,y
///  vals[2]    - reserved for Z, not used here
///  vals[3]   	- Psi
///  vals[4]	- Curvature
///  flag	- +1=xy fix,+2=Psi fix,+4 =curv fix

   assert(fEmx[0]>0);
   assert(flag);
   double g[6]={1,0,1,0,0,1},e[6],adj[3]={0,0,0},amda[3],dlt[2];
   int sel[3] ={!!(flag&1), !!(flag&2), !!(flag&4)};
   int nFix=0;
   if (sel[0]) {  	// h corr
     nFix++;
     dlt[0] = vals[0]-fX[0]; dlt[1] = vals[1]-fX[1];
     adj[0] = -dlt[0]*fD[1]+dlt[1]*fD[0];
   }
   if (sel[1]) {	// angle corr
     nFix++;
     adj[1] = vals[3]-atan2(fD[1],fD[0]);
     if (adj[1]< -M_PI) adj[1] += 2*M_PI;
     if (adj[1]>  M_PI) adj[1] -= 2*M_PI;
   }
   if (sel[2]) {	//curv corr
     nFix++;
     adj[2] = vals[4]-fRho;
   }

   TCL::trsinv(fEmx,e,3);
   for (int i=0,li=0;i< 3;li+=++i) {
     for (int j=0   ;j<=i;j++    ) {
       if (!(sel[i]|sel[j])) continue;
       e[li+j] = (i==j);
       if (!(sel[i]&sel[j])) continue;
       g[li+j] = fEmx[li+j];
   } }
   TCL::trsinv(g        ,g   ,3  );
   TCL::trsa  (g   ,adj ,amda,3,1);
   TCL::trsa  (fEmx,amda,adj ,3,1);
   TCL::trsinv(e        ,fEmx,3  );

   for (int i=0,li=0;i< 3;li+=++i) {if (sel[i]) fEmx[li+i]=0;}
//     	update x,y
   fX[0] += -adj[0]*fD[1];
   fX[1] +=  adj[0]*fD[0];
//  	update direction
//    double S = adj[1]*(1-adj[1]*adj[1]/6);
//    double C = 1-adj[1]*adj[1]/2;
   double S = sin(adj[1]);
   double C = cos(adj[1]);
   double d0 = fD[0];
   fD[0] = d0*C-fD[1]*S;
   fD[1] = d0*S+fD[1]*C;
//  	update curvature
   fRho += adj[2];
   fNdf+=nFix;
}
#if 0
//_____________________________________________________________________________
void TCircleFitter::FixAt(const double vals[5],int flag) 
{
///  void TCircleFitter::FixAt(const double vals[4],double curv,int flag) 
///  fix circle at specific x,y;Psi;Curv
///  vals[0,1]	- x,y
///  vals[2]    - reserved for Z, not used here
///  vals[3]   	- Psi
///  vals[4]	- Curvature
///  flag	- +1=xy fix,+2=Psi fix,+4 =curv fix

   assert(fEmx[0]>0);
   assert(flag);
   double g[15],adj[5],dlt[2];
   int sel[3] ={!!(flag&1), !!(flag&2), !!(flag&4)};
   int idx[2];
   memset(adj,0,sizeof(adj));
   memset(g  ,0,sizeof(g  ));
   int n=3;
   if (sel[0]) {  	// h corr
     dlt[0] = vals[0]-fX[0]; dlt[1] = vals[1]-fX[1];
     adj[n] = -dlt[0]*fD[1]+dlt[1]*fD[0];
     idx[-3+n++]=0;
   }
   if (sel[1]) {	// angle corr
     adj[n] = vals[3]-atan2(fD[1],fD[0]);
     if (adj[1]< -M_PI) adj[n] += 2*M_PI;
     if (adj[1]>  M_PI) adj[n] -= 2*M_PI;
     idx[-3+n++]=1;
   }
   if (sel[2]) {	//curv corr
     adj[n] = vals[4]-fRho;
     idx[-3+n++]=2;
   }

   TCL::trsinv(fEmx,g,3);
   for (int i=3,li=6;i< n;li+=++i) {g[li+idx[i-3]] = 1;}

   myEqu(g, n, adj, 1);

//     	update x,y
   fX[0] += -adj[0]*fD[1];
   fX[1] +=  adj[0]*fD[0];
//  	update direction
   double S = adj[1]*(1-adj[1]*adj[1]/6);
   double C = 1-adj[1]*adj[1]/2;
   double d0 = fD[0];
   fD[0] = d0*C-fD[1]*S;
   fD[1] = d0*S+fD[1]*C;
//  	update curvature
   fRho += adj[2];
   


   memcpy(fEmx,g,sizeof(fEmx));
}
#endif
//______________________________________________________________________________
double TCircleFitter::FitZ()
{
  enum {kS,kZ,kSS,kSZ,kZZ,kWT,kZ0=0,kTan=1};
  double aver[6] = {0,0,0,0,0,0};
  TCircle MC = *this;
  double dS = MC.Path(&(fAux[0].x));
  MC.Move(dS);
  double s = 0;
  int n = 0;
  for (int i=0;i<fN;i++) {
    double ds = MC.Path(&(fAux[i].x));
    MC.Move(ds);
    s+=ds;
    double z   = fAux[i].z;
    double ezz = fAux[i].ezz;
    double wt = (ezz>0) ? 1./ezz : 1.;
    aver[kWT]+=wt; 
    aver[kS ]+=s	*wt; 
    aver[kZ ]+=z	*wt; 
    aver[kSS]+=s*s	*wt; 
    aver[kSZ]+=z*s	*wt; 
    aver[kZZ]+=z*z	*wt; 
    n++;
  }
  for (int j=kS;j<kWT;j++) {aver[j]/=aver[kWT];}
  aver[kSS]-=aver[kS]*aver[kS];
  aver[kSZ]-=aver[kS]*aver[kZ];
  aver[kZZ]-=aver[kZ]*aver[kZ];
  fTanL = aver[kSZ]/aver[kSS];
  fZ0   = aver[kZ]-fTanL*(aver[kS]+dS);


  double res = aver[kZZ]-2*fTanL*aver[kSZ];
  return res;
}
//______________________________________________________________________________
void TCircleFitter::Print(const char* txt) const
{
  if (!txt) txt="";
  printf("TCircleFitter::NPoints = %d method=%d",fN,fKase);
  if (fChi2) printf(" Chi2 = %g",fChi2);
  printf("\n");
  TCircle::Print();

  int iP = (strchr(txt,'P') || strchr(txt,'p'));
  int iE = (strchr(txt,'E') || strchr(txt,'e'));
  int iF = (strchr(txt,'F') || strchr(txt,'f'));
  int iZ = (strchr(txt,'Z') || strchr(txt,'z'));if(iZ){};
  TCircleFitterAux *aux = GetAux(0);
  if (iP) { //Print points
    for (int ip=0;ip<fN;ip++) {
      printf("%3d - X: %g\tY:%g \tZ:%g",ip,aux[ip].x,aux[ip].y,aux[ip].z);
      if (iE)  
      printf(" \tExy: %g %g %g \tEz:%g "
            ,aux[ip].exy[0],aux[ip].exy[1],aux[ip].exy[2],aux[ip].ezz);
      printf("\n");
  }}
  if (iF) { //Print fit
    TCircle circ(*this);
    const double *xy = GetX(0);
    double ds=circ.Path(xy);
    circ.Move(ds);
    double s=0;
    for (int i=0;i<fN;i++) {
      xy = GetX(i);
      ds = circ.Path(xy);
      s+=ds;
      circ.Move(ds);
      if (fabs( s)<1e-6) s=0;
      if (fabs(ds)<1e-6)ds=0;
      printf("%3d - S=%g(%g) \tX: %g=%g \tY:%g=%g \tdirX=%g dirY=%g\n"
          ,i,s,ds
          ,xy[0],circ.Pos()[0]
          ,xy[1],circ.Pos()[1]
          ,circ.Dir()[0],circ.Dir()[1]);
  }}

}
//______________________________________________________________________________
void TCircleFitter::Test() 
{
  enum {nPts=20};
  double e[4],x[3];    
  double aShift[5];
  aShift[0]=-acos(0.25);
  aShift[1]=-acos(0.50);
  aShift[2]= 0;
  aShift[3]= acos(0.25);
  aShift[5]= acos(0.50);
  double RERR = 0.1;
TRandom ran;
static TCanvas* myCanvas=0;
static TH1F *hh[6]={0,0,0,0,0,0};
static const char *hNams[]={"dH","pH","dA","pA","dC","pC",0};
static const char *hTits[]=
{"delta H","pull H","delta Psi","pull Psi","delta Curv","pull Curv",0};
  if(!myCanvas)  myCanvas=new TCanvas("C1","",600,800);
  myCanvas->Clear();
  myCanvas->Divide(1,6);

  for (int i=0;i<6;i++) { 
    double lim = (i&1)? 5:0.1;
    delete hh[i]; hh[i]= new TH1F(hNams[i],hTits[i],100,-lim,lim);
    myCanvas->cd(i+1); hh[i]->Draw();
  }

  int nFit = 0;
  for (int ir = 50; ir <= 1000; ir +=5) 		{//loop over radius
    double aR = ir;
    double len = 100; if (len>aR*3) len = aR*3;
    for (double ang0 = -3; ang0 < 3.1; ang0+=0.05)	{//loop over ang 
      for (int sgn = -1; sgn<=1; sgn+=2)    		{//loop over signes of curv
	double R = sgn*aR;
	double dang = len/R/nPts;
	double C0 = cos(ang0);
	double S0 = sin(ang0);
        TCircleFitter helx;
	for (int is=0;is<nPts;is++) {	//loop over points
          double ang = ang0 + dang*is;
          double S = sin(ang),C = cos(ang);
          double eR = ran.Gaus(0,RERR)*sgn;
          double shift = aShift[is%5];
//shift=0;//???????????????????
          double SS = sin(ang+shift);
          double CC = cos(ang+shift);
          e[0] = pow(RERR*SS,2);
          e[1] =-pow(RERR   ,2)*CC*SS;
          e[2] = pow(RERR*CC,2);

          x[0] = 100 + (R)*(S-S0);
          x[1] = 200 - (R)*(C-C0);
          x[0]+= -SS*eR; 
          x[1]+=  CC*eR; 
          helx.Add (x[0],x[1],e);
	}		//end points
	helx.Fit();
//if (helx.GetCase()!=1) continue;
//if (R<0) continue;
	nFit++;
	helx.MakeErrs();
	x[0] = 100 ;
	x[1] = 200 ;
	double s = helx.Path(x);
        assert(s<0);
	assert(fabs(s) < len);
	helx.Move(s);
	double dd[6];
	double dx = x[0]-helx.Pos()[0];
	double dy = x[1]-helx.Pos()[1];
	dd[0] = -dx*S0+dy*C0;
	dd[1] = dd[0]/sqrt(helx.Emx()[0]);
	dd[2] = atan2(helx.Dir()[1],helx.Dir()[0])-ang0;
	if (dd[2]> M_PI) dd[2]-=2*M_PI;
	if (dd[2]<-M_PI) dd[2]+=2*M_PI;
	dd[3] = dd[2]/sqrt(helx.Emx()[2]);
	dd[4] = helx.Rho()-1./R;
	dd[5] = dd[4]/sqrt(helx.Emx()[5]);
	for (int ih=0;ih<6;ih++) { hh[ih]->Fill(dd[ih]);}
    } 		//end sign
  }		//end ang0
  } 		// curv
  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){}; 

}
//______________________________________________________________________________
void TCircleFitter::Test(int iTest) 
{
  enum {nPts=50};
  double e[4],x[3],chi2[2]={0,0};    
  double aShift[5];
  aShift[0]=-acos(0.25);
  aShift[1]=-acos(0.50);
  aShift[2]= 0;
  aShift[3]= acos(0.25);
  aShift[5]= acos(0.50);

  double R = 1000.;
  if (iTest<0) R = -R;
  iTest = abs(iTest);
  double dang = 3./100;// *0.1;
  printf("TCircle::Test R=%g dS=%g\n",R,dang*R);
  double dR[2],qa0;
  dR[0] = 0.016*(R);
  dR[1] = 4*dR[0];
//  dR = 1e-5;
  double dZ[] = {1,3};
//double ang0=0.5;
  double ang0=0.0;
  double S0=sin(ang0),C0=cos(ang0);
  TCircleFitter helx;
  gRandom->SetSeed();
  for (int i=0;i<nPts;i++) {
    double ang = ang0 + dang*i;
    double S = sin(ang),C = cos(ang);
    if (iTest==1) {
    double dRR = dR[i&1];
    double eR = gRandom->Gaus(0,dRR);
    double shift = aShift[i%5];
    double SS = sin(ang+shift);
    double CC = cos(ang+shift);

    double dif = eR*(CC*C+SS*S);
    chi2[0]+=pow(dif,2);
    chi2[1]+=pow(dif/(dRR*(CC*C+SS*S)),2);
    double eZ = gRandom->Gaus(0,dZ[i&1]);
    e[0] = pow(dRR*SS,2);
    e[1] =-pow(dRR   ,2)*CC*SS;
    e[2] = pow(dRR*CC,2);
    e[3] = pow(dZ[i&1],2);
    x[0] = 100 + fabs((R))*(S-S0);
    x[1] = 200 -     ((R))*(C-C0);
    x[0]+= -SS*eR; 
    x[1]+=  CC*eR; 

    x[2] = 300 -       R*ang*0.5 +eZ;
    helx.Add (x[0],x[1],e);
////    helx.Add (x[0],x[1],0);
    helx.AddZ(x[2],e[3]);

    } else {
    helx.Add(100. + i*1,200 - i*2);
    helx.AddZ(300 - i*3);
    }
  } 
  chi2[0]/=nPts-3;
  chi2[1]/=nPts-3;

  helx.SetCase(1);
  qa0=helx.Fit();
  helx.MakeErrs();
  x[0]=100;x[1]=0;
  S0 = helx.Path(x);
  helx.Move(S0);
  helx.Print("");
  double myChi2 = helx.EvalChi2();
  printf("Res2Ideal=%g Chi2Ideal=%g evalChi2=%g\n\n",chi2[0],chi2[1],myChi2);

  helx.SetCase(2);
  qa0=helx.Fit();
  helx.MakeErrs();
  S0 = helx.Path(x);
  helx.Move(S0);

  helx.Print("");
  myChi2 = helx.EvalChi2();
  printf("Res2Ideal=%g Chi2Ideal=%g evalChi2=%g\n\n",chi2[0],chi2[1],myChi2);


  helx.Move( 100.);
  helx.Print("");
  helx.Move(-100.);
  helx.Print("");


//  helx.Print("F");

}
//______________________________________________________________________________
void TCircleFitter::TestCorr(int kase) 
{
// 1=fit case    1 alowed
// 2=fit case    2 alowed
// 4=fit +ive curv alowed
// 8=fit -ive curv alowed
//16=fit -ive curv alowed

  if (!(kase&3 ))kase+=1+2;
  if (!(kase&12))kase+=4+8;
  enum {nPts=20};
  double e[4],x[3],ex[3];    
  double aShift[5];
  aShift[0]=-acos(0.25);
  aShift[1]=-acos(0.50);
  aShift[2]= 0;
  aShift[3]= acos(0.25);
  aShift[5]= acos(0.50);
  double RERR = 0.001;
TRandom ran;
static TCanvas* myCanvas=0;
static TH1F *hh[6]={0,0,0,0,0,0};
static const char *hNams[]={"HA","HA-","HC","HC-","AC","AC-",0};
  if(!myCanvas)  myCanvas=new TCanvas("C1","",600,800);
  myCanvas->Clear();
  myCanvas->Divide(1,6);

  for (int i=0;i<6;i++) { 
    delete hh[i]; hh[i]= new TH1F(hNams[i],hNams[i],100,-1,1);
    myCanvas->cd(i+1); hh[i]->Draw();
  }

  int nFit = 0;
  for (int ir = 50; ir <= 1000; ir +=5) 		{//loop over radius
    double aR = ir;
    double len = 100; if (len>aR*3) len = aR*3;
    for (double ang0 = -3; ang0 < 3.1; ang0+=0.05)	{//loop over ang 
      for (int sgn = -1; sgn<=1; sgn+=2)    		{//loop over signes of curv
if ((sgn>0) && !(kase&4)) continue;
if ((sgn<0) && !(kase&8)) continue;
	double R = sgn*aR;
	double dang = len/R/nPts;
	double C0 = cos(ang0);
	double S0 = sin(ang0);
        TCircleFitter helx;
	for (int is=0;is<nPts;is++) {	//loop over points
          double ang = ang0 + dang*is;
          double S = sin(ang),C = cos(ang);
          double eR = ran.Gaus(0,RERR)*sgn;
          double shift = aShift[is%5];
//shift=0;//???????????????????
          double SS = sin(ang+shift);
          double CC = cos(ang+shift);
          e[0] = pow(RERR*SS,2);
          e[1] =-pow(RERR   ,2)*CC*SS;
          e[2] = pow(RERR*CC,2);

          x[0] = 100 + (R)*(S-S0);
          x[1] = 200 - (R)*(C-C0);
          ex[0]= x[0]-SS*eR; 
          ex[1]= x[1]+CC*eR; 
          helx.Add (ex[0],ex[1],e);
	}		//end points
	helx.Fit();
if (!(helx.GetCase()&kase)) continue;
	nFit++;
	helx.MakeErrs();
        int iFix = 0;
        if (kase&16) iFix +=1;
        if (kase&32) iFix +=4;
	if (iFix) {
	  double vals[5],xold[3];
	  TCL::ucopy(x,vals,3);
	  TCL::ucopy(helx.Pos(),xold,3);
	  vals[3]=0;
	  vals[4]=1./R;
          double ss = helx.Path(x);
          helx.Move(ss);
	  helx.FixAt(vals,iFix);
          ss = helx.Path(xold);
          helx.Move(ss);
	}
	x[0] = 100 ;
	x[1] = 200 ;
	double s = helx.Path(x);
        assert(s<0);
	assert(fabs(s) < len);
	helx.Move(s);
	double dd[6],hf[6];
	double dx = helx.Pos()[0]-x[0];
	double dy = helx.Pos()[1]-x[1];
        const double *emx = helx.Emx();
	dd[0] = -dx*S0+dy*C0;
	dd[1] = atan2(helx.Dir()[1],helx.Dir()[0])-ang0;
	if (dd[1]> M_PI) dd[1]-=2*M_PI;
	if (dd[1]<-M_PI) dd[1]+=2*M_PI;
	dd[2] = helx.Rho()-1./R;
        hf[0] = (dd[0]*dd[1])	*1e1/(RERR*RERR);
        hf[1] = (emx[1])	*1e1/(RERR*RERR);
        hf[2] = dd[0]*dd[2]	*1e3/(RERR*RERR);
        hf[3] = (emx[3])	*1e3/(RERR*RERR);
        hf[4] = dd[1]*dd[2]	*1e4/(RERR*RERR);
        hf[5] = (emx[4])	*1e4/(RERR*RERR);

        
	for (int ih=0;ih<6;ih++) { hh[ih]->Fill(hf[ih]);}
    } 		//end sign
  }		//end ang0
  } 		// curv
  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){}; 

}
//______________________________________________________________________________
void TCircle::Show(int nPts,const double *Pts,int pstep) 
{
static TCanvas *myCanvas = 0;
static TGraph  *ptGraph  = 0;
static TGraph  *ciGraph  = 0;
  double x[100],y[100];
  if (nPts>100) nPts=100;
  for (int i=0;i<nPts;i++) { x[i]=Pts[i*pstep+0];  y[i]=Pts[i*pstep+1]; }


  if(!myCanvas) myCanvas = new TCanvas("C1","",600,800);
  myCanvas->Clear();
  delete ptGraph; delete ciGraph;

  ptGraph  = new TGraph(nPts  , x, y);
  ptGraph->SetMarkerColor(kRed);
  ptGraph->Draw("A*");

  TCircle tc(*this);
  double xy[2];
  xy[0] = x[0];
  xy[1] = y[0];
  double s = tc.Path(xy);
  tc.Move(s);
  xy[0] = x[nPts-1];
  xy[1] = y[nPts-1];
  s = tc.Path(xy);
  if (s<0) { tc.Backward(); s = tc.Path(xy);}
  double ds = s/99;
  for (int i=0;i<100;i++) {x[i]=tc.Pos()[0];y[i]=tc.Pos()[1];tc.Move(ds);}
  
  ciGraph  = new TGraph(100  , x, y);
  ciGraph->Draw("Same CP");
  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){}; 

}
//______________________________________________________________________________
THelixFitter::THelixFitter():fPoli1Fitter(1)
{
  Clear();
}
//______________________________________________________________________________
void THelixFitter::Clear(const char*)
{
  fCircleFitter.Clear();
  fPoli1Fitter.Clear();
  fPoli1Fitter.SetCoefs(1);
  fChi2=0;
}
//______________________________________________________________________________
void THelixFitter::Print(const char*) const
{
  THelixTrack::Print();
  fCircleFitter.Print();
  fPoli1Fitter.Print();
}
//______________________________________________________________________________
void THelixFitter::Add (double x,double y,double z) 
{
  fCircleFitter.Add(x,y,z);
}  
//______________________________________________________________________________
void THelixFitter::AddErr(const double *err2xy,double err2z) 
{  
  fCircleFitter.AddErr(err2xy,err2z);
}  
//______________________________________________________________________________
void  THelixFitter::Skip(int idx) 
{
   fCircleFitter.Skip(idx);
   fPoli1Fitter.Skip(idx);
}
//______________________________________________________________________________
double THelixFitter::Fit()
{
  TCircleFitterAux* myAux= GetAux(0);
  int nDat = Size();
  double Xi2xy = fCircleFitter.Fit();
  int ndfXY = fCircleFitter.Ndf();
  TCircle circ(fCircleFitter);
  const double *xy=0;
  xy = &(myAux[nDat-1].x);
  double z1 = xy[2];
  double s1 = circ.Path(xy);

  xy = &(myAux[0].x);
  double z0 = xy[2];
  double s  = circ.Path(xy);
//	estimation of tan(dip) to correct z errs
  double tanDip = (z1-z0)/(s1-s);

  circ.Move(s);
//  set lengths
  const double *dc = circ.Dir();
  for (int iDat=0;iDat<nDat;iDat++) {
    TCircleFitterAux* aux = myAux+iDat;
    xy = &(aux->x);
    double ds = circ.Path(xy);
    circ.Move(ds); s+=ds;
//		correct errors
    double corErr = tanDip*tanDip*
                   (dc[0]*dc[0]*aux->exy[0]
                   +dc[1]*dc[1]*aux->exy[2]
                   +dc[0]*dc[1]*aux->exy[1]*2);
    fPoli1Fitter.Add(s,aux->z,aux->ezz+corErr);
  }
  double Xi2z = fPoli1Fitter.Fit();
//	Now set THelixTrack
  int ndfSz = fPoli1Fitter.Ndf();
  Update(1);
  int ndf = ndfSz+ndfXY;
  fChi2 = Xi2xy*ndfXY+Xi2z*ndfSz;
  if (ndf) fChi2/=ndf;
  return fChi2;
   
}  
//_____________________________________________________________________________
void THelixFitter::FixAt(const double val[5],int flag) 
{
  double xx[3],s;
  memcpy(xx,fX,sizeof(xx));
  int move = (flag&1); 
  if (move) {
    s = fCircleFitter.Path(val);
    fCircleFitter.Move(s);
    fPoli1Fitter.Move(s);
  }
  fCircleFitter.FixAt(val,flag);
  if (flag&1)  fPoli1Fitter.FixY0(val[2]);
//  Update(1+2);
  if (move) {
    s = fCircleFitter.Path(xx);
    fCircleFitter.Move(s);
    fPoli1Fitter.Move(s);
  }
  Update(1+2);
  double Xi2c = fCircleFitter.EvalChi2();
  double Xi2z = fPoli1Fitter.EvalChi2();
  int ndfc = fCircleFitter.Ndf();
  int ndfz = fPoli1Fitter.Ndf();
  
  int ndf = ndfc+ndfz;
  fChi2 = Xi2c*ndfc+Xi2z*ndfz;
  if (ndf) fChi2/=ndf;
}
//______________________________________________________________________________
void THelixFitter::Update(int kase)
{
  if(kase&1) {
    const double *pol = fPoli1Fitter.Coe();
    fCosL = 1./sqrt(pol[1]*pol[1]+1);
    double *haslet = fCircleFitter.Pos();
    fX[0] = haslet[0];
    fX[1] = haslet[1];
    fX[2] = pol[0];
    fP[0] = haslet[2]*fCosL;
    fP[1] = haslet[3]*fCosL;
    fP[2] = pol[1]*fCosL;
    fRho  = haslet[4];
  }
  if(kase&2) {
    memcpy(fEmxXY,fCircleFitter.Emx(),sizeof(fEmxXY));
    memcpy(fEmxSZ, fPoli1Fitter.Emx(),sizeof(fEmxSZ));
  }
}
//______________________________________________________________________________
void THelixFitter::MakeErrs()
{
  fCircleFitter.MakeErrs();
  fPoli1Fitter.MakeErrs();
  Update(2);
}
//______________________________________________________________________________
double THelixFitter::EvalChi2() 
{
  double Xi2c = fCircleFitter.EvalChi2();
  double Xi2z = fPoli1Fitter.EvalChi2();
  fChi2 = Xi2c*fCircleFitter.Ndf()+Xi2z*fPoli1Fitter.Ndf();
  fChi2/=(fCircleFitter.Ndf()+fPoli1Fitter.Ndf()+1e-3);
  return fChi2;
}
//______________________________________________________________________________
void THelixFitter::Test(int kase)
{
// 1=fit case    1 alowed
// 2=fit case    2 alowed
// 4=fit +ive curv alowed
// 8=fit -ive curv alowed
// 16=fix last point 
// 32=fix curvature 
// 64=fix angle (not implemented in test) 
//128=show each track 
  if (!(kase&3 ))kase+=1+2;
  if (!(kase&12))kase+=4+8;
  enum {nPts=20,nHH=7};
  double e[4],x[3],xe[3];    
  double aShift[5];
  aShift[0]=-acos(0.25);
  aShift[1]=-acos(0.50);
  aShift[2]= 0;
  aShift[3]= acos(0.25);
  aShift[5]= acos(0.50);
  double RERR = 0.1;
  double ZERR = 0.1;
TRandom ran;
static TCanvas* myCanvas[9]={0,0,0,0,0,0,0,0,0};
static TH1F *hh[nHH]={0,0,0,0,0,0,0};
static const char *hNams[]={"pH","pA","pC","pZ","pD","Xi2","Xi2E",0};
  if(!myCanvas[0])  myCanvas[0]=new TCanvas("C1","",600,800);
  myCanvas[0]->Clear();
  myCanvas[0]->Divide(1,nHH);

  for (int i=0;i<nHH;i++) { 
    double low = (i>=5)? 0:-5;
    double upp = 5;
    delete hh[i]; hh[i]= new TH1F(hNams[i],hNams[i],100,low,upp);
    myCanvas[0]->cd(i+1); hh[i]->Draw();
  }

//		Init Second histo group 
static TH1F *h2h[4]={0,0,0,0};
static const char *h2Nams[]={"targYY","targZZ","targYZ","calcYZ",0};
  int n2h=4;
  if(!myCanvas[1])  myCanvas[1]=new TCanvas("C2","",600,800);
  myCanvas[1]->Clear();
  myCanvas[1]->Divide(1,n2h);
  for (int i=0;i<n2h;i++) { 
    delete h2h[i]; h2h[i]= new TH1F(h2Nams[i],h2Nams[i],100,-5,5);
    myCanvas[1]->cd(i+1); h2h[i]->Draw();
  }
//		End Init Second histo group 

//		Init 3rd histo group 
static TH1F *h3h[4]={0,0,0,0};
static const char *h3Nams[]={"dcaXY","dcaXYNor","dcaZ","dcaZNor",0};
  int n3h=4;
  if(!myCanvas[2])  myCanvas[2]=new TCanvas("C3","",600,800);
  myCanvas[2]->Clear();
  myCanvas[2]->Divide(1,n3h);
  for (int i=0;i<n3h;i++) { 
    delete h3h[i]; h3h[i]= new TH1F(h3Nams[i],h3Nams[i],100,-5,5);
    myCanvas[2]->cd(i+1); h3h[i]->Draw();
  }
//		End Init 3rd histo group 


  double spotSurf[4]= {-100,1,0,0};
  double spotAxis[3][3]= {{0,1,0},{0,0,1},{1,0,0}};


  int nFit = 0;
for (double idip=-1;idip<=1;idip+=0.2){
  double dip = idip;
//  dip = 0;
  double cosDip = cos(dip);
  double sinDip = sin(dip);
  double tanDip = tan(dip); if(tanDip){};
  for (int ir = 50; ir <= 1000; ir +=20) 		{//loop over radius
    double aR = ir;
    double len = 100; if (len>aR*3) len = aR*3;
    for (double ang00 = -3; ang00 < 3.1; ang00+=0.2)	{//loop over ang 
      double ang0 = ang00;
//      ang0 = 0;
      for (int sgn = -1; sgn<=1; sgn+=2)    		{//loop over signes of curv
if(sgn>0 && !(kase&4)) continue; 
if(sgn<0 && !(kase&8)) continue; 

	double R = sgn*aR;
	double dang = len/R/nPts;
	double C0 = cos(ang0);
	double S0 = sin(ang0);
        THelixFitter helx;

        double trakPars[7]={100,200,300,C0*cosDip,S0*cosDip,sinDip,1/R};
        THelixTrack trak(trakPars+0,trakPars+3,trakPars[6]);

	for (int is=0;is<nPts;is++) {	//loop over points
          double ang = ang0 + dang*is;
          double S = sin(ang),C = cos(ang);
          double eR = ran.Gaus(0,RERR)*sgn;
          double eZ = ran.Gaus(0,ZERR);
          double shift = aShift[is%5];
//shift=0;//???????????????????
          double SS = sin(ang+shift);
          double CC = cos(ang+shift);
          e[0] = pow(RERR*SS,2);
          e[1] =-pow(RERR   ,2)*CC*SS;
          e[2] = pow(RERR*CC,2);
          e[3] = pow(ZERR,2);
          x[0] = 100 + (R)*(S-S0);
          x[1] = 200 - (R)*(C-C0);
          double len = (R)*(ang-ang0);
          x[2] = 300 + len*tan(dip);
          xe[0]= x[0]-SS*eR; 
          xe[1]= x[1]+CC*eR; 
          xe[2]= x[2]+eZ; 
          helx.Add (xe[0],xe[1],xe[2]);
          helx.AddErr(e,e[3]);
	}		//end points
	double Xi2 =helx.Fit();
if(!(kase&helx.GetCase())) continue; 

	helx.MakeErrs();
	nFit++;
        int iFix = 0;
if (kase&16) iFix +=1;
if (kase&32) iFix +=4;
	if (iFix) {
	  double vals[5];
	  TCL::ucopy(x,vals,3);
	  vals[3]=0;
	  vals[4]=1./R;
	  helx.FixAt(vals,iFix);
	}
if (kase&128) helx.Show();
	double Xi2E =helx.EvalChi2();

        trak.Move(0.3*len/cosDip);
	memcpy(x,trak.Pos(),sizeof(x));
        ang0 = atan2(trak.Dir()[1],trak.Dir()[0]);
//	double s = helx.Path(x[0],x[1]);
	double s = helx.Path(x);
//      assert(s<0);
//	assert(fabs(s) < len*1.1);

        double eXY[6],eSZ[3];
        double pos[3],dir[3],rho;
	helx.Move(s);
        helx.GetEmx(eXY,eSZ);
        helx.Get   (pos,dir,rho);
	double psi = atan2(dir[1],dir[0]);
	double sinPsi = sin(psi);
	double cosPsi = cos(psi);
	double tanPsi = sinPsi/cosPsi; if(tanPsi){};
	double dd[10],hf[10];
	double dx = x[0]-pos[0];
	double dy = x[1]-pos[1];
	dd[0] = -dx*sinPsi+dy*cosPsi;
	hf[0] = dd[0]/sqrt(eXY[0]+1e-20);
	dd[2] = psi-ang0;
	if (dd[2]> M_PI) dd[2]-=2*M_PI;
	if (dd[2]<-M_PI) dd[2]+=2*M_PI;
	hf[1] = dd[2]/sqrt(eXY[2]+1e-20);
	dd[4] = rho-1./R;
	hf[2] = dd[4]/sqrt(eXY[5]+1e-20);
        dd[6] = (helx.Pos()[2]-x[2])/pow(helx.GetCos(),2);
        hf[3] = dd[6]/sqrt(eSZ[0]+1e-20);
        dd[8] = asin(dir[2])-dip;
	if (dd[8]> M_PI) dd[8]-=2*M_PI;
	if (dd[8]<-M_PI) dd[8]+=2*M_PI;
        hf[4] = dd[8]/(sqrt(eSZ[2])*(1-dir[2]*dir[2]));
        hf[5] = Xi2;
        hf[6] = Xi2E;
	for (int ih=0;ih<nHH;ih++) { hh[ih]->Fill(hf[ih]);}

//		Fill 2nd histo group
        double xIde[3],pIde[3],xFit[3],pFit[3],eSpot[3],hfil,sIde,sFit;
//        if(fabs(dip)>1) continue;
        int closePoint=0;
        spotSurf[0] = -110;
  
        { spotSurf[0] = -x[0]; closePoint=2006;}
        sIde = trak.Step(200.,spotSurf,4, xIde,pIde,closePoint);
 
        if (fabs(spotSurf[0]+TCL::vdot(xIde,spotSurf+1,3))>0.001) {
          printf("***Wrong point found**\n");
          trak.Print();
          assert(0);
	}
        sFit = helx.Step(200.,spotSurf,4, xFit,pFit,closePoint);
        if (sFit>=1000 ) continue;
        if (fabs(pIde[0]-pFit[0])>0.1) continue;
        helx.Move(sFit);
        helx.GetEmx(eXY,eSZ);
        helx.GetSpot(spotAxis,eSpot);
        hfil = (xFit[1]-xIde[1]); hfil/= sqrt(eSpot[0]); 
        h2h[0]->Fill(hfil);
        hfil = (xFit[2]-xIde[2]); hfil/= sqrt(eSpot[2]); 
        h2h[1]->Fill(hfil);
        hfil = (xFit[1]-xIde[1])*(xFit[2]-xIde[2]);
        h2h[2]->Fill(hfil*100);
        h2h[3]->Fill(hfil/sqrt(eSpot[0]*eSpot[2]));
//        h2h[3]->Fill(eSpot[1]*100);
//		End 2nd histo group

//		Fill 3rd histo group
        double dcaXY,dcaZ,dcaEmx[3];
        double sDca = helx.Dca(trakPars,dcaXY,dcaZ,dcaEmx);
        if (fabs(sDca)<1000) {
          h3h[0]->Fill(dcaXY);
          h3h[1]->Fill(dcaXY/sqrt(dcaEmx[0]));
          h3h[2]->Fill(dcaZ );
          h3h[3]->Fill(dcaZ /sqrt(dcaEmx[2]));
        }
//		End 3rd histo group

    } 		//end sign
  }		//end ang0
  } 		// curv
}		// dip
  for (int ih=0;myCanvas[ih];ih++) {
    myCanvas[ih]->Modified();
    myCanvas[ih]->Update();
  }
  while(!gSystem->ProcessEvents()){}; 
}
//______________________________________________________________________________
void THelixFitter::Show() const
{
static TCanvas *myCanvas = 0;
static TGraph  *ptGraph[2]  = {0,0};
static TGraph  *ciGraph[2]  = {0,0};
  double x[100],y[100],z[100],l[100];
  int nPts = Size();
  if (nPts>100) nPts=100;
  TCircleFitterAux* aux=GetAux(0);
  l[0]=0;
  for (int i=0;i<nPts;i++) {
    x[i]=aux[i].x;  y[i]=aux[i].y; z[i]=aux[i].z;
    if(!i) continue; 
    double dl = sqrt(pow(x[i]-x[i-1],2)+pow(y[i]-y[i-1],2));
    if (fabs(fRho)>1e-6) dl = fabs(2./fRho*asin(0.5*dl*fRho));
    l[i]=l[i-1]+dl;
  }


  if(!myCanvas) myCanvas = new TCanvas("C1","",600,800);
  myCanvas->Clear();
  myCanvas->Divide(1,2);

  delete ptGraph[0]; delete ciGraph[0];
  ptGraph[0]  = new TGraph(nPts  , x, y);
  ptGraph[0]->SetMarkerColor(kRed);
  myCanvas->cd(1); ptGraph[0]->Draw("A*");
  delete ptGraph[1]; delete ciGraph[1];
  ptGraph[1]  = new TGraph(nPts  , l, z);
  ptGraph[1]->SetMarkerColor(kRed);
  myCanvas->cd(2); ptGraph[1]->Draw("A*");

  THelixTrack tc(*this);
  double xyz[3];
  xyz[0] = x[0];xyz[1] = y[0];xyz[2] = z[0];
  double s = tc.Path(xyz);
  tc.Move(s);
  xyz[0] = x[nPts-1];xyz[1] = y[nPts-1];xyz[2] = z[nPts-1];
  s = tc.Path(xyz);
  if (s<0) { tc.Backward(); s = tc.Path(xyz);}
  double ds = s/99;
  s = 0;
  for (int i=0;i<100;i++) {
    x[i]=tc.Pos()[0];
    y[i]=tc.Pos()[1];
    z[i]=tc.Pos()[2];
    l[i]=s*fCosL;
    s+=ds;tc.Move(ds);
  }
  
  ciGraph[0]  = new TGraph(100  , x, y);
  myCanvas->cd(1); ciGraph[0]->Draw("Same CP");
  ciGraph[1]  = new TGraph(100  , l, z);
  myCanvas->cd(2); ciGraph[1]->Draw("Same CP");

  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){}; 

}
 
