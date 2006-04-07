#include <stdlib.h>
#include <math.h>
#include "TError.h"
#include "TArrayD.h"
#include "TCL.h"
#include "THelixTrack.h"
#include <complex>
// Complex numbers
typedef std::complex<double > Complex;
const Complex Im(0,1);
inline static double dot(const Complex &a,const Complex &b)
{return a.real()*b.real()+a.imag()*b.imag();}

const double Zero = 1.e-6;
static Complex sgCX1,sgCX2,sgCD1,sgCD2,sgImTet,sgCOne,sgCf1;
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
		        ,const double *hxyz,double drho)
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
  Set(xyz,dir,rho,hxyz,drho);
}
//_____________________________________________________________________________
THelixTrack::THelixTrack(const THelixTrack &from)
{
  int n = (char*)&fKind - (char*)fX + sizeof(fKind);
  memcpy(fX,from.fX,n);
}
//_____________________________________________________________________________
THelixTrack::THelixTrack()
{
  int n = (char*)&fKind - (char*)fX + sizeof(fKind);
  memset(fX,0,n);
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
		     ,const double *hxyz,double drho)
{
  fX[0] = xyz[0]; fX[1] = xyz[1]; fX[2] = xyz[2];
  fP[0] = dir[0]; fP[1] = dir[1]; fP[2] = dir[2];
  fKind=0;
  if (hxyz) {
    if (fH!=hxyz) memcpy(fH, hxyz,sizeof(fH));
    if (!fH[0] && !fH[1]) fKind=1;
  } else {
    fH[0] = 0.;     fH[1] =0.;      fH[2] = 1.; fKind=1;}
  fRho = rho; fDRho=drho;
  Build();
}
//_____________________________________________________________________________
void THelixTrack::Set(double rho,double drho)
{
   fRho = rho; fDRho=drho; fMax = 1./(fabs(fRho*fCosL)+1.e-10);
}
//_____________________________________________________________________________
void THelixTrack::Backward()
{

  double x[3],d[3],h[3],rho;
  for (int i=0;i<3;i++) { x[i]=fX[i]; d[i]=-fP[i];h[i]=fH[i];}
  rho = -fRho;
  Set(x,d,rho,h,fDRho); 
}

//_____________________________________________________________________________
void THelixTrack::Build()
{

  double tmp;
  tmp = fH[0]*fH[0]+ fH[1]*fH[1]+ fH[2]*fH[2];
  if (fabs(tmp-1.) > 1.e-12) {
    tmp = ::sqrt(tmp); fH[0] /=tmp; fH[1] /=tmp; fH[2] /=tmp; }
    
  tmp = fP[0]*fP[0]+ fP[1]*fP[1]+ fP[2]*fP[2];
  if (fabs(tmp-1.) > 1.e-12) {
    tmp = ::sqrt(tmp); fP[0] /=tmp; fP[1] /=tmp; fP[2] /=tmp; }
    
  fHXP[0] = fH[1]*fP[2] - fH[2]*fP[1];
  fHXP[1] = fH[2]*fP[0] - fH[0]*fP[2];
  fHXP[2] = fH[0]*fP[1] - fH[1]*fP[0];

  fHP   =        fH[0]*fP[0]  +  fH[1]*fP[1]  +  fH[2]*fP[2];
  fCosL = ::sqrt(fHXP[0]*fHXP[0]+fHXP[1]*fHXP[1]+fHXP[2]*fHXP[2]);
  for (int i=0;i<3;i++) {fPxy[i] = (fP[i]-fH[i]*fHP)/fCosL;}


  fMax = 1./(fabs(fRho*fCosL)+1.e-10);
}

//_____________________________________________________________________________
double THelixTrack::Steb(double step, double *xyz, double *dir) const
{
//_______________________________________________________________________
//	
//  X[i] = fX[i] + (sin(tet)/tet)       *fP  [i]*step
//               + (1-cos(tet))/tet)    *fHXP[i]*step
//               + (1-sin(tet)/tet)*fHP *fH  [i]*step
//  .or.
//
//  X[i] = fX[i] + (sin(tet)     /(fRho*fCosL)     *fP  [i]
//               + (1-cos(tet))  /(fRho*fCosL)     *fHXP[i]
//               + (step-sin(tet)/(fRho*fCosL)*fHP *fH  [i]
//
//
//  P[i] =  cos(tet)        *fP  [i]
//       +  sin(tet)        *fHXP[i]
//       + (1-cos(tet))*fHP *fH  [i]
//
//_______________________________________________________________________

  double tet,sint,sintt,tsint,cos1t,tmp,f1,f2,f3,f4,f5,f6;
  int i;
  tet = fRho * step * fCosL;

  if (fabs(tet) > 0.01)	{
    sint  = sin(tet);		
    sintt = (sint/tet);		//sintt = sin(tet)/tet
    tsint = (tet-sint)/tet;
    tmp   = sin(0.5*tet);
    cos1t = 2.*(tmp*tmp)/tet;	//cos1t  = (1-cos(tet))/tet
  } else {
    tsint = tet*tet/6.;		//tsint  = 1 - sin(tet)/tet
    sintt = (1. - tsint);
    sint  = tet*sintt;
    cos1t = 0.5*tet;
  }

  f1 = step * sintt;		//sin(tet)    /(fRho*fCosL)
  f2 = step * cos1t;		//(1-cos(tet))/(fRho*fCosL)
  f3 = step * tsint * fHP;      //fHp*step - fHp*sin(tet)/(fRho*fCosL)
  f4 = -tet*cos1t;
  f5 = sint;
  f6 = tet * cos1t * fHP;

  if (xyz) {
    for (i=0;i<3;i++){
      xyz[i] = fX[i] + (f1*fP[i] + f2*fHXP[i] + f3*fH[i]);}}

  if (dir) {
    for (i=0;i<3;i++){
      dir[i] = fP[i] + (f4*fP[i] + f5*fHXP[i] + f6*fH[i]);}}

  return step;
}
//_____________________________________________________________________________
double THelixTrack::Move(double step) 
{
  double xyz[3],dir[3],rho;
  Step(step,xyz,dir,rho);
  Set(xyz,dir,rho,fH,fDRho);
  return step;
}

//_____________________________________________________________________________
double THelixTrack::Step(double stmax,const  double *surf, int nsurf,
                         double *xyz, double *dir) const
{
  int i;
  double s[10]={0,0,0,0,0,0,0,0,0,0},tmp=0;
  memcpy(s,surf,nsurf*sizeof(surf[0]));
  
  fKind&=(-3);
  for(i=1;i<nsurf;i++) if (fabs(s[i])>tmp) tmp = fabs(s[i]);
  if(fabs(tmp-1.)>0.1) {for(i=0;i<nsurf;i++) s[i]/=tmp;}
  if (fKind&1 && !s[3] && !s[6] && !s[8] && !s[9] && fabs(s[4]-s[5])<1.e-12) 
    fKind |= 2;
  if ((fKind&3)!=3)  return Step(0.0,stmax,s,nsurf,xyz,dir);
  else               return StepHZ(s,nsurf,xyz,dir);
}


//_____________________________________________________________________________
double THelixTrack::Step(double stmin,double stmax, const double *s, int nsurf,
                         double *xyz, double *dir) const
{
  int ix,jx,nx,ip,jp;
  double poly[4][3],tri[3],sol[2],cos1t,f1,f2,step,ss;
  const double *sp[4][4] = {{s+0,s+1,s+2,s+3}, {s+1,s+4,s+7,s+9}, 
                            {s+2,s+7,s+5,s+8}, {s+3,s+9,s+8,s+6}}; 

  THelixTrack th(fX,fP,fRho,fH);
  cos1t = 0.5*fRho*fCosL;
  double totStep=0;
  while (2005) {
    poly[0][0]=1.;poly[0][1]=0.;poly[0][2]=0.;
    tri[0]=tri[1]=tri[2]=0;
    for(ix=1;ix<4;ix++) {
      poly[ix][0] =th.fX  [ix-1]; 
      poly[ix][1] =th.fP  [ix-1]; 
      poly[ix][2] =th.fHXP[ix-1]*cos1t;
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

    if (nsol) step = sol[0];
    if (step < stmin && nsol > 1) step = sol[1];
    if (step < stmin)	{nsol = 0; step = stmax;}
    if (step > stmax) 	{nsol = 0; step = stmax ; }   
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

    stmax -=step; stmin -=step; if (stmin < -fMax) stmin = -fMax; 
    th.Move(step);
    totStep+=step;
  }

}

//_____________________________________________________________________________
double THelixTrack::StepHZ(const double *su, int nsurf, 
                           double *xyz, double *dir) const
{
   double tri[3] = {0,0,0};
   double f0,fc,fs,R,tet,tet0,tet1,tet2,costet,su45=0,fcs;
   

   R = 1./fRho/fCosL;
//		X
   f0 = fX[0] + fHXP[0]*R;
   fc = -fHXP[0]*R;
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
   f0 = fX[1] + fHXP[1]*R;
   fc = -fHXP[1]*R;
   fs = fP[1]*R;

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
   if (tet1 < 0.    ) tet1 += 2*M_PI;
   if (tet2 < 0.    ) tet2 += 2*M_PI;
   if (tet1 > tet2  ) { tet0 = tet1; tet1 = tet2; tet2 = tet0;}
   if (fRho < 0.)   tet1 = 2*M_PI - tet2;
   return Step(fabs(tet1*R),xyz,dir);

}

//_____________________________________________________________________________
double THelixTrack::Steb(const double *point,double *xyz, double *dir) const
{
    double pnt[3],pnd[3],step[3],tg;
    int i;    
    for (i=0;i<3;i++) pnt[i] = point[i] - fX[i];

    pnd[0] = (pnt[0]*fPxy[0]+pnt[1]*fPxy[1]+pnt[2]*fPxy[2]);
    pnd[1] = (pnt[0]*fHXP[0]+pnt[1]*fHXP[1]+pnt[2]*fHXP[2])/fCosL;
    pnd[2] = (pnt[0]*fH  [0]+pnt[1]*fH  [1]+pnt[2]*fH  [2]);

//		Z estimated step 

    int zStep=0;
    step[1] = 0;
    if (fabs(fHP) > 0.01){ //Z approximation
      zStep = 1;
      step[1] = pnd[2]/fHP;
    }
//angle approximation
//		R estimated step
    {
      double xRho = pnd[0]*fRho;
      double yRho = 1. - pnd[1]*fRho;
      if (fabs(yRho) > 0.1 && fabs((tg = xRho/yRho))<0.2) {
        tg *=tg; step[2] = pnd[0]/yRho*(1.+tg*(-1./3 + 1./5*tg));
      } else {
        step[2] = atan2(xRho,yRho)/fRho;
      }
      step[2] /= fCosL;
    }
    if (zStep && fabs(step[1]-step[2]) > GetPeriod()) {
      int nperd = (int)((step[1]-step[2])/GetPeriod());
      step[2] += nperd*GetPeriod();
    }
    step[0] = step[2];

    double ds = step[1]-step[2];
    if (zStep && fabs(ds)>1.e-5) {
      double dz = ds*fHP;
      step[0] += dz*dz/ds;
    }


    double xnear[6],ss=0;  double* pnear=xnear+3;
    memset(xnear,0,sizeof(xnear));
    pnear[0]= fCosL; pnear[2]=fHP;
//		iterations
    double dstep = 1.e+10,dztep;
    double lMax = step[0]+0.25*GetPeriod();
    double lMin = step[0]-0.25*GetPeriod();
    if (zStep) {
      lMax = (step[1]>step[2])? step[1]:step[2];
      lMin = (step[1]>step[2])? step[2]:step[1];}
    int iter=40,icut=1;
    THelixTrack local(xnear,pnear,fRho);
    local.Move(step[0]);
    lMax-=step[0];lMin-=step[0];
    for (; iter; iter--)
    { 
      double diff = (icut)? lMax-lMin: fabs(dstep);
      if (diff < 0.1) {
        if (diff < 1.e-5) 	break;
        double tmp = 0;
        for (i=0;i<3;i++) {tmp += fabs(pnd[i]-xnear[i]);}
        if (diff < tmp*1.e-4) 	break;
        if (tmp < 1.e-5) 	break;
      }
      
      local.Step(ss,xnear,pnear);
      dstep = 0; icut = 0;
      for (i=0;i<3;i++) {dstep += pnear[i]*(pnd[i]-xnear[i]);}
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
    fDCA[0] = ((point[0]-xnear[0])*(-pnear[1]) +(point[1]-xnear[1])*(pnear[0]))/fCosL;
    if (fRho<0) fDCA[0] = - fDCA[0];
    fDCA[1] = point[2]-xnear[2];
    
  return (xyz) ? Step(step[0],xyz,dir) : step[0];
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
    if (fabs(fHP) > 0.01){ //Z approximation
      zStep = 1;
      step[1] = (point[2]-fX[2])/fHP;
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
      double dz = ds*fHP;
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
//_____________________________________________________________________________
void THelixTrack::Streamer(TBuffer &){}
//_____________________________________________________________________________
void THelixTrack::Print(Option_t *) const
{
  printf("\n THelixTrack::this = %p\n",(void*)this);
  printf(" THelixTrack::fX[3] = { %f , %f ,%f }\n",fX[0],fX[1],fX[2]);
  printf(" THelixTrack::fP[3] = { %f , %f ,%f }\n",fP[0],fP[1],fP[2]);
  printf(" THelixTrack::fH[3] = { %f , %f ,%f }\n",fH[0],fH[1],fH[2]);
  printf(" THelixTrack::fRho  =   %f \n\n",fRho);

  printf("double xyz[3] = {%g,%g,%g};\n" ,fX[0],fX[1],fX[2]); 
  printf("double dir[3] = {%g,%g,%g};\n" ,fP[0],fP[1],fP[2]); 
  printf("double hhh[3] = {%g,%g,%g};\n" ,fH[0],fH[1],fH[2]); 
  printf("double Rho = %g;\n" ,fRho); 
  printf("THelixTrack *ht = new THelixTrack(xyz,dir,Rho,hhh);\n");
  
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
double THelixTrack::Step(double step, double *xyz, double *dir,double &rho) const
{
   Step(step,xyz,dir);
   rho = fRho +(step*fCosL)*fDRho;
   return step;
}
//_____________________________________________________________________________
double THelixTrack::Step(double step, double *xyz, double *dir) const
{
  if (!step) {
    if (xyz) memcpy(xyz,fX,sizeof(fX));
    if (dir) memcpy(dir,fP,sizeof(fP));
    return 0.;
  }

  double ztep = step*fCosL;
  double teta = ztep*(fRho+0.5*ztep*fDRho);

  Complex CX(fX[0]  ,fX[1]  ),CXn;
  Complex CP(fP[0],fP[1]),CPn;
  CP /=std::abs(CP);
  Complex ImTet(0,teta);
  Complex ImRho(0,fRho);
  if (fabs(ImTet.imag()) > 0.01)	{
//    Complex Cf1 = std::exp(ImTet)-1.;
    Complex hlf = std::exp(0.5*ImTet);
    Complex Cf1 = 2.*Im*hlf*hlf.imag();
    CPn = CP + CP*Cf1;
    CXn = CX + CP*Cf1/(ImRho);
    if (fDRho) {//
      CXn += CP*(Cf1+1.)/(ImRho)*MyFactor(fRho,fDRho,ztep);
    }

  } else { 
    Complex COne = (1.+ImTet*(0.5 +ImTet*(1./6+ImTet*(1./24+ImTet/120.))));
    Complex Cf1 = ImTet*COne;
    CPn = CP + CP*Cf1; 
    CXn = CX + CP*ztep*COne;
  }
  if (xyz) {
    xyz[0] = CXn.real();
    xyz[1] = CXn.imag();
    xyz[2] = fX[2]+fP[2]*step;
  } 
  if (dir) {
    dir[0] = CPn.real()*fCosL;
    dir[1] = CPn.imag()*fCosL;
    dir[2] = fP[2];
  }

  return step;
}
//_____________________________________________________________________________
double THelixTrack::Steb(double stmin,double stmax, const double *s, int nsurf,
                         double *xyz, double *dir) const
{
  int ix,jx,nx,ip,jp;
  double poly[3][4],tri[3]={0,0,0},sol[2],f1,f2,step,ss;
  static const int idx[4][4] = {{0,1,2,3}, {1,4,7,9},{2,7,5,8}, {3,9,8,6}}; 

  
  poly[0][0]=1.;poly[1][0]=0.;poly[2][0]=0.;
  memcpy(poly[0]+1,fX,sizeof(fX));
  memcpy(poly[1]+1,fP,sizeof(fP));
  Complex CP(fP[0],fP[1]); CP /=fCosL;
  Complex C2 = 0.5*Im*fRho;
  if (fDRho) C2 += 0.5*fDRho/fRho;
  C2 *=CP*fCosL*fCosL;
  poly[2][1] = C2.real();
  poly[2][2] = C2.imag();
  poly[2][3] = 0;

  nx = (nsurf<=4) ? 1:4;
  for(ix=0;ix<nx;ix++) {
    for(jx=ix;jx<4;jx++) {  
      int ii = idx[ix][jx];
      if (ii>=nsurf || !s[ii]) 		continue;     
      ss = s[ii]; 
      for (ip=0;ip<3;ip++) {
        f1 = poly[ip][ix]; if(!f1) 	continue;
        f1 *=ss;
        for (jp=0;jp+ip<3;jp++) {
          f2 = poly[jp][jx]; if(!f2) 	continue;
          tri[ip+jp] += f1*f2;
  } } } }

  int nsol = SqEqu(tri,sol);
  step = 1.e+12;
  if (nsol<0) 	return step;

  if (nsol) step = sol[0];
  if (step < stmin && nsol > 1) step = sol[1];
  if (step < stmin)	{nsol = 0; step = stmax;}
  if (step > stmax) 	{nsol = 0; step = stmax ; }   
  if (!nsol && fabs(step) < 0.1*fMax) return 1.e+12;
  
  double xx[4],*x=xx+1,d[3];
  Step(step,x,d);
  if (nsol) {//test it
  xx[0]=1;ss = 0;
  for(ix=0;ix<nx;ix++) {
    for(jx=ix;jx<4;jx++) {  
      int ii = idx[ix][jx];
      if (ii>=nsurf || !s[ii]) 	continue;     
      ss += s[ii]*xx[jx]*xx[ix];
  } } 
  if (fabs(ss)<1.e-7) {
    if (xyz) memcpy(xyz,x,sizeof(*xyz)*3);
    if (dir) memcpy(dir,d,sizeof(*dir)*3);
      return step;
  } }

  stmax -=step; stmin -=step; if (stmin < -fMax) stmin = -fMax; 
  THelixTrack th(x,d,fRho,fH,fDRho);
  return th.Steb(stmin,stmax,s,nsurf,xyz,dir)+step;


}
//_____________________________________________________________________________
double THelixTrack::Steb(double stmax,const  double *surf, int nsurf,
                         double *xyz, double *dir) const
{
  int i;
  double s[10]={0,0,0,0,0,0,0,0,0,0},tmp=0;
  memcpy(s,surf,nsurf*sizeof(surf[0]));
  
  for(i=1;i<nsurf;i++) if (fabs(s[i])>tmp) tmp = fabs(s[i]);
  if(fabs(tmp-1.)>0.1) {for(i=0;i<nsurf;i++) s[i]/=tmp;}
  return Steb(0.0,stmax,s,nsurf,xyz,dir);
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
  memset(fErr,0,sizeof(fErr));
}
//______________________________________________________________________________
void TCircle::Clear(const char *)   
{
 memset(fX,0,(char*)(fErr+6)-(char*)fX);
}


//______________________________________________________________________________
void TCircle::SetErr(const double *err)   
{ memcpy(fErr,err,sizeof(fErr));}

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
  if (!fErr[0]) return;
  printf("Errs: %g\n"          ,fErr[0]); 
  printf("    : %g \t%g\n"     ,fErr[1],fErr[2]); 
  printf("    : %g \t%g \t%g\n",fErr[3],fErr[4],fErr[5]); 
}
//______________________________________________________________________________
double TCircle::Path(const double *pnt) const
{
  Complex CX1(pnt[0]-fX[0],pnt[1]-fX[1]);
  Complex CP(fD[0],fD[1]);
  Complex CXP = Im*CX1/CP;
  Complex CXPRho = CXP*fRho;
  double s;
  if (std::abs(CXPRho)>0.01) {
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

  if (fabs(sgImTet.imag()) > 0.01)	{
    sgCf1= std::exp(sgImTet)-1.;		//  exp(I*Rho*L)-1
    sgCOne = sgCf1/sgImTet;			// (exp(I*Rho*L)-1)/(I*Rho*L) ~=1
  } else { 
    sgCOne = (1.+(sgImTet/2.)*(1.+sgImTet/3.*(1.+sgImTet/4.*(1.+sgImTet/5.))));
    sgCf1 = sgImTet*sgCOne;
  }
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
  if (fErr[0]>0) MoveErrs(step);
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
  F[0][1] = sgCOne.real()*l;
  if (fabs(sgImTet.imag()) < 0.01) {
    F[0][2] = 0.5*l*l;
  } else {
    F[0][2] = l*l*((1.-std::conj(sgCOne))/sgImTet).real();
  }
  if (fRho<0) {F[0][1]=-F[0][1]; F[0][2]=-F[0][2];}
  F[1][1] = 1;
  F[1][2] = l;
  F[2][2] = 1;
  memcpy(oErr,fErr,sizeof(fErr));
  TCL::trasat(F[0],oErr,fErr,3,3); 
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
  fRho=-fRho;fD[0]=-fD[0];fD[1]=-fD[1];fErr[3]=-fErr[3];fErr[4]=-fErr[4];
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
  
#include "TRandom.h"
#include "TRandom2.h"
//______________________________________________________________________________
void TCircle::Test(int iTest) 
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
  dR[1] =   dR[0];//?????????????????????????????????????????
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
class TCircleFitterAux
{
  public:
  static int dSize() {return sizeof(TCircleFitterAux)/sizeof(double);}
  public:
  double x,y,z;		//x,y,z of measured point
  double exy[3];	//err matrix(xx,xy,yy) of x,y
  double ezz;		//error of z
  double wt;		//calculated weight

};
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
void  TCircleFitter::Add(double x,double y,const double *errs) 
{
  fN++;
  int n = fN*TCircleFitterAux::dSize();
  if (fArr.GetSize()<n) {fArr.Set(n*2);fAux=0;}
  if (!fAux) fAux = GetAux(0);
  TCircleFitterAux *aux = fAux+fN-1;
  aux->x = x; aux->y=y; aux->exy[0]=0; aux->exy[2]=0;
  if (errs) AddErr(errs);
}
//______________________________________________________________________________
void  TCircleFitter::AddErr(const double *errs) 
{
  TCircleFitterAux *aux = fAux+fN-1;
  memcpy(aux->exy,errs,sizeof(aux->exy));
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
 * $Id: THelixTrack.cxx,v 1.18 2006/04/07 17:31:42 perev Exp $
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
 * Revision 1.18  2006/04/07 17:31:42  perev
 * TCircle +errs now.
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
    double f, g, h, p, q, t, g0, g02, a, b, c, d, det;
    double xroot, ff, fp;
    double dx, dy, nx,ny, xnom,wt,hord,tmp,radius2,radiuc2;
    fKase = fCase;
    if (fN < 3) return 3e33;
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

      if (!fKase && (fYy < fXx *(0.5)*(0.5)/210)) fKase=1;
      if (fKase==1) {//Try 1st method
        det = 4*(fXx*fRrrr-fXx*fRr*fRr-fXrr*fXrr);
        fCov[0] =  (fRrrr-fRr*fRr)		/det;
        fCov[1] =  (2*fXrr)			/det;
        fCov[2] =  (4*fXx)			/det;
        fCov[3] =  (2*fXrr*fRr)		/det;
        fCov[4] =  (4*fXx*fRr)		/det;
        fCov[5] =  (4*(fXx*fRrrr-fXrr*fXrr))	/det;
        double ra=-4*fXy,rb = 2*fYrr; //rc=0;
        a = fCov[0]*ra+fCov[1]*rb;
        b = fCov[1]*ra+fCov[2]*rb;
        c = fCov[3]*ra+fCov[4]*rb;
        fYd = (fabs(b)>1e-6) ? 1./b : 1e6;
        fXd = a*fYd;

      } else {
	fKase = 2;
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
      }

      fXCenter = fXd*fCos-fYd*fSin + fXgravity;
      fYCenter = fXd*fSin+fYd*fCos + fYgravity;
    }
    
//	Update TCircle
    if (fKase==1) {//Big R approx
      fCorrR = sqrt(1+a*a+c*b );
      fCorrB = sqrt(1+a*a     );
      fRho = b/fCorrR;
      int sgB = (b<0)? -1:1;
      ny = sgB/sqrt(1+a*a);
      nx = a*ny;
      fH = -c*sgB/(fCorrR+fCorrB);
      fChi2XY = (4*a*fXy +4*fYy- 2*b*fYrr)/4;
      fChi2XY /= (fCorrR*fCorrR);
    } else    {//Ososkov
      radiuc2  = fXd*fXd+fYd*fYd;
      radius2  = radiuc2+fG1;
      double radius = ::sqrt(radius2);
      double radiuc = ::sqrt(radiuc2);
      fRho  = 1./radius;
      fH = -fG1/(radius+radiuc);
      nx = fXd/radiuc;
      ny = fYd/radiuc;
      fChi2XY = (fG1-fRr)/2;
    }
    if (fN>3) fChi2XY *= fWtot/(fN-3);
    fA=a;fB=b;fC=c;
    fX[0] = nx*fH; fX[1] = ny*fH;
    if (fRho>0) {fD[0] = ny; fD[1] =-nx;} 
    else        {fD[0] =-ny; fD[1] = nx;}
    Rot(fCos,fSin);
    fX[0] +=  fXgravity;
    fX[1] +=  fYgravity;
    tmp = fD[0]*(aux[0].x-fX[0])+fD[1]*(aux[0].y-fX[1]);
    fBack = 0;
    if (tmp>0) {fBack=1; fD[0]=-fD[0];fD[1]=-fD[1];fRho=-fRho;}

    return fChi2XY;
}
//______________________________________________________________________________
double TCircleFitter::EvalChi2() const
{
  if (!fN) return 0;
  TCircle M = *this;
  double sum = 0,wtot=0,wt;
  TCircleFitterAux *aux = GetAux(0);
  const double *p = M.Pos();
  for (int i=0;i<fN;i++) {
    double s = M.Path(&(aux[i].x));
    M.Move(s);
    wt = aux[i].wt;
    sum+= (pow(p[0]-aux[i].x,2)+pow(p[1]-aux[i].y,2))*wt;
    wtot +=wt;
  }
  if (fN>3) sum /= fN-3;
  return sum;
}
//______________________________________________________________________________
void TCircleFitter::MakeErrs() 
{
   memset(fErr,0,sizeof(fErr));
   double F[3][3]; memset(F[0],0,sizeof(F));
   double myFact = 1.;
   if (fKase==1) { //For BigYC  fit
      for (int i=0;i<6;i++) {fCov[i]*=4;}
      int sgB = (fB<0)? -1:1;
      double corrRB = fCorrR+fCorrB;
      double corrR3 = fCorrR*fCorrR*fCorrR;
//      fH = -c*sgB/(fCorrR+fCorrB);
      
      F[0][0] =      sgB*fA*fC/(corrRB*fCorrB*fCorrR);
      F[0][1] =  0.5*sgB*fC*fC/(corrRB*corrRB*fCorrR);
      F[0][2] =  0.5*sgB*fC*fB/(corrRB*corrRB*fCorrR)
              -      sgB      /(corrRB              );
      F[1][0] =  -1/(fCorrB*fCorrB);
      F[2][0] =  -    fA*fB/(corrR3);
      F[2][1] =  -0.5*fC*fB/(corrR3)+1/fCorrR;
      F[2][2] =  -0.5*fB*fB/(corrR3);
      myFact  = (fCorrR*fCorrR);

   } else     { //For Ososkov/Chernov fit
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
     fCov[1]  = (fXy - Fm*aRho*fXd*fYd)*aRho2;  //  <dF/dA * dF/dB>
     fCov[2]  = (fYy - Fm*aRho*fYd*fYd)*aRho2;	//  <dF/dB * dF/dB>
     double aRho3 = aRho*aRho2;
     fCov[3]  = -0.5*(Fm*fXd    )*aRho3;	//  <dF/dC * dF/dA>
     fCov[4]  = -0.5*(Fm*fYd    )*aRho3;	//  <dF/dC * dF/dB>
     fCov[5]  =  0.25*(1-Fm*aRho)*aRho2;	//  <dF/dC * dF/dC>

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
   }
   TCL::trasat(F[0],fCov,fErr,3,3); 
   TCL::vscale(fErr,myFact/fWtot,fErr,6);
   if (fBack) {/* fErr[1]=-fErr[1];*/ fErr[3]=-fErr[3];fErr[4]=-fErr[4];}
}
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
  if (fChi2XY) printf(" Chi2XY = %g",fChi2XY);
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
#include "TCanvas.h"
#include "TGraph.h"
#include "TSystem.h"
//______________________________________________________________________________
void TCircleFitter::Test() 
{
  enum {nPts=20};
  double e[4],x[3],chi2[2]={0,0};    
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
	dd[1] = dd[0]/sqrt(helx.Err()[0]);
	dd[2] = atan2(helx.Dir()[1],helx.Dir()[0])-ang0;
	if (dd[2]> M_PI) dd[2]-=2*M_PI;
	if (dd[2]<-M_PI) dd[2]+=2*M_PI;
	dd[3] = dd[2]/sqrt(helx.Err()[2]);
	dd[4] = helx.Rho()-1./R;
	dd[5] = dd[4]/sqrt(helx.Err()[5]);
	for (int ih=0;ih<6;ih++) { hh[ih]->Fill(dd[ih]);}
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

