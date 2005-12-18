#include <stdlib.h>
#include <math.h>
#include "TError.h"
#include "TArrayD.h"
#include "THelixTrack.h"
#include <complex>
typedef std::complex<double > Complex;
const Complex Im(0,1);
const double Zero = 1.e-6;


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
}
//______________________________________________________________________________
void TCircle::Nor(double *norVec) const  
{
// direction from center of circle

  norVec[0] =  fD[1];    norVec[1] = -fD[0];
  if (fRho>=0) return;
  norVec[0] = -norVec[0];norVec[1] = -norVec[1];
}
//______________________________________________________________________________
void TCircle::Print(const char*) const
{
  printf("TCircle: x,y=%g %g dir=%g %g curv=%g\n",fX[0],fX[1],fD[0],fD[1],fRho);
}
//______________________________________________________________________________
double TCircle::Path(const double *pnt)
{
  Complex CX1(pnt[0]-fX[0],pnt[1]-fX[1]);
  Complex CP(fD[0],fD[1]);
  Complex CXP = Im*CX1/CP;
  Complex CXPRho = CXP*fRho;
  double s;
  if (std::abs(CXPRho)>0.01) {
    s = std::log(1.+CXPRho).imag()/fRho;
  } else {
    s = (CXP*(1.-CXPRho*(1.-CXPRho))).imag();
  }
  return s;
}
//______________________________________________________________________________
double TCircle::Eval(double step,double *X, double *D) const
{
  
  Complex CX(fX[0],fX[1]),CXn;
  Complex CP(fD[0],fD[1]),CPn;
  Complex ImTet(0,step*fRho);
  Complex COne,Cf1;

  if (fabs(ImTet.imag()) > 0.01)	{
    Cf1= std::exp(ImTet)-1.;
    COne = Cf1/ImTet;
  } else { 
    COne = (1.+(ImTet/2.)*(1.+ImTet/3.*(1.+ImTet/4.*(1.+ImTet/5.))));
    Cf1 = ImTet*COne;
  }
  CPn = CP + CP*Cf1; 
  CXn = CX + CP*COne*step;

  X[0] = CXn.real();
  X[1] = CXn.imag();
  if (D) {
    D[0] = CPn.real();
    D[1] = CPn.imag();
  }
  return step;
}
//______________________________________________________________________________
double TCircle::Move(double step)
{
  return Eval(step,fX,fD);
}
//______________________________________________________________________________
void TCircle::Rot(double angle)
{
  Complex CX(fX[0],fX[1]),CP(fD[0],fD[1]);
  Complex A = std::exp(Im*angle);
  CX *=A; fX[0] = CX.real(); fX[1]=CX.imag();
  CP *=A; fD[0] = CP.real(); fD[1]=CP.imag();
}
//______________________________________________________________________________
void TCircle::Backward()
{
  fRho=-fRho;fD[0]=-fD[0];fD[1]=-fD[1];
}
//______________________________________________________________________________
double TCircle::Approx(int npoints,const double *points,int istep)
{
	
  double xmed[12];memset(xmed,0,sizeof(xmed));
  double &xx  =xmed[2],&yy  =xmed[ 3],&xy  =xmed[4 ]
        ,&xxx =xmed[5],&yyy =xmed[ 6],&xxy =xmed[7 ],&xyy=xmed[8]
        ,&xxxx=xmed[9],&yyyy=xmed[10],&xxyy=xmed[11];

  double x, y,tmp;int i;
  double const *p;
  for (i=0,p=points;i<npoints;i++,p+=istep) {xmed[0]+=p[0]; xmed[1]+=p[1];}
  for (i=0;i<2;i++)      {xmed[i]/=npoints;}  

  for (i=0,p=points;i<npoints;i++,p+=istep){
    x = p[0]-xmed[0], y = p[1]-xmed[1];
    xx  += x*x;   yy += y*y;   xy +=x*y;   }
  for (i=2;i<5;i++)      {xmed[i]/=npoints;}  

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

  for (i=0,p=points;i<npoints;i++,p+=istep){
    double xt = p[0]-xmed[0], yt = p[1]-xmed[1];
    x = Ei[0][0]*xt+Ei[0][1]*yt;
    y = Ei[1][0]*xt+Ei[1][1]*yt;
    xxx  += (x*x-xx)*x;       yyy  += (y*y-yy)*y; 
    xxy  += (x*x-xx)*y;       xyy  += (y*y-yy)*x;   
    xxxx += (x*x-xx)*(x*x-xx);yyyy += (y*y-yy)*(y*y-yy);
    xxyy += (x*x-xx)*(y*y-yy);             }
  for (i=5;i<12;i++)      {xmed[i]/=npoints;}  


  double det = xx*yy-xy*xy;
  double Rho,a,b,c,r,r2 = 0;
  double pars[2][6]; //contains qality,x0,y0,cos,sin,curv

  pars[0][0] = 3e33;pars[1][0] = 3e33;
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
  Rot( atan2(Ei[0][1],Ei[0][0]));
  fX[0]+=xmed[0]; fX[1]+=xmed[1];

  double s = Path(points);
  Move(s);
  if (s>0) Backward();
  return pars[i][0];
}

//______________________________________________________________________________
double TCircle::Resid(int npoints,const double *points,int istep)
{
  if (!npoints) return 0;
  TCircle M = *this;
  const double *p = points;
  double sum = 0;
  for (int i=0;i<npoints;i++,p+=istep) {
    double s = M.Path(p);
    M.Move(s);
    sum+= pow(M.fX[0]-p[0],2)+pow(M.fX[1]-p[1],2);
  }
  return sqrt(sum/npoints);
}

//______________________________________________________________________________
void TCircle::FitZet(int npoints,const double *points,const double *zets
			              ,double *Z0Tan, int pstep, int zstep)
{
  double aver[4] = {0,0,0,0};
  TCircle MC = *this;
  double s = 0;
  const double *p = points;
  const double *z = zets;
  int n = 0;
  for (int i=0;i<npoints;i++) {
    double ds = MC.Path(p);
    MC.Move(ds);
    s+=ds;
    aver[0]+=s; 
    aver[1]+=*z; 
    aver[2]+=s*s; 
    aver[3]+=z[0]*s; 
    p+=pstep; z+=zstep;n++;
  }
  for (int j=0;j<4;j++) {aver[j]/=n;}
  aver[2]-=aver[0]*aver[0];
  aver[3]-=aver[0]*aver[1];
  Z0Tan[1] = aver[3]/aver[2];
  Z0Tan[0] = aver[1]-Z0Tan[1]*aver[0];
}
//______________________________________________________________________________
double TCircle::Fit(int nPts,const double *Pts,const double *Err,int pstep,int estep)
{
//  C,S direction to the center
//  Dx,Dy direction of moving
//  C = -Dy, S=Dx  for positve curvature

  TArrayD ta(nPts*4); double *ar = ta.GetArray();
  double *C = ar,*S = (ar+=nPts),*R = (ar+=nPts),*W = (ar+=nPts);
  enum {kC=0,kS,kR,kCC,kSS,kCS,kCR,kSR,kRR,kXX=0,kXY,kYY};
  double av[9];
  double mySize = sqrt(pow(Pts[0]-Pts[0+(nPts-1)*pstep],2)+
                     pow(Pts[1]-Pts[1+(nPts-1)*pstep],2));


//  calc average
  TCircle TCbeg = *this;
  double dl = TCbeg.Path(Pts);
  TCbeg.Move(dl);
  double begNor[2],curNor[2];
  for (int iter=0;iter<20;iter++) {
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
    double dx  = 0, dy  = 0;
    if (det > 1e-5 *av[kCC]*av[kSS]) {
      dx  = ( av[kSS]*av[kCR]-av[kCS]*av[kSR])/det;
      dy  = (-av[kCS]*av[kCR]+av[kCC]*av[kSR])/det;
    } else {
      dx = av[kCC]; dy = av[kCS];
      double alfa = av[kCR]/(dx*dx+dy*dy);
      dx *= alfa;   dy*=alfa; 
    }
    
    double h = (av[kR]-av[kC]*dx-av[kS]*dy);
    double dRR   = h*rho - (begNor[0]*dx+begNor[1]*dy);
    double sinD  = (TCbeg.fD[0]*dx+TCbeg.fD[1]*dy);
    double cosD  = sqrt(1-sinD)*(1+sinD);
    double newDx =  TCbeg.fD[0]*cosD + TCbeg.fD[1]*sinD;
    double newDy = -TCbeg.fD[0]*sinD + TCbeg.fD[1]*cosD;
    TCbeg.fD[0]  = newDx;
    TCbeg.fD[1]  = newDy;
    TCbeg.fX[0] += h*begNor[0];
    TCbeg.fX[1] += h*begNor[1];
    TCbeg.fRho  -= dRR*rho;
    if (fabs(h/mySize) + fabs(dRR) + fabs(sinD) <1e-5) break;

  }
  *this = TCbeg;
  return av[kRR];
}    

#include "TRandom.h"
//______________________________________________________________________________
void TCircle::Test(int iTest) 
{
  TCircle helx;
  double R = 1000.;
  if (iTest<0) R = -R;
  iTest = abs(iTest);
  double xy[20][3],er[20][3];
  double dang = 3.14/100;
  double dR[2],qa0,qa1;
  dR[0] = 0.01*R;
  dR[1] = 3*dR[0];
//  dR = 1e-5;
  double dZ = 1;
//double ang0=0.5;
  double ang0=0.0;
  double S0=sin(ang0),C0=cos(ang0);
  for (int i=0;i<20;i++) {
    double ang = ang0 + dang*i;
    double S = sin(ang),C = cos(ang);
    if (iTest==1) {
    double eR = gRandom->Gaus(0,dR[i&1]);
    double eZ = gRandom->Gaus(0,dZ);
    xy[i][0]= 100 + fabs((R+eR))*(S-S0);
    xy[i][1]= 200 -     ((R+eR))*(C-C0);
    er[i][0] = pow(C*dR[i&1],2);
    er[i][2] = pow(S*dR[i&1],2);
    er[i][1] = pow(dR[i&1],2)*C*S;

    xy[i][2]= 300 - R*ang*0.5 +eZ;

    } else {
    xy[i][0]= 100 + i*1;
    xy[i][1]= 200 - i*2;
    xy[i][2]= 300 - i*3;
    }
  } 
  qa0=helx.Approx(20,xy[0],3);
  helx.Print();
  qa1 =helx.Resid(20,xy[0],3);
  printf("Approx qa0 = %g qa1=%g\n",qa0,qa1);
  qa0=helx.Fit(20,xy[0],0,3);
  helx.Print();
  qa1 =helx.Resid(20,xy[0],3);
  printf("Fit qa0 = %g qa1=%g\n",qa0,qa1);

  double z0tan[2];
  helx.FitZet(20,xy[0],&xy[0][2],z0tan,3,3);
  printf("Z0=%g Tan=%g\n",z0tan[0],z0tan[1]);
  double s=0;
  for (int i=0;i<20;i++) {
    double ds = helx.Path(xy[i]);
    s+=ds;
    helx.Move(ds);

    printf("S=%g \tX: %g=%g \tY:%g=%g \tZ:%g=%g  dirX=%g dirY=%g\n"
          ,s
          ,xy[i][0],helx.Pos()[0]
          ,xy[i][1],helx.Pos()[1]
          ,xy[i][2],z0tan[0]+z0tan[1]*s
          ,helx.Dir()[0],helx.Dir()[1]);
  }
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


#if 0
TCanvas *myCanvas = new TCanvas("C1","",600,800);
TGraph  *myGraph  = new TGraph(4  , x, y);
myGraph->Draw("AC*");
myCanvas->Modified();
myCanvas->Update();
#endif //0
//return;

TCircle TC;
double qa0 = TC.Approx(4,xyz[0],3);
double qa1 = TC.Resid (4,xyz[0],3);
printf("Approx qa0 = %g qa1=%g\n",qa0,qa1);
TC.Print();


}

