#include <stdlib.h>
#include <math.h>
#include "THelixTrack.h"
const double Zero = 1.e-5;


ClassImp(THelixTrack)
//_____________________________________________________________________________
THelixTrack::THelixTrack(const double *xyz,const double *dir,double rho,const double *hxyz)
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
  Set(xyz,dir,rho,hxyz);
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
       zm += z; sm += s; ss += s*s; zs += z*s; 
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
void THelixTrack::Set(const double *xyz,const double *dir,double rho,const double *hxyz)
{
  fX[0] = xyz[0]; fX[1] = xyz[1]; fX[2] = xyz[2];
  fP[0] = dir[0]; fP[1] = dir[1]; fP[2] = dir[2];
  fKind=0;
  if (hxyz) {
    fH[0] = hxyz[0];fH[1] = hxyz[1];fH[2] = hxyz[2];
    if (!fH[0] && !fH[1]) fKind=1;
  } else {
    fH[0] = 0.;     fH[1] =0.;      fH[2] = 1.; fKind=1;}
  fRho = rho;
  Build();
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
double THelixTrack::Step(double step, double *xyz, double *dir) const
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
  Step(step,fX,fP);
  fHXP[0] = fH[1]*fP[2] - fH[2]*fP[1];
  fHXP[1] = fH[2]*fP[0] - fH[0]*fP[2];
  fHXP[2] = fH[0]*fP[1] - fH[1]*fP[0];
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
  double poly[4][3],tri[3]={0,0,0},sol[2],cos1t,f1,f2,step,ss;
  const double *sp[4][4] = {{s+0,s+1,s+2,s+3}, {s+1,s+4,s+7,s+9}, 
                            {s+2,s+7,s+5,s+8}, {s+3,s+9,s+8,s+6}}; 

  
  cos1t = 0.5*fRho*fCosL;
  poly[0][0]=1.;poly[0][1]=0.;poly[0][2]=0.;
  for(ix=1;ix<4;ix++) {
    poly[ix][0] =fX  [ix-1]; 
    poly[ix][1] =fP  [ix-1]; 
    poly[ix][2] =fHXP[ix-1]*cos1t;
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
  
  double x[3],d[3];
  Step(step,x,d);
  if (nsol) {//test it
    ss = s[0]+s[1]*x[0]+s[2]*x[1]+s[3]*x[2];
    if (nsurf > 4) ss += s[4]*x[0]*x[0]+s[5]*x[1]*x[1]+s[6]*x[2]*x[2];
    if (nsurf > 7) ss += s[7]*x[0]*x[1]+s[8]*x[1]*x[2]+s[9]*x[2]*x[0];
    if (fabs(ss)<1.e-7) {
      if (xyz) memcpy(xyz,x,sizeof(*xyz)*3);
      if (dir) memcpy(dir,d,sizeof(*dir)*3);
      return step;
  } }

  stmax -=step; stmin -=step; if (stmin < -fMax) stmin = -fMax; 
  THelixTrack th(x,d,fRho,fH);
  return th.Step(stmin,stmax,s,nsurf,xyz,dir)+step;


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
double THelixTrack::Step(const double *point,double *xyz, double *dir) const
{
    double pnt[3],xx,yy,zz,tmp,step,dx,dy;
    int i;    
    for (i=0;i<3;i++) pnt[i] = point[i] - fX[i];

    xx = (pnt[0]*fPxy[0]+pnt[1]*fPxy[1]+pnt[2]*fPxy[2]);
    yy = (pnt[0]*fHXP[0]+pnt[1]*fHXP[1]+pnt[2]*fHXP[2])/fCosL;
    zz = (pnt[0]*fH  [0]+pnt[1]*fH  [1]+pnt[2]*fH  [2]);
    double arho = fabs(fRho);

//		Z estimated step 

    step = 0.;
    if (fabs(fHP) > 0.01) {
      step = zz/fHP;
    } else {
//		R estimated step
      if (fRho < 0) yy = -yy;

      tmp = ::sqrt(::pow(xx*fRho,2)+::pow(yy*fRho-1.,2));
      step = xx/tmp;
      if (arho > Zero) {
        dx = step*arho; if (fabs(dx) <= Zero) dx = 0;
        dy = yy*arho -1;if (fabs(dy) <= Zero) dy = 0;
        if (fabs(arho*step)> 0.01 || dy >0 || dx < 0) {
          step = atan2(dx,-dy); if (step<0) step +=2.*M_PI;
          step /=arho;}
      }
      step /= fCosL;
    }
    double xnear[3],pnear[3];
//		iterations
    double dstep = 1.e+10;
    for (int iter=0; iter <5 && (fabs(arho*dstep)> 0.001 || !iter); iter++)
    { 
      Step(step,xnear,pnear);
      dstep = 0;
      for (i=0;i<3;i++) {dstep += pnear[i]*(point[i]-xnear[i]);}
      step += dstep;
    }
  return (xyz) ? Step(step,xyz,dir) : step;
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
