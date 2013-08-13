#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TCernLib.h"
#include "TMath.h"
#include "Sti/StiNodePars.h"
#include "Sti/StiToolkit.h"

  static const int idx66[6][6] =
  {{ 0, 1, 3, 6,10,15},{ 1, 2, 4, 7,11,16},{ 3, 4, 5, 8,12,17}
  ,{ 6, 7, 8, 9,13,18},{10,11,12,13,14,19},{15,16,17,18,19,20}};
static const double recvCORRMAX  = 0.99;
static const double chekCORRMAX  = 0.9999;
static double MAXPARS[]={555,555,555,6.66,111,111};

void Multiply(Mtx55D_t &res, const Mtx55D_t &A,const Mtx55D_t &B)
{
  memset(res[0],0,5*5*sizeof(res[0][0]));
  for (int j=0;j<5;j++) {
  for (int i=0;i<5;i++) {
    const double &aij = A[i][j];
    if (!aij) continue;
    for (int k=0;k<5;k++) {
      const double &bjk = B[j][k];
      if (!bjk) continue;
      res[i][k] += aij*bjk;
  } } }
}


#if 0
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
#endif //0
//______________________________________________________________________________
int StiNodePars::check(const char *pri) const
{

  int ierr=0;
//?? temp test
  double tmp = _curv - _ptin* _hz;
//		1km for 1GeV is a zero field
//  assert(fabs(_hz)<1e-5 || fabs(tmp)<= 1e-3*fabs(_curv));
  if (fabs(_hz)>=1e-5 && fabs(tmp)> 1e-3*fabs(_curv)) {ierr=1001; goto FAILED;}
  for (int i=0;i<kNPars;i++) {if (fabs(P[i]) > MAXPARS[i]) {ierr = i+1 ; break;}} 
  if(ierr) goto FAILED;
  for (int i=-2;i<0;i++)     {if (fabs(P[i]) > 1.)         {ierr = i+12; break;}} 
  if (ierr) goto FAILED;
  tmp = fabs(cos(_psi)-_cosCA);
  ierr = 1002; if (tmp>1e-4) goto FAILED;
  tmp = fabs(sin(_psi)-_sinCA);
  ierr = 1003; if (tmp>1e-4) goto FAILED;
  ierr = 0;
FAILED: 
  if (!ierr) return ierr;
  if (!pri ) return ierr;
  printf("StiNodePars::check(%s) == FAILED(%d)\n",pri,ierr);
  print();
  return ierr;
} 
//______________________________________________________________________________
StiNodePars &StiNodePars::merge(double wt,StiNodePars &other)
{
   double wt0 = 1.-wt;
   for (int i=0;i<kNPars+1;i++) {P[i] = wt0*P[i] + wt*other.P[i];}
   ready();
   return *this;
}
//______________________________________________________________________________
void StiNodePars::set(const THelixTrack *th, double Hz)
{
  memcpy(&_x,th->Pos(),3*sizeof(_x));
  _psi = atan2(th->Dir()[1],th->Dir()[0]);
  double sinL = th->Dir()[2];
  double cosL = sqrt((1-sinL)*(1+sinL));
  _tanl = sinL/cosL;
  _cosCA = th->Dir()[0]/cosL;
  _sinCA = th->Dir()[1]/cosL;
  _curv = th->GetRho();
  _hz =  Hz;
  _ptin = _curv/_hz;
}

//______________________________________________________________________________
void StiNodePars::fill(THelixTrack *th) const
{
  double dir[3]={_cosCA,_sinCA,_tanl};
  th->Set(&_x,dir,_curv);
}
//______________________________________________________________________________
int StiNodePars::diff(const StiNodePars &other,double tol) const
{
static const float small[10]={0.01,0.01,0.1,0.1,0.1,0.01,0.1,0.01,0.01,0.0001};
  const double *a =       &_cosCA;
  const double *b = &other._cosCA;
  int n = &_hz-a+1; 
  for (int i=0;i<n;i++) {
    if (fabs(a[i]-b[i]) < small[i]) continue;
    if (fabs(a[i]-b[i]) > tol*(fabs(a[i])+fabs(b[i]))*0.5) return i;
  }  
  return -1;
}
//______________________________________________________________________________
void StiNodePars::move(double dLxy)
{
  double dcCA,dsCA,dC,dS,dX,dY,dZ;
  double dPhi = _curv*dLxy;

  if (fabs(dPhi) < 0.2) {
    
    dC = -dPhi*dPhi/2; dS = dPhi*(1-dPhi*dPhi/6);
    dX = dLxy*(_cosCA*(1-dPhi*dPhi/6) - _sinCA*(dPhi/2));
    dY = dLxy*(_sinCA*(1-dPhi*dPhi/6) + _cosCA*(dPhi/2));
  } else {

    dC = cos(dPhi)-1; dS = sin(dPhi);
    dX = (_cosCA*dS + _sinCA*dC)/_curv;
    dY = (_sinCA*dS - _cosCA*dC)/_curv;
  }
  dZ = dLxy*_tanl;
  dcCA = _cosCA*dC - _sinCA*dS;
  dsCA = _sinCA*dC + _cosCA*dS;
  _cosCA+=dcCA; _sinCA+=dsCA; _x+=dX; _y+=dY; _z+=dZ; _psi+=dPhi;
}
//______________________________________________________________________________
void StiNodePars::moveToR(double R)
{
  double myR2  = _x*_x     + _y*_y;
  double myDot = _x*_cosCA + _y*_sinCA;
  double dR2 = R*R-myR2;
  double dis = myDot*myDot+dR2;
  if (dis<0) dis = 0;
  dis = sqrt(dis);
  double dL = (dR2)/(dis+fabs(myDot));
  if (myDot<0) dL = -dL;
  move(dL);
}

//______________________________________________________________________________
void StiNodePars::print() const
{
static const char* tit[]={"cosCA","sinCA","X","Y","Z","Eta","Ptin","TanL","Curv",0};
  for (int i=-2;i<kNPars+1;i++) {printf("%s = %g, ",tit[i+2],P[i]);}
  printf("\n");
}   
//______________________________________________________________________________
//______________________________________________________________________________
void StiHitErrs::rotate(double angle)
{
  double t[2][2];
  t[0][0] = cos(angle); t[0][1] = -sin(angle);
  t[1][0] = -t[0][1]  ; t[1][1] = t[0][0];
  double r[3];
  TCL::trasat(t[0],&hXX,r,2,2);
  TCL::ucopy(r,&hXX,3);
}
//______________________________________________________________________________
void StiFitErrs::Trans(const StiFitErrs &whom,const Mtx55D_t &how)
{
  TCL::trasat(how[0],whom.Arr(),Arr(),5,5);
}  
//______________________________________________________________________________
void StiFitErrs::Reset()
{
  memset(this,0,sizeof(*this));
  mHH =  (1.00 *1.00 );
  mZZ =  (1.0  *1.0 );
  mAA =  (0.03 *0.03);
  mLL =  (0.03 *0.03);
  mCC =  (0.9  *0.9 );
}

//______________________________________________________________________________
void StiFitErrs::Set(const THEmx_t *he, double hz)
{
mHz = hz;
mHH = he->mHH ;
mHZ = he->mHZ ;
mZZ = he->mZZ ;
mHA = he->mHA ;
mZA = he->mAZ ;
mAA = he->mAA ;
mHL = he->mHL ;
mZL = he->mZL ;
mAL = he->mAL ;
mLL = he->mLL ;
mHC = he->mHC/mHz ;
mZC = he->mCZ/mHz ;
mAC = he->mAC/mHz ;
mLC = he->mCL/mHz ;
mCC = he->mCC/mHz/mHz ;
}  
//______________________________________________________________________________
void StiFitErrs::Get(THEmx_t *he) const
{
  assert(mHz);
  he->mHH = mHH ;
  he->mHZ = mHZ ;
  he->mZZ = mZZ ;
  he->mHA = mHA ;
  he->mAZ = mZA ;
  he->mAA = mAA ;
  he->mHL = mHL ;
  he->mZL = mZL ;
  he->mAL = mAL ;
  he->mLL = mLL ;
  he->mHC = mHC*mHz ;
  he->mCZ = mZC*mHz ;
  he->mAC = mAC*mHz ;
  he->mCL = mLC*mHz ;
  he->mCC = mCC*mHz*mHz ;
}  
//_____________________________________________________________________________
void StiFitErrs::Backward()
{
  mHA*=-1; mAC*=-1; mHZ*=-1; mZC*=-1; mAL*=-1; mZL*=-1;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

//_____________________________________________________________________________
/**
   returns the node information
   double x[6],  : state, for a definition, in radial implementation
                   rad  - radius at start (cm). See also comments
                   phi  - azimuthal angle  (in rad)      
                   z    - z-coord. (cm)                 
                   tanl - tan(dip) =pz/pt               
                   psi  - azimuthal angle of pT vector (in rads)     
                   pti  - signed invert Pt.//  pti = curv/hz 
   double cc[15] : error matrix of the state "x" rad is fixed
                       code definition adopted here, where:
   PhiPhi;
   ZPhi     ,ZZ;                       
   TanlPhi  ,TanlZ ,TanlTanl,                 
   PhiPsi   ,ZPsi  ,TanlPsi , PsiPsi ,           
   PhiPti  ,ZPti ,TanlPti, PsiPti, PtiPti     

*/
//_____________________________________________________________________________
void StiNodePars::GetRadial(double radPar[6],double radErr[15],const StiFitErrs *fitErr) const
{
//Remind StiFitPars:
//double mH;	// direction perpendicular movement and Z
//double mZ;	// Z position
//double mA;	// Angle in XY. cos(A),sin(A),T moving direction
//double mL;	// Angle lambda in Rxy/Z
//double mC;	// Curvature

  enum {jRad=0,jPhi,jZ,jTan,jPsi,jPti};
  enum {jPhiPhi=0
       ,jPhiZ  ,jZZ
       ,jPhiTan,jZTan,jTanTan
       ,jPhiPsi,jZPsi,jTanPsi,jPsiPsi,
       ,jPhiPti,jZPti,jTanPti,jPsiPti,jPtiPti};

  radPar[jRad] = getRxy();
  radPar[jPhi] = atan2(_y,_x);
  radPar[jZ  ] = _z;
  radPar[jTan] = _tanl;
  radPar[jPsi] = _psi;
  radPar[jPti] = _ptin;
  if (!radErr) return;

  double R2 = radPar[jRad]*radPar[jRad];
  double cos2L = 1./(1+_tanl*_tanl);
  double cosL = sqrt(cos2L);
  double sinL = _tanl*cosL;
			
  double dcaFrame[3][3]={{  cosL*_cosCA, cosL*_sinCA, sinL}	
                        ,{      -_sinCA,      _cosCA,    0}
                        ,{ -sinL*_cosCA,-sinL*_sinCA, cosL}};
			
  double dXdH[3],dXdZ[3];
  double dLdH = -( dcaFrame[1][0]*_x+dcaFrame[1][1]*_y)/( dcaFrame[0][0]*_x+dcaFrame[0][1]*_y);
  double dLdZ = -( dcaFrame[2][0]*_x+dcaFrame[2][1]*_y)/( dcaFrame[0][0]*_x+dcaFrame[0][1]*_y);
  for (int i=0;i<3;i++) {dXdH[i]= dcaFrame[0][i]*dLdH+dcaFrame[1][i];}
  for (int i=0;i<3;i++) {dXdZ[i]= dcaFrame[0][i]*dLdZ+dcaFrame[2][i];}

  double testh = dXdH[0]*_x + dXdH[1]*_y; 
  double testz = dXdZ[0]*_x + dXdZ[1]*_y; 
  assert(fabs(testh) <1e-10);  
  assert(fabs(testz) <1e-10);  

  double dPhi_dH = (_x*dXdH[1]-_y*dXdH[0])/R2;
  double dPhi_dZ = (_x*dXdZ[1]-_y*dXdZ[0])/R2;
  double dPsi_dH = _curv*dLdH;  
  double dPsi_dZ = _curv*dLdZ;
  double dTan_dLam = 1./cos2L;
//                       d/dH     d/dZ       d/dA     d/dLam d/dPti
  double T[5][5] = {{ dPhi_dH, dPhi_dZ,         0,         0,     0}  //dPhi
                   ,{ dXdH[2], dXdZ[2],         0,         0,     0}  //dZ
  		   ,{       0,       0,         0, dTan_dLam,     0}  //dTan
  		   ,{ dPsi_dH, dPsi_dZ,         1,         0,     0}  //dPsi
  		   ,{       0,       0,         0,         0,     1}};//dPti
  




  TCL::trasat(T[0],fitErr->Arr(),radErr,5,5); 


}
//_____________________________________________________________________________
void StiNodePars::GetImpact(StiImpact *imp,const StiFitErrs *fe)  const
{
    /// signed impact parameter; Signed in such a way that:
    ///     x =  -impact*sin(Psi)
    ///     y =   impact*cos(Psi)
  imp->mImp = _x*(-_sinCA) + _y*(_cosCA);
  imp->mZ   = _z;
  imp->mPsi = _psi;
  imp->mPti = _ptin;
  imp->mTan = _tanl;
  imp->mCurv= _curv;
  if (!fe) return;

  float cos2L = 1./(1+_tanl*_tanl);
  imp->mImpImp=fe->mHH;
  imp->mZImp  =fe->mHZ; imp->mZZ  =fe->mZZ;
  imp->mPsiImp=fe->mHA; imp->mPsiZ=fe->mHZ; imp->mPsiPsi=fe->mAA;
  imp->mPtiImp=fe->mHC; imp->mPtiZ=fe->mZC; imp->mPtiPsi=fe->mAC; imp->mPtiPti=fe->mCC;
  imp->mTanImp=fe->mHL/cos2L; 
  imp->mTanZ  =fe->mZL/cos2L; 
  imp->mTanPsi=fe->mAL/cos2L; 
  imp->mTanPti=fe->mLC/cos2L;
  imp->mTanTan=fe->mLL/cos2L/cos2L;

}
//_____________________________________________________________________________
void StiFitErrs::Get(const StiNodePars *np,  StiNodeErrs *ne) const
{
  double dTdL = np->_tanl*np->_tanl+1;

//                       d/dH     d/dZ       d/dA     d/dLam d/dPti
  double T[6][5] = {{ np->_cosCA,   0,         0,         0,  0}   //dX
  		   ,{ np->_sinCA,   0,         0,         0,  0}   //dY
  		   ,{         0,    1,         0,         0,  0}   //dZ
  		   ,{         0,    0,         1,         0,  0}   //dEta
  		   ,{         0,    0,         0,         0,  1}   //dPti
  		   ,{         0,    0,         0,      dTdL,  0}}; //dTan

  TCL::trasat(T[0],this->Arr(),ne->A,5,6); 

}
//_____________________________________________________________________________
//_____________________________________________________________________________
#include "TRandom.h"
#include "TMatrixTSym.h"
#include "TMatrixT.h"
#include "TVectorT.h"
#include "StarRoot/TRandomVector.h"
//_____________________________________________________________________________
ClassImp(StiNodeParsTest);
void StiNodeParsTest::Test()
{
  double thPars[7+15],Hz=0.000299792458 * 4.98478;
  for (int i=0;i<7+15;i++) {thPars[i]=gRandom->Rndm();}
  THelixTrack th(thPars,thPars+3,thPars[6]),thh;
  memcpy(thPars+3,th.Dir(),3*sizeof(double));
  th.SetEmx(thPars+7);
  
  StiNodePars pars;
  StiFitErrs  errs;
  pars.set(&th,Hz);
  errs.Set(th.Emx(),Hz);
  pars.reverse();
  errs.Backward();
  pars.fill(&thh);
  thh.SetEmx(0);
  errs.Get(thh.Emx());
  thh.Backward();

  double thhPars[7+15];
  memcpy(thhPars,thh.Pos(),7*sizeof(double));
  memcpy(thhPars+7,thh.Emx()->Arr(),15*sizeof(double));
  int nerr=0;
  for (int i=0;i<7+15;i++) {
    if (fabs(thhPars[i]-thPars[i]) <1e-6) continue;
    nerr++;printf("%d = %g %g \n",i,thPars[i],thhPars[i]);}
  printf("nmErrs = %d\n",nerr);
  
}
//_____________________________________________________________________________
void StiNodeParsTest::TestGetRadial(int nEv)
{
StiFitErrs fE;
  fE.mHH = 1.0*1.0;
  fE.mZZ = 2.0*2.0;
  fE.mAA = 0.01*0.01;
  fE.mLL = 0.01*0.01;
  fE.mCC = 0.1*0.1;
  double dia[5];
  double *e = &fE.mHH;
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=e[li+i];
    for (int j=0;j<i;j++) {
       e[li+j] = sqrt(dia[i]*dia[j])*(0.5-gRandom->Rndm())*0.2;
//       printf("[%d][%d]=%d ",i,j,e[li+j]);
    } 
  }
  TMatrixTSym<double> S(5);
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++    ) {
       S[i][j]=e[li+j]; S[j][i]=e[li+j];
    } }
  S.Print();
  StiNodePars node;
  node.reset();
  double myRad = 100;
  double phi = gRandom->Rndm()*2*M_PI;
  node._x    = myRad*cos(phi);
  node._y    = myRad*sin(phi);
  node._z    = (gRandom->Rndm()-0.5)*200;
  node._hz   = 0.000299792458 * 4.98478;
  node._psi  = phi + (gRandom->Rndm()-0.5);
  node._tanl = gRandom->Rndm();
  node._ptin = (gRandom->Rndm()-0.5);
  node.ready();

  TRandomVector RV(S);
  double radPar[6],radErr[15];
  node.GetRadial(radPar,radErr,&fE);
  TMatrixTSym<double> SS(5);
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++    ) {
       SS[i][j]=radErr[li+j]; SS[j][i]=radErr[li+j];
  } }
  SS.Print();

  double rE[15]={0};
  for (int ev=0;ev<nEv;ev++) 
  {
    const TVectorT<double> &res = RV.Gaus();
    StiNodePars myNode(node);
    StiFitPars  fitPars(res.GetMatrixArray());
    myNode +=fitPars;
//		Project it to x*x+y*y=r*r
    myNode.moveToR(myRad);
    double rP[6];
    myNode.GetRadial(rP);
    
    for (int i=0,li=0;i< 5;li+=++i) {
      for (int j=0;j<=i;j++    ) {
      rE[li+j]+= (rP[i+1]-radPar[i+1])*(rP[j+1]-radPar[j+1]);
    } }
  }  //End events
  for (int j=0;j<15;j++) {rE[j]/=(nEv);}
   
  double qA=0,qAmax=0;
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=radErr[li+i];
    for (int j=0;j<=i;j++) {
    double dif = (rE[li+j]-radErr[li+j])/sqrt(dia[i]*dia[j]);
    printf("(%d %d) \t%g = \t%g \t%g\n",i,j,radErr[li+j],rE[li+j],dif);
    dif = fabs(dif);
    qA+= (dif); if (dif>qAmax) qAmax=dif;
  } }
  qA/=15;
  printf("Quality %g < %g < 1\n",qA,qAmax);

}
