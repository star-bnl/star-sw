/**************************************************************************
 * Copyright(c) 2001, STAR  Experiment at BNL, All rights reserved.       *
 *                                                                        *
 * Author: STAR Integrated Track Task Force                               *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 *                                                                        *
 **************************************************************************/

/**************************************************************************
 *                                                                        *
 * StiDefaultTrack 	                                                  *				   
 *                                                                        *
 * Author:  Claude Pruneau, Wayne State University                        *
 * Created: March 2001                                                    *
 *                                                                        *
 * Important Note: The Kalman Filter Code imbedded in this class was given*
 *                 to us gracioulsy by Jouri Belikov from the ALICE       *
 *                 collaboration. i.e. code reproduced with autorization. *
 *                                                                        *
 * Description: A work class used to represent tracks in star for the     *
 * purpose of event reconstruction. The track have all attributes         *
 * inherited from the base class StiTrack. ie. they have momenta, vertex  *
 * etc, etc. In addition, this derived class adds attributes related to   *
 * the tracks as containers of hits and as Kalman track.                  *
 *                                                                        *
 **************************************************************************/

#include "StiDefaultTrack.h"
#include "StiSvtDedxCalculator.h"
#include "StiTpcDedxCalculator.h"

ClassImp(StiDefaultTrack)

/**
 * Static Variable Allocation and Methods
 */
static StiDedxCalculator    * svtDedxCalculator = 0;
static StiDedxCalculator    * tpcDedxCalculator = 0;
static StiTrackNodeFactory   * treeNodeFactory   = 0;

static void StiDefaultTrack::setSvtDedxCalculator(const StiDedxCalculator * calculator)
{
  if (svtDedxCalculator!=0 && svtDedxCalculator!=calculator)
    delete svtDedxCalculator;
  svtDedxCalculator = calculator;
}

static void StiDefaultTrack::setTpcDedxCalculator(const StiDedxCalculator * calculator)
{
  if (tpcDedxCalculator!=0 && tpcDedxCalculator!=calculator)
    delete tpcDedxCalculator;
  tpcDedxCalculator = calculator;
}

static void StiDefaultTrack::setTrackNodeFactory(const StiTrackNodeFactory * factory)
{
  if (treeNodeFactory!=0 && treeNodeFactory!=factory)
    delete treeNodeFactory;
  treeNodeFactory = factory;
}

static StiDedxCalculator * StiDefaultTrack::getSvtDedxCalculator()
{
  return svtDedxCalculator;
}

static StiDedxCalculator * StiDefaultTrack::getTpcDedxCalculator()
{
  return tpcDedxCalculator;
}

static StiTrackNodeFactory * StiDefaultTrack::getTrackNodeFactory()
{
  return  treeNodeFactory;
}


StiDefaultTrack::StiDefaultTrack()
{
  //-----------------------------------------------------------------
  // This is the main track constructor.
  //-----------------------------------------------------------------

  firstNode = 0;
  lastNode  = 0;
  refNode   = 0; // mostly refNode will be equal to lastNode but it need
                 // not be the case all the time.
}


StiDefaultTrack::initialize(UInt_t index, 
			   const double xx[5],
			   const double cc[15], 
			   double xref, 
			   double alpha) : KalmanTrack() 
{
  firstNode = trackNodeFactory->getTrackNode();
  firstNode->reset();

  // reference position
  firstNode->fX=xref;

  // local reference frame angle
  firstNode->fAlpha=alpha;
  if (firstNode->fAlpha<-TMath::Pi()) firstNode->fAlpha += 2*TMath::Pi();
  if (firstNode->fAlpha>=TMath::Pi()) firstNode->fAlpha -= 2*TMath::Pi();

  // dedx
  firstNode->fdEdx=0.;

  // state vector
  firstNode->fP0=xx[0]; 
  firstNode->fP1=xx[1]; 
  firstNode->fP2=xx[2]; 
  firstNode->fP3=xx[3]; 
  firstNode->fP4=xx[4];
  
  // covariance error matrix
  firstNode->fC00=cc[0];
  firstNode->fC10=cc[1];  
  firstNode->fC11=cc[2];
  firstNode->fC20=cc[3];  
  firstNode->fC21=cc[4];  
  firstNode->fC22=cc[5];
  firstNode->fC30=cc[6];  
  firstNode->fC31=cc[7];  
  firstNode->fC32=cc[8];  
  firstNode->fC33=cc[9];
  firstNode->fC40=cc[10]; 
  firstNode->fC41=cc[11]; 
  firstNode->fC42=cc[12]; 
  firstNode->fC43=cc[13]; 
  firstNode->fC44=cc[14];
  firstNode->chi2= 0;
  fIndex[0]=index;

	

}

//_____________________________________________________________________________
/*
StiDefaultTrack::StiDefaultTrack(const StiDefaultTrack& t) : KalmanTrack(t) 
{
  //-----------------------------------------------------------------
  // This is a track copy constructor.
  //-----------------------------------------------------------------
  fX     = t.fX;
  fAlpha = t.fAlpha;
  fdEdx  = t.fdEdx;

  fP0=t.fP0; fP1=t.fP1; fP2=t.fP2; fP3=t.fP3; fP4=t.fP4;

  fC00=t.fC00;
  fC10=t.fC10;  fC11=t.fC11;
  fC20=t.fC20;  fC21=t.fC21;  fC22=t.fC22;
  fC30=t.fC30;  fC31=t.fC31;  fC32=t.fC32;  fC33=t.fC33;
  fC40=t.fC40;  fC41=t.fC41;  fC42=t.fC42;  fC43=t.fC43;  fC44=t.fC44;

  firstNode = t.firstNode;
  lastNode  = t.lastNode;
}
*/

//_____________________________________________________________________________
void StiDefaultTrack::getExternalCovariance(double cc[15]) const 
{
  //-------------------------------------------------------------------------
  // This function returns an external representation of the covriance matrix.
  //   (See comments in StiDefaultTrack.h about external track representation)
  //
  // External State 
  // x[0] : "y"
  // x[1] : "z"
  // x[2] : sin(phi)
  // x[3] : tan(lambda)
  // x[4] : 1/pt
  //-------------------------------------------------------------------------
  double a=kConversionConstant;

  double c22=refNode->fX*refNode->fX*refNode->fC33-2*refNode->fX*refNode->fC32+refNode->fC22;
  double c42=refNode->fX*refNode->fC43-refNode->fC42;
  double c20=refNode->fX*refNode->fC30-refNode->fC20, c21=refNode->fX*refNode->fC31-refNode->fC21, c32=refNode->fX*refNode->fC33-refNode->fC32;
  
  cc[0 ]=refNode->fC00;
  cc[1 ]=refNode->fC10;   cc[2 ]=refNode->fC11;
  cc[3 ]=c20;    cc[4 ]=c21;    cc[5 ]=c22;
  cc[6 ]=refNode->fC40;   cc[7 ]=refNode->fC41;   cc[8 ]=c42;   cc[9 ]=refNode->fC44; 
  cc[10]=refNode->fC30*a; cc[11]=refNode->fC31*a; cc[12]=c32*a; cc[13]=refNode->fC43*a; cc[14]=refNode->fC33*a*a;

}

//_____________________________________________________________________________
double StiDefaultTrack::getPredictedChi2(const StHit *hit) const 
{
  //-----------------------------------------------------------------
  // This function calculates a predicted chi2 increment. i.e. it 
  // calculates the increment in the chi2 were the given hit added 
  // to the track
  //-----------------------------------------------------------------
  double r00=hit->getSigmaY2();
  double r01=0.;
  double r11=hit->getSigmaZ2();
  r00+=refNode->fC00; 
  r01+=refNode->fC10; 
  r11+=refNode->fC11;

  double det=r00*r11 - r01*r01;
  if (TMath::Abs(det) < 1.e-10) 
    {
      if (getPtsCount()>4) 
	cerr<<n<<" KalmanTrack warning: Singular matrix !\n";
      return 1e10;
    }
  double tmp=r00; 
  r00=r11; 
  r11=tmp; 
  r01=-r01;
  
  double dy=hit->getY() - refNode->fP0;
  double dz=hit->getZ() - refNode->fP1;
  
  return (dy*r00*dy + 2*r01*dy*dz + dz*r11*dz)/det;
}

//_____________________________________________________________________________
int StiDefaultTrack::propagateTo(double xk,double x0,double rho,double pm)
{
  //-----------------------------------------------------------------
  // This function propagates a track to a reference plane x=xk.
  // xk  x coordinate to propagate the track to.
  // x0  radiation length of the material 
  // rho density of the material
  // pm  mass to be used in propagation
  //-----------------------------------------------------------------
  if (TMath::Abs(refNode->fP3*xk - refNode->fP2) >= 0.99999) 
    {
      if (getPtsCount()>4) 
	cerr<<n<<" StiDefaultTrack warning: Propagation failed !\n";
      return 0;
    }

  double x1=refNode->fX, x2=x1+(xk-x1), dx=x2-x1, y1=refNode->fP0, z1=refNode->fP1;
  double c1=refNode->fP3*x1 - refNode->fP2, r1=sqrt(1.- c1*c1);
  double c2=refNode->fP3*x2 - refNode->fP2, r2=sqrt(1.- c2*c2);

  // create a new node on this track

  lastNode = trackNodeFactory->getTrackNode();
  if (lastNode==0)
    {
      cout << "StiDefaultTrack::propagateTo(xk,x0,rho,pm) - SEVERE ERROR" << endl
	   << "        >>> Unable to get a trackNode from factory" << endl;
      return -1;
    }

  // set properties of this node

  lastNode->fP0 = refNode->fP0 + dx*(c1+c2)/(r1+r2);
  lastNode->fP1 = refNode->fP1 + dx*(c1+c2)/(c1*r2 + c2*r1)*refNode->fP4;

  //f = F - 1
  double rr=r1+r2, cc=c1+c2, xx=x1+x2;
  double f02=-dx*(2*rr + cc*(c1/r1 + c2/r2))/(rr*rr);
  double f03= dx*(rr*xx + cc*(c1*x1/r1+c2*x2/r2))/(rr*rr);
  double cr=c1*r2+c2*r1;
  double f12=-dx*refNode->fP4*(2*cr + cc*(c2*c1/r1-r1 + c1*c2/r2-r2))/(cr*cr);
  double f13=dx*refNode->fP4*(cr*xx-cc*(r1*x2-c2*c1*x1/r1+r2*x1-c1*c2*x2/r2))/(cr*cr);
  double f14= dx*cc/cr; 

  //b = C*ft
  double b00=f02*refNode->fC20 + f03*refNode->fC30;
  double b01=f12*refNode->fC20 + f13*refNode->fC30 + f14*refNode->fC40;
  double b10=f02*refNode->fC21 + f03*refNode->fC31;
  double b11=f12*refNode->fC21 + f13*refNode->fC31 + f14*refNode->fC41;
  double b20=f02*refNode->fC22 + f03*refNode->fC32;
  double b21=f12*refNode->fC22 + f13*refNode->fC32 + f14*refNode->fC42;
  double b30=f02*refNode->fC32 + f03*refNode->fC33;
  double b31=f12*refNode->fC32 + f13*refNode->fC33 + f14*refNode->fC43;
  double b40=f02*refNode->fC42 + f03*refNode->fC43;
  double b41=f12*refNode->fC42 + f13*refNode->fC43 + f14*refNode->fC44;
  
  //a = f*b = f*C*ft
  double a00=f02*b20+f03*b30;
  double a01=f02*b21+f03*b31;
  double a11=f12*b21+f13*b31+f14*b41;

  //F*C*Ft = C + (a + b + bt)
  lastNode->fC00 = refNode->fC00 + a00 + 2*b00;
  lastNode->fC10 = refNode->fC10 + a01 + b01 + b10; 
  lastNode->fC20 = refNode->fC20 + b20;
  lastNode->fC30 = refNode->fC30 + b30;
  lastNode->fC40 = refNode->fC40 + b40;
  lastNode->fC11 = refNode->fC11 + a11 + 2*b11;
  lastNode->fC21 = refNode->fC21 + b21; 
  lastNode->fC31 = refNode->fC31 + b31; 
  lastNode->fC41 = refNode->fC41 + b41; 

  lastNode->fX=x2;

  // Multiple scattering
  double d=sqrt((x1-refNode->fX)*(x1-refNode->fX)
		+(y1-refNode->fP0)*(y1-refNode->fP0)
		+(z1-refNode->fP1)*(z1-refNode->fP1));
  double tanl  = getTanL();
  double invPt = getInvPt();
  double p2=(1.+tanl*tanl)/(invPt*invPt);
  double beta2=p2/(p2 + pm*pm);
  double theta2=14.1*14.1/(beta2*p2*1e6)*d/x0*rho;
  //double theta2=1.0259e-6*10*10/20/(beta2*p2)*d*rho;

  double ey=refNode->fP3*refNode->fX - refNode->fP2, ez=refNode->fP4;
  double xz=refNode->fP3*ez, zz1=ez*ez+1, xy=refNode->fP2+ey;

  lastNode->fC33 = refNode->fC33 + xz*xz*theta2;
  lastNode->fC32 = refNode->fC32 + xz*ez*xy*theta2;
  lastNode->fC43 = refNode->fC43 + xz*zz1*theta2;
  lastNode->fC22 = refNode->fC22 + (2*ey*ez*ez*refNode->fP2+1-ey*ey+ez*ez+refNode->fP2*refNode->fP2*ez*ez)*theta2;
  lastNode->fC42 = refNode->fC42 + ez*zz1*xy*theta2;
  lastNode->fC44 = refNode->fC44 + zz1*zz1*theta2;

  // Energy losses
  double dE=0.153e-3/beta2*(log(5940*beta2/(1-beta2)) - beta2)*d*rho;
  if (x1 < x2) dE=-dE;
  cc=refNode->fP3;
  lastNode->fP3 = refNode->fP3 *(1.- sqrt(p2+pm*pm)/p2*dE);
  lastNode->fP2 = refNode->fP2 + lastNode->fX*(lastNode->fP3-cc);

  // last added node is now the reference node

  refNode = lastNode;

  return 1;
}

//_____________________________________________________________________________
int StiDefaultTrack::propagateToVertex(double x0,double rho,double pm) 
{
  //-----------------------------------------------------------------
  // This function propagates tracks to the "vertex".
  //-----------------------------------------------------------------
  double c=refNode->fP3*refNode->fX - refNode->fP2;
  double tgf=-refNode->fP2/(refNode->fP3*refNode->fP0 + sqrt(1-c*c));
  double snf=tgf/sqrt(1.+ tgf*tgf);
  double xv=(refNode->fP2+snf)/refNode->fP3;
  return PropagateTo(xv,x0,rho,pm);
}

//_____________________________________________________________________________
int StiDefaultTrack::update(const Hit *hit, double chisq, Uint index) {
  //-----------------------------------------------------------------
  // This function associates a cluster with this track.
  //-----------------------------------------------------------------
  double r00=hit->getSigmaY2();
  double r01=0.;
  double r11=hit->getSigmaZ2();
  r00+=refNode->fC00; 
  r01+=refNode->fC10; 
  r11+=refNode->fC11;

  // now inverse this matrix
  double det=r00*r11 - r01*r01;
  double tmp=r00; 
  r00=r11/det; 
  r11=tmp/det; 
  r01=-r01/det;

  double k00=refNode->fC00*r00+refNode->fC10*r01;
  double k01=refNode->fC00*r01+refNode->fC10*r11;
  double k10=refNode->fC10*r00+refNode->fC11*r01;
  double k11=refNode->fC10*r01+refNode->fC11*r11;
  double k20=refNode->fC20*r00+refNode->fC21*r01;
  double k21=refNode->fC20*r01+refNode->fC21*r11;
  double k30=refNode->fC30*r00+refNode->fC31*r01;
  double k31=refNode->fC30*r01+refNode->fC31*r11;
  double k40=refNode->fC40*r00+refNode->fC41*r01;
  double k41=refNode->fC40*r01+refNode->fC41*r11;

  double dy=hit->getY() - refNode->fP0;
  double dz=hit->getZ() - refNode->fP1;
  double cur=refNode->fP3 + k30*dy + k31*dz;
  double eta=refNode->fP2 + k20*dy + k21*dz;
  if (TMath::Abs(cur*refNode->fX-eta) >= 0.99999) 
    {
      int n=getPtsCount();
      if (n>4) cerr<<n<<" StiDefaultTrack warning: Filtering failed !\n";
      return 0;
    }

  lastNode->fP0 = refNode->fP0 + k00*dy + k01*dz;
  lastNode->fP1 = refNode->fP1 + k10*dy + k11*dz;
  lastNode->fP2 = eta; 
  lastNode->fP3 = cur;
  lastNode->fP4 = refNode->fP4 + k40*dy + k41*dz;

  double c00=refNode->fC00;
  double c01=refNode->fC10;
  double c11=refNode->fC11;
  double c02=refNode->fC20;
  double c03=refNode->fC30;
  double c04=refNode->fC40;
  double c12=refNode->fC21;
  double c13=refNode->fC31;
  double c14=refNode->fC41;

  lastNode->fC00 = refNode->fC00 - k00*c00+k01*c10;
  lastNode->fC10 = refNode->fC10 - k00*c01+k01*c11;
  lastNode->fC20 = refNode->fC20 - k00*c02+k01*c12;   
  lastNode->fC30 = refNode->fC30 - k00*c03+k01*c13;
  lastNode->fC40 = refNode->fC40 - k00*c04+k01*c14; 

  lastNode->fC11 = refNode->fC11 - k10*c01+k11*c11;
  lastNode->fC21 = refNode->fC21 - k10*c02+k11*c12;   
  lastNode->fC31 = refNode->fC31 - k10*c03+k11*c13;
  lastNode->fC41 = refNode->fC41 - k10*c04+k11*c14; 

  lastNode->fC22 = refNode->fC22 - k20*c02+k21*c12;   
  lastNode->fC32 = refNode->fC32 - k20*c03+k21*c13;
  lastNode->fC42 = refNode->fC42 - k20*c04+k21*c14; 

  lastNode->fC33 = refNode->fC33 - k30*c03+k31*c13;
  lastNode->fC43 = refNode->fC43 - k30*c04+k31*c14; 

  lastNode->fC44 = refNode->fC44 - k40*c04+k41*c14; 

  // this next line is fishy...
  lastNode->chi2 = getChi2()+chisq;
  refNode->add(lastNode);
  refNode = lastNode;

  return 1;
}

//_____________________________________________________________________________
int StiDefaultTrack::rotate(double alpha)
{
  //-----------------------------------------------------------------
  // This function rotates the track representation by alpha.
  // Note only the current node or reference node is rotated
  //-----------------------------------------------------------------
  refNode->fAlpha += alpha;
  if (refNode->fAlpha < -TMath::Pi()) refNode->fAlpha += 2*TMath::Pi();
  if (refNode->fAlpha >= TMath::Pi()) refNode->fAlpha -= 2*TMath::Pi();
  
  double x1=refNode->fX;
  double y1=refNode->fP0;
  double ca=cos(alpha);
  double sa=sin(alpha);
  double r1=refNode->fP3*refNode->fX - refNode->fP2;
  
  refNode->fX = x1*ca + y1*sa;
  refNode->fP0=-x1*sa + y1*ca;
  refNode->fP2=refNode->fP2*ca + (refNode->fP3*y1 + sqrt(1.- r1*r1))*sa;
  
  double r2=refNode->fP3*refNode->fX - refNode->fP2;
  if (TMath::Abs(r2) >= 0.99999) {
    int n=getPtsCount();
    if (n>4) cerr<<n<<" StiDefaultTrack warning: Rotation failed !\n";
    return 0;
  }
  
  double y0=refNode->fP0 + sqrt(1.- r2*r2)/refNode->fP3;
  if ((refNode->fP0-y0)*refNode->fP3 >= 0.) {
    int n=getPtsCount();
    if (n>4) cerr<<n<<" StiDefaultTrack warning: Rotation failed !!!\n";
    return 0;
  }

  //f = F - 1
  double f00=ca-1;
  double f23=(y1 - r1*x1/sqrt(1.- r1*r1))*sa;
  double f20=refNode->fP3*sa;
  double f22=(ca + sa*r1/sqrt(1.- r1*r1))-1;
  
  //b = C*ft
  double b00=refNode->fC00*f00, b02=refNode->fC00*f20+refNode->fC30*f23+refNode->fC20*f22;
  double b10=refNode->fC10*f00, b12=refNode->fC10*f20+refNode->fC31*f23+refNode->fC21*f22;
  double b20=refNode->fC20*f00, b22=refNode->fC20*f20+refNode->fC32*f23+refNode->fC22*f22;
  double b30=refNode->fC30*f00, b32=refNode->fC30*f20+refNode->fC33*f23+refNode->fC32*f22;
  double b40=refNode->fC40*f00, b42=refNode->fC40*f20+refNode->fC43*f23+refNode->fC42*f22;

  //a = f*b = f*C*ft
  double a00=f00*b00, a02=f00*b02, a22=f20*b02+f23*b32+f22*b22;

  // *** double dy2=fCyy;

  //F*C*Ft = C + (a + b + bt)
  refNode->fC00 += a00 + 2*b00;
  refNode->fC10 += b10;
  refNode->fC20 += a02+b20+b02;
  refNode->fC30 += b30;
  refNode->fC40 += b40;
  refNode->fC21 += b12;
  refNode->fC32 += b32;
  refNode->fC22 += a22 + 2*b22;
  refNode->fC42 += b42; 

  // *** fCyy+=dy2*sa*sa*r1*r1/(1.- r1*r1);
  // *** fCzz+=d2y*sa*sa*fT*fT/(1.- r1*r1);

  return 1;
}

//_____________________________________________________________________________
double StiDefaultTrack::sigmaY2(double r)
{
  //
  // Parametrised error of the cluster reconstruction (pad direction)   
  //
  // Sigma rphi
  const Float_t kArphi=0.41818e-2;
  const Float_t kBrphi=0.17460e-4;
  const Float_t kCrphi=0.30993e-2;
  const Float_t kDrphi=0.41061e-3;
  
  pt=TMath::Abs(pt)*1000.;
  double x=r/pt;
  double tgl=TMath::Abs(tanL);
  double s=kArphi - kBrphi*r*tanL + kCrphi*x*x + kDrphi*x;
  if (s<0.4e-3) s=0.4e-3;
  s*=1.3; //Iouri Belikov
  return s;
}

//_____________________________________________________________________________
double StiDefaultTrack::sigmaZ2(double r)
{
  //
  // Parametrised error of the cluster reconstruction (drift direction)
  //
  // Sigma z
  const Float_t kAz=0.39614e-2;
  const Float_t kBz=0.22443e-4;
  const Float_t kCz=0.51504e-1;
  

  double tgl=TMath::Abs(tanL);
  double s=kAz - kBz*r*tgl + kCz*tgl*tgl;
  if (s<0.4e-3) s=0.4e-3;
  s*=1.3; //Iouri Belikov

  return s;
}

//_____________________________________________________________________________
float  StiDefaultTrack::getYWindow() const
{
  double sy2a = sigmaY2();
  double sy2b = getSigmaY2();
  double width = 4.*sqrt(sy2a+sy2b);
  if (width>30) width = 30;
  return width;  
}

//_____________________________________________________________________________
float  StiDefaultTrack::getZWindow() const
{
  double sz2a = sigmaZ2();
  return getSigmaZ2()+sz2a;
}

//_____________________________________________________________________________
float StiDefaultTrack::getSigmaPx2()                     const
{
  // Return Square of error on px
  return 0;
}

//_____________________________________________________________________________
float StiDefaultTrack::getSigmaPy2()                     const
{
  // Return Square of error on py
  return 0;
}

float StiDefaultTrack::getSigmaPz2()                     const
{  
  // Return Square of error on py
  return 0;
}

float StiDefaultTrack::getSigmaPt2()                     const
{  
  // Return Square of error on py
  return 0;
}

float StiDefaultTrack::getSigmaP2()                      const
{  
  // Return Square of error on py
  return 0;
}

float StiDefaultTrack::getSigmaE2()                      const
{  
  // Return Square of error on py
  return 0;
}

float StiDefaultTrack::getSigmaRapidity2()               const
{  
  // Return Square of error on py
  return 0;
}

float StiDefaultTrack::getSigmaPseudoRapidity2()         const
{  
  // Return Square of error on py
  return 0;
}

//_____________________________________________________________________________
float StiDefaultTrack::getSigmaTanL2()                   const
{  
  // Return Square of error on py
  return 0;
}

//_____________________________________________________________________________
float StiDefaultTrack::getSigmaTanPhi2()                 const
{
  // Return Square of error on py
  return 0;
}

//_____________________________________________________________________________
void  StiDefaultTrack::getErrorMatrix(double cc[15])      const
{ 
  cc[0 ]=refNode->fC00;
  cc[1 ]=refNode->fC10;  cc[2 ]=refNode->fC11;
  cc[3 ]=refNode->fC20;  cc[4 ]=refNode->fC21;  cc[5 ]=refNode->fC22;
  cc[6 ]=refNode->fC30;  cc[7 ]=refNode->fC31;  cc[8 ]=refNode->fC32;  cc[9 ]=refNode->fC33;
  cc[10]=refNode->fC40;  cc[11]=refNode->fC41;  cc[12]=refNode->fC42;  cc[13]=refNode->fC43;  cc[14]=refNode->fC44;
}

//_____________________________________________________________________________
void StiDefaultTrack::addHit(StHit * hit)
{
  // Add given hit as a child of the last node known to this track
  // and subsequntly make this hit the last hit of the track.
  if (hit)
    {
      StiDefaultTrackNode * node = treeNodeFactory->getTrackNode();
      if (node)
	{
	  if (lastNode)
	    {
	      node->reset();  
	      node->setUserObject(hit);
	      lastNode->add(node);
	      lastNode = node;
	    }
	  else
	    {
	      // no lastNode yet, this node is both first and last
	      lastNode  = node;
	      firstNode = node;
	    }
	}
      else
	{
	  cout << "StiDefaultTrack::addHit(StHit * hit) - SEVERE ERROR" << endl
	       << "  >>>   Could not obtain TrackNode object to insert given hit" << endl;
	}
    }
  else
    {
      cout << "StiDefaultTrack::addHit(StHit * hit) - SEVERE ERROR" << endl
	   << "  >>>   Given hit is null" << endl;
    }
}

//_____________________________________________________________________________
void StiDefaultTrack::addHitToNode(StHit * hit, StiTrackNode * node)
{
  // Add given hit as a child of the given node
  // and subsequntly make this hit the last hit of the track.
  if (hit && node)
    {
      StiDefaultTrackNode * newNode = treeNodeFactory->getTrackNode();
      if (newNode)
	{
	  newNode->reset();  
	  newNode->setUserObject(hit);
	  node->add(newNode);
	  // if node is actually last node, make newNode the last node. 
	  if (node==lastNode)
	      lastNode = newNode;
	}
    }
  else
    {
      cout << "StiDefaultTrack::addHit(StHit * hit, StiTrackNode * node) - SEVERE ERROR" << endl
	   << "  >>>   Given hit or node is null pointer" << endl;
    }
}
  
//_____________________________________________________________________________
void StiDefaultTrack::addHitAsParent(StHit * hit, StiTrackNode * node)
{
  // Add a hit as a parent to given node. A new node is obtain from the factory to 
  // contain the given hit. This new node is added as child to the parent of given
  // node. Given node is removed from its parent. Given node is added as child to
  // new node. This is effectively an insert before node operation.
  if (hit && node)
    {
      StiDefaultTrackNode * newNode = treeNodeFactory->getTrackNode();
      if (newNode)
	{
	  newNode->reset();  
	  newNode->setUserObject(hit);
	  StiDefaultTrackNode * parent  = node->getParent();
	  if (parent)
	    {
	      // insert new node between parent and node
	      node->removeFromParent();
	      parent->add(newNode);
	      newNode->add(node);
	    }
	  else
	    {
	      // node has no parent and should thus be the first node on the 
	      // track - error if it is not.
	      if (node==firstNode)
		{
		  // insert new node as parent to node and as first node of this track
		  newNode->add(node);
		  firstNode = newNode;
		}
	      else
		{
		  cout << "StiDefaultTrack::addHitAsParent(StHit * hit, StiTrackNode * node) - SEVERE ERROR" << endl
		       << "  >>>   Given hit or node is null pointer" << endl;
		}
	    }
	}
    }
  else
    {
      cout << "StiDefaultTrack::addHitAsParent(StHit * hit, StiTrackNode * node) - SEVERE ERROR" << endl
	   << "  >>>   Given hit or node is null pointer" << endl;
    }
    
  }

//_____________________________________________________________________________
void StiDefaultTrack::removeHit(StHit * hit)
{
  // Remove a hit from the Tree Track - This method is slow because it must first 
  // search for the node containing the given hit and then proceed to remove it.
  // Use "removeHitNode()" whenever you can.
  
}

//_____________________________________________________________________________
void StiDefaultTrack::removeNode(StiTrackNode * node)
{
  // Remove given node from this track. Implicitely, all children
  
  
}

void StiDefaultTrack::pruneNodes(StiTrackNode * node)
{
  // Prune unnecessary nodes on the track starting at given node. 
  // All siblings of the given node, are removed, and iteratively
  // all siblings of its parent are removed from the parent of the
  // parent, etc.

  StiTrackNode * parent = node->getParent();
  while (parent)
    {
      parent->removeAllChildrenExcept(node);
      node = parent;
      parent = node->getParent();
    }
}

StiTrackNode * StiDefaultTrack::findBestBranch(StiTrackNode * node)
{
  // starting at given node, find the best branch and return the end node.
  // Only the leafs are looked at. Basically, the best track is taken as 
  // that being the longest and the lowest chi-square. Return the last node 
  // of the best branch.
  float chi2;
  float bestChi2;
  StiTrackNode * bestNode;
  StiTrackNode * leaf;
  
  leaf = node->getFirstLeaf();
  bestChi2 = leaf->getChi2();
  bestNode = leaf;
  leaf = leaf->getNextLeaf();
  while (leaf)
    {
      chi2 = leaf->getChi2();
      if (chi2<bestChi2)
	{
	  bestChi2 = chi2;
	  bestNode = leaf;
	}
      leaf = leaf->getNextLeaf();
    }
  return bestNode;
}


//_____________________________________________________________________________
void StiDefaultTrack::reset()
{
  vertex = 0;
  removeAllHits();
  svtDedx = 0;
  tpcDedx = 0;
  nPts    = 0;
  nFitPts = 0;
}

//_____________________________________________________________________________
void StiDefaultTrack::removeAllHits()
{
  firstNode = 0;
  lastNode  = 0;
}

void StiDefaultTrack::calculateSvtDedx()
{
  //svtDedx = svtDedxCalculator->getDedx(nPts, dedxSamples);
}

void StiDefaultTrack::calculateTpcDedx()
{
  //svtDedx = svtDedxCalculator->getDedx(nPts, dedxSamples);
}

