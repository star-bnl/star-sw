#include "StiDefaultTrack.h"
#include "StiSvtDedxCalculator.h"
#include "StiTpcDedxCalculator.h"

ClassImp(StiDefaultTrack)

/**
 * Static Variable Allocation and Methods
 */
static StiDedxCalculator    * svtDedxCalculator = 0;
static StiDedxCalculator    * tpcDedxCalculator = 0;
static StiTreeNodeFactory   * treeNodeFactory   = 0;

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

static void StiDefaultTrack::setTreeNodeFactory(const StiTreeNodeFactory * factory)
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

static StiTreeNodeFactory * StiDefaultTrack::getTreeNodeFactory()
{
  return  treeNodeFactory;
}


StiDefaultTrack::StiDefaultTrack(UInt_t index, 
			   const double xx[5],
			   const double cc[15], 
			   double xref, 
			   double alpha) : KalmanTrack() 
{
  //-----------------------------------------------------------------
  // This is the main track constructor.
  //-----------------------------------------------------------------

	  // reference position
  fX=xref;

  // local reference frame angle
  fAlpha=alpha;
  if (fAlpha<-TMath::Pi()) fAlpha += 2*TMath::Pi();
  if (fAlpha>=TMath::Pi()) fAlpha -= 2*TMath::Pi();

  // dedx
  fdEdx=0.;

  // state vector
  fP0=xx[0]; fP1=xx[1]; fP2=xx[2]; fP3=xx[3]; fP4=xx[4];

  // covariance error matrix
  fC00=cc[0];
  fC10=cc[1];  fC11=cc[2];
  fC20=cc[3];  fC21=cc[4];  fC22=cc[5];
  fC30=cc[6];  fC31=cc[7];  fC32=cc[8];  fC33=cc[9];
  fC40=cc[10]; fC41=cc[11]; fC42=cc[12]; fC43=cc[13]; fC44=cc[14];

  fIndex[0]=index;

	

}

//_____________________________________________________________________________
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

  firstHit = t.firstHit;
  lastHit  = t.lastHit;
}

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

  double c22=fX*fX*fC33-2*fX*fC32+fC22;
  double c42=fX*fC43-fC42;
  double c20=fX*fC30-fC20, c21=fX*fC31-fC21, c32=fX*fC33-fC32;
  
  cc[0 ]=fC00;
  cc[1 ]=fC10;   cc[2 ]=fC11;
  cc[3 ]=c20;    cc[4 ]=c21;    cc[5 ]=c22;
  cc[6 ]=fC40;   cc[7 ]=fC41;   cc[8 ]=c42;   cc[9 ]=fC44; 
  cc[10]=fC30*a; cc[11]=fC31*a; cc[12]=c32*a; cc[13]=fC43*a; cc[14]=fC33*a*a;

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
  r00+=fC00; 
  r01+=fC10; 
  r11+=fC11;

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
  
  double dy=hit->getY() - fP0;
  double dz=hit->getZ() - fP1;
  
  return (dy*r00*dy + 2*r01*dy*dz + dz*r11*dz)/det;
}

//_____________________________________________________________________________
int StiDefaultTrack::propagateTo(double xk,double x0,double rho,double pm)
{
  //-----------------------------------------------------------------
  // This function propagates a track to a reference plane x=xk.
  //-----------------------------------------------------------------
  if (TMath::Abs(fP3*xk - fP2) >= 0.99999) 
    {
      if (getPtsCount()>4) 
	cerr<<n<<" StiDefaultTrack warning: Propagation failed !\n";
      return 0;
    }

  double x1=fX, x2=x1+(xk-x1), dx=x2-x1, y1=fP0, z1=fP1;
  double c1=fP3*x1 - fP2, r1=sqrt(1.- c1*c1);
  double c2=fP3*x2 - fP2, r2=sqrt(1.- c2*c2);
  
  fP0 += dx*(c1+c2)/(r1+r2);
  fP1 += dx*(c1+c2)/(c1*r2 + c2*r1)*fP4;

  //f = F - 1
  double rr=r1+r2, cc=c1+c2, xx=x1+x2;
  double f02=-dx*(2*rr + cc*(c1/r1 + c2/r2))/(rr*rr);
  double f03= dx*(rr*xx + cc*(c1*x1/r1+c2*x2/r2))/(rr*rr);
  double cr=c1*r2+c2*r1;
  double f12=-dx*fP4*(2*cr + cc*(c2*c1/r1-r1 + c1*c2/r2-r2))/(cr*cr);
  double f13=dx*fP4*(cr*xx-cc*(r1*x2-c2*c1*x1/r1+r2*x1-c1*c2*x2/r2))/(cr*cr);
  double f14= dx*cc/cr; 

  //b = C*ft
  double b00=f02*fC20 + f03*fC30, b01=f12*fC20 + f13*fC30 + f14*fC40;
  double b10=f02*fC21 + f03*fC31, b11=f12*fC21 + f13*fC31 + f14*fC41;
  double b20=f02*fC22 + f03*fC32, b21=f12*fC22 + f13*fC32 + f14*fC42;
  double b30=f02*fC32 + f03*fC33, b31=f12*fC32 + f13*fC33 + f14*fC43;
  double b40=f02*fC42 + f03*fC43, b41=f12*fC42 + f13*fC43 + f14*fC44;
  
  //a = f*b = f*C*ft
  double a00=f02*b20+f03*b30;
  double a01=f02*b21+f03*b31;
  double a11=f12*b21+f13*b31+f14*b41;

  //F*C*Ft = C + (a + b + bt)
  fC00 += a00 + 2*b00;
  fC10 += a01 + b01 + b10; 
  fC20 += b20;
  fC30 += b30;
  fC40 += b40;
  fC11 += a11 + 2*b11;
  fC21 += b21; 
  fC31 += b31; 
  fC41 += b41; 

  fX=x2;

  // Multiple scattering
  double d=sqrt((x1-fX)*(x1-fX)+(y1-fP0)*(y1-fP0)+(z1-fP1)*(z1-fP1));
  double tanl  = getTanL();
  double invPt = getInvPt();
  double p2=(1.+tanl*tanl)/(invPt*invPt);
  double beta2=p2/(p2 + pm*pm);
  double theta2=14.1*14.1/(beta2*p2*1e6)*d/x0*rho;
  //double theta2=1.0259e-6*10*10/20/(beta2*p2)*d*rho;

  double ey=fP3*fX - fP2, ez=fP4;
  double xz=fP3*ez, zz1=ez*ez+1, xy=fP2+ey;

  fC33 += xz*xz*theta2;
  fC32 += xz*ez*xy*theta2;
  fC43 += xz*zz1*theta2;
  fC22 += (2*ey*ez*ez*fP2+1-ey*ey+ez*ez+fP2*fP2*ez*ez)*theta2;
  fC42 += ez*zz1*xy*theta2;
  fC44 += zz1*zz1*theta2;

  // Energy losses
  double dE=0.153e-3/beta2*(log(5940*beta2/(1-beta2)) - beta2)*d*rho;
  if (x1 < x2) dE=-dE;
  cc=fP3;
  fP3*=(1.- sqrt(p2+pm*pm)/p2*dE);
  fP2+=fX*(fP3-cc);

  return 1;
}

//_____________________________________________________________________________
int StiDefaultTrack::propagateToVertex(double x0,double rho,double pm) 
{
  //-----------------------------------------------------------------
  // This function propagates tracks to the "vertex".
  //-----------------------------------------------------------------
  double c=fP3*fX - fP2;
  double tgf=-fP2/(fP3*fP0 + sqrt(1-c*c));
  double snf=tgf/sqrt(1.+ tgf*tgf);
  double xv=(fP2+snf)/fP3;
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
  r00+=fC00; r01+=fC10; r11+=fC11;
  double det=r00*r11 - r01*r01;
  double tmp=r00; r00=r11/det; r11=tmp/det; r01=-r01/det;

  double k00=fC00*r00+fC10*r01, k01=fC00*r01+fC10*r11;
  double k10=fC10*r00+fC11*r01, k11=fC10*r01+fC11*r11;
  double k20=fC20*r00+fC21*r01, k21=fC20*r01+fC21*r11;
  double k30=fC30*r00+fC31*r01, k31=fC30*r01+fC31*r11;
  double k40=fC40*r00+fC41*r01, k41=fC40*r01+fC41*r11;

  double dy=hit->getY() - fP0;
  double dz=hit->getZ() - fP1;
  double cur=fP3 + k30*dy + k31*dz, eta=fP2 + k20*dy + k21*dz;
  if (TMath::Abs(cur*fX-eta) >= 0.99999) 
    {
      int n=getPtsCount();
      if (n>4) cerr<<n<<" StiDefaultTrack warning: Filtering failed !\n";
      return 0;
    }

  fP0 += k00*dy + k01*dz;
  fP1 += k10*dy + k11*dz;
  fP2  = eta;
  fP3  = cur;
  fP4 += k40*dy + k41*dz;

  double c01=fC10, c02=fC20, c03=fC30, c04=fC40;
  double c12=fC21, c13=fC31, c14=fC41;

  fC00-=k00*fC00+k01*fC10; fC10-=k00*c01+k01*fC11;
  fC20-=k00*c02+k01*c12;   fC30-=k00*c03+k01*c13;
  fC40-=k00*c04+k01*c14; 

  fC11-=k10*c01+k11*fC11;
  fC21-=k10*c02+k11*c12;   fC31-=k10*c03+k11*c13;
  fC41-=k10*c04+k11*c14; 

  fC22-=k20*c02+k21*c12;   fC32-=k20*c03+k21*c13;
  fC42-=k20*c04+k21*c14; 

  fC33-=k30*c03+k31*c13;
  fC43-=k30*c04+k31*c14; 

  fC44-=k40*c04+k41*c14; 

  int n=getPtsCount();
  fIndex[n]=index;
  setPtsCount(n+1);
  setChi2(getChi2()+chisq);

  return 1;
}

//_____________________________________________________________________________
int StiDefaultTrack::rotate(double alpha)
{
  //-----------------------------------------------------------------
  // This function rotates this track representation by alpha.
  //-----------------------------------------------------------------
  fAlpha += alpha;
  if (fAlpha<-TMath::Pi()) fAlpha += 2*TMath::Pi();
  if (fAlpha>=TMath::Pi()) fAlpha -= 2*TMath::Pi();
  
  double x1=fX, y1=fP0;
  double ca=cos(alpha), sa=sin(alpha);
  double r1=fP3*fX - fP2;
  
  fX = x1*ca + y1*sa;
  fP0=-x1*sa + y1*ca;
  fP2=fP2*ca + (fP3*y1 + sqrt(1.- r1*r1))*sa;
  
  double r2=fP3*fX - fP2;
  if (TMath::Abs(r2) >= 0.99999) {
    int n=getPtsCount();
    if (n>4) cerr<<n<<" StiDefaultTrack warning: Rotation failed !\n";
    return 0;
  }
  
  double y0=fP0 + sqrt(1.- r2*r2)/fP3;
  if ((fP0-y0)*fP3 >= 0.) {
    int n=getPtsCount();
    if (n>4) cerr<<n<<" StiDefaultTrack warning: Rotation failed !!!\n";
    return 0;
  }

  //f = F - 1
  double f00=ca-1;
  double f23=(y1 - r1*x1/sqrt(1.- r1*r1))*sa;
  double f20=fP3*sa;
  double f22=(ca + sa*r1/sqrt(1.- r1*r1))-1;
  
  //b = C*ft
  double b00=fC00*f00, b02=fC00*f20+fC30*f23+fC20*f22;
  double b10=fC10*f00, b12=fC10*f20+fC31*f23+fC21*f22;
  double b20=fC20*f00, b22=fC20*f20+fC32*f23+fC22*f22;
  double b30=fC30*f00, b32=fC30*f20+fC33*f23+fC32*f22;
  double b40=fC40*f00, b42=fC40*f20+fC43*f23+fC42*f22;

  //a = f*b = f*C*ft
  double a00=f00*b00, a02=f00*b02, a22=f20*b02+f23*b32+f22*b22;

  // *** double dy2=fCyy;

  //F*C*Ft = C + (a + b + bt)
  fC00 += a00 + 2*b00;
  fC10 += b10;
  fC20 += a02+b20+b02;
  fC30 += b30;
  fC40 += b40;
  fC21 += b12;
  fC32 += b32;
  fC22 += a22 + 2*b22;
  fC42 += b42; 

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
  cc[0 ]=fC00;
  cc[1 ]=fC10;  cc[2 ]=fC11;
  cc[3 ]=fC20;  cc[4 ]=fC21;  cc[5 ]=fC22;
  cc[6 ]=fC30;  cc[7 ]=fC31;  cc[8 ]=fC32;  cc[9 ]=fC33;
  cc[10]=fC40;  cc[11]=fC41;  cc[12]=fC42;  cc[13]=fC43;  cc[14]=fC44;
}

//_____________________________________________________________________________
void StiDefaultTrack::addHit(StHit * hit)
{
  // Add given hit as a child of the last node known to this track
  // and subsequntly make this hit the last hit of the track.
  if (hit)
    {
      StiDefaultMutableTreeNode * node = treeNodeFactory->getTreeNode();
      if (node)
	{
	  if (lastHit)
	    {
	      node->reset();  
	      node->setUserObject(hit);
	      lastHit->add(node);
	      lastHit = node;
	    }
	  else
	    {
	      // no lastHit yet, this node is both first and last
	      lastHit  = node;
	      firstHit = node;
	    }
	}
      else
	{
	  cout << "StiDefaultTrack::addHit(StHit * hit) - SEVERE ERROR" << endl
	       << "  >>>   Could not obtain TreeNode object to insert given hit" << endl;
	}
    }
  else
    {
      cout << "StiDefaultTrack::addHit(StHit * hit) - SEVERE ERROR" << endl
	   << "  >>>   Given hit is null" << endl;
    }
}

//_____________________________________________________________________________
void StiDefaultTrack::addHitToNode(StHit * hit, StiMutableTreeNode * node)
{
  // Add given hit as a child of the last node known to this track
  // and subsequntly make this hit the last hit of the track.
  if (hit && node)
    {
      StiDefaultMutableTreeNode * newNode = treeNodeFactory->getTreeNode();
      if (newNode)
	{
	  newNode->reset();  
	  newNode->setUserObject(hit);
	  node->add(newNode);
	  // if node is actually last node, make newNode the last node. 
	  if (node==lastHit)
	      lastHit = newNode;
	}
    }
  else
    {
      cout << "StiDefaultTrack::addHit(StHit * hit, StiMutableTreeNode * node) - SEVERE ERROR" << endl
	   << "  >>>   Given hit or node is null pointer" << endl;
    }
}
  
//_____________________________________________________________________________
void StiDefaultTrack::addHitAsParent(StHit * hit, StiMutableTreeNode * node)
{
  // Add a hit as a parent to given node. A new node is obtain from the factory to 
  // contain the given hit. This new node is added as child to the parent of given
  // node. Given node is removed from its parent. Given node is added as child to
  // new node. This is effectively an insert before node operation.
  if (hit && node)
    {
      StiDefaultMutableTreeNode * newNode = treeNodeFactory->getTreeNode();
      if (newNode)
	{
	  newNode->reset();  
	  newNode->setUserObject(hit);

	}
    }
  else
    {
      cout << "StiDefaultTrack::addHitAsParent(StHit * hit, StiMutableTreeNode * node) - SEVERE ERROR" << endl
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
  firstHit = 0;
  lastHit  = 0;
}

void StiDefaultTrack::calculateSvtDedx()
{
  //svtDedx = svtDedxCalculator->getDedx(nPts, dedxSamples);
}

void StiDefaultTrack::calculateTpcDedx()
{
  //svtDedx = svtDedxCalculator->getDedx(nPts, dedxSamples);
}

