//Std
#include <iostream.h>
#include <stdexcept>
#include <math.h>
using namespace std;

//Sti
#include "StiDebug.h"
#include "Messenger.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiMaterial.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiKalmanTrackNode.h"
#include "StiMaterialInteraction.h"

//_____________________________________________________________________________
// Local Track Model
//
// x[0] = y  ordinate
// x[1] = z  position along beam axis
// x[2] = eta=C*x0
// x[3] = C  (local) curvature of the track
// x[4] = tan(l) 

// initialize static vairables
bool StiKalmanTrackNode::recurse = false;
bool StiKalmanTrackNode::elossCalculated = false;
bool StiKalmanTrackNode::mcsCalculated   = false;
double StiKalmanTrackNode::kField = 0.5;
double StiKalmanTrackNode::massHypothesis = 0.13957018;


int StiKalmanTrackNode::minContiguousHitCountForNullReset = 2;
int StiKalmanTrackNode::maxNullCount = 40;  
int StiKalmanTrackNode::maxContiguousNullCount = 25;

int    StiKalmanTrackNode::shapeCode = 0;
double StiKalmanTrackNode::x1=0;
double StiKalmanTrackNode::x2= 0; 
double StiKalmanTrackNode::y1= 0; 
double StiKalmanTrackNode::z1= 0; 
double StiKalmanTrackNode::dx= 0; 
double StiKalmanTrackNode::r1= 0; 
double StiKalmanTrackNode::r2= 0; 
double StiKalmanTrackNode::c1= 0; 
double StiKalmanTrackNode::c2= 0; 
double StiKalmanTrackNode::c1sq= 0; 
double StiKalmanTrackNode::c2sq= 0; 
double StiKalmanTrackNode::x0= 0; 
double StiKalmanTrackNode::y0= 0; 
double StiKalmanTrackNode::density = 0;
double StiKalmanTrackNode::gasDensity= 0;
double StiKalmanTrackNode::matDensity= 0;
double StiKalmanTrackNode::gasRL= 0;
double StiKalmanTrackNode::matRL= 0;
double StiKalmanTrackNode::radThickness= 0;
const StiDetector * StiKalmanTrackNode::det = 0;
const StiPlanarShape *StiKalmanTrackNode::planarShape = 0;
const StiCylindricalShape *StiKalmanTrackNode::cylinderShape = 0;
StiMaterial * StiKalmanTrackNode::gas = 0;
StiMaterial * StiKalmanTrackNode::prevGas = 0;
StiMaterial * StiKalmanTrackNode::mat = 0;
StiMaterial * StiKalmanTrackNode::prevMat = 0;

//_____________________________________________________________________________
void StiKalmanTrackNode::reset()
{ 
    // Base class reset
    StiTrackNode::reset();
    // Reference angle
    fAlpha= 0.;
    // Reference position
    fX    = 0.;
    // Track State at this node
    fP0   = 0.;
    fP1   = 0.;
    fP2   = 0.;
    fP3   = 0.;
    fP4   = 0.;
    // covariance error matrix
    fC00  = 0.;
    fC10  = 0.; fC11  = 0.;
    fC20  = 0.; fC21  = 0.; fC22  = 0.;
    fC30  = 0.; fC31  = 0.; fC32  = 0.; fC33  = 0.;
    fC40  = 0.; fC41  = 0.; fC42  = 0.; fC43  = 0.; fC44  = 0.;
    // fit quality
    fChi2  = 0;
    // dedx information
    fdEdx = 0.;
    // path length for this node
    pathLength = 0.;
    hitCount = 0;
    nullCount = 0;
    contiguousHitCount = 0;
    contiguousNullCount = 0;
}


//_____________________________________________________________________________
void StiKalmanTrackNode::set(int   depth,
			     StiHit * hit,
			     const double alpha,
			     const double xRef,
			     const double xx[5], 
			     const double cc[15], 
			     const double dEdx,
			     const double chi2)
{
    StiTrackNode::set(depth, hit);
    fAlpha  = alpha;
    if (fAlpha < -M_PI) fAlpha += 2*M_PI;
    if (fAlpha >= M_PI) fAlpha -= 2*M_PI;
    fX      = xRef;
    fdEdx   = dEdx;
    fChi2   = chi2;
    fP0=xx[0];   fP1=xx[1];   fP2=xx[2];   fP3=xx[3];   fP4=xx[4];  // covariance error matrix
    fC00=cc[0];  fC10=cc[1];  fC11=cc[2];
    fC20=cc[3];  fC21=cc[4];  fC22=cc[5];
    fC30=cc[6];  fC31=cc[7];  fC32=cc[8];  fC33=cc[9];
    fC40=cc[10]; fC41=cc[11]; fC42=cc[12]; fC43=cc[13]; fC44=cc[14];
}

void StiKalmanTrackNode::setState(const StiKalmanTrackNode * n)
{
    fAlpha = n->fAlpha;
    if (fAlpha < -M_PI) fAlpha += 2*M_PI;
    if (fAlpha >= M_PI) fAlpha -= 2*M_PI;
    fX   = n->fX;
    // state matrix
    fP0  = n->fP0; fP1  = n->fP1; fP2  = n->fP2; fP3  = n->fP3; fP4  = n->fP4;
    // covariance error matrix
    fC00 = n->fC00;
    fC10 = n->fC10; fC11 = n->fC11;
    fC20 = n->fC20; fC21 = n->fC21; fC22 = n->fC22;
    fC30 = n->fC30; fC31 = n->fC31; fC32 = n->fC32; fC33 = n->fC33;
    fC40 = n->fC40; fC41 = n->fC41; fC42 = n->fC42; fC43 = n->fC43; fC44 = n->fC44;
}


//_____________________________________________________________________________
void StiKalmanTrackNode::setAsCopyOf(const StiKalmanTrackNode * n)
{
    StiTrackNode::setAsCopyOf(n);
    fX    = n->fX;
    fAlpha= n->fAlpha;
    if (fAlpha < -M_PI) fAlpha += 2*M_PI;
    if (fAlpha >= M_PI) fAlpha -= 2*M_PI;
    fChi2  = n->fChi2;
    fP0   = n->fP0; fP1   = n->fP1; fP2   = n->fP2; fP3   = n->fP3; fP4   = n->fP4;
    // covariance error matrix
    fC00  = n->fC00;
    fC10  = n->fC10; fC11  = n->fC11;
    fC20  = n->fC20; fC21  = n->fC21; fC22  = n->fC22;
    fC30  = n->fC30; fC31  = n->fC31; fC32  = n->fC32; fC33  = n->fC33;
    fC40  = n->fC40; fC41  = n->fC41; fC42  = n->fC42; fC43  = n->fC43; fC44  = n->fC44;
    fdEdx = n->fdEdx;
    pathLength = n->pathLength;
    targetDet  = n->targetDet;
    hitCount   = n->hitCount;
    nullCount  = n->nullCount;
    contiguousHitCount  =  n->contiguousHitCount;
    contiguousNullCount =  n->contiguousNullCount;
}

//_____________________________________________________________________________
void StiKalmanTrackNode::get(double& alpha,
			     double& xRef,
			     double  x[5], 
			     double  e[15], 
			     double& dEdx,
			     double& chi2)
{
    /** returns the node information
	double& alpha : angle of the local reference frame
	double& xRef  : refence position of this node in the local frame
	double x[5],  : state, for a definition, see the top of this file
	double cc[15] : error matrix of the state "x"
	double& dEdx  : energy loss info
	double& chi2) : chi2 of the track at this node
    */
    alpha = fAlpha;
    xRef  = fX;
    x[0] = fP0; x[1] = fP1; x[2] = fP2; x[3] = fP3; x[4] = fP4;
    e[0] = fC00;
    e[1] = fC10; e[2] = fC11;
    e[3] = fC20; e[4] = fC21; e[5] = fC22; 
    e[6] = fC30; e[7] = fC31; e[8] = fC32; e[9] = fC33;
    e[10] = fC40; e[11] = fC41; e[12] = fC42; e[13] = fC43; e[14] = fC44;
    dEdx = fdEdx;
    chi2 = fChi2;
}


//_____________________________________________________________________________
void StiKalmanTrackNode::getMomentum(double p[3], double e[6]) const
{	
    // returns momentum and its error matrix 
    // in cartesian coordinates in the _local_
    // ref frame of this node.
    // p[0] = px  - outward
    // p[1] = py  - 
    // p[2] = pz  - along beam axis
    
    double pt, sinPhi;
    pt = getPt();
    sinPhi = fP3*fX-fP2;
    double ss = sinPhi*sinPhi;
    if (ss>1.)
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage))
		<< "StiKalmanTrackNode::getMomentum - ERROR - sin(phi)^2 > 1" << endl
		<< " fP3/fx/fP2/sin(phi):" << fP3 << "\t" << fX << "\t" 
		<< fP2 << "\t" << sinPhi << endl;
#endif
	    ss = 1.;
	}
    p[0] = pt*sqrt(1-ss);
    p[1] = pt*sinPhi;
    p[2] = pt*fP4;
    
    if (e==0)
	return;
    double sa = 1-ss;
    if (sa<0)
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) 
		<< "StiKalmanTrackNode::getMomentum() - Error - sa<0 - Value was:" 
		<< sa << " - reset to sa=0." << endl;
#endif
	    sa = 0.;
	}
    double c = fP3;
    if (c==0) c=1e-12;
    double cc = c*c;
    sa = sqrt(sa);
    // should I include a factor of 0.3 here???????????????
    double a00=kField*(fX-fP2/c)/sa;
    double a01=-kField*(fP2*fP2-fX*fP2*c-1)/(cc*sa);
    double a02=0;
    double a10=-kField/c; 
    double a11=kField*fP2/(cc);
    double a12=0;
    double a20=0;
    double a21=-kField*fP4/c;
    double a22=kField/c;
    // original error matrix
    double b00=fC22, b01=fC32, b02=fC42;
    double b10=fC32, b11=fC33, b12=fC43;
    double b20=fC42, b21=fC43, b22=fC44;
    // intermediate results matrices
    double c00, c01, c02;
    double c10, c11, c12;
    double c20, c21, c22;
    double d00, d01, d02;
    double      d11, d12;
    double           d22;
    // C=A*B
    c00 = a00*b00+a01*b10+a02*b20;
    c01 = a00*b01+a01*b11+a02*b21;
    c02 = a00*b02+a01*b12+a02*b22;
    
    c10 = a10*b00+a11*b10+a12*b20;
    c11 = a10*b01+a11*b11+a12*b21;
    c12 = a10*b02+a11*b12+a12*b22;
    
    c20 = a20*b00+a21*b10+a22*b20;
    c21 = a20*b01+a21*b11+a22*b21;
    c22 = a20*b02+a21*b12+a22*b22;
    // D=C*At 
    d00 = c00*a00+c01*a01+c02*a02;
    d01 = c00*a10+c01*a11+c02*a12;
    d02 = c00*a20+c01*a21+c02*a22;
    //d10 = c10*a00+c11*a01+c12*a02;
    d11 = c10*a10+c11*a11+c12*a12;
    d12 = c10*a20+c11*a21+c12*a22;
    //d20 = c20*a00+c21*a01+c22*a02;
    //d21 = c20*a10+c21*a11+c22*a12;
    d22 = c20*a20+c21*a21+c22*a22;
    
    e[0] = d00;  // px-px
    e[1] = d01;  // px-py
    e[2] = d02;  // px-pz
    e[3] = d11;  // py-py
    e[4] = d12;  // py-pz
    e[5] = d22;  // pz-pz
}

//_____________________________________________________________________________
void StiKalmanTrackNode::getGlobalMomentum(double p[3], double e[6]) const
{	
    // returns momentum and its error matrix 
    // in cartesian coordinates in the _global_
    // ref frame of the experiment
    // p[0] = px
    // p[1] = py
    // p[2] = pz
    // e[0] = // px-px
    // e[1] = // px-py
    // e[2] = // px-pz
    // e[3] = // py-py
    // e[4] = // py-pz
    // e[5] = // pz-pz
    
    // first get p & e in the local ref frame
    getMomentum(p,e);
    
    // now rotate the p & e in the global ref frame
    // for the time being, assume an azimuthal rotation 
    // by alpha is sufficient.
    // transformation matrix - needs to be set
    double ca = cos(fAlpha);
    double sa = sin(fAlpha);
    double a00=ca, a01=-sa, a02=0;
    double a10=sa, a11= ca, a12=0;
    double a20= 0, a21=  0, a22=1.;
    
    double px=p[0];
    double py=p[1];
    double pz=p[2];
    p[0] = a00*px + a01*py + a02*pz;
    p[1] = a10*px + a11*py + a12*pz;
    p[2] = a20*px + a21*py + a22*pz;
    
    // original error matrix
    double b00=e[0], b01=e[1], b02=e[2];
    double b10=e[1], b11=e[3], b12=e[4];
    double b20=e[2], b21=e[4], b22=e[5];
    // intermediate results matrices
    double c00, c01, c02;
    double c10, c11, c12;
    double c20, c21, c22;
    double d00, d01, d02;
    double      d11, d12;
    double           d22;
    // C=A*B
    c00 = a00*b00+a01*b10+a02*b20;
    c01 = a00*b01+a01*b11+a02*b21;
    c02 = a00*b02+a01*b12+a02*b22;
    
    c10 = a10*b00+a11*b10+a12*b20;
    c11 = a10*b01+a11*b11+a12*b21;
    c12 = a10*b02+a11*b12+a12*b22;
    
    c20 = a20*b00+a21*b10+a22*b20;
    c21 = a20*b01+a21*b11+a22*b21;
    c22 = a20*b02+a21*b12+a22*b22;
    // D=C*At
    d00 = c00*a00+c01*a01+c02*a02;
    d01 = c00*a10+c01*a11+c02*a12;
    d02 = c00*a20+c01*a21+c02*a22;
    //d10 = c10*a00+c11*a01+c12*a02;
    d11 = c10*a10+c11*a11+c12*a12;
    d12 = c10*a20+c11*a21+c12*a22;
    //d20 = c20*a00+c21*a01+c22*a02;
    //d21 = c20*a10+c21*a11+c22*a12;
    d22 = c20*a20+c21*a21+c22*a22;
    
    e[0] = d00;  // px-px
    e[1] = d01;  // px-py
    e[2] = d02;  // px-pz
    e[3] = d11;  // py-py
    e[4] = d12;  // py-pz
    e[5] = d22;  // pz-pz
}

double StiKalmanTrackNode::getTanL() const
{
    return fP4;
}

double StiKalmanTrackNode::getPt() const
{
    double c;
    c = fabs(fP3);
    if (c<1e-12) 
	return 0.003e12*kField;
    else
	return 0.003*kField/c;
}



int StiKalmanTrackNode::propagate(StiKalmanTrackNode *pNode, 
				  const StiDetector * tDet)
{
  det = tDet;
  int position = 0;
  setState(pNode);
  StiPlacement * place = tDet->getPlacement();
  double tAlpha = place->getNormalRefAngle();
  if (tAlpha < -M_PI) tAlpha += 2*M_PI;
  if (tAlpha >= M_PI) tAlpha -= 2*M_PI;
    
  double dAlpha = tAlpha - fAlpha;
  if (fabs(dAlpha)>1e-4)   // perform rotation if needed
    rotate(dAlpha);
  StiShape * sh = tDet->getShape();
  planarShape = 0;
  cylinderShape = 0;
  shapeCode = sh->getShapeCode();
    
  x1=fX;  // current
  y1=fP0;		z1=fP1;		c1=fP3*x1 - fP2;
  c1sq = c1*c1; 
  if (c1sq>=1.) 
    {
#ifdef DEBUG
      *(Messenger::instance(MessageType::kNodeMessage)) << "c1sq:" << c1sq << endl;
#endif
      throw runtime_error("SKTN::propagate() - c1sq>=1");
    }
  r1=sqrt(1.-c1sq);		
    
  switch (shapeCode)
    {
    case kPlanar:
      {
	//cout << "SKTN::propagate() Planar detector encountered." << endl;
	x2= place->getNormalRadius(); // target
	break;
      }
    case kCylindrical:
      {
	double x_p, x_m, R, L, x0, y0,r0sq, a, b, sq;
	//cout << "SKTN::propagate() Cylindrical detector encountered." << endl;
	//cout << "fP0,fP3,c1sq:"<<fP0<<"\t"<<fP3<<"\t"<<c1sq<<endl;
	if (fP3>0)
	  y0 = fP0+r1/fP3;
	else if (fP3<0)
	  y0 = fP0-r1/fP3;
	else
	  return -1;
	R = 1/fP3;
	L = place->getNormalRadius(); // target
		
	x0 = fP2/fP3;
	//cout << "R,L,x0,y0:"<<R<< "\t" << L<<"\t"<< x0 << "\t" << y0 << endl;
	r0sq= x0*x0+y0*y0;
	if (r0sq<=0.)
	  {
	    cout << "SKTN::propagate() - r0sq<=0" << endl;
	    return -1;
	  }
	a = 0.5*(r0sq+L*L-R*R);
	if (a<=0.)
	  {
	    cout << "SKTN::propagate() - a<=0" << endl;
	    return -1;
	  }
	b = L*L/(a*a);
	sq = b*r0sq-1;
	if (sq<0)
	  {
	    cout << "SKTN::propagate() - sq<0" << endl;
	    return -1;
	  }
	sq = sqrt(sq);				
	x_p = a*(x0+y0*sq)/r0sq;
	//cout << "x_p:"<< x_p << endl;
	if (x_p>0)
	  x2 = x_p;
	else 
	  {
	    x_m = a*(x0-y0*sq)/r0sq;
	    //cout << "x_m:"<< x_m << endl;
	    if (x_m>0)
	      x2 = x_m;
	    else
	      return -1; // no suitable solution 
	  }
	/*double pathLength;
	  position =  StiMaterialInteraction::findIntersection(pNode,tDet,x2,radThickness,density,pathLength);	
	  if (position<5) propagate(x2,radThickness,density);
	*/
	break;
      }
    case kConical:
      {
#ifdef DEBUG
	*(Messenger::instance(MessageType::kNodeMessage)) 
	  << "SKTN::propagate() - Encountered Conical Volume "
	  << "- Option not currently supported - abort track" << endl;
#endif
	throw runtime_error("SKTN::propagate() - c1sq>=1");
      }
    }
  dx=x2-x1;
  c2=fP3*x2 - fP2; 
  c2sq = c2*c2; 
  if (c2sq>=1.) 
    {
#ifdef DEBUG
      *(Messenger::instance(MessageType::kNodeMessage)) << "c2sq>=1 value:" << c2sq << endl;
#endif
      throw runtime_error("SKTN::propagate() - c2sq>=1");
    }
  r2=sqrt(1.- c2sq );	
  double cSum = c1+c2;
	
  fP0 += dx*cSum/(r1+r2);
  double dddd = c1*r2 + c2*r1;
  if (fabs(dddd)==0)
    {
#ifdef DEBUG
      *(Messenger::instance(MessageType::kNodeMessage)) 
	<< "StiKalmanTrackNode::propagate() - dddd: " << dddd << endl;
#endif
      throw runtime_error("SKTN::propagate() - fabs(dddd)==0.");
    }
  fP1 += dx*fP4*cSum/dddd;
  fX=x2;
	
  // intersection?
  double yOff, yAbsOff, detHW, detHD,	edge,innerY, outerY, innerZ, outerZ, zOff, zAbsOff;
	
  yOff = fP0 - place->getNormalYoffset();
  yAbsOff = fabs(yOff);
  zOff = fP1 - place->getZcenter();
  zAbsOff = fabs(zOff);
  switch (shapeCode)
    {
    case kPlanar:
      {
	StiPlanarShape * planarShape = static_cast<StiPlanarShape *>(sh);
	detHW = planarShape->getHalfWidth();
	detHD = planarShape->getHalfDepth();
	edge  = 4.;//shape->getEdgeHalfWidth();
	break;
      }
    case kCylindrical:
      {
	StiCylindricalShape * cylinderShape = static_cast<StiCylindricalShape *>(sh);
	detHW = 100.; // will never be outside
	detHD = cylinderShape->getHalfDepth();
	edge  = 4.;//shape->getEdgeHalfWidth();
	break;
      }
    default:
      {
#ifdef DEBUG
	*(Messenger::instance(MessageType::kNodeMessage)) 
	  << "SKTN::propagate() - Severe Error" << endl;
#endif
	throw runtime_error("SKTN::propagate() - Bad shape code");
      }
    }
  innerY = detHW - edge;
  outerY = innerY + 2*edge;
  innerZ = detHD - edge;
  outerZ = innerZ + 2*edge;
#ifdef DEBUG
  *(Messenger::instance(MessageType::kNodeMessage)) 
    << tDet->getName() << ":" << endl
    << " innerY:"  << innerY
    << " outerY:"  << outerY
    << " innerZ:"  << innerZ
    << " outerZ:"  << outerZ << endl
    << " yOffset:" << yOff 
    << " zOffset:" << zOff << endl;
#endif
  if (yAbsOff<innerY && zAbsOff<innerZ)
    position = kHit; 
  else if (yAbsOff>outerY && (yAbsOff-outerY)>(zAbsOff-outerZ))
    // outside detector to positive or negative y (phi)
    position = yOff>0 ? kMissPhiPlus : kMissPhiMinus;
  else if (zAbsOff>outerZ && (zAbsOff-outerZ)>(yAbsOff-outerY))
    // outside detector to positive or negative z (west or east)
    position = zOff>0 ? kMissZplus : kMissZminus;
  else if ((yAbsOff-innerY)>(zAbsOff-innerZ))
    // positive or negative phi edge
    position = yOff>0 ? kEdgePhiPlus : kEdgePhiMinus;
  else
    // positive or negative z edge
    position = zOff>0 ? kEdgeZplus : kEdgeZminus;
	
  //cout << "position:"<< position<< endl;
	
  // bail out if this projection is not "interesting"
  if (position>kEdgeZplus || position<0)
    return position;

  //f = F - 1
  double rr=r1+r2, cc=c1+c2, xx=x1+x2;
  double f02=-dx*(2*rr + cc*(c1/r1 + c2/r2))/(rr*rr);
  double f03= dx*(rr*xx + cc*(c1*x1/r1+c2*x2/r2))/(rr*rr);
  double cr=c1*r2+c2*r1;
  double f12=-dx*fP4*(2*cr + cc*(c2*c1/r1-r1 + c1*c2/r2-r2))/(cr*cr);
  double f13=dx*fP4*(cr*xx-cc*(r1*x2-c2*c1*x1/r1+r2*x1-c1*c2*x2/r2))/(cr*cr);
  double f14= dx*cc/cr; 
    
  //b = C*ft
  double b00=f02*fC20 + f03*fC30;
  double b01=f12*fC20 + f13*fC30 + f14*fC40;
  double b10=f02*fC21 + f03*fC31;
  double b11=f12*fC21 + f13*fC31 + f14*fC41;
  double b20=f02*fC22 + f03*fC32;
  double b21=f12*fC22 + f13*fC32 + f14*fC42;
  double b30=f02*fC32 + f03*fC33;
  double b31=f12*fC32 + f13*fC33 + f14*fC43;
  double b40=f02*fC42 + f03*fC43;
  double b41=f12*fC42 + f13*fC43 + f14*fC44;
    
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
    
  // Multiple scattering
  if (mcsCalculated)
    {
      prevGas = gas;
      prevMat = mat;
      gas = det->getGas();
      mat = det->getMaterial();
      if (prevGas!=gas || prevMat!=mat)
	{
	  gasDensity = gas->getDensity();
	  matDensity = mat->getDensity();
	  gasRL      = gas->getRadLength();
	  matRL      = mat->getRadLength();
	}
      double detHT, gapT, cosLinv,s;
      switch (shapeCode)
	{
	case kPlanar:
	  {
	    detHT = 0.5*planarShape->getThickness();
	    gapT = dx-2*detHT;
	    cosLinv = sqrt(1.+fP4*fP4);
	    s = asin(fP3*dx/2)*cosLinv/fP3;
	    break;
	  }
	case kCylindrical:
	  {
	    detHT = 0.;
	    gapT  = 0.;
	    s=1.;
	  }
	}
      density = (gasDensity*gapT + 2*matDensity*detHT)/s;
      radThickness  = s*(gapT/gasRL+2*detHT/matRL)/dx;
      if (density<0) // problem abort MCS
	return;
      double d=sqrt((x1-fX)*(x1-fX)
		    +(y1-fP0)*(y1-fP0)  +(z1-fP1)*(z1-fP1));
      double tanl  = fP4;
      double pt = getPt();
      double p2=(1.+tanl*tanl)*pt*pt;
      double beta2=p2/(p2 + massHypothesis*massHypothesis);
      double theta2=14.1*14.1/(beta2*p2*1e6)*d/radThickness*density;
      //double theta2=1.0259e-6*10*10/20/(beta2*p2)*d*density;
      double ey=fP3*fX - fP2, ez=fP4;
      double xz=fP3*ez, zz1=ez*ez+1, xy=fP2+ey;
      fC33 = fC33 + xz*xz*theta2;
      fC32 = fC32 + xz*ez*xy*theta2;
      fC43 = fC43 + xz*zz1*theta2;
      fC22 = fC22 + (2*ey*ez*ez*fP2+1-ey*ey+ez*ez+
		     fP2*fP2*ez*ez)*theta2;
      fC42 = fC42 + ez*zz1*xy*theta2;
      fC44 = fC44 + zz1*zz1*theta2;
      // Energy losses
      if (elossCalculated)
	{
	  double dE=0.153e-3/beta2*(log(5940*beta2/(1-beta2)) - beta2)*d*density;
	  if (x1 < x2) dE=-dE;
	  cc=fP3;
	  //cout << "ELOSS: c:" << cc;
	  fP3 = fP3 *(1.- sqrt(p2+massHypothesis*massHypothesis)/p2*dE);
	  //cout << " c':" << fP3 << endl;
	  fP2 = fP2 + fX*(fP3-cc);
	}
    }
  return position;
}

double  StiKalmanTrackNode::evaluateDedx()
{
    if (hit)
	{
	    // node has a hit
	    if (pathLength>0)
		{
		    double eloss = hit->getEloss();
		    if (eloss>0)
			// eloss of hit is valid
			dedx = eloss/pathLength;
		    else
			// elos of hit is not valid
			dedx = -3; // signals error condition/absence of data.
		}
	    else
		// path length is less than "0"
		dedx = -1.; // signals error condition!
		}
    else
	{
	    // node has no hit and is not	a measurement node
	    dedx = -2; 
	}
    return dedx;
}


void  StiKalmanTrackNode::propagate(double xk)//, double _radThickness,double _density)
{
    x1=fX;
    x2=x1+(xk-x1);
    //radThickness = _radThickness;
    //density = _density;
    dx=x2-x1;
    y1=fP0;
    z1=fP1;
    c1=fP3*x1 - fP2;
    double c1sq = c1*c1; 
    if (c1sq>=1.) 
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) << "c1sq:" << c1sq << endl;
#endif
	    throw runtime_error("SKTN::propagate() - c1sq>=1");
	}
    c2=fP3*x2 - fP2; 
    double c2sq = c2*c2; 
    if (c2sq>=1.) 
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) << "c2sq>=1 value:" << c2sq << endl;
#endif
	    throw runtime_error("SKTN::propagate() - c2sq>=1");
	}
    double cSum = c1+c2;
    r1=sqrt(1.- c1sq );
    r2=sqrt(1.- c2sq );
    fP0 += dx*cSum/(r1+r2);
    double dddd = c1*r2 + c2*r1;
    if (fabs(dddd)==0)
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) 
		<< "StiKalmanTrackNode::propagate() - dddd: " << dddd << endl;
#endif
	    throw runtime_error("SKTN::propagate() - fabs(dddd)==0.");
	}
    fP1 += dx*fP4*cSum/dddd;
    fX=x2;
}


StThreeVector<double> StiKalmanTrackNode::getPointAt(double xk) const
{
    x1=fX;
    x2=x1+(xk-x1);
    dx=x2-x1;
    y1=fP0;
    z1=fP1;
    c1=fP3*x1 - fP2;
    double c1sq = c1*c1; 
    if (c1sq>=1.) 
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) << "c1sq:" << c1sq << endl;
#endif
	    throw runtime_error("SKTN::propagate() - c1sq>=1");
	}
    c2=fP3*x2 - fP2; 
    double c2sq = c2*c2; 
    if (c2sq>=1.) 
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) << "c2sq>=1 value:" << c2sq << endl;
#endif
	    throw runtime_error("SKTN::propagate() - c2sq>=1");
	}
    double cSum = c1+c2;
    r1=sqrt(1.- c1sq );
    r2=sqrt(1.- c2sq );

    double xx, yy, zz, alpha, ca, sa;
    double gx, gy;
    xx = x2;
    yy = fP0 + dx*cSum/(r1+r2);
    double dddd = c1*r2 + c2*r1;
    if (fabs(dddd)==0)
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) 
		<< "StiKalmanTrackNode::propagate() - dddd: " << dddd << endl;
#endif
	    throw runtime_error("SKTN::propagate() - fabs(dddd)==0.");
	}
    zz = fP1 + dx*fP4*cSum/dddd;
    ca = cos(alpha);
    sa = sin(alpha);
    gx = ca*xx-sa*yy;
    gy = sa*xx+ca*yy;
    return (StThreeVector<double>(gx,gy, zz));
}



double 
StiKalmanTrackNode::evaluateChi2() 	//throw ( Exception)
{
    //-----------------------------------------------------------------
    // This function calculates a chi2 increment given the track state
    // and the hit currently stored. The chi2 is stored in this node.
    // The methods return the increment for convenience.
    //
    // return : increment in chi2 implied by the node/hit assocition.
    //-----------------------------------------------------------------
    // Update Measurement Error Matrix, calculate its determinant
    if (isnan(fC00)) {
#ifdef DEBUG
	*(Messenger::instance(MessageType::kNodeMessage)) << "SKTN::evaluateChi2() fC00 NaN " << endl;
#endif
    }
    if (isnan(fC10)) {
#ifdef DEBUG
	*(Messenger::instance(MessageType::kNodeMessage)) << "SKTN::evaluateChi2() fC10 NaN " << endl;
#endif
    }
    if (isnan(fC11)) {
#ifdef DEBUG
	*(Messenger::instance(MessageType::kNodeMessage)) << "SKTN::evaluateChi2() fC11 NaN " << endl;
#endif
    }
    
    double r00=hit->syy()+fC00;
    double r01=hit->syz()+fC10;
    double r11=hit->szz()+fC11;
    double det=r00*r11 - r01*r01;
    
#ifdef DEBUG
    *(Messenger::instance(MessageType::kNodeMessage)) 
	<< "chi2()  fC00;fC10;fc11:" << fC00 << "\t" << fC10 << "\t" << fC11 << endl
	<< "           syy;syz;szz:" << hit->syy() << "\t" << hit->syz() << "\t" << hit->szz() << "\t" << det << endl
	<< "       r00;r01;r11;det:" << r00 << "\t" << r01 << "\t" << r11 << "\t" << det << endl;
#endif
    if (fabs(det)==0.)
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) <<"StiKalmanTrackNode::evaluateChi2(). ERROR:\t";
	    *(Messenger::instance(MessageType::kNodeMessage)) <<"det test failed line 535.  return 0."<<endl;
#endif
	    throw runtime_error("SKTN::evaluateChi2() Singular matrix !\n");
	}
    // inverse matrix
    double tmp=r00; r00=r11; r11=tmp; r01=-r01;  
    double dy=hit->y() - fP0;
    double dz=hit->z() - fP1;
    double chi2inc = (dy*r00*dy + 2*r01*dy*dz + dz*r11*dz)/det;
    fChi2 += chi2inc;
    return chi2inc;
}

void StiKalmanTrackNode::updateNode() //throw (Exception)
{
    // Updates this node with the information 
    // from its associated hit. The given chisq is set as the chi2 
    // of this node.
    //__________________________________________________________________
    // Update Measurement Error Matrix, calculate its determinant
    if (hit==0)		
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) 
		<< "StiKalmanTrackNode::updateNode(). ERROR:\t"
		<<" - Null HIT, line 558.  return" << endl;
#endif
	    throw runtime_error("SKTN::updateNode() - Error - No hit associated to node!\n");
	}
    double r00=hit->syy()+fC00;
    double r01=hit->syz()+fC10;
    double r11=hit->szz()+fC11;
    double det=r00*r11 - r01*r01;
    if (fabs(det)==0)
	throw runtime_error("SKTN::updateNode() - Singular matrix; fabs(det)==0 !\n");
    // inverse matrix
    double tmp=r00; r00=r11/det; r11=tmp/det; r01=-r01/det;
    // update error matrix
    double k00=fC00*r00+fC10*r01, k01=fC00*r01+fC10*r11;
    double k10=fC10*r00+fC11*r01, k11=fC10*r01+fC11*r11;
    double k20=fC20*r00+fC21*r01, k21=fC20*r01+fC21*r11;
    double k30=fC30*r00+fC31*r01, k31=fC30*r01+fC31*r11;
    double k40=fC40*r00+fC41*r01, k41=fC40*r01+fC41*r11;
    
    double dy  = hit->y() - fP0;
    double dz  = hit->z() - fP1;
    double cur = fP3 + k30*dy + k31*dz;
    double eta = fP2 + k20*dy + k21*dz;
    double ddd = cur*fX-eta;
    if (fabs(ddd) >= 1.)
	{
#ifdef DEBUG
	    *(Messenger::instance(MessageType::kNodeMessage)) << "StiKalmanTrackNode::updateNode(). ERROR:\t";
	    *(Messenger::instance(MessageType::kNodeMessage)) <<" - extrapolation failed line 588. return"<< endl;
#endif
	    throw runtime_error("SKTN:updateNode() - Filtering failed !\n");
	}
    
    // update state
    fP0 += k00*dy + k01*dz;
    fP1 += k10*dy + k11*dz;
    fP2  = eta;
    fP3  = cur;
    fP4 += k40*dy + k41*dz;
#ifdef DEBUG
    *(Messenger::instance(MessageType::kNodeMessage))
	<< "fx;fP0;fp1;fP2;fP3;fP4:" 
	<< "\t" << fX 
	<< "\t" << fP0 
	<< "\t" << fP1 
	<< "\t" << fP2 
	<< "\t" << fP3 
	<< "\t" << fP4
	<< endl;
#endif
    
    // update error matrix
    double c01=fC10, c02=fC20, c03=fC30, c04=fC40;
    double c12=fC21, c13=fC31, c14=fC41;
    
    fC00-=k00*fC00+k01*fC10; 
    fC10-=k00*c01+k01*fC11;
    fC20-=k00*c02+k01*c12;   
    fC30-=k00*c03+k01*c13;
    fC40-=k00*c04+k01*c14; 
    fC11-=k10*c01+k11*fC11;
    fC21-=k10*c02+k11*c12;   
    fC31-=k10*c03+k11*c13;
    fC41-=k10*c04+k11*c14; 
    fC22-=k20*c02+k21*c12;   
    fC32-=k20*c03+k21*c13;
    fC42-=k20*c04+k21*c14; 
    fC33-=k30*c03+k31*c13;
    fC43-=k30*c04+k31*c14; 
    fC44-=k40*c04+k41*c14; 
}

//_____________________________________________________________________________
void StiKalmanTrackNode::rotate(double alpha) //throw ( Exception)
{
    //-----------------------------------------------------------------
    // This function rotates by an angle alpha the track representation 
    // held by this node.
    //-----------------------------------------------------------------
    fAlpha += alpha;
    if (fAlpha < -M_PI) fAlpha += 2*M_PI;
    if (fAlpha >= M_PI) fAlpha -= 2*M_PI;
    
#ifdef DEBUG
    *(Messenger::instance(MessageType::kNodeMessage))  
	<< "rotate() - new fAlpha:" << fAlpha*180/M_PI 
	<< " degs" << endl;
#endif
    double x1=fX;
    double y1=fP0;
    double ca=cos(alpha);
    double sa=sin(alpha);
    double r1=fP3*fX - fP2; 
    if (r1>=1) r1 = 0.99999;
    if (r1<=-1) r1 = -0.99999;
    
    fX = x1*ca + y1*sa;
    fP0=-x1*sa + y1*ca;
    fP2=fP2*ca + (fP3*y1 + sqrt(1.- r1*r1))*sa;
    
    double r2=fP3*fX - fP2;
    if (r2>=1) r2 = 0.99999;
    if (r2<=-1) r2 = -0.99999;
    double y0=fP0 + sqrt(1.- r2*r2)/fP3;
    if ((fP0-y0)*fP3 >= 0.)   
	throw runtime_error("SKTN::rotate() - Error - Rotation failed - case 2 !\n");
    
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
}

//_____________________________________________________________________________
void StiKalmanTrackNode::add(StiKalmanTrackNode * newChild)
{
    // set counters of the newChild node
    if (newChild->hit!=0)
	{
	    //cout << "SKTN::add() Has a HIT" << endl;
	    // newChild has an associate hit
	    newChild->hitCount = hitCount+1;
	    newChild->contiguousHitCount = contiguousHitCount+1; 
	    if (contiguousHitCount>minContiguousHitCountForNullReset)
		newChild->contiguousNullCount = 0;
	    else
		newChild->contiguousNullCount = contiguousNullCount;
	    newChild->nullCount = nullCount;
	}
    else
	{
	    // a null hit
	    //cout << "SKTN::add() NO HIT" << endl;
	    newChild->nullCount            = nullCount+1;
	    newChild->contiguousNullCount  = contiguousNullCount+1;
	    newChild->hitCount             = hitCount;
	    newChild->contiguousHitCount   = 0;//contiguousHitCount; 
	}
    /*
     *(Messenger::instance(MessageType::kNodeMessage))
     << "SKTN::add()"
     << "            hitCount:" << newChild->hitCount << endl
     << "  contiguousHitCount:" << newChild->contiguousHitCount << endl
     << "           nullCount:" << newChild->nullCount << endl
     << " contiguousNullCount:" << newChild->contiguousNullCount << endl;
    */
// insert the newChild node as a child to this
 if(newChild != 0 && newChild->getParent() == this)
     insert(newChild, getChildCount() - 1);
 else
     insert(newChild, getChildCount());
}



//_____________________________________________________________________________
void StiKalmanTrackNode::extendToVertex() //throw (Exception)
{
    //-----------------------------------------------------------------
    // This function propagates tracks to the "vertex".
    //-----------------------------------------------------------------
    double c=fP3*fX - fP2;
    
    double tgf=-fP2/(fP3*fP0 + sqrt(1-c*c));
    double snf=tgf/sqrt(1.+ tgf*tgf);
    double xv=(fP2+snf)/fP3;
    propagate(xv);//,0.,0.);
}

//_____________________________________________________________________________
void StiKalmanTrackNode::setElossCalculated(bool option)
{
    elossCalculated = option;
}

//_____________________________________________________________________________
void StiKalmanTrackNode::setMCSCalculated(bool option)
{
    mcsCalculated = option;
}

//_____________________________________________________________________________
bool StiKalmanTrackNode::getElossCalculated()
{
    return elossCalculated;
}

//_____________________________________________________________________________
bool StiKalmanTrackNode::getMCSCalculated()
{
    return mcsCalculated;
}

//_____________________________________________________________________________
void   StiKalmanTrackNode::setMassHypothesis(double m) 
{
    massHypothesis=m;
}

//_____________________________________________________________________________
double StiKalmanTrackNode::getMassHypothesis() 
{ 
    return massHypothesis;
}

//_____________________________________________________________________________
ostream& operator<<(ostream& os, const StiKalmanTrackNode& n)
{
    // print to the ostream "os" the parameters of this node 
    // and all its children recursively
    int nChildren = n.getChildCount();
    os << "Level: " << n.mDepth << "\t"
       << " x:" << n.fX  <<"\t"
       << "alpha:" << 180*n.fAlpha/M_PI<<" degs\t"
       << "dedx:" << n.fdEdx <<"\t"
       << "chi2:" << n.fChi2 << endl
       << "P0/1/2/3/4:" << n.fP0 << " " 
       << n.fP1 <<" "
       << n.fP2 <<" "
       << n.fP3 <<" "
       << n.fP4 <<" "
       << "CC:" << nChildren << endl;
    /*		 << "cov:" << n.fC00 << "\t"
		 << n.fC10<<"\t"
		 << n.fC11<<"\t"
		 << n.fC20<<"\t"
		 << n.fC21<<"\t"
		 << n.fC22<<"\t"
		 << n.fC30<<"\t"
		 << n.fC31<<"\t"
		 << n.fC32<<"\t"
		 << n.fC33<<"\t"
		 << n.fC40<<"\t"
		 << n.fC41<<"\t"
		 << n.fC42<<"\t"
		 << n.fC43<<"\t"
		 << n.fC44 << endl
    */
 if (StiKalmanTrackNode::recurse)
     {
	 for (int i=0;i<nChildren;i++)
	     {
		 const StiKalmanTrackNode * child = static_cast<const StiKalmanTrackNode *>(n.getChildAt(i));
		 os << *child;
	     }
     }
 return os;
}


//_____________________________________________________________________________
void StiKalmanTrackNode::setTargetDet(const StiDetector * target)
{
    targetDet = target;
}

//_____________________________________________________________________________
const StiDetector * StiKalmanTrackNode::getTargetDet()
{
    return targetDet;
}




