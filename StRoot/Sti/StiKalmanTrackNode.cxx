#include <iostream.h>
#include <stdexcept>
#include <math.h>
using namespace std;

#include "Sti/Base/Messenger.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiMaterial.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiKalmanTrackNode.h"
#include "StiElossCalculator.h"
#include "StiKalmanTrackFinderParameters.h"
#include "StiHitErrorCalculator.h"

// Local Track Model
//
// x[0] = y  coordinate
// x[1] = z  position along beam axis
// x[2] = eta=C*x0
// x[3] = C  (local) curvature of the track
// x[4] = tan(l) 

StiKalmanTrackFinderParameters * StiKalmanTrackNode::pars = 0;
bool StiKalmanTrackNode::recurse = false;

int    StiKalmanTrackNode::shapeCode = 0;
double StiKalmanTrackNode::x1=0;
double StiKalmanTrackNode::x2= 0; 
double StiKalmanTrackNode::y1= 0; 
double StiKalmanTrackNode::z1= 0; 
double StiKalmanTrackNode::dx= 0; 
double StiKalmanTrackNode::cosCA1= 0; 
double StiKalmanTrackNode::sinCA1= 0; 
double StiKalmanTrackNode::cosCA2= 0; 
double StiKalmanTrackNode::sinCA2= 0; 
double StiKalmanTrackNode::sumSin= 0; 
double StiKalmanTrackNode::sumCos= 0; 
double StiKalmanTrackNode::sinCA1plusCA2= 0; 
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
bool StiKalmanTrackNode::useCalculatedHitError = true;

//_____________________________________________________________
/// Set the Kalman state of this node to be identical 
/// to that of the given node.
/// This method is useful to initial the state of a node
/// while propagating a track.
//_____________________________________________________________
void StiKalmanTrackNode::setState(const StiKalmanTrackNode * n)
{
  _alpha    = n->_alpha;
  _cosAlpha = n->_cosAlpha;
  _sinAlpha = n->_sinAlpha;
  _cosCA = n->_cosCA;
  _sinCA = n->_sinCA;
  _refX  = n->_refX;
  _x     = n->_x;
  _p0    = n->_p0;  _p1  = n->_p1;  _p2  = n->_p2; _p3  = n->_p3; _p4  = n->_p4;
  _c00   = n->_c00;
  _c10   = n->_c10; _c11 = n->_c11;
  _c20   = n->_c20; _c21 = n->_c21; _c22 = n->_c22;
  _c30   = n->_c30; _c31 = n->_c31; _c32 = n->_c32; _c33 = n->_c33;
  _c40   = n->_c40; _c41 = n->_c41; _c42 = n->_c42; _c43 = n->_c43; _c44 = n->_c44;
}


//_____________________________________________________________________________
/// Set all the attributes of this node to be identical to 
/// that of the given node.
//_____________________________________________________________________________
void StiKalmanTrackNode::setAsCopyOf(const StiKalmanTrackNode * n)
{
  StiTrackNode::setAsCopyOf(n);
  _x        = n->_x;
  _refX     = n->_refX;
  _alpha    = n->_alpha;
  _cosAlpha = n->_cosAlpha;
  _sinAlpha = n->_sinAlpha;
  _cosCA = n->_cosCA;
  _sinCA = n->_sinCA;
  _chi2  = n->_chi2;
  _p0    = n->_p0;  _p1   = n->_p1; _p2   = n->_p2; _p3   = n->_p3; _p4   = n->_p4;
  _c00   = n->_c00;
  _c10   = n->_c10; _c11  = n->_c11;
  _c20   = n->_c20; _c21  = n->_c21; _c22  = n->_c22;
  _c30   = n->_c30; _c31  = n->_c31; _c32  = n->_c32; _c33  = n->_c33;
  _c40   = n->_c40; _c41  = n->_c41; _c42  = n->_c42; _c43  = n->_c43; _c44  = n->_c44;
  hitCount   = n->hitCount;
  nullCount  = n->nullCount;
  contiguousHitCount  =  n->contiguousHitCount;
  contiguousNullCount =  n->contiguousNullCount;
  eyy = n->eyy;
  ezz = n->ezz;
}

/**
   returns the node information
   double& alpha : angle of the local reference frame
   double& xRef  : refence position of this node in the local frame
   double x[5],  : state, for a definition, see the top of this file
   double cc[15] : error matrix of the state "x"
   double& dEdx  : energy loss info
   double& chi2) : chi2 of the track at this node
*/
void StiKalmanTrackNode::get(double& alpha,
			     double& xRef,
			     double  x[5], 
			     double  e[15], 
			     double& chi2)
{
  alpha = _alpha;
  xRef  = _refX;
  x[0] = _p0; x[1] = _p1; x[2] = _p2; x[3] = _p3; x[4] = _p4;
  e[0] = _c00;
  e[1] = _c10; e[2] = _c11;
  e[3] = _c20; e[4] = _c21; e[5] = _c22; 
  e[6] = _c30; e[7] = _c31; e[8] = _c32; e[9] = _c33;
  e[10] = _c40; e[11] = _c41; e[12] = _c42; e[13] = _c43; e[14] = _c44;
  chi2 = _chi2;
}

/*! Calculate/return track 3-momentum and error.
  <p>
  Calculate the 3-momentum of the track in the local reference frame.
  <P>
    
  <h3>Momentum Representation</h3>
  <TABLE BORDER="0" CELLPADDING="2" CELLSPACING="0" WIDTH="100%">
  <TR>
  <TD WIDTH="10%">p[0]</TD>
  <TD WIDTH="10%">px</TD>
  <TD WIDTH="50%">outward</TD>
  </TR>
  <TR>
  <TD WIDTH="10%">p[1]</TD>
  <TD WIDTH="10%">py</TD>
  <TD WIDTH="50%">along detector plane</TD>
  </TR>
  <TR>
  <TD WIDTH="10%">p[2]</TD>
  <TD WIDTH="10%">pz</TD>
  <TD WIDTH="50%">along beam direction</TD>
  </TR>
  </TABLE>
  <h3>Notes:</h3>
  <ol>
  <li>Throws runtime_error exception if |sin(phi)^2|>1.</li>
  <li>Bypasses error calculation if error array "e" is a null pointer.</li>
  </ol>
*/
void StiKalmanTrackNode::getMomentum(double p[3], double e[6]) const
{	
  double pt = getPt();
  p[0] = pt*_cosCA;
  p[1] = pt*_sinCA;
  p[2] = pt*_p4;
  // if e==0, error calculation is not needed, then return
  if (e==0) return;
  // Error calculation from here.
  double c = _p3;
  if (c==0) c=1e-12;
  double cc = c*c;
  // should I include a factor of 0.3 here???????????????
  double kField = pars->field;
  double a00=kField*(_x-_p2/c)/_cosCA;
  double a01=-kField*(_p2*_p2-_x*_p2*c-1)/(cc*_cosCA);
  double a02=0;
  double a10=-kField/c; 
  double a11=kField*_p2/cc;
  double a12=0;
  double a20=0;
  double a21=-kField*_p4/c;
  double a22=kField/c;
  // original error matrix
  double b00=_c22, b01=_c32, b02=_c42;
  double b10=_c32, b11=_c33, b12=_c43;
  double b20=_c42, b21=_c43, b22=_c44;
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

double StiKalmanTrackNode::getField()  const
{
  return pars->field;
}

double StiKalmanTrackNode::getHelicity()  const
{
  return (getCharge()*getField() <= 0) ? 1 : -1;
}

double StiKalmanTrackNode::getPhase() const
{
  const StThreeVector<double> p=getGlobalMomentum();
  double h = getHelicity();
  return (p.y()==0&&p.x()==0) ? (1-2.*h)*M_PI/4. : atan2(p.y(),p.x())-h*M_PI/2.;
}

/// returns momentum and its error matrix 
/// in cartesian coordinates in the _global_
/// ref frame of the experiment
/// p[0] = px
/// p[1] = py
/// p[2] = pz
/// e[0] = // px-px
/// e[1] = // px-py
/// e[2] = // px-pz
/// e[3] = // py-py
/// e[4] = // py-pz
/// e[5] = // pz-pz
void StiKalmanTrackNode::getGlobalMomentum(double p[3], double e[6]) const
{	
  // first get p & e in the local ref frame
  getMomentum(p,e);
  // now rotate the p & e in the global ref frame
  // for the time being, assume an azimuthal rotation 
  // by alpha is sufficient.
  // transformation matrix - needs to be set
  double a00=_cosAlpha, a01=-_sinAlpha, a02=0;
  double a10=_sinAlpha, a11= _cosAlpha, a12=0;
  double a20= 0, a21=  0, a22=1.;
  double px=p[0];
  double py=p[1];
  double pz=p[2];
  p[0] = a00*px + a01*py + a02*pz;
  p[1] = a10*px + a11*py + a12*pz;
  p[2] = a20*px + a21*py + a22*pz;
  if (e==0) return;
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


/*! Steering routine that propagates the track encapsulated by the given node "pNode" to the given detector "tDet". The propagation involves the following steps.
 <OL>
 <LI>Extrapolation of the existing track to the next layer, by "transporting" the
     track a smaller radius.</LI>
 <LI>Determine if the extrapolation actually intersects an existing volume.</LI>
 <LI>Exit with status code if no intersection is found.</LI>
 <LI>Transport the error matrix to the new radius.</LI>
 <LI>If mcsCalculated==true, proceed to calculate MCS effects on the error matrix.</LI>
 <LI>if elossCalculated==true, proceed to calculate Eloss effects on the track parameters.</LI>
 </OL>
 <p>Currently, propagate can handle kPlaner and kCylindrical geometries only. An exception is thrown if other geometry shape are used.
*/
int StiKalmanTrackNode::propagate(StiKalmanTrackNode *pNode, 
				  const StiDetector * tDet)
{
  *(Messenger::instance(MessageType::kNodeMessage))
    <<"StiKalmanTrackNode::propagate(pNode,tDet) -INFO- Started"<<endl;
  det = tDet;
  int position = 0;
  setState(pNode);
  StiPlacement * place = tDet->getPlacement();
  double tAlpha = nice(place->getNormalRefAngle());
  double dAlpha = tAlpha - _alpha;
  if (fabs(dAlpha)>1e-4) rotate(dAlpha);
  StiShape * sh = tDet->getShape();
  planarShape = 0;
  cylinderShape = 0;
  _refX = place->getNormalRadius();
  position = propagate(_refX,sh->getShapeCode()); 
  if (position<0) return position;
  position = locate(place,sh);
  *(Messenger::instance(MessageType::kNodeMessage)) 
    << "StiKalmanTrackNode::propagate(pNode,tDet) -INFO- (2) position:"<<position<<endl;
  if (position>kEdgeZplus || position<0) return position;
  propagateError();
  double dx=0;
  // Multiple scattering
  if (pars->mcsCalculated)
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
      switch (sh->getShapeCode())
	{
	case kPlanar:
	  {
	    detHT = 0.5*planarShape->getThickness();
	    gapT = dx-2*detHT;
	    double sphi=_p3*_x-_p2;
	    s = dx*sqrt(1.+_p4*_p4)/sqrt(1-sphi*sphi);
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
      if (density<0) 
	throw runtime_error("SKTN::propagate() - density<0");
      //mass
      //radThickness*density;
      double sign;
      if (dx>0)
	sign = -1.;
      else
	sign = 1.;
	  
      double zOverA = 0.5;
      double ionization = 5;
      //propagateMCS(pathLength,radThickness,zOverA,ionization,pars->massHypothesis,sign);
    }
  return position;
}

/*! Propagate the track encapsulated by pNode to the given vertex. Use this node
 to represent the track parameters at the vertex.
  <p>
  This method propagates the track from the given parent node
  "pNode" to the given vertex effectively calculating the
  location (x,y,z) of the track near the given vertex. It use "this" node
 to represent/hold the track parameters at the vertex.
<p>
*/
void StiKalmanTrackNode::propagate(const StiKalmanTrackNode *parentNode, StiHit * vertex)
{
  setState(parentNode);
  double locVx = _cosAlpha*vertex->x() + _sinAlpha*vertex->y();
  propagate(locVx,0);
  //propagateError();
  _hit = vertex;
}


/*! Work method used to perform the tranport of "this" node from 
  its current "_x" position to the given position "xk". 
  Returns -1 if the propagation cannot be carried out, i.e.
  if the track curvature is such it cannot reach the desired 
  location.
  option == 0 Planar
  option == 1 Cylinder
 */
int  StiKalmanTrackNode::propagate(double xk, int option)
{
  x1=_x;  y1=_p0;  z1=_p1; cosCA1 =_cosCA; sinCA1 =_sinCA;
  switch (option)
    {
    case 0: 
      x2=xk;  // target position
      break;
    case 1: // cylinder
      if (_p3==0) throw runtime_error("SKTN::propagateCylinder() - _p3==0");
      double L = xk;
      double y0 = _p0+cosCA1/_p3;
      double R = 1/_p3;
      double x0 = _p2/_p3;
      double r0sq= x0*x0+y0*y0;
      if (r0sq<=0.) return -1;
      double a = 0.5*(r0sq+L*L-R*R);
      if (a<=0.) return -1;
      double b = L*L/(a*a);
      double sq = b*r0sq-1;
      if (sq<0) return -1;
      sq = sqrt(sq);				
      double x_p = a*(x0+y0*sq)/r0sq;
      if (x_p>0)
	x2 = x_p;
      else 
	{
	  double x_m = a*(x0-y0*sq)/r0sq;
	  if (x_m>0)
	    x2 = x_m;
	  else
	    return -1;
	}
    }
  dx=x2-x1;
  sinCA2=_p3*x2 - _p2; 
  if (fabs(sinCA2)>1.) return -1;
  cosCA2   = sqrt(1.-sinCA2*sinCA2);
  sumSin   = sinCA1+sinCA2;
  sinCA1plusCA2    = sinCA1*cosCA2 + sinCA2*cosCA1;
  if (fabs(sinCA1plusCA2)==0) return -1;
  sumCos   = cosCA1+cosCA2;
  _p0      += dx*sumSin/sumCos;
  _p1      += dx*_p4*sumSin/sinCA1plusCA2;
  _x       = x2;
  _sinCA   = sinCA2;
  _cosCA   = cosCA2;
  return 0;
}

/// Propagate the track error matrix
/// \note This method must be called ONLY after a call to the propagate method.
void StiKalmanTrackNode::propagateError()
{  
  //f = F - 1
  double xx=x1+x2;
  double tanCA1=sinCA1/cosCA1;
  double tanCA2=sinCA2/cosCA2;
  double f02=-dx*(2*sumCos + sumSin*(tanCA1+tanCA2))/(sumCos*sumCos);
  double f03= dx*(sumCos*xx + sumSin*(tanCA1*x1+tanCA2*x2))/(sumCos*sumCos);
  double sinCA1plusCA2=sinCA1*cosCA2+sinCA2*cosCA1;
  double f12=-dx*_p4*(2*sinCA1plusCA2 + 
		      sumSin*(sinCA2*tanCA1-cosCA1 + sinCA1*tanCA2-cosCA2))/(sinCA1plusCA2*sinCA1plusCA2);
  double f13=dx*_p4*(xx/sinCA1plusCA2 
		     -sumSin*(x1*(cosCA2-sinCA2*tanCA1) 
			      +x2*(cosCA1-sinCA1*tanCA2))/sinCA1plusCA2/sinCA1plusCA2);
  double f14= dx*sumSin/sinCA1plusCA2; 
  //b = C*ft
  double b00=f02*_c20 + f03*_c30;
  double b01=f12*_c20 + f13*_c30 + f14*_c40;
  double b10=f02*_c21 + f03*_c31;
  double b11=f12*_c21 + f13*_c31 + f14*_c41;
  double b20=f02*_c22 + f03*_c32;
  double b21=f12*_c22 + f13*_c32 + f14*_c42;
  double b30=f02*_c32 + f03*_c33;
  double b31=f12*_c32 + f13*_c33 + f14*_c43;
  double b40=f02*_c42 + f03*_c43;
  double b41=f12*_c42 + f13*_c43 + f14*_c44;
  //a = f*b = f*C*ft
  double a00=f02*b20+f03*b30;
  double a01=f02*b21+f03*b31;
  double a11=f12*b21+f13*b31+f14*b41;
  //*(Messenger::instance(MessageType::kNodeMessage)) 
  //  <<"SKTN::propagateError() - dx:"<<dx<<" sumCos:"<<sumCos<<" cosCA1:"<<cosCA1<<" cosCA2:"<<cosCA2
  //  <<" sinCA1:"<<sinCA1<<" sinCA2:"<<sinCA2<<" sumSin:"<<sumSin<<endl;
  //  <<" sinCA1plusCA2:"<<sinCA1plusCA2<<endl;
  //  <<" f02:"<<f02<<" f03:"<<f03<<" f12:"<<f12<<" f13:"<<f13<<" f14:"<<f14<<endl;
  //  <<" b00:"<<b00<<" b01:"<<b01<<" b10:"<<b10<<" b11:"<<b11<<" b20:"<<b20
  //  <<" b21:"<<b21<<" b30:"<<b30<<" b31:"<<b31<<" b40:"<<b40<<" b41:"<<b41<<endl;
  //  <<" a00:"<<a00<<" a01:"<<a01<<" a11:"<<a11<<endl;
  //F*C*Ft = C + (a + b + bt)
  _c00 += a00 + 2*b00;
  _c10 += a01 + b01 + b10; 
  _c20 += b20;
  _c30 += b30;
  _c40 += b40;
  _c11 += a11 + 2*b11;
  _c21 += b21; 
  _c31 += b31; 
  _c41 += b41; 
}

/*! Calculate the effect of MCS on the track error matrix.
  <p>
  The track is assumed to propagate from (x0,y0,z0) to (x1,y1,z1). The calculation
  is performed for the given mass hypothesis which given a momentum determines the
  speed "beta" of the particle. The calculation of the average scattering angle
  is delegated to the function "mcs2". The calculation of energy loss is done
  by the function eloss.
 */
void StiKalmanTrackNode::propagateMCS(double pathLength, 
				      double radThickness, 
				      double zOverA,
				      double ionization,
				      double massHypo,
				      double elossSign)
{  
  double pt = getPt();
  double p2=(1.+_p4*_p4)*pt*pt;
  double m2=massHypo*massHypo;
  double e2=p2+m2;
  double beta2=p2/e2;
  double theta2=mcs2(pathLength,radThickness,beta2,p2);
  double ey  = _p3*_x-_p2;
  double ez  = _p4;
  double xz  = _p3*ez;
  double zz1 = ez*ez+1;
  double xy  = _p2+ey;
  _c33 += xz*xz*theta2;
  _c32 += xz*ez*xy*theta2;
  _c43 += xz*zz1*theta2;
  _c22 += (2*ey*ez*ez*_p2+1-ey*ey+ez*ez+_p2*_p2*ez*ez)*theta2;
  _c42 += ez*zz1*xy*theta2;
  _c44 += zz1*zz1*theta2;
  //if(!eloss) return;
  double dE=0;
  //double dE=elossSign*eloss->calculate(1.,massHypo, beta2);
  double cc=_p3;
  _p3 = _p3 *(1.- sqrt(e2)*dE/p2);
  _p2 = _p2 + _x*(_p3-cc);
}



StThreeVector<double> StiKalmanTrackNode::getPointAt(double xk) const
{
  double cosCA1, sinCA1, cosCA2, sinCA2, 
  x1=_x;  y1=_p0; z1=_p1; cosCA1=_cosCA; sinCA1=_sinCA;
  x2=x1+(xk-x1);
  dx=x2-x1;
  sinCA2=_p3*x2 - _p2;
  if (fabs(sinCA2)>1.) throw runtime_error("SKTN::propagate() - WARNING - fabs(sinCA2)>1.");
  cosCA2=sqrt(1.- sinCA2*sinCA2);
  double sumSin = sinCA1+sinCA2;
  double yy = _p0 + dx*sumSin/(cosCA1+cosCA2);
  double sinCA1plusCA2  = sinCA1*cosCA2 + sinCA2*cosCA1;
  if (sinCA1plusCA2==0) throw runtime_error("SKTN::propagate() - WARNING - sinCA1plusCA2==0.");
  return StThreeVector<double>(_cosAlpha*x2-_sinAlpha*yy, _sinAlpha*x2+_cosAlpha*yy, _p1+dx*_p4*sumSin/sinCA1plusCA2);
}

/*! Calculate the increment of chi2 caused by the addition of this node to the track.
  <p>
  Uses the track extrapolation to "_x", and hit position to evaluate an update to the track chi2.
  The new chi2 is stored internally in this node. The increment is returned as a convenience
  for the caller method. 
  <p>
  <h3>Notes</h3>
  <ol>
  <li>Use full error matrices.</li>
  <li>Return increment in chi2 implied by the node/hit assocition.</li>
  <li>Throws an exception if numerical problems arise.</li>
  </ol>
*/
double StiKalmanTrackNode::evaluateChi2() 
{
  double r00, r01,r11;
  if (useCalculatedHitError)
    {
      _hit->detector()->getHitErrorCalculator()->calculateError(*this);
      r00=_c00+eyy;
      r01=_c10; r11=_c11+ezz;
    }
  else
    {
      r00=_hit->syy()+_c00;
      r01=_hit->syz()+_c10;  r11=_hit->szz()+_c11;
    }
  //*(Messenger::instance(MessageType::kNodeMessage)) << " r00:"<<r00<<" r01:"<<r01<<" r11:"<<r11<<endl;
  double det=r00*r11 - r01*r01;
  if (fabs(det)==0.) throw runtime_error("SKTN::evaluateChi2() Singular matrix !\n");
  // invert error matrix
  double tmp=r00; r00=r11; r11=tmp; r01=-r01;  
  double dy=_hit->y()-_p0;
  double dz=_hit->z()-_p1;
  double chi2inc = (dy*r00*dy + 2*r01*dy*dz + dz*r11*dz)/det;
  _chi2 += chi2inc;
  *(Messenger::instance(MessageType::kNodeMessage)) << "chi2inc:"<<chi2inc<<endl;
  return chi2inc;
}

/*! Update the track parameters using this node.
  <p>
  This method uses the hit contained by node to update the track 
  parameters contained by this node and thus complete the propagation
  of this track to the location x=_x.
  <p>
  <OL>
  <li>Throw a runtime_error exception if no hit is actually associated with this node.</li>
  <li>Compute the measurement error matrix "r". Invert it.
  <li>Update the measurement matrix "k" and calculate updated curvature, eta, and pitch.
  <li>Update track error matrix.</li>
  </OL>
  <p>
  <h3>Notes</h3>
  <ol>
  <li>Throw logic_error if no hit is associated with this node.</li>
  <li>Throw runtime_error if determinent of "r" matrix is null.
  </ol>
*/
void StiKalmanTrackNode::updateNode() 
{
  double r00,r01,r11;
  if (useCalculatedHitError)
    {
      r00=_c00+eyy;
      r01=_c10;      r11=_c11+ezz;
    }
  else
    {
      r00=_hit->syy()+_c00;
      r01=_hit->syz()+_c10;  r11=_hit->szz()+_c11;
    }  
  double det=r00*r11 - r01*r01;
  if (fabs(det)==0) throw runtime_error("SKTN::updateNode() - WARNING - Singular matrix; fabs(det)==0");
  // inverse matrix
  double tmp=r00; r00=r11/det; r11=tmp/det; r01=-r01/det;
  // update error matrix
  double k00=_c00*r00+_c10*r01, k01=_c00*r01+_c10*r11;
  double k10=_c10*r00+_c11*r01, k11=_c10*r01+_c11*r11;
  double k20=_c20*r00+_c21*r01, k21=_c20*r01+_c21*r11;
  double k30=_c30*r00+_c31*r01, k31=_c30*r01+_c31*r11;
  double k40=_c40*r00+_c41*r01, k41=_c40*r01+_c41*r11;
  double dy  = _hit->y() - _p0;
  double dz  = _hit->z() - _p1;
  double cur = _p3 + k30*dy + k31*dz;
  double eta = _p2 + k20*dy + k21*dz;
  // update state
  _p0 += k00*dy + k01*dz;
  _p1 += k10*dy + k11*dz;
  _p2  = eta;
  _p3  = cur;
  _p4 += k40*dy + k41*dz;
  _sinCA  =  _p3*_x-_p2;
  if (_sinCA>1.) throw runtime_error("SKTN::updateNode() - WARNING - _sinCA>1");
  _cosCA = sqrt(1.-_sinCA*_sinCA); 
  // update error matrix
  double c01=_c10, c02=_c20, c03=_c30, c04=_c40;
  double c12=_c21, c13=_c31, c14=_c41;
  _c00-=k00*_c00+k01*_c10; 
  _c10-=k00*c01+k01*_c11;
  _c20-=k00*c02+k01*c12;   
  _c30-=k00*c03+k01*c13;
  _c40-=k00*c04+k01*c14; 
  _c11-=k10*c01+k11*_c11;
  _c21-=k10*c02+k11*c12;   
  _c31-=k10*c03+k11*c13;
  _c41-=k10*c04+k11*c14; 
  _c22-=k20*c02+k21*c12;   
  _c32-=k20*c03+k21*c13;
  _c42-=k20*c04+k21*c14; 
  _c33-=k30*c03+k31*c13;
  _c43-=k30*c04+k31*c14; 
  _c44-=k40*c04+k41*c14; 
}

/*! Rotate this node track representation azymuthally by given angle.
  <p>
  This method rotates by an angle alpha the track representation 
  held by this node. 
  <h3>Notes</h3>
  <ol>
  <li>The rotation is bound between -M_PI and M_PI.</li>
  <li>Throws runtime_error if "(_p0-y0)*_p3>=0" in order to avoid math exception.</li>
  <li>Avoid undue rotations as they are CPU intensive...</li>
  </ol>
*/
void StiKalmanTrackNode::rotate(double alpha) //throw ( Exception)
{
  _alpha += alpha;
  _alpha = nice(_alpha);
  x1=_x; y1=_p0; 
  _cosAlpha=cos(alpha); 
  _sinAlpha=sin(alpha); 
  _x = x1*_cosAlpha + y1*_sinAlpha;
  _p0=-x1*_sinAlpha + y1*_cosAlpha;
  _p2=_p2*_cosAlpha + (_p3*y1 + _cosCA*_sinAlpha);
  _sinCA = _p3*_x - _p2;
  if (_sinCA>1)
    {
      _sinCA = 1.; _cosCA = 0.;
    }
  else if (_sinCA<-1) 
    {
      _sinCA = -1.;
      _cosCA =  0.;
    }
  else
    _cosCA = sqrt(1.- _sinCA*_sinCA);
  double y0=_p0 + _cosCA/_p3;
  if ((_p0-y0)*_p3 >= 0.) throw runtime_error("SKTN::rotate() - Error - Rotation failed!\n");
  //f = F - 1
  double f00=_cosAlpha-1;
  double f23=(y1 - _sinCA*x1/_cosCA)*_sinAlpha;
  double f20=_p3*_sinAlpha;
  double f22=(_cosAlpha + _sinAlpha*_sinCA/_cosCA)-1;
  //b = C*ft
  double b00=_c00*f00, b02=_c00*f20+_c30*f23+_c20*f22;
  double b10=_c10*f00, b12=_c10*f20+_c31*f23+_c21*f22;
  double b20=_c20*f00, b22=_c20*f20+_c32*f23+_c22*f22;
  double b30=_c30*f00, b32=_c30*f20+_c33*f23+_c32*f22;
  double b40=_c40*f00, b42=_c40*f20+_c43*f23+_c42*f22;
  //a = f*b = f*C*ft
  double a00=f00*b00, a02=f00*b02, a22=f20*b02+f23*b32+f22*b22;
  //F*C*Ft = C + (a + b + bt)
  _c00 += a00 + 2*b00;
  _c10 += b10;
  _c20 += a02+b20+b02;
  _c30 += b30;
  _c40 += b40;
  _c21 += b12;
  _c32 += b32;
  _c22 += a22 + 2*b22;
  _c42 += b42; 
  
  _cosAlpha=cos(_alpha); 
  _sinAlpha=sin(_alpha);
}

//_____________________________________________________________________________
void StiKalmanTrackNode::add(StiKalmanTrackNode * newChild)
{
  // set counters of the newChild node
  if (newChild->_hit!=0)
    {
      newChild->hitCount = hitCount+1;
      newChild->contiguousHitCount = contiguousHitCount+1; 
      if (contiguousHitCount>pars->minContiguousHitCountForNullReset)
	newChild->contiguousNullCount = 0;
      else
	newChild->contiguousNullCount = contiguousNullCount;
      newChild->nullCount = nullCount;
    }
  else
    {
      newChild->nullCount            = nullCount+1;
      newChild->contiguousNullCount  = contiguousNullCount+1;
      newChild->hitCount             = hitCount;
      newChild->contiguousHitCount   = 0;//contiguousHitCount; 
    } 
  children.push_back(newChild);
  newChild->setParent(this);
  *(Messenger::instance(MessageType::kNodeMessage))
    << "SKTN::add(child) -I- "<<*this<<endl;
}

/// print to the ostream "os" the parameters of this node 
/// and all its children recursively
ostream& operator<<(ostream& os, const StiKalmanTrackNode& n)
{
  os << "\ta:" << 180*n._alpha/M_PI<<"\tdegs"
     << "\tx:" << n._x
     << "\ty:" << n._p0 << "\tz:" << n._p1 <<"\teta:"<< n._p2 <<"\tc:"<< n._p3 <<"\ttanl:"<< n._p4
     << "\tc00:" <<n._c00<< "\tc11:"<<n._c11<<"\tc22:"<<n._c22
     << "\tchi2:" << n._chi2 
     << "\tHITS:"<<n.hitCount
     << "\tcHITS:"<<n.contiguousHitCount
     << "\tnull:"<<n.nullCount<<endl;
  return os;
}

double StiKalmanTrackNode::getWindowY() const
{	 
  double window = pars->searchWindowScale*_c00;
  if (window<pars->minSearchWindow)
    window = pars->minSearchWindow;
  else if (window>pars->maxSearchWindow)
    window = pars->maxSearchWindow;
  *(Messenger::instance(MessageType::kNodeMessage)) <<" winY:"<<window;
  return window;
}

//_____________________________________________________________________________
double StiKalmanTrackNode::getWindowZ() const
{	 
  double window = pars->searchWindowScale*_c11;
  if (window<pars->minSearchWindow)
    window = pars->minSearchWindow;
  else if (window>pars->maxSearchWindow)
    window = pars->maxSearchWindow;
  *(Messenger::instance(MessageType::kNodeMessage)) <<"winZ:"<<window<<endl;
  return window;
}

StThreeVector<double> StiKalmanTrackNode::getHelixCenter() const
{
  if (_p3==0) throw logic_error("StiKalmanTrackNode::getHelixCenter() - FATAL - _p3==0 ");
  double xx0 = _p2/_p3;
  double yy0 = _p0+_cosCA/fabs(_p3);
  double zz0 = _p1+_p4*asin(_sinCA)/_p3;
  return (StThreeVector<double>(_cosAlpha*xx0-_sinAlpha*yy0,_sinAlpha*xx0+_cosAlpha*yy0,zz0));
}

void StiKalmanTrackNode::setParameters(StiKalmanTrackFinderParameters *parameters)
{
  pars = parameters;
}
