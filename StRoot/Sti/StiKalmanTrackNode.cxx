//StiKalmanTrack.cxx
/*
 * $Id: StiKalmanTrackNode.cxx,v 2.32 2004/01/30 21:40:21 pruneau Exp $
 *
 * /author Claude Pruneau
 *
 * $Log: StiKalmanTrackNode.cxx,v $
 * Revision 2.32  2004/01/30 21:40:21  pruneau
 * some clean up of the infinite checks
 *
 * Revision 2.31  2003/09/02 17:59:41  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.30  2003/08/13 21:04:21  pruneau
 * transfered relevant tracking pars to detector builders
 *
 * Revision 2.29  2003/08/02 08:23:10  pruneau
 * best performance so far
 *
 * Revision 2.28  2003/07/30 19:18:58  pruneau
 * sigh
 *
 * Revision 2.26  2003/07/15 13:56:19  andrewar
 * Revert to previous version to remove bug.
 *
 * Revision 2.24  2003/05/22 18:42:33  andrewar
 * Changed max eloss correction from 1% to 10%.
 *
 * Revision 2.23  2003/05/09 22:07:57  pruneau
 * Added protection to avoid 90deg tracks and ill defined eloss
 *
 * Revision 2.22  2003/05/09 14:57:20  pruneau
 * Synching
 *
 * Revision 2.21  2003/05/08 18:49:09  pruneau
 * fudge=1
 *
 * Revision 2.20  2003/05/07 03:01:39  pruneau
 * *** empty log message ***
 *
 * Revision 2.19  2003/05/03 14:37:22  pruneau
 * *** empty log message ***
 *
 * Revision 2.18  2003/05/01 20:46:47  pruneau
 * changed error parametrization
 *
 * Revision 2.17  2003/04/22 21:20:17  pruneau
 * Added hit filter
 * Tuning og finder pars
 * Tuning of KalmanTrackNode
 *
 * Revision 2.16  2003/04/17 22:49:36  andrewar
 * Fixed getPhase function to conform to StHelixModel convention.
 *
 * Revision 2.15  2003/03/31 17:18:56  pruneau
 * various
 *
 * Revision 2.14  2003/03/13 21:21:27  pruneau
 * getPhase() fixed. MUST inclde -helicity()*pi/2
 *
 * Revision 2.13  2003/03/13 18:59:13  pruneau
 * various updates
 *
 * Revision 2.12  2003/03/12 17:57:31  pruneau
 * Elss calc updated.
 *
 * Revision 2.11  2003/03/04 21:31:05  pruneau
 * Added getX0() and getGasX0() conveninence methods.
 *
 * Revision 2.10  2003/03/04 18:41:27  pruneau
 * Fixed StiHit to use global coordinates as well as locals.
 * Fixed Logic Bug in StiKalmanTrackFinder
 *
 * Revision 2.9  2003/03/04 15:25:48  andrewar
 * Added several functions for radlength calculation.
 *
 */

#include <Stiostream.h>
#include <stdexcept>
#include <math.h>
using namespace std;

#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiMaterial.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiKalmanTrackNode.h"
#include "StiElossCalculator.h"
#include "StiTrackingParameters.h"
#include "StiKalmanTrackFinderParameters.h"
#include "StiHitErrorCalculator.h"
#include "Sti/StiElossCalculator.h"

// Local Track Model
//
// x[0] = y  coordinate
// x[1] = z  position along beam axis
// x[2] = eta=C*x0
// x[3] = C  (local) curvature of the track
// x[4] = tan(l) 

StiKalmanTrackFinderParameters * StiKalmanTrackNode::pars = 0;
bool StiKalmanTrackNode::recurse = false;

const StiElossCalculator * StiKalmanTrackNode::_elossCalculator = new StiElossCalculator();

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
#define MESSENGER *(Messenger::instance(MessageType::kNodeMessage))

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
  hitCount = n->hitCount;
  nullCount = n->nullCount;
  contiguousHitCount = n->contiguousHitCount;
  contiguousNullCount = n->contiguousNullCount;
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

double StiKalmanTrackNode::getPhase() const
{
  //! This function translates between ITTF helix parameters and
  //! StHelixModel phi. It is only used to fill StTrackGeometry.
  //! For a StPhysicalHelix, phi must be transformed by -h*pi/2.
  const StThreeVector<double> p=getGlobalMomentum();
  double h = getHelicity();
  return (p.y()==0&&p.x()==0) ? (1-2.*h)*M_PI/4. : p.phi();

}

/// returns momentum and its error matrix 
/// in cartesian coordinates in the _global_
/// ref frame of the experiment
/// p[0] = px
/// p[1] = py
/// p[2] = pz
/// e[0] = px-px
/// e[1] = px-py
/// e[2] = px-pz
/// e[3] = py-py
/// e[4] = py-pz
/// e[5] = pz-pz
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
  double a20= 0, a21=  0, a22=(pars->field<0)? -1:1;
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


/*! Steering routine that propagates the track encapsulated by the given node "pNode" to the given detector "tDet". 
	<p>
	The propagation involves the following steps.
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
  det = tDet;
  int position = 0;
  setState(pNode);
  StiPlacement * place = tDet->getPlacement();
  double tAlpha = nice(place->getNormalRefAngle());
  double dAlpha = tAlpha - _alpha;
  if (fabs(dAlpha)>1e-3) rotate(dAlpha);
  StiShape * sh = tDet->getShape();
  planarShape = 0;
  cylinderShape = 0;
  _refX = place->getNormalRadius();
  position = propagate(_refX,sh->getShapeCode()); 
  if (position<0) 
    return position;
  position = locate(place,sh);
  if (position>kEdgeZplus || position<0) return position;
  propagateError();
  // Multiple scattering
  if (pars->mcsCalculated)
      propagateMCS(pNode,tDet);
  return position;
}

/*! Propagate the track encapsulated by pNode to the given vertex. Use this node
	to represent the track parameters at the vertex.
  <p>
  This method propagates the track from the given parent node
  "pNode" to the given vertex effectively calculating the
  location (x,y,z) of the track near the given vertex. It use "this" node
 to represent/hold the track parameters at the vertex.
 return true when the propagation is successfull and false otherwise.
<p>
*/
bool StiKalmanTrackNode::propagate(const StiKalmanTrackNode *parentNode, StiHit * vertex)
{
  setState(parentNode);
  //double locVx = _cosAlpha*vertex->x() + _sinAlpha*vertex->y();
  if (propagate(vertex->x(),kPlanar) < 0)
    return false; // track does not reach vertex "plane"
  propagateError();
  _hit = vertex;
  _detector = 0;
  return true;
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
    case kPlanar: 
      x2=xk;  // target position
      break;
    case kCylindrical: // cylinder
      if (_p3==0) throw runtime_error("SKTN::propagateCylinder() - _p3==0");
      double L = xk;
      double y0 = _p0+cosCA1/_p3;
      double R = 1/_p3;
      double x0 = _p2/_p3;
      double r0sq= x0*x0+y0*y0;
      if (r0sq==0.) return -1;
      double a = 0.5*(r0sq+L*L-R*R);
      if (a==0.) return -1;
      double b = L*L/(a*a);
      double sq = b*r0sq-1;
      if (sq<0) return -1;
      sq = ::sqrt(sq);				
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
  /*if (sinCA2>1.) 
    sinCA2 = 0.999999;
  else if (sinCA2<-1.) 
  sinCA2 = -0.999999;*/
  cosCA2   = ::sqrt(1.-sinCA2*sinCA2);
  sumSin   = sinCA1+sinCA2;
  sinCA1plusCA2    = sinCA1*cosCA2 + sinCA2*cosCA1;
  if (sinCA1plusCA2==0) 
    {
      //cout << "======================================================= fabs(sinCA1plusCA2)==0" << endl;
      return -1;
    }
  sumCos   = cosCA1+cosCA2;
  _p0      += dx*sumSin/sumCos;
  _p1      += dx*_p4*sumSin/sinCA1plusCA2;
  _x       = x2;
  _sinCA   = sinCA2;
  _cosCA   = cosCA2;
  return 0;
}

void StiKalmanTrackNode::nudge()
{
  double deltaX = _hit->x()-_x;
  sinCA2=_p3*(_x+deltaX) - _p2; 
  //cout << " StiKalmanTrackNode::nudge() -I- sin(CA2):"<<sinCA2<<endl;
  if (fabs(sinCA2)>1.) return;
  cosCA2   = ::sqrt(1.-sinCA2*sinCA2);
  sumSin   = sinCA1+sinCA2;
  sinCA1plusCA2    = sinCA1*cosCA2 + sinCA2*cosCA1;
  if (fabs(sinCA1plusCA2)==0) return;
  sumCos   = cosCA1+cosCA2;
  _p0      += deltaX*sumSin/sumCos;
  _p1      += deltaX*_p4*sumSin/sinCA1plusCA2;
  _x       = _x+deltaX;
  _sinCA   = sinCA2;
  _cosCA   = cosCA2;
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
void StiKalmanTrackNode::propagateMCS(StiKalmanTrackNode * previousNode, const StiDetector * tDet)
{  
  double relRadThickness;
  // Half path length in previous node
  double pL1,pL2,pL3,d1,d2,d3,dxEloss;
  pL1=previousNode->pathlength()/2.;
  // Half path length in this node
  pL3=pathlength()/2.;
  // Gap path length
  pL2= pathLToNode(previousNode);
  if (pL1<0) pL1=0;
  if (pL2<0) pL2=0;
  if (pL3<0) pL3=0;
  if (!finite(pL1) ||
      !finite(pL2) ||
      !finite(pL3))
    {
      //we are dealing with a track parallel to the 
      // pad row - let's not try to correct it.
      return;
    }
  double x0p =-1;
  double x0Gas=-1;
  double x0=-1;
  d1    = previousNode->getDensity();
  x0p   = previousNode->getX0();
  d3    = tDet->getMaterial()->getDensity();
  x0    = tDet->getMaterial()->getX0();

  if (!finite(d1) ||
      !finite(x0p) ||
      !finite(d3) ||
      !finite(x0))
    {
      cout << " StiKalmanTrackNode::propagate() -F- Infinite values detected"
					 << " d1:"  << d1
					 << " x0p:" <<x0p
					 << " d3:"  <<d3
					 << " x0:"  <<x0<<endl;
      throw logic_error("StiKalmanTrackNode::propagate() -F- Infinite values detected");
    }

  if (pL2> (pL1+pL3)) 
    {
      pL2=pL2-pL1-pL3;
      if (dx>0)
				{
					x0Gas = tDet->getGas()->getX0();
					d2    = tDet->getGas()->getDensity();
				}
      else
				{
					x0Gas = previousNode->getGasX0(); 
					d2    = previousNode->getGasDensity();
				}
      relRadThickness = 0.;
      dxEloss = 0;
      if (x0p>0.) 
	{
	  relRadThickness += pL1/x0p;
	  dxEloss += d1*pL1;
	}
      if (x0Gas>0.)
				{
					relRadThickness += pL2/x0Gas;
					dxEloss += d2*pL2;
				}
      if (x0>0.)
				{
					relRadThickness += pL3/x0;
					dxEloss += d3*pL3;
				}
    }
  else 
    {
      relRadThickness = 0.; 
      dxEloss = 0;
      if (x0p>0.) 
				{
					relRadThickness += pL1/x0p;
					dxEloss += d1*pL1;
				}
      if (x0>0.)
				{
					relRadThickness += pL3/x0;
					dxEloss += d3*pL3;
				}
    }
  double pt = getPt();
  if (!finite(pt) || !finite(dxEloss) || !finite(relRadThickness))
    {
			cout << "StiKalmanTrackNode::propagateMCS( ) -F- Infite values detected" <<endl
					 <<"          dx: "<< dx<<endl
					 <<"         x0p: "<< x0p<<endl
					 <<"          x0: "<< x0<<endl
					 <<"       x0Gas: "<< x0Gas<<endl
					 <<" relRadThick: "<< relRadThickness<<endl
					 <<"          pt: "<< pt<<endl
					 << *this<<endl
					 << *getDetector()<<endl;
      throw logic_error("StiKalmanTrackNode::propagateMCS() -F- Infinite values detected");
    }
  double p2=(1.+_p4*_p4)*pt*pt;
  double m=pars->massHypothesis;
  double m2=m*m;
  double e2=p2+m2;
  double beta2=p2/e2;
  //cout << " m2:"<<m2<<" p2:"<<p2<<" beta2:"<<beta2;
  double theta2=mcs2(relRadThickness,beta2,p2);
  //cout << " theta2:"<<theta2;
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
  double dE=0;
  double sign;
  if (dx>0)
    sign = 1.;
  else
    sign = -1.;
  double eloss = _elossCalculator->calculate(1.,0.5,m, beta2,5.);
  double fudge = 2.;
  dE = fudge*sign*dxEloss*eloss;
  if(!finite(dxEloss) || !finite(beta2) || !finite(m) || m==0 || !finite(eloss) || !finite(_p3) || p2==0 )
    {
      cout << "STKN::propagate() -E- Null or Infinite values detected" << endl
					 << "     beta2 : " << beta2
					 << "   dxEloss : " << dxEloss
					 << "         m : " << m
					 << "     eloss : " << _p3
					 << "        p2 : " << p2
					 << "  Logic error => ABORT" << endl;
      throw logic_error("StiKalmanTrackNode::propagate() -F- Infinite values detected. dxEloss!=finite");
    }
  if (fabs(dE)>0)
    {
      double cc=_p3;
      double correction;
      correction =1.- ::sqrt(e2)*dE/p2;
      if (correction>1.1) correction = 1.1;
      else if (correction<0.9) correction = 0.9;
      _p3 = _p3 *correction;
      _p2 = _p2 + _x*(_p3-cc);
    }
}

/*!Calulates length between center of this node and provided node, which
  is assumed to be on the same helix. Have to use global coords, since 
  nodes may not be in the same detector volume.

  \returns (double) length
*/
//delta(dx,dy,dz) = here - there
double StiKalmanTrackNode::pathLToNode(const StiKalmanTrackNode * const oNode)
{
  const StThreeVector<double> delta = 
    getGlobalPoint() - oNode->getGlobalPoint();
  double R = getCurvature();
  // s = 2c * asin( t/(2c)); t=::sqrt(dx^2+dy^2+dz^2)
  return length(delta, R);
}

inline double StiKalmanTrackNode::length(const StThreeVector<double>& delta, double curv)
{
  double tR = fabs(2./curv);
  return  tR*asin(delta.magnitude()/tR);
}

StThreeVector<double> StiKalmanTrackNode::getPointAt(double xk) const
{
  double cosCA1, sinCA1, cosCA2, sinCA2, 
  x1=_x;  y1=_p0; z1=_p1; cosCA1=_cosCA; sinCA1=_sinCA;
  x2=x1+(xk-x1);
  dx=x2-x1;
  sinCA2=_p3*x2 - _p2;
  if (fabs(sinCA2)>1.) throw runtime_error("SKTN::getPointAt() -W- fabs(sinCA2)>1.");
  cosCA2=::sqrt(1.- sinCA2*sinCA2);
  double sumSin = sinCA1+sinCA2;
  double yy = _p0 + dx*sumSin/(cosCA1+cosCA2);
  double sinCA1plusCA2  = sinCA1*cosCA2 + sinCA2*cosCA1;
  if (sinCA1plusCA2==0) throw runtime_error("SKTN::getPointAt() -W- sinCA1plusCA2==0.");
  return StThreeVector<double>(_cosAlpha*x2-_sinAlpha*yy, _sinAlpha*x2+_cosAlpha*yy, _p1+dx*_p4*sumSin/sinCA1plusCA2);
}

/*! Calculate the increment of chi2 caused by the addition of this node to the track.
  <p>
  Uses the track extrapolation to "_x", and hit position to evaluate and return the 
  increment to the track chi2.
  The chi2 is not stored internally in this node. 
  <p>
  <h3>Notes</h3>
  <ol>
  <li>Use full error matrices.</li>
  <li>Return increment in chi2 implied by the node/hit assocition.</li>
  <li>Throws an exception if numerical problems arise.</li>
  </ol>
*/
double StiKalmanTrackNode::evaluateChi2(const StiHit * hit) 
{
  double r00, r01,r11;
  //If required, recalculate the errors of the detector hits.
  //Do not attempt this calculation for the main vertex.
  if (!hit)throw runtime_error("SKTN::evaluateChi2(const StiHit &) - hit==0");
  const StiDetector * detector = hit->detector();
  if (useCalculatedHitError && detector)
    {
      r00=_c00+eyy;
      r01=_c10;     r11=_c11+ezz;
    }
  else
    {
      r00=hit->syy()+_c00;
      r01=hit->syz()+_c10;  
      r11=hit->szz()+_c11;
    }
  double det=fabs(r00*r11 - r01*r01);
  if (fabs(det)==0.) throw runtime_error("SKTN::evaluateChi2() Singular matrix !\n");
  double tmp=r00; r00=r11; r11=tmp; r01=-r01;  
  double dy=hit->y()-_p0;
  double dz=hit->z()-_p1;
  double cc= (dy*r00*dy + 2*r01*dy*dz + dz*r11*dz)/det;
  return cc;
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
  const StiDetector * detector = _hit->detector();
  if (useCalculatedHitError && detector)
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
  if (fabs(det)==0) throw runtime_error("SKTN::updateNode() -W- Singular matrix; fabs(det)==0");
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

	// Check if any of the quantities required to pursue the update
	// are infinite. If so, it means the tracks cannot be update/propagated
	// any longer and should therefore be abandoned. Just return. This is 
	// not a big but rather a feature of the fact a helicoidal tracks!!!
  if (!finite(_c00)||!finite(_c11)||!finite(k30)||!finite(k31))  return;
  // update Kalman state
  _p0 += k00*dy + k01*dz;
  _p1 += k10*dy + k11*dz;
  _p2  = eta;
  _p3  = cur;
  _p4 += k40*dy + k41*dz;
  _sinCA  =  _p3*_x-_p2;
  // The following test introduces a track propagation error but happens
  // only when the track should be aborted so we don't care...
  if (_sinCA>1.) 
    {
      //cout << " SKTN    _sinCA>1";
      //throw runtime_error("SKTN::updateNode() -W- _sinCA>1");
      _sinCA = 0.999999;
    }
  else if (_sinCA<-1.) 
    {
      _sinCA = -0.999999;
    }
  _cosCA = ::sqrt(1.-_sinCA*_sinCA); 
  if (_cosCA==0)
    throw logic_error("StiKalmanTrackNode::updateNode() -E- _cosCA==0");

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
  //cout << "rotate by alpha:"<< 180.*alpha/3.1415927<<endl;
  //cout << "         _alpha:"<< 180.*_alpha/3.1415927<<endl;
  _alpha += alpha;
  _alpha = nice(_alpha);
  //cout << "    new  _alpha:"<< 180.*_alpha/3.1415927<<endl;
  double x1=_x; 
  double y1=_p0; 
  double ca = cos(alpha);
  double sa = sin(alpha);
  _x = x1*ca + y1*sa;
  _p0=-x1*sa + y1*ca;
  _p2=_p2*ca + (_p3*y1 + _cosCA)*sa;
  _sinCA = _p3*_x - _p2;
  //cout << " _sinCA:"<<_sinCA<<endl;
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
    _cosCA = ::sqrt(1.- _sinCA*_sinCA);
  //double y0=_p0 + _cosCA/_p3;
  ////if ((_p0-y0)*_p3 >= 0.) throw runtime_error("SKTN::rotate() - Error - Rotation failed!\n");
  //f = F - 1
  double f00=ca-1;
  double f23=(y1 - _sinCA*x1/_cosCA)*sa;
  double f20=_p3*sa;
  double f22=(ca + sa*_sinCA/_cosCA)-1;
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
  /*
  if (newChild->_hit)
    {
      //cout<<"SKTN::add(SKTN*) -I- Add node with hit:"<<endl;
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
      //cout<<"SKTN::add(SKTN*) -I- Add node WIHTOUT hit:"<<endl;
      newChild->nullCount           = nullCount+1;
      newChild->contiguousNullCount = contiguousNullCount+1;
      newChild->hitCount            = hitCount;
      newChild->contiguousHitCount  = 0;
      }*/ 
  children.push_back(newChild);
  newChild->setParent(this);
  //cout<< *this<<endl;
}

/// print to the ostream "os" the parameters of this node 
/// and all its children recursively
ostream& operator<<(ostream& os, const StiKalmanTrackNode& n)
{
  os << " a:" << 180*n._alpha/M_PI<<" degs"
     << " xRef:" << n._x
     << " x:" << n._x
     << " p0:" << n._p0 
     << " p1:" << n._p1 
     << " p2:" << n._p2 
     << " p3:" << n._p3 
     << " p4:" << n._p4
     << " c00:" <<n._c00<< " c11:"<<n._c11
     << " chi2:" << n._chi2 
     << " n:"<<n.hitCount
     << " null:"<<n.nullCount<<endl;
  StiHit * hit = n.getHit();
  if (hit) os << "   hit:"<<*hit<<endl;
  return os;
}

double StiKalmanTrackNode::getWindowY()
{	  
  const StiDetector * detector = getDetector();
  const StiTrackingParameters * parameters = detector->getTrackingParameters();
  double searchWindowScale = parameters->getSearchWindowScale();
  double minSearchWindow   = parameters->getMinSearchWindow();
  double maxSearchWindow   = parameters->getMaxSearchWindow();

  const StiHitErrorCalculator * calc = detector->getHitErrorCalculator();
  if (!calc)throw runtime_error("SKTN::getWindowY() -E- calc==0");
  calc->calculateError(this);
  double window = searchWindowScale*::sqrt(_c00+eyy);
  if (window<minSearchWindow)
    window = minSearchWindow;
  else if (window>maxSearchWindow)
    window = maxSearchWindow;
  return window;
}

//_____________________________________________________________________________
double StiKalmanTrackNode::getWindowZ()
{	 
  const StiDetector * detector = getDetector();
  const StiTrackingParameters * parameters = detector->getTrackingParameters();
  double searchWindowScale = parameters->getSearchWindowScale();
  double minSearchWindow   = parameters->getMinSearchWindow();
  double maxSearchWindow   = parameters->getMaxSearchWindow();

  const StiHitErrorCalculator * calc = detector->getHitErrorCalculator();
  if (!calc)throw runtime_error("SKTN::getWindowY() -E- calc==0");
  calc->calculateError(this);
  double window = searchWindowScale*::sqrt(_c11+ezz); 
  if (window<minSearchWindow)
    window = minSearchWindow;
  else if (window>maxSearchWindow)
    window = maxSearchWindow;
  return window;
}

StThreeVector<double> StiKalmanTrackNode::getHelixCenter() const
{
  if (_p3==0) throw logic_error("StiKalmanTrackNode::getHelixCenter() -F- _p3==0 ");
  double xx0 = _p2/_p3;
  double yy0 = _p0+_cosCA/fabs(_p3);
  double zz0 = _p1+_p4*asin(_sinCA)/_p3;
  return (StThreeVector<double>(_cosAlpha*xx0-_sinAlpha*yy0,_sinAlpha*xx0+_cosAlpha*yy0,zz0));
}

void StiKalmanTrackNode::setParameters(StiKalmanTrackFinderParameters *parameters)
{
  pars = parameters;
}
