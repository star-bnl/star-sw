//StiKalmanTrack.cxx
/*
 * $Id: StiKalmanTrackNode.cxx,v 2.52 2004/12/13 22:52:23 perev Exp $
 *
 * /author Claude Pruneau
 *
 * $Log: StiKalmanTrackNode.cxx,v $
 * Revision 2.52  2004/12/13 22:52:23  perev
 * Off testError
 *
 * Revision 2.51  2004/12/13 20:01:38  perev
 * old version of testError temporary activated
 *
 * Revision 2.50  2004/12/12 01:34:24  perev
 * More smart testError, partial error reset
 *
 * Revision 2.49  2004/12/11 22:17:49  pruneau
 * new eloss calculation
 *
 * Revision 2.48  2004/12/11 04:31:36  perev
 * set of bus fixed
 *
 * Revision 2.47  2004/12/10 15:51:44  fisyak
 * Remove fudge factor from eloss calculation, add more debug printout and tests, reorder calculation of cov. matrix for low triangular form
 *
 * Revision 2.46  2004/12/08 16:56:16  fisyak
 * Fix sign in dE/dx; move from upper to lower triangular matrix convention (StEvent) for px,py,pz
 *
 * Revision 2.45  2004/12/05 00:39:07  fisyak
 * Add test suit for matrix manipulation debugging under overall CPPFLAGS=-DSti_DEBUG
 *
 * Revision 2.44  2004/12/01 14:04:57  pruneau
 * z propagation fix
 *
 * Revision 2.43  2004/11/24 17:59:26  fisyak
 * Set ionization potential for Ar in eloss calculateion instead 5
 *
 * Revision 2.42  2004/11/22 19:43:06  pruneau
 * commented out offending cout statement
 *
 * Revision 2.41  2004/11/22 19:23:20  pruneau
 * minor changes
 *
 * Revision 2.40  2004/11/10 21:46:02  pruneau
 * added extrapolation function; minor change to updateNode function
 *
 * Revision 2.39  2004/11/08 15:32:54  pruneau
 * 3 sets of modifications
 * (1) Changed the StiPlacement class to hold keys to both the radial and angle placement. Propagated the use
 * of those keys in StiSvt StiTpc StiSsd and all relevant Sti classes.
 * (2) Changed the StiKalmanTrackFinder::find(StiTrack*) function's algorithm for the navigation of the
 * detector volumes. The new code uses an iterator to visit all relevant volumes. The code is now more robust and compact
 * as well as much easier to read and maintain.
 * (3) Changed the chi2 calculation in StiKalmanTrack::getChi2 and propagated the effects of this change
 * in both StiTrackingPlots and StiStEventFiller classes.
 *
 * Revision 2.38  2004/10/27 03:25:49  perev
 * Version V3V
 *
 * Revision 2.37  2004/10/26 21:53:23  pruneau
 * No truncation but bad hits dropped
 *
 * Revision 2.36  2004/10/26 06:45:37  perev
 * version V2V
 *
 * Revision 2.35  2004/10/25 14:15:56  pruneau
 * various changes to improve track quality.
 *
 * Revision 2.34  2004/03/24 22:01:07  pruneau
 * Removed calls to center representation and replaced by normal representation
 *
 * Revision 2.33  2004/03/17 21:01:53  andrewar
 * Trapping for negative track error (^2) values _c00 and _c11. This should
 * be a temporary fix until the root of the problem is found. Problem seems
 * localized to trackNodes without hits.
 * Also trapping for asin(x), x>1 in ::length; point to point cord length
 * on the helix is greater than twice radius of curvature. This should also be
 * resovled.
 *
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
#ifdef Sti_DEBUG
#include "TRMatrix.h"
#include "TRVector.h"
#define PrP(A)    cout << "\t" << (#A) << " = \t" << ( A )
#define PrPP(A,B) cout << "=== StiKalmanTrackNode::" << (#A); PrP((B)); cout << endl;
#endif
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
double StiKalmanTrackNode::y2= 0; 
double StiKalmanTrackNode::z1= 0; 
double StiKalmanTrackNode::dx= 0; 
double StiKalmanTrackNode::dl= 0; 
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

int StiKalmanTrackNode::counter = 0;
//debug vars
#ifdef STI_NODE_TEST
int    StiKalmanTrackNode::fDerivTestOn=0;   
#endif
#ifndef STI_NODE_TEST
int    StiKalmanTrackNode::fDerivTestOn=-10;   
#endif

double StiKalmanTrackNode::fDerivTest[5][5];   

#ifdef STI_NODE_DEBUG
void StiKalmanTrackNode::Break(int kase)
{
//VP  printf("*** Break(%d) ***\n",kase);
}		
#endif
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
  _refAngle  = n->_refAngle;
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
  setChi2(1e59);   //VP
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
  _refAngle = n->_refAngle;
  _alpha    = n->_alpha;
  _cosAlpha = n->_cosAlpha;
  _sinAlpha = n->_sinAlpha;
  _cosCA = n->_cosCA;
  _sinCA = n->_sinCA;
  setChi2 (n->_chi2);
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
#ifdef  Sti_DEBUG
  TRMatrix A(3,5,
	     0.               ,                                0.,  0.,
	     0.               ,                                0.,  0.,
	     (_x-_p2/c)/_cosCA, -(_p2*_p2-_x*_p2*c-1)/(cc*_cosCA),  0.,
	     -1/c             ,                            _p2/cc,  0.,
	     0.               ,                           -_p4/c,1./c); 
  A *= kField;
  TRSymMatrix C(5,&_c00);
  TRSymMatrix E(A,TRArray::kAxSxAT,C);
#endif
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
  // switch to lower triangular matrix convention
  e[0] = d00;  // px-px
  e[1] = d01;  // px-py
  e[2] = d11;  // py-py
  e[3] = d02;  // px-pz
  e[4] = d12;  // py-pz
  e[5] = d22;  // pz-pz
#ifdef Sti_DEBUG
  TRSymMatrix E1(3,e);
  E1.Verify(E);//,1e-7,2);
#endif
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
  return getPsi()-getHelicity()*M_PI/2;

}
double StiKalmanTrackNode::getPsi() const
{
  return getGlobalMomentum().phi();
}

/// returns momentum and its error matrix 
/// in cartesian coordinates in the _global_
/// ref frame of the experiment
/// p[0] = px
/// p[1] = py
/// p[2] = pz
/// Use lower triangular matrix
/// e[0] = px-px
/// e[1] = px-py
/// e[2] = py-py
/// e[3] = px-pz
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
#ifdef Sti_DEBUG
  TRMatrix A(3,3,
	     _cosAlpha, -_sinAlpha, 0.,
	     _sinAlpha,  _cosAlpha, 0.,
	     0.       ,         0.,  ((pars->field<0)? -1.:1.));      
  PrPP(getGlobalMomentum,A);
  TRVector P(3,p); PrPP(getGlobalMomentum,P);
  P = TRVector(A,TRArray::kAxB,P); PrPP(getGlobalMomentum,P);
#endif
  double a00=_cosAlpha, a01=-_sinAlpha, a02=0;
  double a10=_sinAlpha, a11= _cosAlpha, a12=0;
  double a20= 0, a21=  0, a22=(pars->field<0)? -1:1;
  double px=p[0];
  double py=p[1];
  double pz=p[2];
  p[0] = a00*px + a01*py + a02*pz;
  p[1] = a10*px + a11*py + a12*pz;
  p[2] = a20*px + a21*py + a22*pz;
#ifdef Sti_DEBUG
  TRVector P1(3,p);
  P1.Verify(P);//,1e-7,2);
#endif
  if (e==0) return;
#ifdef Sti_DEBUG
  TRSymMatrix E(3,e);
  E = TRSymMatrix(A,TRArray::kAxSxAT,E);
#endif
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
  // Lower triangular matrix (before it was upper one)
  e[0] = d00;  // px-px
  e[1] = d01;  // px-py
  e[2] = d11;  // py-py
  e[3] = d02;  // px-pz
  e[4] = d12;  // py-pz
  e[5] = d22;  // pz-pz
#ifdef Sti_DEBUG
  TRSymMatrix E1(3,e);
  E1.Verify(E);//,1e-7,2);
#endif
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
				  const StiDetector * tDet,int dir)
{
  det = tDet;
  int position = 0;
  setState(pNode);
  StiPlacement * place = tDet->getPlacement();
  StiShape * sh = tDet->getShape();
  int shapeCode = sh->getShapeCode();
  _refX = place->getLayerRadius();
  _refAngle = place->getLayerAngle();

  if(shapeCode!=kCylindrical)
    { //flat volume
      double tAlpha = nice(place->getNormalRefAngle());
      double dAlpha = tAlpha - _alpha;
      // bail out if the rotation fails...
      if (fabs(dAlpha)>0.5e-2)
	if (rotate(dAlpha)) return -10;
    }
  //planarShape = 0;
  //cylinderShape = 0;


  //if (false&&_refX<20.) cout << "Node:refX<4.5 pNode:"<< *pNode;
  position = propagate(place->getNormalRadius(),sh->getShapeCode(),dir); 
  if (position<0) 
    {
      //if (shapeCode==kCylindrical) cout << "cylinder returns:"<<position<<endl;
      return position;
    }
  if (shapeCode==kCylindrical)
    {
      double rotA = atan2(_p0,_x);
      /*      cout << " rotA:"<<180.*rotA/3.1415<<endl;
      cout << " cylinder has x:" << _x << " p0:" << _p0 << " p1:" << _p1 << " _p2:"<<_p2<<" _p3:" << _p3 <<" p4:"<<_p4 << endl;
      cout << " sinCA:"<< _sinCA; if (fabs(_sinCA)<=1.) cout << " CA:"<<asin(_sinCA);
      cout << endl;
      cout << " while alpha:" << 180*_alpha/3.1415<<endl;
      */
      if (rotate(rotA)!=0)
	{
	  cout << " cylinder return -50"<<endl;
	  return -50;
	}
    }
  position = locate(place,sh);
  if (position>kEdgeZplus || position<0) return position;
  propagateError();
  // Multiple scattering
  if (pars->mcsCalculated && fabs(pars->field)>0 )  propagateMCS(pNode,tDet);
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
bool StiKalmanTrackNode::propagate(const StiKalmanTrackNode *parentNode, StiHit * vertex,int dir)
{
  setState(parentNode);
  //double locVx = _cosAlpha*vertex->x() + _sinAlpha*vertex->y();
  if (propagate(vertex->x(),kPlanar,dir) < 0)
    return false; // track does not reach vertex "plane"
  propagateError();
  _hit = vertex;
  _detector = 0;
  return true;
}


///Propagate track from the given node to the beam line with x==0.
///Set the hit and detector pointers to null to manifest this is an extrapolation
bool StiKalmanTrackNode::propagateToBeam(const StiKalmanTrackNode *parentNode,int dir)
{
  setState(parentNode);
  if (propagate(0., kPlanar,dir) < 0) return false; // track does not reach vertex "plane"
  propagateError();
  _hit = 0;
  _detector = 0;
  return true;
}

///Extrapolate the track defined by the given node to the given radius.
///Return a negative value if the operation is impossible.
int StiKalmanTrackNode::propagateToRadius(StiKalmanTrackNode *pNode, double radius,int dir)
{
  int position = 0;
  setState(pNode);
  _refX = radius;
  position = propagate(radius,kCylindrical,dir);
  if (position<0) return position;
  propagateError();
  return position;
}


/*! Work method used to perform the tranport of "this" node from 
  its current "_x" position to the given position "xk". 
  Returns -1 if the propagation cannot be carried out, i.e.
  if the track curvature is such it cannot reach the desired 
  location.
  option == 0 Planar
  option == 1 Cylinder
 */
int  StiKalmanTrackNode::propagate(double xk, int option,int dir)
{
  static int nCalls=0;
  nCalls++;
  numeDeriv(xk,1,option,dir);
  assert(fabs(_x )<1.e+10);
  assert(fabs(_p0)<1.e+10);
  assert(fabs(_sinCA)<1.0001);
  assert(fabs(_cosCA)<1.0001);
  x1=_x;  y1=_p0;  z1=_p1; cosCA1 =_cosCA; sinCA1 =_sinCA;
  double rho = _p3;

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
      if (sq<0) return -2;
      sq = ::sqrt(sq);				
      double x_p = a*(x0+y0*sq)/r0sq;
      double x_m = a*(x0-y0*sq)/r0sq;
      x2 = ( x_p>x_m) ? x_p : x_m;
      if (x2<=0) return -3;

    }
  dx=x2-x1;  
  double test = (dir)? dx:-dx;  
//   if track is coming back stop tracking
  if (test<0) return -3;
//   strange case, temporary return;
  if (fabs(test) < 1.e-6) return 0;

  double dsin = _p3*dx;
  sinCA2=sinCA1 + dsin; 
  if (fabs(sinCA2)>0.99) return -4;
  cosCA2   = ::sqrt((1.-sinCA2)*(1.+sinCA2));
  sumSin   = sinCA1+sinCA2;
  sumCos   = cosCA1+cosCA2;
  sinCA1plusCA2    = sinCA1*cosCA2 + sinCA2*cosCA1;
  double sind = 0;
  double cosd = cosCA2*cosCA1+sinCA2*sinCA1;
  if (fabs(dsin) < 0.02 && cosd >0.1) { //tiny angle
    dl = dx*sumSin/sinCA1plusCA2;
  } else {
    sind = sinCA2*_cosCA-cosCA2*_sinCA;
    dl = atan2(sind,cosd)/rho;
  }

  _p1 += dl*_p4;
  _p0      += dx*sumSin/sumCos;
  //if (fabs(_p1)>200.) cout << "propagate()[2] -W- _p0:"<<_p0<<" _p1:"<<_p1<<endl;
  // sanity check - to abandon the track
  if (fabs(_p0)>200. || fabs(_p1)>200. ) return -6;
  _x       = x2;
  _sinCA   = sinCA2;
  _cosCA   = cosCA2;
  assert(fabs(_x )<1.e+10);
  assert(fabs(_p0)<1.e+10);
  assert(fabs(_sinCA)<1.0001);
  assert(fabs(_cosCA)<1.0001);
  
  return 0;
}

int StiKalmanTrackNode::nudge()
{
  double deltaX = _hit->x()-_x;
  sinCA2=_p3*(_x+deltaX) - _p2; 
  //cout << " StiKalmanTrackNode::nudge() -I- sin(CA2):"<<sinCA2<<endl;
  if (fabs(sinCA2)>1.) return -7;
  cosCA2   = ::sqrt((1.-sinCA2)*(1.+sinCA2));
  if (_cosCA<0) cosCA2=-cosCA2;
  sumSin   = sinCA1+sinCA2;
  sinCA1plusCA2    = sinCA1*cosCA2 + sinCA2*cosCA1;
  if (fabs(sinCA1plusCA2)==0) return -8;
  sumCos   = cosCA1+cosCA2;
  double dp0 = deltaX*sumSin/sumCos;
  double dp1 = deltaX*_p4*sumSin/sinCA1plusCA2;
  if (fabs(_p0+dp0)>200. || fabs(_p1+dp1)>200. ) return -9;
  _p0      += dp0;
  _p1      += dp1;
  _x       = _x+deltaX;
  _sinCA   = sinCA2;
  _cosCA   = cosCA2;

  assert(fabs(_sinCA) < 1.0001);
  assert(fabs(_cosCA) < 1.0001);
	return 0;
}



/// Propagate the track error matrix
/// \note This method must be called ONLY after a call to the propagate method.
void StiKalmanTrackNode::propagateError()
{  
  testError(&_c00);
  bool debug = false;//counter<300;
  if (_c00<0 || _c11<0 || _c22<0) Break(201);
  if (_c00>1.e6 || _c11> 1.e6|| _c22>1.e6) Break(201);
  if (debug) 
    {
      counter++;
      cout << "Prior Error:"
	   << "c00:"<<_c00<<endl
	   << "c10:"<<_c10<<" c11:"<<_c11<<endl
	   << "c20:"<<_c20<<" c21:"<<_c21<<endl
	   << "c30:"<<_c30<<" c31:"<<_c31<<endl
	   << "c40:"<<_c40<<" c41:"<<_c41<<endl;
    }

  //f = F - 1
  double xx=x1+x2;
  double tanCA1=sinCA1/cosCA1;
  double tanCA2=sinCA2/cosCA2;
//VP  double f02=-dx*(2*sumCos + sumSin*(tanCA1+tanCA2) )/(sumCos*sumCos);
  double dy = _p0-y1;   
  double dxCA = (dx*cosCA2+dy*sinCA2);
  double f02 = -dxCA/cosCA1/cosCA2;
  double f03= dx*(sumCos*xx + sumSin*(tanCA1*x1+tanCA2*x2))/(sumCos*sumCos);
  sinCA1plusCA2=sinCA1*cosCA2+sinCA2*cosCA1;
//VP  double f12=-dx*_p4*(2*sinCA1plusCA2 + 
//VP		      sumSin*(sinCA2*tanCA1-cosCA1 + sinCA1*tanCA2-cosCA2))/(sinCA1plusCA2*sinCA1plusCA2);
  double f12 = _p4*f02*dy/dxCA;

//VP  double f13=dx*_p4*(xx/sinCA1plusCA2 
//VP		     -sumSin*(x1*(cosCA2-sinCA2*tanCA1) 
//VP			      +x2*(cosCA1-sinCA1*tanCA2))/sinCA1plusCA2/sinCA1plusCA2);
  double f13;
  double dang = dl*_p3;
  if (fabs(dang) < 0.2) {
    f13 = (sinCA2 -dang*(cosCA2/3. +dang*(sinCA2/12.)))/(1. -dang*dang/12.);
  } else {
    double Dsin= sinCA2-sinCA1, Dcos = cosCA2-cosCA1;
    f13 = (Dsin-cosCA2*dang)/(cosCA2*Dcos+sinCA2*Dsin);
  }
  f13 = _p4*f03*f13;
  f13 = _p4*((x2-x1)-cosCA2*dl)/(cosCA2*_p3);

//  double f14= dx*sumSin/sinCA1plusCA2; 
  double f14= dl; 

  double f[5][5]; memset(f,0,sizeof(f));
  f[0][2]=f02; f[0][3]=f03; f[1][2]=f12;f[1][3]=f13;f[1][4]=f14;
  testDeriv(f[0]);
#ifdef Sti_DEBUG
  TRMatrix F(5,5,
	     1., 0., f02, f03,  0.,
	     0., 1., f12, f13, f14,
	     0., 0.,  1.,  0.,  0.,
	     0., 0.,  0.,  1.,  0.,
	     0., 0.,  0.,  0.,  1.);
  TRSymMatrix C(5,&_c00);
#endif
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
  if (debug)
    {  
      cout << "SKTN::propagateError()"<<endl
	   << " dx:"<<dx<<" sumCos:"<<sumCos<<" cosCA1:"<<cosCA1<<" cosCA2:"<<cosCA2
	   <<" sinCA1:"<<sinCA1<<" sinCA2:"<<sinCA2<<" sumSin:"<<sumSin<<endl
	   <<" sinCA1plusCA2:"<<sinCA1plusCA2<<endl
	   <<" f02:"<<f02<<" f03:"<<f03<<endl
	   <<" f12:"<<f12<<" f13:"<<f13<<" f14:"<<f14<<endl
	   <<" b00:"<<b00<<" b01:"<<b01<<" b10:"<<b10<<" b11:"<<b11<<" b20:"<<b20
	   <<" b21:"<<b21<<" b30:"<<b30<<" b31:"<<b31<<" b40:"<<b40<<" b41:"<<b41<<endl
	   <<" a00:"<<a00<<" a01:"<<a01<<" a11:"<<a11<<endl;
    }
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
  int ierr;if ((ierr=testError(&_c00))) Break(1000+ierr);
#ifdef Sti_DEBUG
  // C^k-1_k = F_k * C_k-1 * F_kT + Q_k
  C = TRSymMatrix(F,TRArray::kAxSxAT,C);
  TRSymMatrix C1(5,&_c00);
  C1.Verify(C);//,1e-7,2);
#endif
  if (debug) 
    {
      cout << "Post Error:"
	   << "c00:"<<_c00<<endl
	   << "c10:"<<_c10<<" c11:"<<_c11<<endl
	   << "c20:"<<_c20<<" c21:"<<_c21<<endl
	   << "c30:"<<_c30<<" c31:"<<_c31<<endl
	   << "c40:"<<_c40<<" c41:"<<_c41<<endl;
    }
  /*
  if(_c00<=0. || _c11<=0. || _c00>1000. || _c11>1000.)
    {//xxxxxxxxxxxxxxxxxxx
      cout << " StiKalmanTrackNode::propagateError() -W- _c00:"<<_c00<<"  _c11:"<<_c11<< " reset"<<endl;
      _c00=_c11=_c22=_c33=_c44=2.;
      _c10=_c20=_c21=_c30=_c31=_c32=_c40=_c41=_c42=_c43=0.;
      }*/
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

  /*
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
  */

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
  double ey  = _p3*_x-_p2; //C*x -eta = sin(Phi)
  double ez  = _p4;        //tan(Lambda)
  double xz  = _p3*ez;     //C*tan(Lambda)
  double zz1 = ez*ez+1;    //1+tan**2(Lambda) = 1/cons**2(Lambda)
  double xy  = _p2+ey;     //eta + C*x - eta = C*x 
#ifdef Sti_DEBUG
  TRSymMatrix C(5,&_c00);
  TRSymMatrix Q(5,                           
		0.,                                                                  //0
		0., 0.,                                                              //1
		0., 0.,2*ey*ez*ez*_p2+1-ey*ey+ez*ez+_p2*_p2*ez*ez,                   //2
		0., 0.,                                  xz*ez*xy, xz*xz,            //3
		0., 0.,                                 ez*zz1*xy, xz*zz1, zz1*zz1); //4
  Q *= theta2;
  C += Q;
#endif
  _c33 += xz*xz*theta2;
  _c32 += xz*ez*xy*theta2;
  _c43 += xz*zz1*theta2;
  _c22 += (2*ey*ez*ez*_p2+1-ey*ey+ez*ez+_p2*_p2*ez*ez)*theta2;
  _c42 += ez*zz1*xy*theta2;
  _c44 += zz1*zz1*theta2;
#ifdef Sti_DEBUG
  TRSymMatrix CP(5,&_c00);
  CP.Verify(C);//,1e-7,2);
#endif
  double dE=0;
  double sign;
  if (dx>0)
    sign = 1.;
  else
    sign = -1.;
  const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
  StiElossCalculator * calculator = tDet->getElossCalculator();
  if ( !calculator) 
    {
      cout <<" merde happens" << endl;
      throw logic_error("CALCULATOR");
    }
  double eloss = tDet->getElossCalculator()->calculate(1.,m, beta2);
  dE = sign*dxEloss*eloss;
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
      correction =1. + ::sqrt(e2)*dE/p2;
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
  
  double m = delta.perp();
  double as = 0.5*m*curv;
  double lxy=0;
  if (fabs(as)<0.01) { lxy = m*(1.+as*as/24);}
  else               { lxy = 2.*asin(as)/curv;}
  return sqrt(lxy*lxy+delta.z()*delta.z());
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
  double dsin =_p3*(hit->x()-_x);
  if (fabs(_sinCA+dsin)>0.99) return 1e+50;

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
#ifdef Sti_DEBUG
  TRSymMatrix R(2,
		r00,
		r01, r11);
  TRSymMatrix G(R,TRArray::kInverted);
#endif
  double det=r00*r11 - r01*r01;
  //if (_c00<=0 || _c11<=0 || det<=0)
  //  cout << endl << "evalChi2 c00:"<<_c00<< " c10:"<<_c10<<" c11:"<<_c11<<" det:"<<det<< " eyy:"<<eyy<<" ezz:"<<ezz<<endl;
  if (fabs(det)==0.) throw runtime_error("SKTN::evaluateChi2() Singular matrix !\n");
  double tmp=r00; r00=r11; r11=tmp; r01=-r01;  
  double dy=hit->y()-_p0;
  double dz=hit->z()-_p1;
  double cc= (dy*r00*dy + 2*r01*dy*dz + dz*r11*dz)/det;
  if (cc<0.) Break(2);
#ifdef Sti_DEBUG
  TRVector r(2,hit->y()-_p0,hit->z()-_p1);
  Double_t chisq = G.Product(r,TRArray::kATxSxA);
  Double_t diff = chisq - cc;
  Double_t sum  = chisq + cc;
  if (diff > 1e-7 || (sum > 2. && (2 * diff ) / sum > 1e-7)) {
    cout << "Failed:\t" << chisq << "\t" << cc << "\tdiff\t" << diff << endl;
  }
#endif
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
int StiKalmanTrackNode::updateNode() 
{
  testError(&_c00);
  double r00,r01,r11;
  const StiDetector * detector = _hit->detector();
  if (_c00<0 || _c11<0 || _c22<0) Break(201);
  if (_c00>1.e6 || _c11> 1.e6|| _c22>1.e6) Break(201);
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
#ifdef Sti_DEBUG
  TRSymMatrix R1(2,
		 r00,
		 r01, r11); PrPP(updateNode,R1);
  static const TRMatrix H(2,5,
			  1., 0., 0., 0., 0.,
			  0., 1., 0., 0., 0.);
  double eYY = eyy;
  double eYZ =  0.;
  double eZZ = ezz;
  if (! (useCalculatedHitError && detector))
    {
      eYY=_hit->syy();
      eYZ=_hit->syz();  eZZ=_hit->szz();
    }  
  TRSymMatrix V(2,
		eYY,
		eYZ, eZZ); PrPP(updateNode,V);
  TRSymMatrix C(5,&_c00);  PrPP(updateNode,C);
  TRSymMatrix R(H,TRArray::kAxSxAT,C); PrPP(updateNode,R);
  R += V;                              PrPP(updateNode,R);
  TRSymMatrix G(R,TRArray::kInverted); PrPP(updateNode,G);
#endif
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
  double dp3  = k30*dy + k31*dz;
  double dp2  = k20*dy + k21*dz;
  double dp4  = k40*dy + k41*dz;
#ifdef Sti_DEBUG
  double dp0  = k00*dy + k01*dz;
  double dp1  = k10*dy + k11*dz;
  // K = C * HT * G
  TRMatrix T(C,TRArray::kSxAT,H); PrPP(updateNode,T);
  TRMatrix K(T,TRArray::kAxS,G);  PrPP(updateNode,K);
  TRMatrix K1(5,2,
	      k00, k01,
	      k10, k11,
	      k20, k21,
	      k30, k31,
	      k40, k41);   PrPP(updateNode,K1);
  K1.Verify(K);
  TRVector dR(2,dy, dz);
  TRVector dP1(5, dp0, dp1, dp2, dp3, dp4);
  TRVector dP(K,TRArray::kAxB,dR);
  dP1.Verify(dP);//,1e-7,2);
#endif
  if (_x>2.)
    {
      if ( (dp4*dp4/16.>_c44) || (dp3*dp3/16. > _c33) || (dp2*dp2/16.>_c22) ) return 1;
    }

  double eta  = _p2 + dp2;
  double cur  = _p3 + dp3;
  double tanl = _p4 + dp4;
  StiKalmanTrackNode * parent = static_cast<StiKalmanTrackNode*>(getParent());
  if (parent && _x>2.)
    {
      double d_p2 = parent->_p2 - eta;
      double d_p3 = parent->_p3 - cur;
      double d_p4 = parent->_p4 - tanl;
      //cannot test on p2 here because parent and this node may not be in same reference frame...
      if ( (d_p4*d_p4/16.>_c44) || (d_p3*d_p3/16. > _c33) ) return 1;
      //if (fabs(eta-parent->_p2)>3 || fabs(tanl-parent->_p4)>0.27) return 2;
    }
  
  // Check if any of the quantities required to pursue the update
  // are infinite. If so, it means the tracks cannot be update/propagated
  // any longer and should therefore be abandoned. Just return. This is 
  // not a big but rather a feature of the fact a helicoidal tracks!!!
  if (!finite(_c00)||!finite(_c11)||!finite(k30)||!finite(k31))  return -11;
  // update Kalman state
   double p0 = _p0 + k00*dy + k01*dz;
  _p0 += k00*dy + k01*dz;
  if (fabs(p0)>200.) 
    {
      cout << "updateNode()[1] -W- _p0:"<<_p0<<" _p1:"<<_p1<<endl;
      return -12;
    }
  double p1 = _p1 + k10*dy + k11*dz;
  if (fabs(p1)>200.) 
    {
      cout << "updateNode()[2] -W- _p0:"<<_p0<<" _p1:"<<_p1<<endl;
      return -13;
    }
  //_p4 += k40*dy + k41*dz;
  double sinCA  =  cur*_x-eta;
  // The following test introduces a track propagation error but happens
  // only when the track should be aborted so we don't care...
  if (fabs(sinCA)>0.9999)
    {
      //cout << " SKTN    _sinCA>1";
      return -14;
    }
  _p0  = p0;
  _p1  = p1;
  _p2  = eta;
  _p3  = cur;
  _p4  = tanl;
  _sinCA = sinCA;
  _cosCA = ::sqrt(1.-_sinCA*_sinCA); 
  // update error matrix
  double c00=_c00;                       
  double c10=_c10, c11=_c11;                 
  double c20=_c20, c21=_c21;//, c22=_c22;           
  double c30=_c30, c31=_c31;//, c32=_c32, c33=_c33;     
  double c40=_c40, c41=_c41;//, c42=_c42, c43=_c43, c44=_c44;
  _c00-=k00*c00+k01*c10;
  _c10-=k10*c00+k11*c10;_c11-=k10*c10+k11*c11;
  _c20-=k20*c00+k21*c10;_c21-=k20*c10+k21*c11;_c22-=k20*c20+k21*c21;
  _c30-=k30*c00+k31*c10;_c31-=k30*c10+k31*c11;_c32-=k30*c20+k31*c21;_c33-=k30*c30+k31*c31;
  _c40-=k40*c00+k41*c10;_c41-=k40*c10+k41*c11;_c42-=k40*c20+k41*c21;_c43-=k40*c30+k41*c31;_c44-=k40*c40+k41*c41;

    int ierr;if ((ierr=testError(&_c00))) Break(1000+ierr);

#ifdef Sti_DEBUG
  TRSymMatrix W(H,TRArray::kATxSxA,G);    PrPP(updateNode,W); 
  TRSymMatrix C0(C);
  C0 -= TRSymMatrix(C,TRArray::kRxSxR,W); PrPP(updateNode,C0);
  TRSymMatrix C1(5,&_c00); PrPP(updateNode,C1);
  C1.Verify(C0);//,1e-7,2);
  //   update of the covariance matrix:
  //    C_k = (I - K_k * H_k) * C^k-1_k * (I - K_k * H_k)T + K_k * V_k * KT_k
  // P* C^k-1_k * PT
  TRMatrix A(K,TRArray::kAxB,H);
  TRMatrix P(TRArray::kUnit,5);
  P -= A;
  TRSymMatrix C2(P,TRArray::kAxSxAT,C); PrPP(updateNode,C2);
  TRSymMatrix Y(K,TRArray::kAxSxAT,V);  PrPP(updateNode,Y);
  C2 += Y;                              PrPP(updateNode,C2);
  C2.Verify(C0);
  C2.Verify(C1);
#endif 
  return 0;
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
int StiKalmanTrackNode::rotate (double alpha) //throw ( Exception)
{
  //cout << "rotate by alpha:"<< 180.*alpha/3.1415927<<endl;
  //cout << "         _alpha:"<< 180.*_alpha/3.1415927<<endl;
  testError(&_c00);
  numeDeriv(alpha,2);
  _alpha += alpha;
  _alpha = nice(_alpha);
//VP  if (fabs(_alpha) > M_PI*0.33) return -15;
    //cout << "    new  _alpha:"<< 180.*_alpha/3.1415927<<endl;

  double x1=_x; 
  double y1=_p0; 
  sinCA1 = _sinCA;
  cosCA1 = _cosCA;
  double ca = cos(alpha);
  double sa = sin(alpha);
  _x = x1*ca + y1*sa;
  _p0=-x1*sa + y1*ca;
  _p2=_p2*ca + (_p3*y1 + cosCA1)*sa;
//VP  _sinCA = _p3*_x - _p2;
  _cosCA =  cosCA1*ca+sinCA1*sa;
  _sinCA = -cosCA1*sa+sinCA1*ca;
  
//cout << " _sinCA:"<<_sinCA<<endl;
  assert(fabs(_sinCA)<=1.001);
  assert(fabs(_cosCA)<=1.001);
#ifdef Sti_DEBUG
  TRSymMatrix C(5,&_c00); PrPP(rotate,C);
  TRMatrix F(5,5,
	     ca    , 0., 0.,                                              0., 0.,
	     0.    , 1., 0.,                                              0., 0.,
	     _p3*sa, 0., (ca + sa*_sinCA/_cosCA), (y1 - _sinCA*x1/_cosCA)*sa, 0.,
	     0.    , 0., 0.,                                              1., 0.,
             0.    , 0., 0.,                                              0., 1.);  PrPP(rotate,F);
  C = TRSymMatrix(F,TRArray::kAxSxAT,C);    PrPP(rotate,C);
#endif
  ////if ((_p0-y0)*_p3 >= 0.) throw runtime_error("SKTN::rotate() - Error - Rotation failed!\n");
  //f = F - 1
  double f[5][5];
  memset(f,0,sizeof(f));
  f[0][0]=ca-1;
  f[2][0]=_p3*sa;
//VP   f[2][2]=(ca + sa*_sinCA/_cosCA)-1;  	//VP WRONG
  f[2][2]=(ca + sa*sinCA1/cosCA1)-1;  
//VP  f[2][3]=(y1 - _sinCA*x1/_cosCA)*sa; 	//VP WRONG
  f[2][3]=(y1 - sinCA1*x1/cosCA1)*sa; 		
  //b = C*ft
  testDeriv(f[0]);
  double b00=_c00*f[0][0], b02=_c00*f[2][0]+_c30*f[2][3]+_c20*f[2][2];
  double b10=_c10*f[0][0], b12=_c10*f[2][0]+_c31*f[2][3]+_c21*f[2][2];
  double b20=_c20*f[0][0], b22=_c20*f[2][0]+_c32*f[2][3]+_c22*f[2][2];
  double b30=_c30*f[0][0], b32=_c30*f[2][0]+_c33*f[2][3]+_c32*f[2][2];
  double b40=_c40*f[0][0], b42=_c40*f[2][0]+_c43*f[2][3]+_c42*f[2][2];
  //a = f*b = f*C*ft
  double a00=f[0][0]*b00, a02=f[0][0]*b02, a22=f[2][0]*b02+f[2][3]*b32+f[2][2]*b22;
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
#ifdef Sti_DEBUG
  TRSymMatrix C1(5,&_c00);  PrPP(rotate,C1);
  C1.Verify(C);//,1e-7,2);
#endif  
  _cosAlpha=cos(_alpha); 
  _sinAlpha=sin(_alpha); 
  int ierr;
  if ((ierr=testError(&_c00))) Break(1000+ierr);
  return 0;
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
  const StiDetector *det = n.getDetector();
  if (det) os  <<"Det:"<<n.getDetector()->getName();
  else     os << "Det:UNknown";
  os << " a:" << 180*n._alpha/M_PI<<" degs"
     << " refX:" << n._refX
     << " refAngle:" << n._refAngle <<endl
     << "\tx:" << n._x
     << " p0:" << n._p0 
     << " p1:" << n._p1 
     << " p2:" << n._p2 
      << " p3:" << n._p3 
     << " p4:" << n._p4
     << " c00:" <<n._c00<< " c11:"<<n._c11 
     << " pT:" << n.getPt() << endl;
  StiHit * hit = n.getHit();
  if (hit) os << "\thas hit with chi2 = " << n.getChi2()
	      << " n:"<<n.hitCount
	      << " null:"<<n.nullCount
	      << endl<<"\t hit:"<<*hit;
  else os << endl;
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
  if (!calc)throw runtime_error("SKTN::getWindowZ() -E- calc==0");
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


int StiKalmanTrackNode::locate(StiPlacement*place,StiShape*sh)
{
  int position;
  double yOff, yAbsOff, detHW, detHD,edge,innerY, outerY, innerZ, outerZ, zOff, zAbsOff;
  //fast way out for projections going out of fiducial volume
  if (fabs(_p1)>200. || fabs(_p0)>200. ) position = -1;
  yOff = _p0 - place->getNormalYoffset();
  yAbsOff = fabs(yOff);
  zOff = _p1 - place->getZcenter();
  zAbsOff = fabs(zOff);
  detHW = sh->getHalfWidth();
  detHD = sh->getHalfDepth();
  if (_x<50.)
    edge  = 0.3;  
  else
    edge  = 2.;

  innerY = detHW - edge;
  innerZ = detHD - edge;
  //outerY = innerY + 2*edge;
  //outerZ = innerZ + 2*edge;
  outerY = innerY + edge;
  outerZ = innerZ + edge;

  if (yAbsOff<innerY && zAbsOff<innerZ)
    position = kHit; 
  else if (yAbsOff>outerY && (yAbsOff-outerY)>(zAbsOff-outerZ))
    // outside detector to positive or negative y (phi)
    // if the track is essentially tangent to the plane, terminate it.
    if (fabs(_sinCA)>0.9999 || _p4>57.2)
      return -16;
    else
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
  return position;
}
#ifdef STI_NODE_DEBUG
//______________________________________________________________________________
void StiKalmanTrackNode::setChi2(double chi2)
{
      _chi2 = chi2;
      if(chi2 < 0.) Break(1);
static const double MyChi2 = 1.18179;
      if (chi2 <  0.9999*MyChi2) return;
      if (chi2 >  1.0001*MyChi2) return;
  printf("BOT OHO: %g %p\n",chi2,(void*)getHit());
}
#endif 
#if 1
//______________________________________________________________________________
int StiKalmanTrackNode::testError(double *emx)
{
  static int nCalls=0; nCalls++;
  static const double big=1.e+6;
  enum {kC00, kC10, kC11, kC20, kC21
       ,kC22, kC30, kC31, kC32, kC33
       ,kC40, kC41, kC42, kC43, kC44};

  return 0;
  int ians;
  ians=1; if (emx[kC00]<0) goto RETN;
  ians=2; if (emx[kC11]<0) goto RETN;
  ians=3; if (emx[kC22]<0) goto RETN;
  ians=4; if (emx[kC33]<0) goto RETN;
  ians=5; if (emx[kC44]<0) goto RETN;

  ians=11; if (emx[kC00]>big) goto RETN;
  ians=22; if (emx[kC11]>big) goto RETN;
  ians=33; if (emx[kC22]>big) goto RETN;
  ians=44; if (emx[kC33]>big) goto RETN;
  ians=55; if (emx[kC44]>big) goto RETN;

  ians=10; if (emx[kC10]*emx[kC10]>emx[kC11]*emx[kC00]) goto RETN;
  ians=20; if (emx[kC20]*emx[kC20]>emx[kC22]*emx[kC00]) goto RETN;
  ians=21; if (emx[kC21]*emx[kC21]>emx[kC22]*emx[kC11]) goto RETN;
  ians=30; if (emx[kC30]*emx[kC30]>emx[kC33]*emx[kC00]) goto RETN;
  ians=31; if (emx[kC31]*emx[kC31]>emx[kC33]*emx[kC11]) goto RETN;
  ians=32; if (emx[kC32]*emx[kC32]>emx[kC33]*emx[kC22]) goto RETN;
  ians=40; if (emx[kC40]*emx[kC40]>emx[kC44]*emx[kC00]) goto RETN;
  ians=41; if (emx[kC41]*emx[kC41]>emx[kC44]*emx[kC11]) goto RETN;
  ians=42; if (emx[kC42]*emx[kC42]>emx[kC44]*emx[kC22]) goto RETN;
  ians=43; if (emx[kC43]*emx[kC43]>emx[kC44]*emx[kC33]) goto RETN;
  return 0;
RETN:
#ifdef STI_NODE_TEST
  printf("***StiKalmanTrackNode::testError =%d ***\n",ians);
#endif
  memset (emx,0,sizeof(double)*15);
  emx[kC00]= 100*100;
  emx[kC11]= 100*100;
  emx[kC22]=  10*10;
  emx[kC33]=   1*1;
  emx[kC44]= 100*100;;


  return ians;
}
#endif//0
#if 0
//______________________________________________________________________________
int StiKalmanTrackNode::testError(double *emx)
{
// Test and correct error matrix. Output : number of fixes
// DO NOT IMPROVE weird if() here. This accounts NaN


  static int nCalls=0; nCalls++;
  static const double dia[5] = { 10000., 10000.,100.,3.,10000.};
  static const int    idx[5][5] = 
  {{0,1,3,6,10},{1,2,4,7,11},{3,4,5,8,12},{6,7,8,9,13},{10,11,12,13,14}};
return 0;
  int ians=0,j1,j2,jj;
  for (j1=0; j1<5;j1++){
    jj = idx[j1][j1];
    if (emx[jj]<=10*dia[j1] && (emx[jj]>0)) continue;
//    assert(finite(emx[jj]));
    ians++; emx[jj]=dia[j1];
    for (j2=0; j2<5;j2++){if (j1!=j2) emx[idx[j1][j2]]=0;}
  }
  for (j1=0; j1< 5;j1++){
  for (j2=0; j2<j1;j2++){
    jj = idx[j1][j2];
//    assert(finite(emx[jj]));
    if (emx[jj]*emx[jj]<0.9*emx[idx[j1][j1]]*emx[idx[j2][j2]]) continue;
    ians++;emx[jj]=0;
  }}
  return ians;
}
#endif//0
//______________________________________________________________________________
void StiKalmanTrackNode::numeDeriv(double val,int kind,int shape,int dir)
{
   if (fDerivTestOn<0) return;
   fDerivTestOn=-1;
   double pend[7],save[20];
   StiKalmanTrackNode myNode;
   double *pars = &myNode._p0;
   int state=0;

   saveStatics(save);
   for (int ipar=-1;ipar<5;ipar++)
   {
     myNode = *this;
     backStatics(save);
     double step;
     if (ipar>=0) {
       step = 1.e-4*pars[ipar];
       if (fabs(step)<1.e-7) step = 1.e-7;
       pars[ipar] +=step;
// 		Update sinCA & cosCA       
       pars[5] = pars[3]*myNode._x-pars[2];
       pars[6] = sqrt((1.-pars[5])*(1.+pars[5]));
     }

     switch (kind) {
       case 1: //propagate
	 state = myNode.propagate(val,shape,dir); break;
       case 2: //rotate
	 state = myNode.rotate(val);break;
       default: assert(0);  
     }  
     if(state  ) {fDerivTestOn=0;backStatics(save);return;}
     if (ipar<0) {memcpy(pend,pars,sizeof(pend));continue;}
   
     for (int jpar=0;jpar<5;jpar++) {
       fDerivTest[jpar][ipar]= (pars[jpar]-pend[jpar])/step;
       if (ipar==jpar) fDerivTest[jpar][ipar]-=1.;
     }
   }
   fDerivTestOn=1;
   backStatics(save);
}
//______________________________________________________________________________
int StiKalmanTrackNode::testDeriv(double *der)
{
   if (fDerivTestOn!=1) return 0;
   double *num = fDerivTest[0];
   int nerr = 0;
   for (int i=0;i<15;i++) {
     double dif = fabs(num[i]-der[i]);
     if (fabs(dif) <= 1.e-5) 				continue;
     if (fabs(dif) <= 0.2*0.5*fabs(num[i]+der[i]))	continue;
     nerr++;
     int ipar = i/5;
     int jpar = i%5;
     printf ("***Wrong deriv [%d][%d] = %g %g %g\n",ipar,jpar,num[i],der[i],dif);
  }
  fDerivTestOn=0;
  return nerr;
}
//______________________________________________________________________________
void StiKalmanTrackNode::saveStatics(double *sav)
{  
  sav[ 0]=x1;
  sav[ 1]=x2; 
  sav[ 2]=y1; 
  sav[ 3]=y2; 
  sav[ 4]=z1; 
  sav[ 5]=dx; 
  sav[ 6]=cosCA1; 
  sav[ 7]=sinCA1; 
  sav[ 8]=cosCA2; 
  sav[ 9]=sinCA2; 
  sav[10]=sumSin; 
  sav[11]=sumCos; 
  sav[12]=sinCA1plusCA2; 
  sav[13]=x0; 
  sav[14]=y0; 
  sav[15]=dl; 
}  
//______________________________________________________________________________
void StiKalmanTrackNode::backStatics(double *sav)
{  
  x1=             sav[ 0];
  x2= 		  sav[ 1]; 
  y1= 		  sav[ 2]; 
  y2= 		  sav[ 3]; 
  z1= 		  sav[ 4]; 
  dx= 		  sav[ 5]; 
  cosCA1= 	  sav[ 6]; 
  sinCA1= 	  sav[ 7]; 
  cosCA2= 	  sav[ 8]; 
  sinCA2= 	  sav[ 9]; 
  sumSin= 	  sav[10]; 
  sumCos= 	  sav[11]; 
  sinCA1plusCA2=  sav[12]; 
  x0= 		  sav[13]; 
  y0= 		  sav[14]; 
  dl=             sav[15];
}
