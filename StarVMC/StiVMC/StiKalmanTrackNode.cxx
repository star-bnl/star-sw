/*
 * $Id: StiKalmanTrackNode.cxx,v 2.149 2014/05/29 12:51:21 fisyak Exp $
 *
 * /author Claude Pruneau
 *
 * $Log: StiKalmanTrackNode.cxx,v $
 * Revision 2.149  2014/05/29 12:51:21  fisyak
 * Clean ups
 *
 * Revision 2.148  2011/02/11 16:13:55  fisyak
 * Fixes for gcc451
 *
 * Revision 2.147  2009/09/01 22:31:35  fisyak
 * The first version with SD parameters
 *
 * Revision 2.146  2009/08/24 14:24:22  fisyak
 * An other freeze before moving to SD parameters
 *
 * Revision 2.145  2009/08/20 22:25:29  fisyak
 * Freeze before moving to SD set os parameters
 *
 * Revision 2.144  2009/08/19 19:56:39  fisyak
 * Clean up
 *
 * Revision 2.143  2009/08/19 18:08:00  fisyak
 * Eliminate StiStarVertexFinder
 *
 * Revision 2.142  2009/08/18 22:14:19  fisyak
 * Fix sign in SD2Sti
 *
 * Revision 2.141  2009/08/17 23:18:20  fisyak
 * Reshape Fit parameters
 *
 * Revision 2.140  2009/08/10 19:06:59  fisyak
 * Add different fitted parameters types (Sti,SD,SC,Dca)
 *
 * Revision 2.139  2009/08/04 18:55:12  fisyak
 * Capitilize method names
 *
 * Revision 2.138  2009/08/03 19:18:13  fisyak
 * Add StHelixModel creator to StiKalmanTrackNode
 *
 * Revision 2.137  2009/08/02 19:05:35  fisyak
 * Add reference track
 *
 * Revision 2.136  2009/07/28 20:53:30  fisyak
 * Eliminate Sti/Base
 *
 * Revision 2.135  2009/07/26 21:34:37  fisyak
 * Add smoother
 *
 * Revision 2.134  2009/07/24 18:28:21  fisyak
 * Split parameters into : Predicted, Fitted and Smoothed
 *
 * Revision 2.133  2009/07/23 19:40:03  fisyak
 * Remove StiKalmanTrackFinder
 *
 * Revision 2.132  2009/07/20 14:10:14  fisyak
 * Remove *Parameter* and exceptions
 *
 * Revision 2.131  2009/07/19 20:13:48  fisyak
 * remove abstract classes
 *
 * Revision 2.130  2009/06/14 21:51:31  fisyak
 * cleanning from dead codes and exceptions
 *
 * Revision 2.129  2009/05/27 19:05:21  fisyak
 * Use geane for propagation to vertex, beam and fixed radius
 *
 * Revision 2.128  2009/05/26 21:56:57  fisyak
 * Comment out material and detector shape related methods
 *
 * Revision 2.127  2009/05/20 21:55:23  fisyak
 * Fix ptin = - q/pT
 *
 * Revision 2.126  2009/05/19 16:01:51  fisyak
 * Fix derivatives
 *
 * Revision 2.125  2009/05/06 16:40:05  fisyak
 * Move to TRArray
 *
 * Revision 2.124  2009/05/01 20:52:01  fisyak
 * Freeze derivative matrices
 *
 * Revision 2.123  2009/04/29 14:36:55  fisyak
 * Freeze 0-th version of VMC base reconstruction
 *
 * Revision 2.122  2009/04/15 20:27:32  fisyak
 * Clean ups, use VMC TGeo for detector description, load hits in the central place
 *
 * Revision 2.121  2009/03/16 13:50:15  fisyak
 * Move out all Sti Chairs into StDetectorDb
 *
 * Revision 2.120  2008/12/26 15:18:00  fisyak
 * Enlarge fitting volume from 200 => 250 cm
 *
 * Revision 2.119  2008/06/11 22:04:37  fisyak
 * Add dead material
 *
 * Revision 2.118  2008/06/09 20:12:09  perev
 * BigStepBack
 *
 * Revision 2.115  2008/04/03 20:03:36  fisyak
 * Straighten out DB access via chairs
 *
 * Revision 2.114  2008/03/25 18:02:53  perev
 * remove field field from everythere
 *
 * Revision 2.113  2007/09/10 21:26:52  perev
 * Pt non positive bug fix. introduces 3 month ago
 *
 * Revision 2.112  2007/08/30 19:13:27  fine
 * replace the repmaining cout with LOG_DEBUG
 *
 * Revision 2.111  2007/08/16 20:21:24  fine
 * replace printf with logger
 *
 * Revision 2.110  2007/07/12 00:21:00  perev
 * Normal radius instead of layer one
 *
 * Revision 2.109  2007/06/25 19:31:52  perev
 * Init of _sinCA and _cosCA non zeros now
 *
 * Revision 2.108  2007/06/07 20:13:42  perev
 * BugFix in Pt()
 *
 * Revision 2.107  2007/06/06 04:03:03  perev
 * Time() cleanup
 *
 * Revision 2.106  2007/04/30 19:53:16  fisyak
 * Correct time of flight calculation, add time of flight corrrection for Laser
 *
 * Revision 2.105  2007/03/21 17:47:24  fisyak
 * Time of Flight
 *
 * Revision 2.104  2006/12/24 02:16:36  perev
 * _inf=0 added in copy constructor
 *
 * Revision 2.103  2006/12/18 01:17:41  perev
 * Info block added and filled for pulls
 *
 * Revision 2.102  2006/10/16 20:29:35  fisyak
 * Clean up useless classes
 *
 * Revision 2.101  2006/10/09 15:47:07  fisyak
 * take out Central represantation, remove StiDedxCalculator
 *
 * Revision 2.100  2006/05/31 03:58:06  fisyak
 * Add Victor's dca track parameters, clean up
 *
 * Revision 2.99  2006/04/15 23:12:10  perev
 * Supress printout
 *
 * Revision 2.98  2006/04/07 18:01:56  perev
 * Back to the latest Sti
 *
 * Revision 2.96  2006/02/16 20:44:50  perev
 * assert changed back
 *
 * Revision 2.95  2006/02/15 19:07:18  perev
 * assrt in nudge cos < 1
 *
 * Revision 2.94  2006/02/14 18:10:41  perev
 * getGlobalHitErrors added+CleanUp
 *
 * Revision 2.93  2005/12/31 01:37:12  perev
 * Primary node perpendicular to track
 *
 * Revision 2.92  2005/12/20 00:41:21  perev
 * unassigned sind fixed(thanxYF)
 *
 * Revision 2.91  2005/12/18 23:41:46  perev
 * Dependency from StiKalmanTrackNode removed
 *
 * Revision 2.90  2005/12/08 22:05:45  perev
 * nudge assert replaced by print. But very strangeStiKalmanTrackNode.cxx
 *
 * Revision 2.89  2005/12/07 22:29:27  perev
 * Big Cleanup
 *
 * Revision 2.88  2005/08/09 14:55:34  perev
 * Extend()/Reduce() of node
 *
 * Revision 2.87  2005/08/04 03:52:54  perev
 * Cleanup
 *
 * Revision 2.86  2005/07/20 17:24:25  perev
 * Nudge actions in EvaluateChi2 added
 *
 * Revision 2.85  2005/06/14 22:22:46  perev
 * Replace assert to error return
 *
 * Revision 2.84  2005/06/03 19:57:04  perev
 * Bug fix, violation of array size
 *
 * Revision 2.83  2005/06/02 17:27:41  perev
 * More weak assert in nudge()
 *
 * Revision 2.82  2005/05/31 16:47:56  perev
 * technical reorganization
 *
 * Revision 2.81  2005/05/12 18:10:04  perev
 * dL/dCurv more accurate
 *
 * Revision 2.80  2005/05/04 19:33:00  perev
 * Supress assert
 *
 * Revision 2.79  2005/04/30 20:45:18  perev
 * Less strong test for assert in PropagateError
 *
 * Revision 2.78  2005/04/25 20:20:25  fisyak
 * replace assert by print out
 *
 * Revision 2.77  2005/04/12 14:35:39  fisyak
 * Add print out for dE/dx
 *
 * Revision 2.76  2005/04/11 22:48:30  perev
 * assert removed
 *
 * Revision 2.75  2005/04/11 17:33:55  perev
 * Wrong sorting accounted, check for accuracy inctreased
 *
 * Revision 2.74  2005/04/11 14:32:18  fisyak
 * Use gdrelx from GEANT for dE/dx calculation with accouning density effect
 *
 * Revision 2.73  2005/03/30 21:01:43  perev
 * asserts replaced to prints
 *
 * Revision 2.72  2005/03/28 05:52:40  perev
 * Reorganization of node container
 *
 * Revision 2.71  2005/03/24 19:28:35  perev
 * Switch off DerivTest
 *
 * Revision 2.70  2005/03/24 18:05:07  perev
 * Derivatives and their test fixed to eta==Psi model
 *
 * Revision 2.69  2005/03/19 00:20:33  perev
 * Assert for zero determinant ==> print
 *
 * Revision 2.68  2005/03/18 17:35:38  perev
 * some asserts removed
 *
 * Revision 2.67  2005/03/18 17:13:07  perev
 * assert in rotate fix
 *
 * Revision 2.66  2005/03/17 06:24:52  perev
 * A lot of changes. _eta now is Psi
 *
 * Revision 2.65  2005/02/25 17:05:41  perev
 * Scaling for errors added
 *
 * Revision 2.64  2005/02/19 20:23:37  perev
 * Cleanup
 *
 * Revision 2.63  2005/02/18 19:02:55  fisyak
 * Add debug print out for ExtendToVertex
 *
 * Revision 2.62  2005/02/17 23:19:02  perev
 * NormalRefangle + Error Reseting
 *
 * Revision 2.61  2005/02/17 19:58:06  fisyak
 * Add debug print out flags
 *
 * Revision 2.60  2005/02/16 17:47:16  perev
 * assert in nudge 1==>5
 *
 * Revision 2.59  2005/02/07 18:33:42  fisyak
 * Add VMC dead material
 *
 * Revision 2.58  2005/01/20 16:51:32  perev
 * Remove redundant print
 *
 * Revision 2.57  2005/01/17 01:31:25  perev
 * New parameter model
 *
 * Revision 2.56  2005/01/06 00:59:41  perev
 * Initial errors tuned
 *
 * Revision 2.55  2005/01/04 01:37:47  perev
 * minor bug fix
 *
 * Revision 2.54  2004/12/23 18:15:46  perev
 * Cut for -ve cosCA added
 *
 * Revision 2.53  2004/12/14 17:10:17  perev
 * Propagate for 0 not called
 *
 * Revision 2.52  2004/12/13 22:52:23  perev
 * Off testError
 *
 * Revision 2.51  2004/12/13 20:01:38  perev
 * old version of testError temporary activated
 *
 * Revision 2.50  2004/12/12 01:34:24  perev
 * More smart testError, partial error Reset
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
 * added extrapolation function; minor change to UpdateNode function
 *
 * Revision 2.39  2004/11/08 15:32:54  pruneau
 * 3 sets of modifications
 * (1) Changed the StiPlacement class to hold keys to both the radial and angle placement. Propagated the use
 * of those keys in StiSvt StiTpc StiSsd and all relevant Sti classes.
 * (2) Changed the StiKalmanTrackFinder::find(StiKalmanTrack*) function's algorithm for the navigation of the
 * detector volumes. The new code uses an iterator to visit all relevant volumes. The code is now more robust and compact
 * as well as much easier to read and maintain.
 * (3) Changed the chi2 calculation in StiKalmanTrack::Chi2 and Propagated the effects of this change
 * in both StiKalmanTrackingPlots and StiStEventFiller classes.
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
 * Trapping for negative track error (^2) values _cYY and _cZZ. This should
 * be a temporary fix until the Root of the problem is found. Problem seems
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
 * transfered relevant tracking pars to detector Builders
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
 * Fixed Phase function to conform to StHelixModel convention.
 *
 * Revision 2.15  2003/03/31 17:18:56  pruneau
 * various
 *
 * Revision 2.14  2003/03/13 21:21:27  pruneau
 * Phase() fixed. MUST inclde -helicity()*pi/2
 *
 * Revision 2.13  2003/03/13 18:59:13  pruneau
 * various updates
 *
 * Revision 2.12  2003/03/12 17:57:31  pruneau
 * Elss calc updated.
 *
 * Revision 2.11  2003/03/04 21:31:05  pruneau
 * Added NormalX0() and getGasX0() conveninence methods.
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
#include <math.h>
#include <stdio.h>
using namespace std;

#include "StiHit.h"
#include "StiDetector.h"
#include "StiKalmanTrackNode.h"
#include "StDetectorDbMaker/StiTrackingParameters.h"
#include "StDetectorDbMaker/StiKalmanTrackFinderParameters.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StiFactory.h"
#include "TString.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "THelixTrack.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"

#include "TRVector.h"
#include "StarMagField.h"
#include "TMath.h"
#include "StMessMgr.h"
#include "StGeanePropagator.h"
#include "TGeoManager.h"
#include "StiToolkit.h"
#include "StiDetectorContainer.h"
#include "TGeoManager.h"
//#include "TestDerivatives.h"
#define PrP(A)    {LOG_INFO << "\t" << (#A) << " = \t" << ( A ) << endm;}
// tracking debug print outs
#define PrPP(A,B) {if (Debug() & 32) {LOG_INFO  << "=== StiKalmanTrackNode::" << (#A) << "\t" << (#B) << " = \t" << ( B ) << endm;}}
// derivatives debug print out
#define PrPD(A,B) {if (Debug() & 16) {LOG_INFO  << "=== StiKalmanTrackNode::" << (#A) << "\t" << (#B) << " = \t" << ( B ) << endm;}}
// Local Track Model
//
// x[0] = y  coordinate
// x[1] = z  position along beam axis
// x[2] = (Psi)
// x[3] = C  (local) curvature of the track
// x[4] = tan(l) 

static const Int_t    idx55[5][5] = 
  {{0,1,3,6,10},{1,2,4,7,11},{3,4,5, 8,12},{6,7, 8, 9,13},{10,11,12,13,14}};
static const Int_t    idx55tpt[5][5] = 
  {{0,1,2,3, 4},{1,5,6,7, 8},{2,6,9,10,11},{3,7,10,12,13},{ 4, 8,11,13,14}};
static const int    idx66[6][6] =
  {{ 0, 1, 3, 6,10,15},{ 1, 2, 4, 7,11,16},{ 3, 4, 5, 8,12,17}
  ,{ 6, 7, 8, 9,13,18},{10,11,12,13,14,19},{15,16,17,18,19,20}};
TString StiKalmanTrackNode::comment("Legend: \tE - extapolation\tM Multiple scattering\tV at Vertex\tB at beam\tR at Radius\tU Updated\n");
TString StiKalmanTrackNode::commentdEdx(""); 

void StiKalmanTrackNode::Break(Int_t kase)
{
static Int_t myBreak=-2005;
if (kase!=myBreak) return;
  LOG_DEBUG << Form("*** Break(%d) ***",kase)<< endm;
}		
/* bit mask for debug printout  
   0   => 1 - covariance and Propagate matrices 
   1   => 2 - hit associated with the node
   2   => 4 - test matrix manipulation
   3   => 8 - test locate
 */
Int_t StiKalmanTrackNode::_debug = 0;
Int_t StiKalmanTrackNode::_laser = 0;

//______________________________________________________________________________
void StiKalmanTrackNode::Reset() { 
  static Int_t myCount=0;
  memset(_beg,0,_end-_beg+1);
   StiTreeNode::Reset();
  _state = kTNReset;
  _chi2 = 1e60;
  mId = ++myCount; 
  Break(mId);
}
//_____________________________________________________________
/// Set the Kalman state of this node to be identical 
/// to that of the given node.
/// This method is useful to initial the state of a node
/// while propagating a track.
//______________________________________________________________________________
void StiKalmanTrackNode::SetState(const StiKalmanTrackNode * n)
{
  _state   = n->_state;
  Predicted()  = n->Predicted();
  Fitted()     = n->Fitted();
  Smoothed()   = n->Smoothed();
  nullCount = n->nullCount;
  contiguousHitCount = n->contiguousHitCount;
  contiguousNullCount = n->contiguousNullCount;
  setChi2(1e62);  
}

/**
   returns the node information
   double& alpha : angle of the local reference frame
   double& xRef  : refence position of this node in the local frame
   Double_t x[6],  : state, for a definition, see the top of this file
   Double_t cc[21] : error matrix of the state "x"
   double& chi2) : chi2 of the track at this node
*/
//______________________________________________________________________________
void StiKalmanTrackNode::Get(
			     Double_t  *x, 
			     Double_t  *e, 
			     double& chi2) {
  FitParameters FP(FitParameters::kSti);
  FP = Fitted();
  TCL::ucopy(FP.P().GetArray(),x, FP.P().GetSize());
  TCL::ucopy(FP.C().GetArray(),e, FP.C().GetSize());
  chi2 = Chi2();
}
//______________________________________________________________________________
/*! Calculate/return the z component of mag field 
  <p>
  Calculate/return the z component of mag field
  <p>
  Field is calcualated via StarMagField class and cashed. 
*/
//______________________________________________________________________________
Double_t StiKalmanTrackNode::Hz() const
{
  static const Double_t EC = 2.99792458e-4;
  static Double_t h[3] = {0, 0, 0};
  StarMagField::Instance()->BField(Gxyz(),h);
  h[2] = EC*h[2];
  if (TMath::Abs(h[2]) < 3e-33) h[2]=3e-33;
  return h[2];
}
#if 1
//______________________________________________________________________________
/**
   returns the node information
   Double_t x[6],  : state, for a definition, in radial implementation
                   rad  - radius at start (cm). See also comments
                   phi  - azimuthal angle  (in rad)      
                   z    - z-coord. (cm)                 
                   psi  - azimuthal angle of pT vector (in rads)     
                   tanl - tan(dip) =pz/pt               
                   curv - Track curvature (1/cm) 
   Double_t cc[15] : error matrix of the state "x" rad is fixed
                       code definition adopted here, where:
   PhiPhi;
   ZPhi     ,ZZ;                       
   TanlPhi  ,TanlZ ,TanlTanl,                 
   PsiPhi   ,PsiZ  ,PsiTanl , PsiPsi ,           
   CurvPhi  ,CurvZ ,CurvTanl, CurvPsi, CurvCurv     

*/
//______________________________________________________________________________
void StiKalmanTrackNode::GlobalRadial(Double_t  x[6],Double_t  e[15])
{
  enum {jRad=0,jPhi,jZ,jTan,jPsi,jCur, kX=0,kY,kZ,kE,kC,kT,kNPars};
  Double_t xx[kNPars],ee[kNPars*(kNPars-1)/2];
  Double_t chi2;
  Get(xx,ee,chi2);
  x[jRad] = TMath::Sqrt(TMath::Power(xx[kX],2)+TMath::Power(xx[kY],2));
  x[jPhi] = atan2(xx[kY],xx[kX]);
  x[jZ  ] = xx[kZ];
  x[jTan] = xx[kT];
  x[jPsi] = xx[kE];
  x[jCur] = xx[kC];
  if (!e) return;

  Double_t F[kNPars][kNPars]; memset(F,0,sizeof(F));
  F[jPhi][kX] = -1e5;
  F[jPhi][kY] =  1e5;
  if (TMath::Abs(xx[kY])>1e-5)  F[jPhi][kX] = -1./(xx[kY]);
  if (TMath::Abs(xx[kX])>1e-5)  F[jPhi][kY] =  1./(xx[kX]);
  F[jZ][kZ]   = 1.;
  F[jTan][kT] = 1;
  F[jPsi][kE] = 1;
  F[jCur][kC] = 1;
  memset(e,0,sizeof(*e)*15);
  for (Int_t k1=0;k1<kNPars;k1++) {
  for (Int_t k2=0;k2<kNPars;k2++) {
    Double_t cc = ee[idx66[k1][k2]]; 
    for (Int_t j1=jPhi;j1<= 5;j1++){
    for (Int_t j2=jPhi;j2<=j1;j2++){
      e[idx55[j1-1][j2-1]]+= cc*F[j1][k1]*F[j2][k2];
  }}}}    
  
}
//______________________________________________________________________________
/**
   returns the node information in TPT representation
   Double_t x[6],  : state, for a definition, in radial implementation
                   rad  - radius at start (cm). See also comments
                   phi  - azimuthal angle  (in rad)      
                   z    - z-coord. (cm)                 
                   psi  - azimuthal angle of pT vector (in rads)     
                   tanl - tan(dip) =pz/pt               
                   q/pt -  
   Double_t cc[15] : error matrix of the state "x" rad is fixed
                       code definition adopted here, where:

                                                 Units
                       ______|________________|____________
                       phi*R |  0  1  2  3  4 |  deg*cm
                        z0   |  1  5  6  7  8 |    cm
                       tanl  |  2  6  9 10 11 |    1         covar(i)
                        psi  |  3  7 10 12 13 |   deg
                       q/pt  |  4  8 11 13 14 | e*1/(GeV/c)
                       -----------------------------------

                       and where phi  = atan2(y0,x0)*(180 deg/pi)
                                 R    = sqrt(x0*x0 + y0*y0)
                                 q/pt = icharge*invpt; (This is what the 
                                        radius of curvature actually
                                        determines)

*/
//______________________________________________________________________________
void StiKalmanTrackNode::GlobalTpt(Float_t  x[6],Float_t  e[15])
{
  enum {jRad=0,jPhi,jZ,jTan,jPsi,jCur,jPt=jCur};
static const Double_t DEG = 180./M_PI;
static       Double_t fak[6] = {1,0,1,1,DEG,0};

  Double_t xx[6],ee[15];
  GlobalRadial(xx,ee);
  Double_t pt = Pt();
  fak[jPhi] = DEG*xx[jRad];
  fak[jPt] = (double(Charge())/pt)/xx[jCur];

  for (Int_t i=0;i<6;i++) {x[i] = (float)(fak[i]*xx[i]);}
  if (!e) return;

  for (Int_t j1=jPhi;j1<= 5;j1++){
  for (Int_t j2=jPhi;j2<=j1;j2++){
    e[idx55tpt[j1-1][j2-1]] = (float)fak[j1]*fak[j2]*ee[idx55[j1-1][j2-1]];
  }}

}
#endif
/*! Steering routine that Propagates the track encapsulated by the given node "pNode" to the given detector "tDet". 
  pNode => tDet
*/ 
//______________________________________________________________________________
void StiKalmanTrackNode::UpdatePrediction(const StiKalmanTrackNode *pNode, TRVector *refFitPar) {
  TRVector dP(pNode->Fitted().P()); PrPP(UpdatePrediction,dP);
  dP -= *refFitPar;                 PrPP(UpdatePrediction,dP); PrPP(UpdatePrediction,*refFitPar);
  *refFitPar = Predicted().P();     PrPP(UpdatePrediction,*refFitPar);
  /* */                             PrPP(UpdatePrediction,F());
  Predicted().P() += TRVector(F(),TRArray::kAxB,dP); PrPP(UpdatePrediction,Predicted().P());
}
//______________________________________________________________________________
Int_t StiKalmanTrackNode::Propagate(StiKalmanTrackNode *pNode, StiDetector *tDet, Int_t dir, TRVector *refFitPar) {
  Int_t position = 0;
  if (! Fitted().BField().GetSize()) {
    Double_t h[3] = {0, 0, 0};
    StarMagField::Instance()->BField(Fitted().XyzG().GetArray(),h);
    Fitted().SetBField(h);
  }
  if (pNode->Detector() == tDet) {PrPP(Propagate,Fitted());
    Predicted() = Fitted();       PrPP(Propagate,Predicted());
    TRMatrix unit(TRArray::kUnit,Fitted().P().GetSize()); 
    SetdSdS(unit); PrPP(Propagate,F()) // <<========
  } else {
    if (pNode) {
      SetState(pNode);
      LoadS2D(pNode,refFitPar); 
    }
    TString Dir("");
    if (dir == kOutsideIn) Dir = "B";
    position = StGeanePropagator::instance()->Propagate(*(pNode->Detector()->GetPhysicalNode()->GetMatrix()), 
							*(tDet->GetPhysicalNode()->GetMatrix()),
							Dir);
    if (position) {
      if (Debug() > 1) cout << "StiKalmanTrackNode::Propagate to detector " << tDet->GetName() 
			    << "\tfailed\t" << position << endl;
      return position;
    }
    StiDetector *tDetC = (StiDetector *) StiToolkit::instance()->DetectorContainer()->FindDetector(gGeoManager->GetPath());
    assert( tDetC || tDetC != tDet);
    if ( tDet) {
      if (Debug()) ResetComment(Form("%30s ",tDet->GetName()));
      LoadPrediction(); PrPP(Propagate,Predicted());
#ifdef __Test_Derivatives__
      if (pNode) TestDerivatives::test(this,pNode,dir);
#endif
      if (refFitPar) {UpdatePrediction(pNode,refFitPar); PrPP(Propagate,Predicted().P());}
    }
    Predicted().SetBField(((TGeant3 *) gMC)->Erwork()->hf);
  }
  return position;
}
/*! Propagate track parameters at node "pNode" to the next sensitive detector */
//______________________________________________________________________________
Int_t StiKalmanTrackNode::Propagate(StiKalmanTrackNode *pNode, Int_t dir, TRVector *refFitPar) {
 static Int_t nCall=0; nCall++;
  Break(nCall);
  Int_t position = 0;
  TString Dir("");
  if (dir == kOutsideIn) Dir = "B";
  if (pNode) {
    SetState(pNode); 
    LoadS2D(pNode,refFitPar);
  } else {Dir += "C";}
  position = StGeanePropagator::instance()->Propagate(Dir);
  if (position) {
    if (Debug() > 1) cout << "StiKalmanTrackNode::Propagate failed\t" << position << endl;
    return position;
  }
  StiDetector *tDet = (StiDetector *) StiToolkit::instance()->DetectorContainer()->FindDetector(gGeoManager->GetPath());
  SetDetector(tDet);
  if ( tDet) {
    if (Debug()) ResetComment(Form("%30s ",tDet->GetName()));
    LoadPrediction(); PrPP(Propagate,Predicted());
#ifdef __Test_Derivatives__
    if (pNode) TestDerivatives::test(this,pNode,dir);
#endif
    if (refFitPar) UpdatePrediction(pNode,refFitPar);
  }
  Predicted().SetBField(((TGeant3 *) gMC)->Erwork()->hf);
  return position;
}

//______________________________________________________________________________
/*! Propagate the track encapsulated by pNode to the given vertex. Use this node
	to represent the track parameters at the vertex.
  <p>
  This method Propagates the track from the given parent node
  "pNode" to the given vertex effectively calculating the
  location (x,y,z) of the track near the given vertex. It use "this" node
 to represent/hold the track parameters at the vertex.
 return true when the propagation is successfull and false otherwise.
<p>
*/
Bool_t StiKalmanTrackNode::Propagate(const StiKalmanTrackNode *parentNode, StiHit * vertex,Int_t dir, TRVector *refFitPar) {
  static Int_t nCall=0; nCall++;
  Break(nCall);
  
  SetState(parentNode);
  Double_t *Dir  = Fitted().PxyzG().GetArray();
  TCircle tc(Fitted().P().GetArray(),Dir,Curvature());
  Double_t xy[3] = {vertex->x_g(),vertex->y_g(), vertex->z_g()};
  Double_t s = tc.Path(xy); 
  if (Debug()) ResetComment(Form("Vtx:%8.3f %8.3f %8.3f",vertex->x_g(),vertex->y_g(),vertex->z_g()));
  TString opt("");
  if (! dir) opt = "B";
  if (StGeanePropagator::instance()->Propagate(*(parentNode->Detector()->GetPhysicalNode()->GetMatrix()), s, opt)) return kFALSE;
  if (Debug() & 8) { PrintpT("V");}
  SetHit(vertex);
  SetDetector(0);
  return kTRUE;
}

//______________________________________________________________________________
///Propagate track from the given node to the beam line with x==0.
///Set the hit and detector pointers to null to manifest this is an extrapolation
Bool_t StiKalmanTrackNode::PropagateToBeam(const StiKalmanTrackNode *parentNode,Int_t dir, TRVector *refFitPar)
{
  SetState(parentNode);
  if (Debug()) {
    if (parentNode->Detector()) 
      ResetComment(Form("%30s ",parentNode->Detector()->GetName()));
    else ResetComment("Unknown Detector");
  }
  Double_t *Dir = Fitted().PxyzG().GetArray();
  TCircle tc(Fitted().P().GetArray(),Dir,Curvature());
  Double_t xy[2] = {0,0};
  Double_t s = tc.Path(xy); 
  TString opt("");
  if (! dir) opt = "B";
  if (StGeanePropagator::instance()->Propagate(*(parentNode->Detector()->GetPhysicalNode()->GetMatrix()), s, opt)) return kFALSE;
  if (Debug() & 8) { PrintpT("B");}
  SetHit(0);
  SetDetector(0);
  return kTRUE;
}

//______________________________________________________________________________
///Extrapolate the track defined by the given node to the given radius.
///Return a negative value if the operation is impossible.
Bool_t StiKalmanTrackNode::PropagateToRadius(StiKalmanTrackNode *pNode, Double_t radius,Int_t dir, TRVector *refFitPar) {
  if (TMath::Abs(radius) < 1.e-7) return PropagateToBeam(pNode,dir,refFitPar);
  SetState(pNode);
  if (Debug()) ResetComment(Form("%30s ",pNode->Detector()->GetName()));
  //                   s[0] + s[1]*x + s[2]*y + s[3]*z + 
  //                   s[4]*x**2 + s[5]*y**2 + s[6]*z**2 + s[7]*x*y + s[8]*y*z + s[9]*z*x = 0
  Double_t surf[10] = {-1./(radius*radius), 0., 0.,
		       1.0, 1.0, 0.0, 0.0, 0.0, 0.0};
  Double_t *xyzG = Fitted().XyzG().GetArray();
  Double_t *dirG = Fitted().PxyzG().GetArray();
  THelixTrack tc(xyzG,dirG,Curvature());
  Double_t s = tc.Step(2.5e2, surf, 5,0,0,1);
  TString opt("");
  if (! dir) opt = "B";
  if (StGeanePropagator::instance()->Propagate(*(pNode->Detector()->GetPhysicalNode()->GetMatrix()), s, opt)) return kFALSE;
  if (Debug() & 8) { PrintpT("R");}
  _detector = 0;
  return kTRUE;
}

//______________________________________________________________________________
inline Double_t StiKalmanTrackNode::Length(const StThreeVector<Double_t>& delta, Double_t curv)
{
  
  Double_t m = delta.perp();
  Double_t as = 0.5*m*curv;
  Double_t lxy=0;
  if (TMath::Abs(as)<0.01) { lxy = m*(1.+as*as/24);}
  else               { lxy = 2.*TMath::ASin(as)/curv;}
  return TMath::Sqrt(lxy*lxy+delta.z()*delta.z());
}
//______________________________________________________________________________
Double_t StiKalmanTrackNode::EvaluateChi2(const StiHit * hit) {
  /*! Calculate the increment of chi2 caused by the addition of this node to the track.*/
#if 0
  SetHitErrors(hit,1);                                     PrPP(EvaluateChi2,Predicted().P());
#endif
  // Residual of prediction            : r^k-1_k = m_k - H_k*x^k-1_k
  Double_t time = TimeOfFlight(Predicted());
  r() = TRVector(hit->Measurement(time));                  PrPP(EvaluateChi2,r());
  r() -= TRVector(Predicted().H(),TRArray::kAxB,Predicted().P());      PrPP(EvaluateChi2,r());
  // Cov. matrix of predicted residuals: R^k-1_k = V_k + H_k * C^k-1_k *HT_k
  //                                   : G_k = (R^k-1_k)^-1
  R() = TRSymMatrix(V(Predicted()));                                  PrPP(EvaluateChi2,R());
  R() += TRSymMatrix(Predicted().H(),TRArray::kAxSxAT,Predicted().C());PrPP(EvaluateChi2,R());
  TRSymMatrix G(R(),TRArray::kInvertedA);                   PrPP(EvaluateChi2,G);
  chi2() = 1e31;
  if (G.IsValid()) {
    chi2()         = G.Product(r(),TRArray::kATxSxA);        PrPP(EvaluateChi2,chi2());
  }
  if (Debug() & 8) {comment += Form(" chi2 = %6.2f",chi2());}
  return chi2();
}
//______________________________________________________________________________
Int_t StiKalmanTrackNode::UpdateNode() {/*! Update the track parameters using this node.*/
  static Int_t nCall=0; nCall++;
  assert(_state>=kTNReady);
  _state = kTNFitBeg;
  //  assert(Predicted().C()[0]<1e-8);
  // Prediction:
  // extrapolation of state vector     : x^k-1_k = F_k-1 * x_k-1
  // extrapolation of cov. matrix      : C^k-1_k = F_k-1 * C_k-1 *FT_k-1 + Q_k-1
  // Residual of prediction            : r^k-1_k = m_k - H_k*x^k-1_k
  // Cov. matrix of predicted residuals: R^k-1_k = V_k + H_k * C^k-1_k *HT_k
  //                                   : G_k = (R^k-1_k)^-1
  // Filtering:
  // Kalman gain matrix                : K_k = C^k-1_k*HT_k*(V_k + H_k * C^k-1_k *HT_k)^-1 =
  //                                         = C^k-1_k*HT_k*G_k
  // update of state vector            : x_k = x^k-1_k + K_k *r^k-1_k
  // update of the cov. matrix         : C_k = (I - K_k*H_k)*C^k-1_k =
  //                                           (I - K_k*H_k)*C^k-1_k*(I - K_k*H_k)T + K_k*V_k*KT_k
  // Filtered residuals                : r_k = m_k - H_k*x_k = (I + H_k*K_k)*r^k-1_k
  // Cov. matrix of filtered residuals : R_k = (I - H_k*K_k)*V_k = V_k - H_k*C_k*HT_k
  // chisq increment                   : chisq = rT_k *(R_k)^-1 *r_k
  //--------------------------------------------------------------------------------
  // Residual of prediction            : r^k-1_k = m_k - H_k*x^k-1_k
  Double_t time = TimeOfFlight(Predicted());
  r() = TRVector(Hit()->Measurement(time));                  PrPP(UpdateNode,r());
  r() -= TRVector(Predicted().H(),TRArray::kAxB,Predicted().P());       
  PrPP(UpdateNode,r()); PrPP(UpdateNode,V(Predicted()));
  // Cov. matrix of predicted residuals: R^k-1_k = V_k + H_k * C^k-1_k *HT_k
  R() = TRSymMatrix(V(Predicted()));                                   PrPP(UpdateNode,R());
  /* */                                                     PrPP(UpdateNode,Predicted().C());
  R() += TRSymMatrix(Predicted().H(),TRArray::kAxSxAT,Predicted().C()); PrPP(UpdateNode,R());
  // G^k-1_k = (R^k-1_k)^-1
  TRSymMatrix G(R(),TRArray::kInverted);                    PrPP(UpdateNode,G);
  chi2() = G.Product(r(),TRArray::kATxSxA);                 PrPP(UpdateNode,chi2());
  // Kalman gain matrix                : K_k = C^k-1_k*HT_k*G^k-1_k
  TRMatrix T(Predicted().C(),TRArray::kSxAT,Predicted().H());           PrPP(UpdateNode,T);
  TRMatrix K(T,TRArray::kAxS,G);                            PrPP(UpdateNode,K);
  // update of state vector            : x_k = x^k-1_k + K_k *r^k-1_k
  TRVector P(K,TRArray::kAxB,r());                          PrPP(UpdateNode,P);
  P += Predicted().P();                                     PrPP(UpdateNode,P);
  // update of the cov. matrix         : C_k = (I - K_k*H_k)*C^k-1_k*(I - K_k*H_k)T + K_k*V_k*KT_k
  // A = K_k*H_k; B = (I - A)          : C_k = B*C^k-1_k*BT + K_k*V_k*KT_k
  TRMatrix A(K,TRArray::kAxB,Predicted().H());              PrPP(UpdateNode,A);
  TRMatrix B(TRArray::kUnit,P.GetSize());                   PrPP(UpdateNode,B);
  B -= A;                                                   PrPP(UpdateNode,B);
  TRSymMatrix C(B,TRArray::kAxSxAT,Predicted().C());        PrPP(UpdateNode,C);
  TRSymMatrix Y(K,TRArray::kAxSxAT,V(Predicted()));         PrPP(UpdateNode,Y);  
  C += Y;                                                   PrPP(UpdateNode,C);
  Fitted() = Predicted();
  Fitted().P() = P;
  Fitted().C() = C;                                         PrPP(UpdateNode,Fitted());
  // Filtered residuals                : r_k = m_k - H_k*x_k = (I + H_k*K_k)*r^k-1_k
  time = TimeOfFlight(Fitted());
  r() = Hit()->Measurement(time);                           PrPP(UpdateNode,r());
  r() -= TRVector(Predicted().H(),TRArray::kAxB,P);         PrPP(UpdateNode,r());
  // Cov. matrix of filtered residuals : R_k = (I - H_k*K_k)*V_k = V_k - H_k*C_k*HT_k
  R() = V(Predicted());
  R() -= TRSymMatrix(Predicted().H(),TRArray::kAxSxAT,C);   PrPP(UpdateNode,R());
  // chisq increment                   : chisq = rT_k *(R_k)^-1 *r_k
  G  = TRSymMatrix(R(),TRArray::kInverted);                 PrPP(UpdateNode,G);
  chi2() = G.Product(r(),TRArray::kATxSxA);                 PrPP(UpdateNode,chi2());
  if (Debug() & 8) PrintpT("U");
  _state = kTNFitEnd;
  return 0; 
}

//______________________________________________________________________________
/*! Rotate this node track representation azymuthally by given angle.
  <p>
  This method rotates by an angle alpha the track representation 
  held by this node. 
  <h3>Notes</h3>
  <ol>
  <li>The rotation is bound between -M_PI and M_PI.</li>
  <li>Throws runtime_error if "(_y-y0)*_curv>=0" in order to avoid math exception.</li>
  <li>Avoid undue rotations as they are CPU intensive...</li>
  </ol>
*/
//_____________________________________________________________________________
/// print to the ostream "os" the parameters of this node 
/// and all its children recursively
ostream& operator<<(ostream& os, const StiKalmanTrackNode& n) {
  const StiDetector *det = n.Detector();
  if (det) os  <<"Det:"<< det->GetName();
  else     os << "Det:UNknown";
  os         << StiKalmanTrackNode::PrintpTasString(n.Predicted(), -1, "Predicted") << endl;
  os << "\t" << StiKalmanTrackNode::PrintpTasString(n.Fitted(), n.chi2(), "Fitted") << endl;
  os << "\t" << StiKalmanTrackNode::PrintpTasString(n.Smoothed(), n.chi2(), "Smoothed") << endl;
  if (n.Debug() & 2) {
    os << n.Fitted() << endl;
    StiHit * hit = n.Hit();
    if (hit) os << endl 
		<< "\thas hit " << *hit << " with chi2 = " << n.Chi2()
		<< " n:"<<n.hitCount
		<< " null:"<<n.nullCount;
  }
  return os;
}

//______________________________________________________________________________
Double_t StiKalmanTrackNode::WindowY()
{	  
  const StiDetector * detector = Detector();
  const StiTrackingParameters * parameters = detector->TrackingParameters();
  Double_t searchWindowScale = parameters->getSearchWindowScale();
  Double_t minSearchWindow   = parameters->getMinSearchWindow();
  Double_t maxSearchWindow   = parameters->getMaxSearchWindow();

  const StiHitErrorCalculator * calc = detector->HitErrorCalculator();
  Double_t myEyy,myEzz;
  calc->calculateError(Predicted().Z(),Predicted().Eta(), Predicted().TanL(),myEyy,myEzz);
  TRSymMatrix CV(Predicted().H(),TRArray::kAxSxAT,Predicted().C());
  Double_t window = searchWindowScale*TMath::Sqrt(CV[0]+myEyy);
  if      (window<minSearchWindow) window = minSearchWindow;
  else if (window>maxSearchWindow) window = maxSearchWindow;
  return window;
}

//_____________________________________________________________________________
Double_t StiKalmanTrackNode::WindowZ() {	 
  const StiDetector * detector = Detector();
  const StiTrackingParameters * parameters = detector->TrackingParameters();
  Double_t searchWindowScale = parameters->getSearchWindowScale();
  Double_t minSearchWindow   = parameters->getMinSearchWindow();
  Double_t maxSearchWindow   = parameters->getMaxSearchWindow();

  const StiHitErrorCalculator * calc = detector->HitErrorCalculator();
  Double_t myEyy,myEzz;
  calc->calculateError(Predicted().Z(),Predicted().Eta(), Predicted().TanL(),myEyy,myEzz);
  TRSymMatrix CV(Predicted().H(),TRArray::kAxSxAT,Predicted().C());
  Double_t window = searchWindowScale*TMath::Sqrt(CV[2]+myEzz); 
  if      (window<minSearchWindow) window = minSearchWindow;
  else if (window>maxSearchWindow) window = maxSearchWindow;
  return window;
}
//______________________________________________________________________________
void StiKalmanTrackNode::Initialize(StiHit *h) {
  Reset();
  SetHit(h);
  _detector = h->Detector();
  Fitted().SetRotation(_detector->GetMatrix());
  _state = kTNInit;
  setChi2(0.1);
}
//______________________________________________________________________________
const StiKalmanTrackNode& StiKalmanTrackNode::operator=(const StiKalmanTrackNode &n) {
  memcpy(_beg,n._beg,_end-_beg+1);
  Fitted()    = n.Fitted();
  Predicted() = n.Predicted();
  Smoothed()  = n.Smoothed();
  fdDdS = n.fdDdS;
  fdSdS = n.fdSdS;
  fdDdD = n.fdDdD;
  fdSdS = n.fdSdS;
  _r    = n._r;
  _R    = n._R;
  _chi2 = n._chi2;
  return *this;
}
#if 0
//______________________________________________________________________________
void StiKalmanTrackNode::SetHitErrors(const StiHit *hit, Int_t kind) {
  if (!hit) hit = _hit;
  const StiDetector *det = hit->Detector();
  const StiHitErrorCalculator *calc = (det)? det->HitErrorCalculator():0;
  if (calc) {//calculate it
    V() = TRSymMatrix(3);
    switch (kind) {
    case 1: calc->calculateError(Predicted().Z(),Predicted().Eta(), Predicted().TanL(),V()[2],V()[5]); break;
    case 2: calc->calculateError(Smoothed().Z(),Smoothed().Eta(), Smoothed().TanL(),V()[2],V()[5]); break;
    default: calc->calculateError(Fitted().Z(),Fitted().Eta(), Fitted().TanL(),V()[2],V()[5]); break;
    };
  } else    {//get from hit
    V() = TRSymMatrix(3,hit->errMtx());
  }
}
#endif
//________________________________________________________________________________
const TRSymMatrix &StiKalmanTrackNode::V(const FitParameters& Prediction)  const {
  assert(_detector);
  const StiHitErrorCalculator *calc = _detector->HitErrorCalculator();
  assert(calc);
  static TRSymMatrix V(2);
  static FitParameters *old = 0;
  if (old != &Prediction) {
    old = (FitParameters *) &Prediction;
    calc->calculateError(Prediction.Z_g(),Prediction.Eta(), Prediction.TanL(),V[0],V[2]); 
  }
  return *&V;
}
#if 0
//______________________________________________________________________________
TRSymMatrix StiKalmanTrackNode::GlobalHitErrs(const StiHit *hit) const {
  TRMatrix R(3,3,Detector()->GetMatrix()->GetRotationMatrix());
  TRSymMatrix hr = TRSymMatrix(R,TRArray::kAxSxAT,V(Fitted()));
  return hr;
}
#endif
//________________________________________________________________________________
Char_t *StiKalmanTrackNode::PrintpTasString(const FitParameters &Fitted, Double_t chi2,const Char_t *opt) {
  return Form(" %s %s chi2 %6.2f",opt,Fitted.PrintpTasString(),chi2);
}
//________________________________________________________________________________
void StiKalmanTrackNode::PrintpT(const FitParameters &Fitted, Double_t chi2, const Char_t *opt) {
  comment += PrintpTasString(Fitted,chi2,opt);
}
//________________________________________________________________________________
void StiKalmanTrackNode::PrintpT(const Char_t *opt) {
  // opt = "E" extapolation
  //       "M" Multiple scattering
  //       "V" at Vertex
  //       "B" at beam
  //       "R" at Radius
  //       "U" Updated
  //       "b" before Smoother
  //       "S" Smoothed
  //       "s" start
  //       Fitted().P() fit parameters
  //       Fitted().C() fit errors
  if (opt[0] != 'S') PrintpT(Fitted(),chi2(),opt);
  else               PrintpT(Smoothed(),chi2(),opt);
}
//________________________________________________________________________________
void StiKalmanTrackNode::PrintStep() {
  LOG_INFO << comment << "\t" << commentdEdx << endm;
  ResetComment();
}
#if 0
//________________________________________________________________________________
const Double_t *StiKalmanTrackNode::Gxyz() const {
  static Double_t xyzG[3];
  static FitParameters *Old = 0;
  if (Old != &_Fitted) {
    Old = (FitParameters *) &_Fitted;
    const StiDetector* detector = Detector();
    if (detector) detector->GetMatrix()->LocalToMaster(_Fitted.P().GetArray(),xyzG);
    else          TCL::ucopy(_Fitted.P().GetArray(),xyzG,3);
  }
  return (const Double_t *) &xyzG[0];
}
#endif
//________________________________________________________________________________
Double_t StiKalmanTrackNode::TimeOfFlight(const FitParameters &Prediction) {
  static const Double_t smax = 1e3; 
  static Double_t time = 0;
  static FitParameters *Old = 0;
  if (Old == &Prediction) return time;
  Old = (FitParameters *) &Prediction;
  TVector3 pxyzG(Prediction.PxyzG().GetArray());
  TVector3 xyzG(Prediction.XyzG().GetArray());
  TVector3 dirG = pxyzG.Unit();
  if (! _laser) {
    Double_t d = xyzG.Perp();
    Double_t sn = TMath::Abs(dirG.X()*xyzG.Y() - dirG.Y()*xyzG.X())/d;
    if (sn> 0.99) sn =  0.99;
    if (sn<0.2) {
      d *= (1.+sn*sn/6);
    } else {
      d *= TMath::ASin(sn)/sn;
    }
    d *= TMath::Sqrt(1.+Prediction.TanL()*Prediction.TanL());
    Double_t beta = 1;   
    Double_t p2 = pxyzG.Mag2();
    Double_t m=StiKalmanTrackFinderParameters::instance()->massHypothesis();
    Double_t m2=m*m;
    Double_t e2=p2+m2;
    Double_t beta2=p2/e2;
    beta = TMath::Sqrt(beta2);
    time = d/(TMath::Ccgs()*beta*1e-6); // mksec  
  } else {
    if (TMath::Abs(Prediction.Z()) > 20.0) {
      static const Double_t Radius = 197.;
      static const Int_t    nsurf  = 6;
      static const Double_t surf[6] = {-Radius*Radius, 0, 0, 0, 1, 1};
      Double_t dir[3]; dirG.GetXYZ(dir);
      Double_t xyz[3]; xyzG.GetXYZ(xyz);
      THelixTrack tc(xyz,dir,Curvature());
      Double_t s = tc.Step(smax, surf, nsurf,0,0,1);
      if (TMath::Abs(s) < smax) 
	time = TMath::Abs(s)/(TMath::Ccgs()*1e-6); // mksec
    }
  }
  return time;
}
//________________________________________________________________________________
void StiKalmanTrackNode::LoadS2D(const StiKalmanTrackNode *node, TRVector *refFitPar) {
  if (! node) node = this;
  LoadS2D(node->Detector(),node->Fitted(), refFitPar);
}
//________________________________________________________________________________
void StiKalmanTrackNode::LoadS2D(const StiDetector *detector,const FitParameters &Fitted, TRVector *refFitPar) {
  // FP = {_y, _z, _eta, _ptin, _tanl}  eta == Psi;
  FitParameters FP = Fitted;
  if (refFitPar) {FP.P() = *refFitPar; PrPP(LoadS2D, *refFitPar);}
  StGeanePropagator *propagator = StGeanePropagator::instance();
  propagator->SetXYZ(FP.XyzG().GetArray());
  propagator->SetpXYZ(FP.PxyzG().GetArray());
  TGeoHMatrix *rotm  = detector->GetMatrix();
  FitParameters SD(FitParameters::kSD,rotm); 
  SD = FP;
  fdDdS = SD.F();     PrPD(LoadS2D,fdDdS);
  Double_t q = SD.Charge();
  if (q > 0) propagator->SetParticle(8); // pion+
  else       propagator->SetParticle(9); // pion-
  if (q != 1.) {
    for (Int_t i = 1; i < 5; i++) {
      SD.C()(0,i) *= q;
    }
  }
  propagator->SetPD(SD.P());  
  propagator->SetRD(SD.C());  PrPD(LoadS2D,SD);
  gGeoManager->cd(detector->GetName());
}
//________________________________________________________________________________
void StiKalmanTrackNode::LoadPrediction(StiKalmanTrackNode *node) {
  if (! node) node = this;
  LoadD2S(node->Detector(),node->Predicted());
}
//________________________________________________________________________________
void StiKalmanTrackNode::LoadD2S(const StiDetector *detector,FitParameters &Prediction) {
  StGeanePropagator *propagator = StGeanePropagator::instance();
  fdDdD = propagator->GetdDdD();  PrPD(LoadD2S,fdDdD);
  TVector3 xyzG  = propagator->GetXYZ();
  TVector3 pxyzG = propagator->GetpXYZ();
  Double_t q     = propagator->Charge();
  TGeoHMatrix *rotm  = detector->GetMatrix();
  Double_t xyz[3]; xyzG.GetXYZ(xyz);
  Double_t xyzL[3];
  rotm->MasterToLocal(xyz,xyzL);
  Double_t nG[3]; pxyzG.Unit().GetXYZ(nG);
  Double_t nL[3]; 
  rotm->MasterToLocalVect(nG,nL);
  TRVector PD = TRVector(5, q/pxyzG.Mag(), nL[1]/nL[0], nL[2]/nL[0], xyzL[1], xyzL[2]); PrPD(LoadS2D,PD);
  Double_t spu = TMath::Sign(1.,nL[0]); 
  FitParameters FP(FitParameters::kSD, spu, detector->GetMatrix(),PD,propagator->GetRD()); 
  PrPD(LoadD2S,FP);
  Prediction = FP;                                PrPD(LoadD2S,Prediction);
  if (Prediction.Type() == FitParameters::kSti) {
    fdSdD = FP.F();                                 PrPD(LoadD2S,fdSdD);
    //  dSti_new/dPD_old = dSti_new/dPD_new x dPD_new/dPD_old
    fdSdDI = TRMatrix(fdSdD,TRArray::kAxBT,fdDdD);  PrPD(LoadD2S,fdSdDI);
    //   dSti_new/dSti_old = dSti_new/dPD_old * dPD_old /d Sti_old        
    fdSdS = TRMatrix(fdSdDI,TRArray::kAxB,fdDdS);   PrPD(LoadD2S,fdSdS);
    // expand from 5x5 => 6x6 
    TRMatrix dSdS6x6(6,6);
    for (Int_t i = 0; i < 5; i++)
      for (Int_t j = 0; j < 5; j++)
	dSdS6x6(i+1,j+1) = fdSdS(i,j);
    fdSdS = dSdS6x6;
  } else {
    fdSdS = TRMatrix(fdDdD, TRArray::kTransposed); PrPD(LoadD2S,fdSdS);
    if (q != 1.) {
      for (Int_t i = 1; i < 5; i++) {
	fdSdS(i,0) /= q;
	fdSdS(0,i) /= q;
	Prediction.C()(0,i) /= q;
      }
    }
  }
  SetReady();
}
//________________________________________________________________________________
StTrackGeometry* StiKalmanTrackNode::Helix() {
  static StHelixModel track;
  StThreeVectorF origin(Gxyz());
  track = StHelixModel(Charge(),
		       Psi(),
		       TMath::Abs(Curvature()),
		       TMath::ATan(Fitted().TanL()),
		       origin, 
		       MomentumF(), 
		       -Charge());
  return (StTrackGeometry*) &track;
}
