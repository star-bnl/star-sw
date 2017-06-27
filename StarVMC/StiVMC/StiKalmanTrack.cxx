//StiKalmanTrack.cxx
/*
 * $Id: StiKalmanTrack.cxx,v 2.140 2014/05/29 12:51:21 fisyak Exp $
 * $Id: StiKalmanTrack.cxx,v 2.140 2014/05/29 12:51:21 fisyak Exp $
 *
 * /author Claude Pruneau
 *
 * $Log: StiKalmanTrack.cxx,v $
 * Revision 2.140  2014/05/29 12:51:21  fisyak
 * Clean ups
 *
 * Revision 2.139  2009/09/01 22:31:35  fisyak
 * The first version with SD parameters
 *
 * Revision 2.138  2009/08/24 14:24:22  fisyak
 * An other freeze before moving to SD parameters
 *
 * Revision 2.137  2009/08/20 22:25:29  fisyak
 * Freeze before moving to SD set os parameters
 *
 * Revision 2.136  2009/08/19 19:56:39  fisyak
 * Clean up
 *
 * Revision 2.135  2009/08/19 18:08:00  fisyak
 * Eliminate StiStarVertexFinder
 *
 * Revision 2.134  2009/08/17 23:18:20  fisyak
 * Reshape Fit parameters
 *
 * Revision 2.133  2009/08/10 19:06:59  fisyak
 * Add different fitted parameters types (Sti,SD,SC,Dca)
 *
 * Revision 2.132  2009/08/04 18:55:12  fisyak
 * Capitilize method names
 *
 * Revision 2.131  2009/08/03 19:18:13  fisyak
 * Add StHelixModel creator to StiKalmanTrackNode
 *
 * Revision 2.130  2009/08/02 19:05:35  fisyak
 * Add reference track
 *
 * Revision 2.129  2009/07/28 20:53:30  fisyak
 * Eliminate Sti/Base
 *
 * Revision 2.128  2009/07/26 21:34:37  fisyak
 * Add smoother
 *
 * Revision 2.127  2009/07/24 18:28:21  fisyak
 * Split parameters into : Predicted, Fitted and Smoothed
 *
 * Revision 2.126  2009/07/23 19:40:03  fisyak
 * Remove StiKalmanTrackFinder
 *
 * Revision 2.125  2009/07/20 14:10:14  fisyak
 * Remove *Parameter* and exceptions
 *
 * Revision 2.124  2009/07/20 13:15:53  fisyak
 * Remove Filters
 *
 * Revision 2.123  2009/07/19 20:56:27  fisyak
 * Eliminate StiKalmanTrackFitter
 *
 * Revision 2.122  2009/07/19 20:13:48  fisyak
 * remove abstract classes
 *
 * Revision 2.121  2009/06/14 21:51:31  fisyak
 * cleanning from dead codes and exceptions
 *
 * Revision 2.120  2009/05/27 19:05:21  fisyak
 * Use geane for propagation to vertex, beam and fixed radius
 *
 * Revision 2.119  2009/05/26 21:56:57  fisyak
 * Comment out material and detector shape related methods
 *
 * Revision 2.118  2009/05/19 16:01:51  fisyak
 * Fix derivatives
 *
 * Revision 2.117  2009/05/06 16:40:05  fisyak
 * Move to TRArray
 *
 * Revision 2.116  2009/04/29 14:36:55  fisyak
 * Freeze 0-th version of VMC base reconstruction
 *
 * Revision 2.115  2009/04/15 20:27:32  fisyak
 * Clean ups, use VMC TGeo for detector description, load hits in the central place
 *
 * Revision 2.114  2009/03/16 13:50:15  fisyak
 * Move out all Sti Chairs into StDetectorDb
 *
 * Revision 2.113  2008/10/27 21:01:30  perev
 * Free not used hits again
 *
 * Revision 2.112  2008/07/23 18:41:52  fisyak
 * Remove 100 cm cut on hit radius to calculate DcaGeometry,,bug 1243, left after big step back
 *
 * Revision 2.111  2008/06/09 20:12:07  perev
 * BigStepBack
 *
 * Revision 2.104  2008/04/08 21:39:43  perev
 * No any cuts in IsPrimary()
 *
 * Revision 2.103  2008/04/08 14:21:17  fisyak
 * 2 cm => StiKalmanTrackFinderParameters::instance()->maxDca3dVertex() in StiKalmanTrack::IsPrimary()
 *
 * Revision 2.102  2008/04/07 19:20:53  perev
 * More Clear IsPrimary()
 *
 * Revision 2.101  2008/04/03 20:03:33  fisyak
 * Straighten out DB access via chairs
 *
 * Revision 2.100  2008/03/24 21:38:46  jeromel
 * Undo setTimesUsed() - seeding seems to not take advantage of more hits (TBC)
 *
 * Revision 2.99  2008/03/24 19:32:03  perev
 * BugFix vertex is not SvtHit
 *
 * Revision 2.98  2008/03/20 01:31:16  perev
 * HitSet rejecting via StiKalmanTrackFinderParameters
 *
 * Revision 2.97  2007/12/20 01:10:18  perev
 * WarnOff
 *
 * Revision 2.96  2007/09/10 00:34:28  perev
 * member mgMaxRefiter added
 *
 * Revision 2.95  2007/08/16 20:21:23  fine
 * replace printf with logger
 *
 * Revision 2.94  2007/06/26 19:17:25  perev
 * Path to hit in 2dim space only
 *
 * Revision 2.93  2007/06/25 19:35:10  perev
 * DEbug off
 *
 * Revision 2.92  2007/06/25 19:28:50  perev
 * New better THelix fit in approx
 *
 * Revision 2.91  2007/03/21 17:49:16  fisyak
 * add includes for ROOT 5.14
 *
 * Revision 2.90  2006/12/19 19:50:01  perev
 * method Point added
 *
 * Revision 2.89  2006/12/18 01:14:08  perev
 * operator= added
 *
 * Revision 2.88  2006/10/16 20:29:35  fisyak
 * Clean up useless classes
 *
 * Revision 2.87  2006/10/09 15:47:06  fisyak
 * take out Central represantation, remove StiDedxCalculator
 *
 * Revision 2.86  2006/05/31 03:58:06  fisyak
 * Add Victor's dca track parameters, clean up
 *
 * Revision 2.85  2006/04/26 19:17:05  perev
 * mIdDb for debug instead of mId
 *
 * Revision 2.84  2006/04/15 23:11:18  perev
 * For zero field min curvature 1/1km
 *
 * Revision 2.83  2006/04/07 18:01:55  perev
 * Back to the latest Sti
 *
 * Revision 2.80  2006/02/21 23:25:51  perev
 * StiConfidence flag added
 *
 * Revision 2.79  2006/02/16 01:58:43  perev
 * StiOldRefit env added
 *
 * Revision 2.78  2006/02/14 18:04:45  perev
 * approx() accounts errors now
 *
 * Revision 2.77  2005/12/31 01:38:50  perev
 * Primary track can loose few nodes
 *
 * Revision 2.76  2005/12/18 23:39:20  perev
 * Cleanup
 *
 * Revision 2.75  2005/12/08 21:18:35  perev
 * track id must < 2*16
 *
 * Revision 2.74  2005/12/07 21:30:32  perev
 * new refit,refitL,approx etc...
 *
 * Revision 2.73  2005/10/26 21:55:02  fisyak
 * get rid off dependencies from StMcEvent
 *
 * Revision 2.72  2005/08/18 02:35:23  perev
 * Cleanup
 *
 * Revision 2.71  2005/08/17 22:00:17  perev
 * AllPointCount(...) added
 *
 * Revision 2.70  2005/08/16 20:11:10  perev
 * Typo corrected
 *
 * Revision 2.69  2005/08/14 01:10:55  perev
 * Non empty Unset(). Free all nodes when track is freed
 *
 * Revision 2.68  2005/08/09 14:51:25  perev
 * Add Reduce method, reducing all the nodes
 *
 * Revision 2.67  2005/08/04 03:50:31  perev
 * RemoveLastNode() added
 *
 * Revision 2.66  2005/07/20 17:21:44  perev
 * MultiVertex
 *
 * Revision 2.65  2005/06/09 03:12:39  perev
 * Fix typo in Nodes()
 *
 * Revision 2.64  2005/05/31 16:33:32  perev
 * Method refitL added
 *
 * Revision 2.63  2005/05/13 19:33:11  perev
 * Defence against all nodes are bad added
 *
 * Revision 2.62  2005/05/12 17:56:17  perev
 * refit tuned
 *
 * Revision 2.61  2005/04/11 17:27:59  perev
 * Error status added to fit()
 *
 * Revision 2.60  2005/03/31 18:14:00  perev
 * MaxPointCount() fixed(thank to Jan)
 *
 * Revision 2.59  2005/03/31 17:25:57  perev
 * MaxPointCount() fixed(thank to Jan)
 *
 * Revision 2.58  2005/03/28 05:48:49  perev
 * Reorganization of node container
 *
 * Revision 2.57  2005/03/24 17:59:38  perev
 * refit() method added
 *
 * Revision 2.56  2005/03/17 06:19:36  perev
 * Cleanup
 *
 * Revision 2.55  2005/02/25 16:36:14  perev
 * Iteration in refit added
 *
 * Revision 2.54  2005/02/18 19:02:31  fisyak
 * Add debug print out for ExtendToVertex
 *
 * Revision 2.53  2005/02/17 23:19:02  perev
 * NormalRefangle + Error Reseting
 *
 * Revision 2.52  2005/02/17 19:58:06  fisyak
 * Add debug print out flags
 *
 * Revision 2.51  2005/02/07 18:33:42  fisyak
 * Add VMC dead material
 *
 * Revision 2.50  2005/01/17 01:31:25  perev
 * New parameter model
 *
 * Revision 2.49  2004/12/23 15:06:28  pruneau
 * use _alpha instead of getRefAngle while Extending to vertex
 *
 * Revision 2.48  2004/12/11 04:31:36  perev
 * set of bus fixed
 *
 * Revision 2.47  2004/12/01 18:04:32  perev
 * test for -ve and too big track length added
 *
 * Revision 2.46  2004/12/01 03:57:08  pruneau
 * d<4
 *
 * Revision 2.45  2004/11/12 22:48:28  fisyak
 * Back to use chi2 instead DCA for Vertex fit
 *
 * Revision 2.44  2004/11/11 03:19:05  pruneau
 * implementation of extrapolation functions for Jan
 *
 * Revision 2.43  2004/11/10 21:44:26  pruneau
 * adding functions for extrapolation
 *
 * Revision 2.42  2004/11/08 15:32:50  pruneau
 * 3 sets of modifications
 * (1) Changed the StiPlacement class to hold keys to both the radial and angle placement. Propagated the use
 * of those keys in StiSvt StiTpc StiSsd and all relevant Sti classes.
 * (2) Changed the StiKalmanTrackFinder::find(StiKalmanTrack*) function's algorithm for the navigation of the
 * detector volumes. The new code uses an iterator to visit all relevant volumes. The code is now more robust and compact
 * as well as much easier to read and maintain.
 * (3) Changed the chi2 calculation in StiKalmanTrack::Chi2 and Propagated the effects of this change
 * in both StiKalmanTrackingPlots and StiStEventFiller classes.
 *
 * Revision 2.41  2004/10/28 19:30:42  perev
 * Hack. Infinite Chi2 skipped in Chi2 calculation. Claude??? (VP)
 *
 * Revision 2.40  2004/10/28 04:59:18  perev
 * Fixed iterator for nodes. v3V2
 *
 * Revision 2.39  2004/10/27 03:25:49  perev
 * Version V3V
 *
 * Revision 2.38  2004/10/26 21:52:07  pruneau
 * No truncation but bad hits dropped
 *
 * Revision 2.37  2004/10/26 06:45:37  perev
 * version V2V
 *
 * Revision 2.36  2004/10/25 14:15:49  pruneau
 * various changes to improve track quality.
 *
 * Revision 2.35  2004/08/17 20:55:42  perev
 * memory cleanup heap==>stack
 *
 * Revision 2.34  2004/08/06 02:28:53  andrewar
 * Added MaxPointCount(int detectorId)< where detectorId corresponds to the
 * StDetectorId value.
 *
 * Revision 2.33  2004/04/04 23:19:28  jeromel
 * isfinite() -> finite()
 *
 * Revision 2.32  2004/03/31 00:23:41  calderon
 * -Fixed memory leak in StiDetectorTreeBuilder::HangWhere (100 chars were lost
 *  every time this function was called)
 * -Changed algorithm to count fit points in StiKalmanTrack.  Now it is based
 *  on counting the nodes that have a chi2 < chi2Max from
 *  StiKalmanTrackFitterParameters.
 * -Which meant that I had to somehow introduce a pointer to it so that the
 *  track could know about the chi2Max used in the fitter.
 * -And I also added a method to retrieve the pointer to the fitterParams
 *  to be used in StiStEventFiller.
 * -Which was then modified to calculate the encoded fit points based on
 *  a similar algorithm (chi2<chi2Max test).
 * -Cleaned up the includes in StiKalmanTrack.h, left only the ones
 *  needed to keep the code compiling.
 * -Which required a slight modification in the include of StiKalmanTrackFinder
 * -StiKalmanTrackKalmanTrackFitter now also sets a pointer to itself in
 *  static StiKalmanTrack::setFitParameters()
 * -Removed some print outs from VectorizedFactory to Reduce the size of the log
 *  files.
 *
 * Revision 2.31  2004/03/23 23:10:37  calderon
 * Check for nan's in TrackLength() calculation.  When the argument for the
 * asin() is >1, the code instead calculates a length iteratively.
 * For these cases, the returned value is negative so that they can be inspected
 * in the gui, or filtered in the StiStEventFiller.
 *
 * Revision 2.30  2004/02/21 18:27:34  pruneau
 * Updates to comply with changes made in abstract interfaces.
 *
 * Revision 2.29  2003/09/02 17:59:41  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.28  2003/08/02 08:22:43  pruneau
 * best performance so far
 *
 * Revision 2.27  2003/07/30 19:18:25  pruneau
 * sigh
 *
 * Revision 2.25  2003/05/14 21:37:59  pruneau
 * Fixed "chi2" problem. 5 first nodes on a track did not have
 * relevant errors. Fix the problem by inserting a call to calculateError()
 * inside the Add(stiHit*...) method used while initializing tracks from the
 * seed finder. CP
 *
 * Revision 2.24  2003/05/06 15:33:49  mmiller
 * iCommitting changes to turn on multiple regions (StiPlacement::StiRegion -> kMidRapidity, kForwardRapidity, etc).
 * Also added a point to StiToolkit for StiMaker.  This allows for the req. GetDataSet calls in the FTPC code.
 * Not so elegant...
 *
 * Revision 2.23  2003/04/29 18:48:21  pruneau
 * *** empty log message ***
 *
 * Revision 2.22  2003/04/22 21:20:05  pruneau
 * Added hit filter
 * Tuning of finder pars
 * Tuning of KalmanTrackNode
 *
 * Revision 2.21  2003/04/10 12:02:13  pruneau
 * various changes
 *
 * Revision 2.20  2003/03/31 17:18:47  pruneau
 * various
 *
 * Revision 2.19  2003/03/17 17:45:31  pruneau
 * *** empty log message ***
 *
 * Revision 2.18  2003/03/14 20:50:29  pruneau
 * Added groupId member and accessor functions to StiDetector, StiDetectorGroup, StiDetectorBuilder,
 * and modified Nodes of the StiKalmanTrack class to use it. This eliminates explicit
 * references to Tpc and Svt within StiKalmanTrack...
 *
 * Revision 2.17  2003/03/14 19:02:20  pruneau
 * various minor updates
 *
 * Revision 2.16  2003/03/13 21:21:26  pruneau
 * Phase() fixed. MUST inclde -helicity()*pi/2
 *
 * Revision 2.15  2003/03/13 18:59:08  pruneau
 * various updates
 *
 * Revision 2.14  2003/03/13 16:38:11  andrewar
 * Made use of X0() calls in getTrackRadLength()
 *
 * Revision 2.13  2003/03/13 15:16:41  pruneau
 * fixed Phi, PseudoRapdity, Phase methods
 *
 * Revision 2.12  2003/03/12 17:57:29  pruneau
 * Elss calc updated.
 *
 * Revision 2.11  2003/03/04 15:16:22  andrewar
 * Added getTrackRadLength function to return radiation thickness along track (%).
 *
 */



#include <Stiostream.h>
#include <stdlib.h>

//Std
#include <cmath>

//SCL
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"

//Sti
#include "StiKalmanTrack.h"
#include "StiToolkit.h"
#include "StiHit.h"
#include "StiKalmanTrackNode.h"
#include "StiDetector.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StPhysicalHelixD.hh"
#include "StHelix.hh"
#include "StDetectorDbMaker/StiKalmanTrackFitterParameters.h"
#include "StDetectorDbMaker/StiKalmanTrackFinderParameters.h"
#include "StiHitContainer.h"
#include "StiKalmanTrackContainer.h"
#include "StiKTNIterator.h"
#include "StiHitContino.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "StarMagField.h"
#include "StMessMgr.h"
#define PrPP(A,B) {if (Debug() & 32) {LOG_INFO  << "=== StiKalmanTrack::" << (#A) << "\t" << (#B) << " = \t" << ( B ) << endm;}}
ostream& operator<<(ostream&, const StiHit&);

Int_t StiKalmanTrack::mgMaxRefiter = 100;
Int_t StiKalmanTrack::_debug = 0;
Int_t gLevelOfFind = 0;
Int_t debugCount=0;
static const Double_t kRMinTpc =  55;
static const Double_t kRMaxTpc = 192/TMath::Cos(TMath::DegToRad()*15);
static const Double_t kZMaxTpc = 210;

//______________________________________________________________________________
Double_t StiKalmanTrack::Value(Int_t key) const
{
  Double_t value;
  switch (key)    {
  case kCharge: value = Charge(); break;
  case kMass:   value = getMass(); break;
  case kChi2: value = Chi2(); break;
  case kDca2: value = 0.;break;// Dca2(); break;
  case kDca3: value = 0.;break;// Dca3(); break;
  case kFlag: value = Flag(); break;
  case kPointCount: value = PointCount(); break;
  case kFitPointCount: value = FitPointCount(); break;
  case kGapCount: value = GapCount(); break;
  case kTrackLength: value = TrackLength(); break;
  case kMaxPointCount: value = MaxPointCount(); break;
  case kTpcDedx: value = 0; break;
  case kSvtDedx: value = 0; break;
  case kCurvature: value = Curvature(); break;
  case kP: value = P(); break;
  case kPt: value = Pt(); break;
  case kRapidity: value = Rapidity(); break;
  case kPseudoRapidity: value = PseudoRapidity(); break;
  case kPhi: value = Phi(); break;
  case kTanL: value = TanL(); break;
  default: value = -999999.; break;
  }
  return value;  
}
/*! 
   Reset the class members to their default state.
   This method is called by the ctor of the class to Initialize the
   members of the class to an "empty" or null track state. The
   method must also be called everytime an instance of this class is
   retrieved from its factory in order to set the first and last
   nodes to "null" thus guaranteeing that the track object is empty
   i.e. does not represent any track and is thus ready for a new
   search and reconstruction.  
 */
//_____________________________________________________________________________
void StiKalmanTrack::Reset() {
  static Int_t mIdCount = 0;
  if ((++mIdCount) >= 1<<16) mIdCount = 1;
  mIdDb = mIdCount; 
  firstNode = 0;
  lastNode  = 0;
  mSeedHitCount = 0;
  m      = -1.;
  mFlag  = 0;
  _dca   = 0;
  _vChi2=-2;
}
//_____________________________________________________________________________
/*! Initialization of this kalman track from external parameters.
  <p>
  This track object is Initialized on the basis of parameters determined externally. The
  parameters consist of the track curvature, the tangent of pitch angle, the origin of 
  the helix, and a vector of hits already associated with the track.
  
  <h3>Arguments:</h3>
  <TABLE BORDER="0" CELLPADDING="2" CELLSPACING="0" WIDTH="100%">
  <TR> <TD WIDTH="10%">curvature</TD> <TD WIDTH="90%">1/radius of the tack.</TD>  </TR>
  <TR> <TD WIDTH="10%">tanl</TD>      <TD WIDTH="90%">tan(pitch angle)</TD> </TR>
  <TR> <TD WIDTH="10%">origin</TD>    <TD WIDTH="90%">origin of the track in global coordinates.</TD> </TR>
  <TR> <TD WIDTH="10%">v</TD>         <TD WIDTH="90%">vector of hits associated with this track.</TD> </TR>
</TABLE>
<h3>Algorithm:</h3>
<ol>
<li>Verify that a valid node factory exists.</li>
<LI>Use local arrays state and error to add and set all nodes of this track.</LI>
<LI>Use the same curvature, and tanl for all nodes as supplied in argument list.</li>
<li>Use Unit matrix for error matrix.</li>
<li>Loop over all hits of the input hit vector and create a track node for each.</LI>
<li>Paramters of the track node are set according to the y,z of the hits added.</LI>
<li>Hits given are transformed in the local coordinates of their detector.
</ol>
<h3>Notes:</h3>
<OL>
<LI>Throws a logic_error exception if no track node factory is available.</li>
<LI>Throws a logic_error exception if the factory
  is not a castable to a factory of StiKalmanTrackNode.</li>
<li>Throws a logic error exception if hits do not have a valid pointer to a detector object.</li>
</OL>
*/

//_____________________________________________________________________________
Int_t StiKalmanTrack::Initialize(const std::vector<StiHit*> &hits)
{
  //cout << "StiKalmanTrack::Initialize() -I- Started"<<endl;
  Reset();
  //StiKalmanTrackNode * node  = 0;
  const StiDetector* detector=0;
  Int_t nhits = hits.size();
  for (Int_t ihit=0;ihit<nhits;ihit++)  {
    StiHit *hit = hits[ihit];
    detector = hit->Detector();
    assert(detector);
    StiKalmanTrackNode * n = StiToolkit::instance()->TrackNodeFactory()->getInstance();
    n->Initialize(hit);
    Add(n,kOutsideIn);
  }
  if (! Approx(0)) return 0;
  BFactory::Free(this);
  return 1;
}
//_____________________________________________________________________________
Int_t StiKalmanTrack::Approx(Int_t mode) {
  Int_t nNode,nNodeIn,iNode=0;
  Double_t Xi2=0;
  //		Loop over nodes and collect global xyz
  
  StiKTNIterator source;
  StiKalmanTrackNode *targetNode;
  nNode=0;
  THelixFitter circ;
  THelixTrack  cirl;
  TRVector xyzG(3);
  for (source=rbegin();(targetNode=source());++source) {
    iNode++;
    if (!targetNode->IsValid()) 	continue;
    const StiHit * hit = targetNode->Hit();
    if (!hit) 				continue;
    if (targetNode->Chi2()>1000)	continue;
    circ.Add(hit->x_g(),hit->y_g(),hit->z_g());
    xyzG += hit->Gxyz();
    const static Double_t hitErrors[6] = 
      {0.1*0.1,
       0.0,     0.1*0.1,
       0.0,     0.0,      0.2*0.2};
    circ.AddErr(hitErrors,hitErrors[5]);
    nNode++;
  }  
  if (!nNode) 				return 1; 
  if (nNode<2)		return 3;
  xyzG /= nNode;
  Double_t h[3] = {0, 0, 0};
  StarMagField::Instance()->BField(xyzG.GetArray(),h);
  static const Double_t EC = 2.99792458e-4;
  Double_t hh = EC*h[2];
  if (TMath::Abs(hh) < 3e-33) hh=3e-33;
  hh = 1./hh;
  
  nNodeIn = nNode;
  Int_t nPnts = nNode;
  if (nPnts==2) {
    nPnts=3;
    circ.Add(0.,0.,0.);
    static const Double_t vErr[3]={1.,0.,1};
    circ.AddErr(vErr,100*100.);
  }
  Xi2 =circ.Fit();
  if (Debug() & 32) {cout << "StiKalmanTrack::Approx circ\t"; circ.Print();}
  static const Double_t BAD_XI2[2]={99,22};
  if (Xi2>BAD_XI2[mode])return 2;
  Double_t s=0,xyz[3]; 
  Double_t curv = circ.GetRho();
  iNode = 0;
  for (source=rbegin();(targetNode=source());++source) {
    iNode++;
    if (!targetNode->IsValid()) 	continue;
    const StiHit *hit = targetNode->Hit();
    if (hit) {
      xyz[0] = hit->x_g();
      xyz[1] = hit->y_g();
      xyz[2] = hit->z_g();
    } else {
      xyz[0] = targetNode->x_g();
      xyz[1] = targetNode->y_g();
      xyz[2] = targetNode->z_g();
    }
    Double_t ds = circ.Path(xyz[0],xyz[1]);
    circ.Move(ds);
    s+=ds;
    cirl = circ;
    Double_t xyzL[3];
    const StiDetector *det = targetNode->Detector();
    TGeoHMatrix *rot = 0;
    if (det) { rot = det->GetMatrix(); rot->MasterToLocal(cirl.Pos(), xyzL);}
    else     TCL::ucopy(cirl.Pos(), xyzL,3);
#ifdef _STI_PARAMETERS_    
    TRVector P(6, xyzL[0], xyzL[1], xyzL[2],              // x,y,z
	       TMath::ATan2(cirl.Dir()[1],cirl.Dir()[0]), // eta
	       (hh)? curv*hh:1e-6,                        // ptin
	       cirl.GetSin()/cirl.GetCos());              // tanl
    static const Double_t DY=0.3,DZ=0.3,DEta=0.03,DPti=1.,DTan=0.05;
    static TRSymMatrix C(6,
			 0.0, 
			 0.0, DY*DY,
			 0.0,   0.0, DZ*DZ,
			 0.0,   0.0,   0.0, DEta*DEta,
			 0.0,   0.0,   0.0,       0.0, DPti*DPti,
			 0.0,   0.0,   0.0,       0.0,       0.0, DTan*DTan);
    targetNode->Fitted() = FitParameters(FitParameters::kSti, rot, P, C);
#else
    // SD
    Double_t dirL[3] = {1, 0, 0};
    if (rot)     rot->MasterToLocalVect(cirl.Dir(), dirL);
    Double_t spu = TMath::Sign(1., dirL[0]);
    TVector3 Dir(cirl.Dir());
    Double_t pInv = (hh)? - curv*hh*Dir.Perp():1e-6;
    TRVector P(5,
	       pInv,               // q/p
	       dirL[1]/dirL[0],    // tY
	       dirL[2]/dirL[0],    // tZ
	       xyzL[1],            // yT
	       xyzL[2]);           // zT
    static const Double_t DY=0.3,DPinv=1.,DTan=0.05;
    static TRSymMatrix C(5,
			 DPinv*DPinv, 
			 0.0, DTan*DTan,
			 0.0,       0.0, DTan*DTan,
			 0.0,       0.0,       0.0, DY*DY,
			 0.0,       0.0,       0.0,   0.0, DY*DY);
    targetNode->Fitted() = FitParameters(FitParameters::kSD, spu, rot, P, C);
#endif
    targetNode->Fitted().SetBField(h);
    PrPP(Approx,targetNode->Fitted());  
  }   
  return 0;
}    
//_____________________________________________________________________________
StThreeVector<double> StiKalmanTrack::MomentumAtOrigin() const
{
  Double_t px,py,pz;
  px=py=pz=0;

  StiKalmanTrackNode * inner = InnerMostNode();

  if (inner==0) {
    cout << "StiKalmanTrack::MomentumAtOrigin() - ERROR - No node" << endl;
    assert(0);
  }
  StiKalmanTrackNode targetNode;
  targetNode.PropagateToBeam(inner,kOutsideIn);
  Double_t p[3];
  inner->Momentum(p,0);
  StThreeVector<double> p3(p[0],p[1],p[2]);
  return p3;
}

//_____________________________________________________________________________
/*! Return the track sign
   <h3>Notes</h3> 
   <ol>
   <li>Use the last node and the field.</li>
   </ol>
*/
//_____________________________________________________________________________
Int_t StiKalmanTrack::Charge() const
{
  StiKalmanTrackNode *node = InnerMostNode();
  if (!node) return 0;
  return  node->Charge();
}

//_____________________________________________________________________________
/// Return the track chi2 per dof
/// <p>
/// The track chi2 is calculated from the incremental chi2 of all nodes carrying a hit that contributed to the fit of the track. 
/// Note that a hit is not counted as contributing to the fit if its chi2 exceeds "StiKalmanTrackFitterParameters::instance()->getMaxChi2()"
/// Note that this function returns "-1" if the number of fit points is smaller than 6
Double_t  StiKalmanTrack::Chi2() const
{
  Double_t fitHits   = 0;
  Double_t trackChi2 = 0;
  Double_t nodeChi2  = 0;
  Double_t maxChi2   = StiKalmanTrackFitterParameters::instance()->getMaxChi2();
  Double_t theChi2 = 1.e+60;
  if (!firstNode) return theChi2;
  theChi2 = 0;
  StiKTNIterator it;
  for (it=begin();it!=end();it++)  {
    StiKalmanTrackNode *node = &(*it);
    if (!node->IsValid()) continue;
    if (!node->Hit() ) continue;
    nodeChi2 = node->Chi2();
    if (nodeChi2>maxChi2) continue;
    trackChi2 += nodeChi2;
    ++fitHits;
  }
  return (fitHits>5)?trackChi2/(2.*fitHits-5.):1e30;
}


/*! 
	Calculate and return the number of hits on this track. 
   <h3>Notes</h3> 
   <ol>
   <li>Iterate through all nodes of this track.</li>
   <li>Count number of hits.</li>
   </ol>
	 \return number of hits.
*/
//_____________________________________________________________________________
Int_t StiKalmanTrack::PointCount(Int_t detectorId) const
{
  const StiDetector *detector=0;   
  Int_t nPts = 0;
  StiKTNIterator it;
  for (it=begin();it!=end();it++) {
    StiKalmanTrackNode *node = &(*it);
    if (!node->IsValid()) 	continue;
    if (!node->Hit())	continue;
    detector = node->Detector();  
    if (!detector) 		continue;
    if (detectorId && detector->GroupId() != detectorId) 	continue;
    nPts++;
  }
  return nPts;
}

//_____________________________________________________________________________
/*! Calculate and return the maximum possible number of hits on this track. 
  <h3>Notes</h3> 
   <ol>
   <li>Iterate through all nodes of this track.</li>
   <li>Count active layers.</li>
   <li>Use the (y,z) position of the node to determine whether point is on
       active region of the detector i.e. RDO were functional.</li>
   </ol>
	 \return maximum number of points
*/
//_____________________________________________________________________________
Int_t StiKalmanTrack::MaxPointCount(Int_t detectorId) const
{
  Int_t nPts = 0;
  StiKTNIterator it;

  for (it=begin();it!=end();it++){
    const StiKalmanTrackNode *node = &(*it);
    if (!node->IsValid()) 					continue;
    const StiDetector *detector = node->Detector();
    if (!detector)						continue;
    StiHit* h = node->Hit();
    if (!h && !detector->IsActive(node->Y(),node->Z()))	continue;
    if (detectorId && detector->GroupId() != detectorId) 	continue;
    nPts++;
  }
  return nPts;
}
//_____________________________________________________________________________
/*! Return the number of gaps (active layers with no hits) along this track.
  <h3>Notes</h3> 
  <ol>
  <li>A gap consists of one or multiple contiguous active layers through which this track
  passes.</li>
  <li>There can be gaps on the inside or the outside of the track if no hits are found there.</li>
  </ol>
  \returns number of gaps.
*/
Int_t    StiKalmanTrack::GapCount() const {
  Int_t gaps = 0;
  if (firstNode) {
    StiKTNIterator it;
    Bool_t inGap = kFALSE;
    for (it=begin();it!=end();it++) {
      const StiDetector * detector = (*it).Detector();
      if (detector && detector->IsActive()) {
	if ((*it).Hit()){if ( inGap) inGap = kFALSE;}
        else            {if (!inGap) {inGap = kTRUE;  gaps++;}}
      }
    }
  }
  return gaps;
}
//_____________________________________________________________________________
/*! Return the number of hits (points) used in the fit of this track.
  <h3>Notes</h3> 
  <ol>
  <li>Currently no difference is made between points on the track and fit points 
  on the track.</li>
  <li>Call "PointCount()" to get the count.</li>
  </ol>
  \return number of hits on this track.
*/
///Get number of fit points in given detector
//_____________________________________________________________________________
Int_t StiKalmanTrack::FitPointCount(Int_t detectorId)    const  
{
  Int_t fitPointCount  = 0;
  StiKTNIterator it;
  for (it=begin();it!=end();it++)  {
    StiKalmanTrackNode* node = &(*it); 
    if(!node->IsValid())		continue;
    StiHit* hit = node->Hit();
    if (!hit)				continue;
    if (!node->IsFitted())		continue;
    const StiDetector *det = hit->Detector();
    if (!det)				continue;  
    if (detectorId && detectorId!=det->GroupId())continue;
    fitPointCount++;
  }
  return fitPointCount;
}
//_____________________________________________________________________________
void StiKalmanTrack::AllPointCount(Int_t count[1][3],Int_t maxDetId) const
{
//  output array actually is count[maxDetId+1][3] 
//  count[0] all detectors
//  count[detId] for particular detector
//  count[detId][0] == number of possible points
//  count[detId][1] == number of measured points
//  count[detId][2] == number of fitted   points
enum {kPP=0,kMP=1,kFP=2};

  memset(count[0],0,(maxDetId+1)*3*sizeof(int));
  StiKTNIterator it;

  for (it=begin();it!=end();it++){
    const StiKalmanTrackNode *node = &(*it);
    if (!node->IsValid()) 	continue;
    const StiDetector *detector = node->Detector();
    if (!detector)		continue;
    Int_t detId = detector->GroupId();
    StiHit* h = node->Hit();

//fill possible points
    if (h || detector->IsActive(node->Y(),node->Z())) {
       count[0][kPP]++; count[detId][kPP]++;
    }
    
    if (!h ) 			continue;
//fill measured points
    count[0][kMP]++; count[detId][kMP]++;
    if (!node->IsFitted()) 	continue;
    count[0][kFP]++; count[detId][kFP]++;
  }
}

//_____________________________________________________________________________
/*! Calculate and return the track length.
  <h3>Notes</h3> 
   <ol>
   <li>Using helix track model in global reference frame.</li>
   <li>Using only inner most and outer most hits associated with this track.</li>
   </ol>
   \return tracklength
   \throws runtime_error
*/
Double_t StiKalmanTrack::TrackLength() const
{
  Double_t x[2][4],len=0;
  Int_t iready=0;
  StiKalmanTrackNode *node;
  StiKTNIterator it = begin();
  for (;(node=it());it++){
    if (!node->IsValid()) 	continue;
    if (!node->Hit()) 	continue;
    if ( node->Chi2()>10000.)continue;
    x[1][0]=node->x_g();
    x[1][1]=node->y_g();
    x[1][2]=node->z_g();
    x[1][3]=node->Curvature();
    if (iready) {
      Double_t dlen = sqrt(pow(x[1][0]-x[0][0],2) + pow(x[1][1]-x[0][1],2));
      Double_t curv = TMath::Abs(0.5*(x[0][3]+x[1][3]));
      Double_t dsin = (0.5*dlen*curv);
      if (dsin>0.9) {
         LOG_DEBUG <<
           Form("StiKalmanTrack::TrackLength ***ERROR*** dsin %g >.9",dsin)
         << endm;
        dsin = 0.9;
      }
      dlen = (dsin<0.1)? dlen*(1.+dsin*dsin/6) : 2*asin(dsin)/curv; 
      len +=sqrt(dlen*dlen + pow(x[1][2]-x[0][2],2));
    }
    memcpy(x[0],x[1],4*sizeof(double)); iready=2005;
  }
  return len;
}
//_____________________________________________________________________________
Double_t StiKalmanTrack::NearBeam(StThreeVectorD *pnt,StThreeVectorD *dir) const {
  StiKalmanTrackNode * inNode = lastNode;
  StThreeVectorD in(inNode->x_g(),inNode->y_g(),inNode->z_g());
  StPhysicalHelixD hlx = inNode->Helix()->helix();
  Double_t per = hlx.period();
  Double_t len = hlx.pathLength(0.,0.);
  //  StHelix can return negative length if -ve path is shorter then +ve one
  //  period ia added in this case;
  if (TMath::Abs(len) > TMath::Abs(len+per)) len+=per;
  if (TMath::Abs(len) > TMath::Abs(len-per)) len-=per;
  
  hlx.moveOrigin(len);
  if (pnt) (*pnt) = hlx.at(0);
  
  if (dir) {
    Double_t phase = hlx.phase();
    Double_t dip   = hlx.dipAngle();
    Int_t h        = hlx.h();
    
    (*dir)[0]= -sin(phase)*cos(dip)*h;	
    (*dir)[1]=  cos(phase)*cos(dip)*h;
    (*dir)[2]=             sin(dip)*h;
  }
  return TMath::Abs(len);
}
//_____________________________________________________________________________
/*! Return the inner most hit associated with this track.
   <h3>Notes</h3>
   <ol>
   <li>Throws logic_error exception if firstNode or lastNode are not defined, or if track has no hit.</li>
   <li>Loop through all nodes from end() to begin() (or vice versa if tracking 
       direction is outside-in) and search for node with hit. Return first hit found.</li>
   </ol>
	 \return inner most hit node on this track
	 \throws logic_error
*/
//_____________________________________________________________________________
StiKalmanTrackNode * StiKalmanTrack::InnOutMostNode(Int_t inot,Int_t qua)  const {
  if (firstNode==0 || lastNode==0) {
    cout << "StiKalmanTrack::InnOutMostNode() -E- firstNode||lastNode==0" << endl;
    assert(0);
  }
  
  StiKalmanTrackNode *node;
  StiKTNIterator it =(inot) ? begin():rbegin();
  for (;(node=it());it++){
    if (!node->IsValid()) 				continue;
    StiHit *hit = node->Hit();
    if ((qua&1) && !hit) 				continue;
    if ((qua&2) && hit && node->Chi2()>10000.)	continue;
    return node;
  }
  cout << "StiKalmanTrack::InnOutMostNode() -E- No requested nodes " << endl;
  return 0;
}
//_____________________________________________________________________________
StiKalmanTrackNode * StiKalmanTrack::OuterMostHitNode(Int_t qua)  const
{
  return InnOutMostNode(1,qua|1);
}


//_____________________________________________________________________________
/*! Return the inner most hit associated with this track.
   <h3>Notes</h3>
   <ol>
   <li>Throws logic_error exception if firstNode or lastNode are not defined, or if track has no hit.</li>
   <li>Loop through all nodes from begin() to end() (or vice versa if tracking 
       direction is outside-in) and search for node with hit. Return first hit found.</li>
   </ol>
	 \return outer most hit node on this track
*/

StiKalmanTrackNode * StiKalmanTrack::InnerMostHitNode(Int_t qua)   const
{
  return InnOutMostNode(0,qua|1);
}
//_____________________________________________________________________________
Int_t StiKalmanTrack::NNodes(Int_t qua)  const
{
  StiKalmanTrackNode *node;
  StiKTNIterator it = begin();
  Int_t nn=0;
  for (;(node=it());it++){
    if (!node->IsValid()) 				continue;
    StiHit *hit = node->Hit();
    if ((qua&1) && !hit) 				continue;
    if ((qua&2) && hit && node->Chi2()>10000.)	continue;
    nn++;
  }
  return nn;
}

//_____________________________________________________________________________
/*! Return true if inner most hit associated with this track is main vertex.
   <h3>Algorithm</h3>
   <ol>
   <li>Find the inner most hit node associated with this tracks.</li>
   <li>Return true if "x" of inner most hit is less than 2 cm.
   </ol>
	 \return true if "x" of inner most hit is less than 2 cm.
*/
//_____________________________________________________________________________
Bool_t  StiKalmanTrack::IsPrimary() const
{
  StiKalmanTrackNode * node = InnerMostHitNode(3);
  if (node->Detector()) 	return 0;
  const StiHit *hit = node->Hit();
  if (hit->IsDca()) 		return 0;
  return 1;
}


//_____________________________________________________________________________
///return vector of nodes with hits
vector<StiKalmanTrackNode*> StiKalmanTrack::Nodes(Int_t detectorId) const
{
  StiKTNIterator it;
  vector<StiKalmanTrackNode*> nodeVec;
  for (it=begin();it!=end();++it) {
          StiKalmanTrackNode* node = &(*it);
    const StiHit* hit = node->Hit();
    if(!hit) 				continue;
    const StiDetector *det = hit->Detector();
    if (!det) 				continue;
    if (detectorId!=det->GroupId())	continue;
    nodeVec.push_back(node);
  }
  return nodeVec;
}

//_____________________________________________________________________________
///return hits;
vector<const StMeasuredPoint*> StiKalmanTrack::StHits() const
{
  StiKTNIterator it;
  vector<const StMeasuredPoint*> hits;
  for (it=begin();it!=end();++it) {
    const StiKalmanTrackNode* node = &(*it);
    if (!node->IsValid()) 	continue;
    if (node->Chi2()>10000.) continue;
    const StiHit* hit = node->Hit();
    if (!hit) 			continue;
    if (!hit->Detector())	continue;
    const StMeasuredPoint *stHit = hit->stHit();
    if (!stHit) 		continue;
    hits.push_back(stHit);
  }
  return hits;
}



//_____________________________________________________________________________
/*! Declare hits associated with given track as used.
  <p>
  Declare hits on the track ending at "node" as used. 
  This method starts with the last node and seeks the
  parent of each node recursively. The hit associated with each
  node (when there is a hit) is set to "used".
*/	
//_____________________________________________________________________________
void StiKalmanTrack::ReserveHits()
{
  StiKTNForwardIterator it(lastNode);
  for_each( it, it.end(), SetHitUsed() );
}

/*! Extend track to the given vertex.
  <p>
  Attempt an extension of the track  the given vertex. 
  <p>
  <ol>
  <li>Get node from node factory.</li>
  <li>Reset node.</li>
  <li>Propagate the node from given parent node "sNode", to the given vertex using a 
  call to "Propagate".</li>
  <li>Evaluate the chi2 of the Extrapolated if the vertex is added to the track. Done
  using a call to "EvaluateChi2".</li>
  <li>If chi2 is less than max allowed "maxChi2ForSelection", update track parameters
  using the vertex as a measurement and add the vertex to the track as the last node.</li>
  </ol>
  <h3>Notes</h3>
  <ul>
  <li>Throws logic_error if no node can be obtained from the node factory.</li>
  <li>The methods "Propagate", "EvaluateChi2", and "UpdateNode" may throw 
  runtime_error exceptions which are NOT caught here...</li>
  </ul>
*/

//_____________________________________________________________________________
StiKalmanTrackNode *StiKalmanTrack::ExtendToVertex(StiHit* vertex) {
  static Int_t nCall=0; nCall++;
  Double_t chi2;
  Int_t dcaHit = vertex->IsDca();
  StiKalmanTrackNode * sNode=0;
  StiKalmanTrackNode * tNode=0;
  Bool_t trackExtended = kFALSE;

  StiKalmanTrackNode * innerMostHitNode = InnerMostHitNode();
  if (!innerMostHitNode) 		return 0;
		
  StiHit localVertex = *vertex;
  sNode = InnerMostNode();
  if (sNode->IsDca()) {//it is fake node. Remove it
    RemoveLastNode();
    sNode = InnerMostNode();
  }

  tNode = StiToolkit::instance()->TrackNodeFactory()->getInstance();
  StiHit *myHit;
  //cout << "SKT::ExtendToVertex() -I- x,y,z:"<< localVertex.x() 
  //     << " " <<  localVertex.y() << " " << localVertex.z() << endl;
  //cout << "SKT::ExtendToVertex() -I- sNode->NormalX():"<<sNode->NormalX()<<endl;
  //cout << "SKT::ExtendToVertex() -I-0 tNode->NormalX():"<< tNode->NormalX()<<endl;
  if (tNode->Propagate(sNode, &localVertex,kOutsideIn))    { 
    //cout << " on vertex plane:";
    Double_t dy=0,dz=0,d=0;
    if (dcaHit) {		//Fake DCA vertex
      tNode->setChi2(0); 
      tNode->SetHit(0);
      tNode->SetDetector(0);
      return tNode;
    } else {			//Normal vertex 
      tNode->setChi2(3e33);
      chi2 = tNode->EvaluateChi2(&localVertex); 
      dy=tNode->Y()- localVertex.y();
      dz=tNode->Z()- localVertex.z();
      d = ::sqrt(dy*dy+dz*dz);
	_vChi2= chi2; _dca = d;
    }
    if (chi2<StiKalmanTrackFinderParameters::instance()->maxChi2Vertex())   {
      myHit = StiToolkit::instance()->HitFactory()->getInstance();
      *myHit = localVertex;
      tNode->SetHit(myHit);
      tNode->setChi2(chi2);
      tNode->SetDetector(0);
      trackExtended = (tNode->UpdateNode()==0);
      if (Debug()) cout << "ExtendToVertex:: " << StiKalmanTrackNode::Comment() << endl;
      if (trackExtended) return tNode;
      StiToolkit::instance()->TrackNodeFactory()->free(tNode);             
    } else if (d < 4) {
      LOG_DEBUG << Form("Primary(%d) not accepted BUT d = %g chi2 = %g",nCall,d,chi2) << endm;
    }
  }
  return 0;
}
///Return all the hits associated with this track, including those with a large incremental
///chi2 that may not contribute to the fit.
//_____________________________________________________________________________
vector<StiHit*> StiKalmanTrack::Hits()
{
  vector<StiHit*> hits;
  StiKalmanTrackNode* leaf = LastNode();
  StiKTNForwardIterator it(leaf);
  StiKTNForwardIterator end = it.end();
  for (;it!=end;++it) 
    {
      const StiKalmanTrackNode& node = *it;
      if (!node.IsValid())		continue;
      if (node.Chi2()>10000.) 	continue;
      StiHit* hit = node.Hit();
      if (!hit) 			continue;
      hits.push_back(hit);
    }
  return hits;
}

//_____________________________________________________________________________
/// Return global dca of the track relative to given vertex or point.
Double_t  StiKalmanTrack::Dca(const StiHit * vertex) const {
  StThreeVectorD vxDD(vertex->Gxyz().GetArray());
  Double_t dca = InnerMostHitNode(2)->Helix()->helix().distance(vxDD);
  return dca;
}
//_____________________________________________________________________________
ostream& operator<<(ostream& os, const StiKalmanTrack& track) {
  os << "Id: " << track.Id()  
     <<" Chi2: "<<track.Chi2()
     <<" q: "<<track.Charge()
     <<" pt: "<<track.Pt()
     <<" eta: "<<track.PseudoRapidity()
     <<" tanLambda: "<<track.TanL()
     <<" phi: "<<track.Phi()
     <<" points/fit/max: "<<track.PointCount()
     <<"/"<<track.FitPointCount()
     <<"/"<<track.MaxPointCount()<<endl;
  if (track.FirstNode()) {
    os <<"List of nodes";
    StiKTNIterator tNode = track.begin();
    StiKTNIterator eNode = track.end();
    //set initial conditions for tNode, the 'current' node;
    //will also need 'nextNode', ie node which is next 
    while (tNode != eNode) {
      StiKalmanTrackNode *thisNode = &(*tNode);
      if (thisNode) os << endl << *thisNode;
      tNode++;
    }
  }
  return os;
}

//_____________________________________________________________________________
///Extrapolate this track to the beam axis (x==0) to provide an estimate of the
///track location at the beam axis.
///Returns a null pointer is the operation cannot be completed i.e. the track does not reach
///the beam axis plane.
StiKalmanTrackNode * StiKalmanTrack::ExtrapolateToBeam()
{
  StiKalmanTrackNode * innerMostNode = InnerMostNode();
  //return null if there is no node to Extrapolate from.
  if (!innerMostNode) return 0;
  StiKalmanTrackNode * n = StiToolkit::instance()->TrackNodeFactory()->getInstance();
  if (n->PropagateToBeam(innerMostNode,kOutsideIn)) return n;
  StiToolkit::instance()->TrackNodeFactory()->free(n);
  return 0;
}

//_____________________________________________________________________________
StiKalmanTrackNode * StiKalmanTrack::ExtrapolateToRadius(Double_t radius)
{
  StiKalmanTrackNode * outerMostNode = OuterMostNode();
  //return null if there is no node to Extrapolate from.
  if (!outerMostNode) return 0;
  StiKalmanTrackNode *n = StiToolkit::instance()->TrackNodeFactory()->getInstance();
  if (n->PropagateToRadius(outerMostNode,radius,kOutsideIn)) return n;
  StiToolkit::instance()->TrackNodeFactory()->free(n);
  return 0;
}

//_____________________________________________________________________________
void StiKalmanTrack::Add(StiKalmanTrackNode *Node,Int_t direction) {
  if (lastNode ==0) {lastNode = firstNode = Node; return;}
  if (direction==0) {lastNode->Add(Node,direction); lastNode = Node; }
  else {            firstNode->Add(Node,direction); firstNode = Node;}
}
//_____________________________________________________________________________
void StiKalmanTrack::SetFirstLastNode(StiKalmanTrackNode * node)
{
 firstNode = (StiKalmanTrackNode*)node->FirstNode();
  lastNode = (StiKalmanTrackNode*)node->LastNode ();
}
//_____________________________________________________________________________
void StiKalmanTrack::RemoveLastNode()
{
  StiKalmanTrackNode *node = lastNode;
  lastNode = (StiKalmanTrackNode*)node->Disconnect();
  BFactory::Free(node);
}
//_____________________________________________________________________________
void StiKalmanTrack::Unset() 
{
  if (!lastNode) return;
  StiKTNIterator source;
  for (source=begin();source!=end();source++) {BFactory::Free(&(*source));}
  lastNode=0; firstNode=0;
}
//_____________________________________________________________________________
StiKalmanTrack &StiKalmanTrack::operator=(const StiKalmanTrack &tk) {
  mId = tk.mId;
  firstNode=0;
  lastNode=0;

  mSeedHitCount=tk.mSeedHitCount; 	//number of points used to seed the track
  mFlag        =tk.mFlag;         	//A flag to pack w/ topo info
  m            =tk.m;             	// mass hypothesis
  _dca	       =tk._dca;
  _vChi2       =tk._vChi2;		//

  StiKTNIterator it;
  for (it=tk.begin();it!=tk.end();it++){
    const StiKalmanTrackNode *node = &(*it);
    if (!node->IsValid()) continue;
    StiKalmanTrackNode *myNode=StiToolkit::instance()->TrackNodeFactory()->getInstance();
    *myNode=*node;
    Add(myNode,kOutsideIn);
  }
  return *this;
}

//_____________________________________________________________________________
StThreeVector<double> StiKalmanTrack::Point(Int_t firstLast) const
{
  const StiKalmanTrackNode* node = InnOutMostNode(firstLast,3);
  return StThreeVector<double>(node->x_g(),node->y_g(),node->z_g());
}
//________________________________________________________________________________
/*! Fit given track with helicoical track model.
  <h3>Notes</h3>
	<ol>
  <li>The fit is performed along a direction (inside-out||outside-in) prescribed by 
      the value of the track flag "FittingDirection".</li>
  <li>In practice, the iteration through track nodes proceeds from first-to-last
      or last-to-first whether "trackingDirection==fitDirection"
  <li>Use track node methods to do the actual propagation. 
  <li>Node with no hits are allowed as the evaluation of the track chi2 
      and updates are performed only if nodes hold a hit.
	</ol>
*/
Int_t StiKalmanTrack::Fit(Int_t fitDirection, TRVector *refFitPar) {
  static Int_t nCall=0; nCall++;
  StiKalmanTrackNode::Break(nCall);
  
  if (Debug() > 2) cout << "StiKalmanTrack::fit() -I- Started:"<<endl;
  StiHit * tarHit;
  StiKalmanTrackNode *targetNode; // parent node
  StiDetector  *targetDet;  // parent detector
  
  StiKTNBidirectionalIterator first;
  StiKTNBidirectionalIterator last;
  StiKTNBidirectionalIterator source;
  Double_t chi2;
  Int_t status = 0,nerr =0;
  if (!fitDirection) {
    first = begin();
    last  = end();
  } else {
    last  = rend();
    first = rbegin();
  }
  if (Debug() > 2) cout << "StiKalmanTrack::fit direction = "  << fitDirection << endl;
  // 1st count number of accepted already good nodes
  Int_t nGoodNodes = NNodes(3);
  if (nGoodNodes<3) 			return 1;
  StiKalmanTrackNode *pNode = 0;
  Int_t iNode=0; status = 0;
  for (source=first;source!=last;source++) {
    iNode++;
    targetNode = &(*source);
    targetDet = (StiDetector *) targetNode->Detector();
    tarHit = targetNode->Hit();
    Double_t oldChi2 = targetNode->Chi2(); if(oldChi2){/*debugonly*/};
    static Int_t myKount=0;myKount++;
    if (!pNode && !targetNode->IsValid()) continue;
    if (!pNode) {
      if (Debug()) {
	targetNode->ResetComment(::Form("%30s start fit",targetDet->GetName()));
	targetNode->PrintpT("s");
	StiKalmanTrackNode::PrintStep();
	targetNode->ResetComment(::Form("%30s start fit",targetDet->GetName()));
      }
      pNode = targetNode;
    }
    status = 0;
    targetNode->setChi2(1e51);
    if      (targetDet) status = targetNode->Propagate(pNode,targetDet,fitDirection,refFitPar);  // hit
    else if (tarHit) status = targetNode->Propagate(pNode,tarHit,fitDirection,refFitPar);  // vertex
    // target node has parameters now but not fitted
    // if targetNode has hit, get chi2 and update track parameters accordingly
    targetNode->setChi2(0.);
    if (!tarHit)   continue; //There is no hit.
    assert(targetNode->Hit()==tarHit);
    StiKalmanTrackNode tryNode = *targetNode;
    targetNode->setChi2(1e52);
    chi2 = tryNode.EvaluateChi2(tarHit);
    if ((chi2>StiKalmanTrackFitterParameters::instance()->getMaxChi2()))	{nerr++; continue;}	//Chi2 is bad
    status = tryNode.UpdateNode();
    if (status) 			{nerr++; continue;}
    tryNode.setChi2(chi2);
    if (Debug()) {cout << Form("%5d ",status); StiKalmanTrackNode::PrintStep();}
    *targetNode=tryNode;
    pNode = targetNode;
  } //end for of nodes
  nGoodNodes = NNodes(3);
  if (nGoodNodes<3) return 1;
  return 0;
}
//________________________________________________________________________________
Bool_t StiKalmanTrack::Find(Int_t direction,Double_t rmin) {
  static Int_t nCall=0; nCall++;
  gLevelOfFind = 0;
  StiKalmanTrackNode::Break(nCall);
  Int_t nnBef,nnAft;
  Double_t lnBef,lnAft;
  
  if(direction) rmin=0; //no limitation to outside
  nnBef = NNodes(3);
  lnBef = TrackLength();

  StiKalmanTrackNode *leadNode = InnOutMostNode(direction,2);
  if (!leadNode) return 0;
  leadNode->CutTail(direction);
  assert(leadNode->IsValid());
  QAFind qa; qa.rmin = rmin;
  Find(direction,leadNode,qa);
  SetFirstLastNode(leadNode);
  nnAft = NNodes(3);
  lnAft = TrackLength();
  return (nnAft>nnBef || lnAft>(lnBef+0.5));
}
//______________________________________________________________________________
void StiKalmanTrack::Find(Int_t direction
			  ,StiKalmanTrackNode *leadNode,QAFind &qa) {
  static Int_t nCall=0; nCall++;
  StiKalmanTrackNode::Break(nCall);
  
  //  const Double_t ref2a  = 2.*3.1415927-ref1a;
  gLevelOfFind++;
  StiDetector *tDet=0;
  Int_t status = 0;
  StiKalmanTrackNode testNode;
  Int_t position = 0;
  StiHit * stiHit;

  assert(leadNode->IsValid());
  StiKalmanTrackNode *pNode = leadNode;
  while (1) {
    testNode.Reset(); testNode.SetDetector(0);
    testNode.setChi2(1e55);
    if (testNode.Propagate(pNode,direction)) break;
    pNode = 0;
    tDet = (StiDetector *) testNode.Detector();
    if (! tDet) continue;
    assert(testNode.IsValid());
    Int_t active = tDet->IsActive(testNode.Y(),testNode.Z());
    if (! active) continue;
    if (Debug() > 2) cout << " vol active:" << active<<endl;
    Double_t maxChi2 = tDet->TrackingParameters()->getMaxChi2ForSelection();
    StiHitContino hitCont;
    vector<StiHit*> & candidateHits = StiToolkit::instance()->HitContainer()->Hits(testNode);//,true);
    if (! candidateHits.size()) continue;
    vector<StiHit*>::iterator hitIter;
    if (Debug() > 2) cout << " candidates:"<< candidateHits.size();
    for (hitIter=candidateHits.begin();hitIter!=candidateHits.end();++hitIter) {
      stiHit = *hitIter;
      if (stiHit->Detector() && stiHit->Detector()!=tDet) continue;
      testNode.SetReady();
      Double_t chi2 = testNode.EvaluateChi2(stiHit);
      if (Debug() > 2)   cout<< " got chi2:"<< chi2 << " for hit:"<< endl << *stiHit<<endl;
      if (chi2>maxChi2) {
	if (Debug() > 1) cout << "chi2 = " << chi2 << " is too high ... skip hit" << endl;
	continue;
      }
      hitCont.Add(stiHit,chi2);
      if (Debug() > 2) cout << " hit selected"<<endl;
    }// for (hitIter)
    Int_t nHits = hitCont.NHits();
    if (! nHits) continue;
    assert(nHits<100);
    testNode.SetHitCand(nHits);
#if 0
    if (direction) {
      nHits=1;
    } else {
      Int_t flg = (testNode.NormalX()< kRMinTpc)? Comb()&3:Comb()>>2;
      if ((flg&2) || !nHits) 	nHits++;
      if ((flg&1)==0) 	nHits=1;
    }
#endif
    QAFind qaBest,qaTry;
    for (Int_t jHit=0;jHit<nHits; jHit++) {//Loop over Hits
      stiHit = hitCont.Hit(jHit);
      StiKalmanTrackNode * node = StiToolkit::instance()->TrackNodeFactory()->getInstance();
      *node = testNode;
      status = 0;
      do {//fake do
	if (!stiHit) break;
	node->SetIHitCand(jHit);
	assert(node->HitCand());
	node->SetHit(stiHit);
#if 0
	node->SetHitErrors(stiHit);
#endif
	status = node->UpdateNode();
	if (status)  break;
	node->setChi2(hitCont.Chi2(jHit));
	if (Debug() > 0) {cout << Form("%5d ",status); StiKalmanTrackNode::PrintStep();}
      } while(0);
      if (status)  {StiToolkit::instance()->TrackNodeFactory()->free(node); continue;}
      if (! leadNode->Parent()) {
	qaBest = qa;
	continue;
      } else {
	qaTry = qa;
	NodeQA(node,position,active,qaTry);
	leadNode->Add(node,direction);
	if (qaTry.qa>-2) Find(direction,node,qaTry);
	
	if (jHit==0) { qaBest=qaTry; continue;}
	Int_t igor = CompQA(qaBest,qaTry,maxChi2);
	if (igor<0)  { leadNode->Remove(0);}
	else         { leadNode->Remove(1);qaBest=qaTry;}
      }
    }
    qa = qaBest; gLevelOfFind--; return;
  }
  gLevelOfFind--;
  return;
}
//________________________________________________________________________________
Int_t StiKalmanTrack::Smooth(Int_t fitDirection) {
  static Int_t nCall=0; nCall++;
  StiKalmanTrackNode::Break(nCall);
  if (Debug() > 2) cout << "StiKalmanTrack::Smooth() -I- Started:"<<endl;
  
  StiKTNBidirectionalIterator first;
  StiKTNBidirectionalIterator last;
  StiKTNBidirectionalIterator source;

  Int_t status = 0;
  if ( fitDirection) {// opposite to to fit
    first = begin();
    last  = end();
  } else {
    last  = rend();
    first = rbegin();
  }
  if (Debug() > 2) cout << "StiKalmanTrack::fit direction = "  << fitDirection << endl;
  // 1st count number of accepted already good nodes
  Int_t nGoodNodes = NNodes(3);
  if (nGoodNodes<3) 			return 1;
  StiKalmanTrackNode *k1 = 0; // parent node k+1
  Int_t iNode=0; status = 0;
  Double_t maxChi2   = StiKalmanTrackFitterParameters::instance()->getMaxChi2();
  for (source=first;source!=last;source++) {
    StiKalmanTrackNode *k = &(*source);
    k->Smoothed() = k->Fitted();
    if (iNode == 0 && ! k->Hit()) {k->SetInvalid();}
    if (!k->IsValid()) continue;
    iNode++;
    const StiDetector *det = k->Detector();
    if (Debug()) {
      if (det) k->ResetComment(::Form("%30s fitted  ",k->Detector()->GetName()));
      else     k->ResetComment(::Form("the vertex fitted"));
      k->PrintpT("b"); k->PrintStep();
    }
    if (! k1) {
      if (Debug()) {
	if (det) k->ResetComment(::Form("%30s start smoother",k->Detector()->GetName()));
	else     k->ResetComment(::Form("start smoother at the vertex"));
	k->PrintpT("S"); k->PrintStep(); 
	k1 = k;
      }
      continue;
    }
    // node "k"        contains : x_k   as Fitted(), x^k-1_k as Predicted(), F_k-1 as F(), x^n_k   as Smoothed()
    // node "k1 = k+1" contains : x_k+1 as Fitted(), x^k_k+1 as Predicted(), F_k   as F(), x^n_k+1 as Smoothed()
    PrPP(Smooth,k1->Fitted());    PrPP(Smooth,k->Fitted());
    PrPP(Smooth,k1->Predicted()); PrPP(Smooth,k->Predicted());
    PrPP(Smooth,k1->Smoothed());
    // Smoothing: Transport Matrix k => k+1 is stored in node (k+1)
    const TRMatrix &F = k1->F();                              PrPP(Smooth,F);
    // Smoothed gain matrix:  A_k = C_k *FT_k (C^k_k+1)^-1
    TRSymMatrix C(k->Fitted().C());                           PrPP(Smooth,C);
    TRMatrix    CF(C,TRArray::kSxAT,F);                       PrPP(Smooth,CF);
    /* */                                                     PrPP(Smooth,k1->Predicted().C());
    TRSymMatrix CpI(k1->Predicted().C(),TRArray::kInverted);  PrPP(Smooth,CpI);
    TRMatrix    A(CF,TRArray::kAxS, CpI);                     PrPP(Smooth,A);
    // Smoothed state vector: x^n_k = x_k + A_k * (x^n_k+1 - x^k_k+1);
    TRVector dP(k1->Smoothed().P());                          PrPP(Smooth,k1->Smoothed().P()); 
    /* */                                                     PrPP(Smooth,k1->Predicted().P()); 
    dP -= k1->Predicted().P();                                PrPP(Smooth,dP);
    TRVector P(k->Fitted().P());                              PrPP(Smooth,k->Fitted().P()); 
    P += TRVector(A,TRArray::kAxB,dP);                        PrPP(Smooth,P); //<<<<<
    // Cov. matrix of the smoothed state vector: C^n_k = C_k + A_k * (C^n_k+1 - C^k_k+1) * AT_k;
   TRSymMatrix dC(k1->Smoothed().C());
    dC -= k1->Predicted().C();                                PrPP(Smooth,dC);
    TRSymMatrix B(A,TRArray::kAxSxAT,dC);                     PrPP(Smooth,B);
    C += B;                                                   PrPP(Smooth,C);
    k->Smoothed().P() = P;
    k->Smoothed().C() = C;
    if ( k->Hit()) {
      // Smoothed residual: r^n_k = r_k - H_k * (x^n_k - x_k) = m_k - H_k*x^n_k
      Double_t time = k->TimeOfFlight(k->Smoothed());
      k->r() =  k->Hit()->Measurement(time);                  PrPP(Smooth,k->r());
      k->r() -= TRVector(k->Fitted().H(),TRArray::kAxB,P); PrPP(Smooth,k->r());
      // Cov. matrix of smoothed residual: R^n_k = R_k + H_k * A_k * (C^n_k+1 - C^k_k+1) AT_k * HT_k = V_k - H_k * C^n_k * HT_k
      k->R() = k->V(k->Smoothed());                                      PrPP(Smooth,k->R());
      PrPP(Smooth,k->Fitted().H()); PrPP(Smooth,C);
      TRSymMatrix RP(k->Fitted().H(),TRArray::kAxSxAT,C);       PrPP(Smooth,RP);
      k->R() -= RP;                                             PrPP(Smooth,k->R());
      // chi2_s = rT^n_k *(R^n_k)^-1 *r^n_k;
      TRSymMatrix G(k->R(),TRArray::kInvertedA);                PrPP(Smooth,G);
      k->chi2() = -1;
      if (G.IsValid())
	k->chi2() = G.Product(k->r(),TRArray::kATxSxA);           PrPP(Smooth,k->chi2());
      if (! G.IsValid() || k->chi2() > maxChi2) {
	// Remove the measurement
	if (Debug()) {
	  if (det) k->ResetComment(::Form("%30s rejected",k->Detector()->GetName()));
	  else     k->ResetComment(::Form("the vertex rejected"));
	  k->PrintpT("S"); k->PrintStep();
	}
	// Remove the measurement and check it with respect to the rest track (reverse sign of K)
	// K^n*_k = C^n_k * HT_k *(V_k - H_k * C^n_k * HT_k)^-1 = C^n_k * HT_k * GG
	// GG = (V_k - H_k * C^n_k * HT_k)^-1
	TRSymMatrix VV(k->V(k->Fitted()));                        PrPP(Smooth,VV);
	VV -= TRSymMatrix(k->Fitted().H(),TRArray::kAxSxAT,k->Smoothed().C()); PrPP(Smooth,VV);
	TRSymMatrix GG(VV,TRArray::kInvertedA);                    PrPP(Smooth,GG);
	if (GG.IsValid()) {
	  TRMatrix D(k->Smoothed().C(),TRArray::kSxAT,k->Fitted().H());PrPP(Smoth,D);
	  TRMatrix K(D,TRArray::kAxS,GG);                           PrPP(Smoth,K);
	  // x^n*_k = x^n_k - K^n*_k * (m_k - H_k*x^n_k):
	  TRVector r(k->Hit()->Measurement(time));                 PrPP(Smoth,r);
	  r -= TRVector(k->Fitted().H(),TRArray::kAxB,P);   PrPP(Smoth,r);
	  k->Smoothed().P() -= TRVector(K,TRArray::kAxB,r);                    PrPP(Smoth,k->Smoothed().P());
	  r = k->Hit()->Measurement(time);
	  r -= TRVector(k->Fitted().H(),TRArray::kAxB,k->Smoothed().P());PrPP(Smoth,r);
	  Double_t chi2 = GG.Product(r,TRArray::kATxSxA);           PrPP(Smooth,chi2);
	  // C^n*_k = (I + K^n*_k*H_k) * C^n_k = 
	  // = C^n_k + C^n_k * HT_k * GG *H_k * C^n_k = 
	  TRSymMatrix HGH(k->Fitted().H(),TRArray::kATxSxA,GG);PrPP(Smoth,HGH);
	  k->Smoothed().C() += TRSymMatrix(k->Smoothed().C(),TRArray::kRxSxR,HGH);        PrPP(Smoth,k->Smoothed().C());
	  assert(k->Smoothed().C()(0,0) > 0);
	  if (Debug()) {
	    if (det) k->ResetComment(::Form("%30s accepted",k->Detector()->GetName()));
	    else     k->ResetComment(::Form("the vertex accepted"));
	    StiKalmanTrackNode::PrintpT(k->Smoothed(),chi2,"R"); StiKalmanTrackNode::PrintStep();
	  }
	}
	k->SetHit(0);
      } else {
	if (Debug()) {
	  if (det) k->ResetComment(::Form("%30s accepted",k->Detector()->GetName()));
	  else     k->ResetComment(::Form("the vertex accepted"));
	}
      }
    } else {
      if (Debug()) {
	if (det) k->ResetComment(::Form("%30s no hit  ",k->Detector()->GetName()));
	else     k->ResetComment(::Form("no vertex"));
      }
    }
    if (Debug()) {
      k->PrintpT("S"); k->PrintStep();
    }
    k->Fitted() = k->Smoothed();
    if (Debug()) {
      if (det) k->ResetComment(::Form("%30s smoothed",k->Detector()->GetName()));
      else     k->ResetComment(::Form("the vertex accepted"));
      k->PrintpT("F"); k->PrintStep();
    }
    // goto next node
    k1 = k;
  } //end for of nodes
  nGoodNodes = NNodes(3);
  if (nGoodNodes<3) return 1;
  return 0;
}
//______________________________________________________________________________
void StiKalmanTrack::NodeQA(StiKalmanTrackNode *node, Int_t position
                                 ,Int_t active,QAFind &qa) {
  Int_t maxNullCount           = StiKalmanTrackFinderParameters::instance()->maxNullCount()+3;
  Int_t maxContiguousNullCount = StiKalmanTrackFinderParameters::instance()->maxContiguousNullCount()+3;
//		Check and count node
  StiHit *hit = node->Hit();
  if (hit) {
    if (Debug() > 2)cout << " got Hit! "<<endl ;
//  const StiDetector *detector = hit->Detector();
    qa.sum += node->Chi2();
    qa.hits++; qa.qa=1;
    if (node->NormalX() < kRMinTpc) {
      qa.wits+=StiKalmanTrackFinderParameters::instance()->hitWeight((int)node->NormalX());
    }
    node->HitCount()++;
    node->ContigHitCount()++;
    if (node->ContigHitCount()>StiKalmanTrackFinderParameters::instance()->minContiguousHitCountForNullReset())
       node->ContigNullCount() = 0;

  } else if (position>0 || !active) {// detectors edge - don't really expect a hit here
    qa.qa=0;

  } else {// there should have been a hit but we found none
      if (Debug() > 2) cout << " no hit but expected one"<<endl;
      node->NullCount()++; 
      node->ContigNullCount()++;
      node->ContigHitCount() = 0;
      qa.nits++; qa.qa=-1;
      if (node->NullCount()>maxNullCount) 			qa.qa= -3;
      if (node->ContigNullCount()>maxContiguousNullCount)	qa.qa= -3;
  }//node->Hit()

}
//______________________________________________________________________________
Int_t StiKalmanTrack::CompQA(QAFind &qaBest,QAFind &qaTry,Double_t maxChi2)
{
   Int_t ians;
   ians = qaTry.wits-qaBest.wits;
//	One SVT hit is worse than zero
   if (!qaBest.wits &&  qaTry.wits &&  qaTry.wits < StiKalmanTrackFinderParameters::instance()->sumWeight()) return -1;
   if ( !qaTry.wits && qaBest.wits && qaBest.wits < StiKalmanTrackFinderParameters::instance()->sumWeight()) return  1;
   				        if (ians)	return ians;
   ians =  qaTry.hits-qaBest.hits;	if (ians)	return ians;
   ians = qaBest.nits- qaTry.nits;	if (ians)	return ians;
   if (qaBest.sum  <= qaTry.sum ) 			return -1;
   							return  1;
}
//________________________________________________________________________________
Int_t StiKalmanTrack::Fit() {
  Bool_t trackExtended = kFALSE, trackExtendedOut = kFALSE;
  SetFlag(-1);
  Int_t status = Fit(kOutsideIn);
  if (status )   return -1;
  if (Debug() > 1) cout << "chi2/ndf after fit    " << Chi2() << endl;
  trackExtended = Find(kOutsideIn);
  if (! trackExtended) return -1;
  if (Debug() > 1) cout << "chi2/ndf after find   " << Chi2() << endl;
  // decide if an outward pass is needed.
  const StiKalmanTrackNode * outerMostNode = OuterMostNode(2);
  if (!outerMostNode) {
    SetFlag(-1);
    return -1;
  }
  Smooth(kOutsideIn);
  if (Debug() > 1) cout << "chi2/ndf after Smooth " << Chi2() << endl;
  if (outerMostNode->NormalX()<185. ) {
    trackExtendedOut= Find(kInsideOut);
    if (Debug()) cout << "StiKalmanTrack::Build (track,kInsideOut)" << *((StiKalmanTrack *) this);
    if (Debug() > 1) cout << "chi2/ndf after find   " << Chi2() << endl;
    Smooth(kInsideOut);
    if (Debug() > 1) cout << "chi2/ndf after Smooth " << Chi2() << endl;
  }
  trackExtended |=trackExtendedOut;
  // Refit
  Int_t iter = 1;
  Double_t chi2Old = Chi2();
  Double_t chi2C   = chi2Old;
  while ((iter == 1) || (iter <= 5 && TMath::Abs(chi2Old - chi2C) > 1e-3)) {
    StiKalmanTrackNode * outerMostNode = OuterMostNode();
    TRVector refFitPar(outerMostNode->Fitted().P()); PrPP(Fit,refFitPar); PrPP(Fit,outerMostNode->Fitted().C()); 
    TRSymMatrix &C = outerMostNode->Fitted().C();
    for (Int_t i = 0; i < outerMostNode->Fitted().P().GetSize(); i++) {
      for (Int_t j = 0; j < i; j++) {
	C(i,j) = 0;
      }
      C(i,i) *= 400;
    }
    PrPP(Fit,outerMostNode->Fitted().C());
    status = Fit(kOutsideIn,&refFitPar); 
    if (status) break;
    if (Debug() > 1) cout << "chi2/ndf after re fit " << Chi2() << endl;
    Smooth(kOutsideIn);
    if (Debug() > 1) cout << "chi2/ndf after Smooth " << Chi2() << endl;
    chi2Old = chi2C;
    chi2C = Chi2();
    if (Debug() > 1) cout << "iteration = " << iter << "\tchi2 Old/New = " << chi2Old << " / " << chi2C << endl;
    iter++;
  }
#if 0  
  //================= not ready yet
  //		Add DCA node
  StiHit dcaHit; dcaHit.makeDca();
  StiKalmanTrackNode *extenDca = ExtendToVertex(&dcaHit);
  if (extenDca) {
    Add(extenDca,kOutsideIn);
    if (Debug() >= 1) StiKalmanTrackNode::PrintStep();
  }
  //		End DCA node
#endif
  return 0;
}

//______________________________________________________________________________
void StiKalmanTrack::ExtendTrackToVertices(const std::vector<StiHit*> &vertices) {
  static const Double_t RMAX2d=StiKalmanTrackFinderParameters::instance()->maxDca2dZeroXY();
  static const Double_t DMAX3d=StiKalmanTrackFinderParameters::instance()->maxDca3dVertex();

  StiKalmanTrackNode *Extended=0;
  Int_t nVertex =         vertices.size();  
  if (!nVertex) return;

  StiKalmanTrackNode *bestNode=0;  
  StThreeVectorD nearBeam;
  NearBeam(&nearBeam);
  if (nearBeam.perp2()>RMAX2d*RMAX2d) 		return;
  for (Int_t iVertex=0;iVertex<nVertex;iVertex++) {
    StiHit *vertex = vertices[iVertex];
    if (TMath::Abs(Dca(vertex)) > DMAX3d)    	continue;
    Extended = (StiKalmanTrackNode*)ExtendToVertex(vertex);
    if (!Extended) 					continue;
    if (!bestNode) {bestNode=Extended;		continue;}
    if (bestNode->Chi2() < Extended->Chi2())    continue;
    BFactory::Free(bestNode);
    bestNode = Extended;
  }//End vertex loop
  
  if(!bestNode) 			return;
  Add(bestNode,kOutsideIn);
  Int_t         ifail = 0;
  // something is wrong. It is not a primary
  if (ifail) { RemoveLastNode(); return;}
}
//______________________________________________________________________________
void StiKalmanTrack::ExtendTrackToVertex(StiHit* vertex) {
  StiKalmanTrackNode *Extended = ExtendToVertex(vertex);
  if (Extended) {
    Add(Extended,kOutsideIn);
    if (Extended && !Extended->IsValid()) 	Extended=0;
    if (Extended && Extended->Chi2()>1000) 	Extended=0;
  }
 }
