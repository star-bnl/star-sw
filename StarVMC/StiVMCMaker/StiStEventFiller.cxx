/***************************************************************************
 *
 * $Id: StiStEventFiller.cxx,v 2.95 2009/08/20 22:25:30 fisyak Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Mar 2002
 ***************************************************************************
 *
 * $Log: StiStEventFiller.cxx,v $
 * Revision 2.95  2009/08/20 22:25:30  fisyak
 * Freeze before moving to SD set os parameters
 *
 * Revision 2.94  2009/08/19 19:56:40  fisyak
 * Clean up
 *
 * Revision 2.93  2009/08/19 18:08:01  fisyak
 * Eliminate StiStarVertexFinder
 *
 * Revision 2.92  2009/08/04 18:55:14  fisyak
 * Capitilize method names
 *
 * Revision 2.91  2009/08/03 19:18:13  fisyak
 * Add StHelixModel creator to StiKalmanTrackNode
 *
 * Revision 2.90  2009/08/02 19:05:36  fisyak
 * Add reference track
 *
 * Revision 2.89  2009/07/19 20:14:35  fisyak
 * remove abstract classes
 *
 * Revision 2.88  2009/05/26 21:57:24  fisyak
 * Comment out material and detector shape related methods
 *
 * Revision 2.87  2009/05/06 16:39:46  fisyak
 * Move to TRArray
 *
 * Revision 2.86  2009/04/15 20:26:53  fisyak
 * Clean ups, use VMC TGeo for detector description, load hits in the central place
 *
 * Revision 2.85  2009/03/16 13:50:14  fisyak
 * Move out all Sti Chairs into StDetectorDb
 *
 * Revision 2.84  2008/08/22 13:32:52  fisyak
 * add one more digit in trakc flag, mFlag=zxyy, where  z = 1 for pile up track in TPC (otherwise 0)
 *
 * Revision 2.83  2008/04/03 20:04:05  fisyak
 * Straighten out DB access via chairs
 *
 * Revision 2.82  2007/10/17 15:32:35  fisyak
 * rename Hft => Pxl
 *
 * Revision 2.81  2007/04/16 22:47:18  perev
 * aux.mPt is +ve
 *
 * Revision 2.80  2007/03/21 17:51:36  fisyak
 * adjust for ROOT 5.14
 *
 * Revision 2.79  2006/12/19 19:46:09  perev
 * Filling pull tracks added
 *
 * Revision 2.78  2006/12/18 01:30:39  perev
 * fillPulls reorganized
 *
 * Revision 2.77  2006/08/31 03:25:58  fisyak
 * Make cut for EEMC pointing track based on StTrackDetectorInfo instead of StTrackFitTraits
 *
 * Revision 2.76  2006/08/29 22:18:37  fisyak
 * move filling of StTrackDetectorInfo into fillTrack
 *
 * Revision 2.75  2006/08/28 17:02:23  fisyak
 * Add +x11 short tracks pointing to EEMC, clean up StiDedxCalculator
 *
 * Revision 2.74  2006/06/16 21:28:57  perev
 * FillStHitErr method added and called
 *
 * Revision 2.73  2006/05/31 03:59:04  fisyak
 * Add Victor's dca track parameters, clean up
 *
 * Revision 2.72  2006/04/07 18:00:30  perev
 * Back to the latest Sti
 *
 * Revision 2.69  2006/02/14 18:56:18  perev
 * setGlobalDca==>setDca
 *
 * Revision 2.68  2006/01/19 22:29:57  jeromel
 * kMaxId -> kMaxDetectorId
 *
 * Revision 2.67  2005/12/08 00:06:27  perev
 * BugFix, Instead of vertex, first hit was used
 *
 * Revision 2.66  2005/08/18 22:31:47  perev
 * More tests
 *
 * Revision 2.65  2005/08/17 22:04:36  perev
 * PoinCount cleanup
 *
 * Revision 2.64  2005/08/16 21:09:06  perev
 * remeve 5fit cut
 *
 * Revision 2.63  2005/08/16 20:37:23  perev
 * remove small pt cut
 *
 * Revision 2.62  2005/08/14 01:24:40  perev
 * test for nhits<5 removed
 *
 * Revision 2.61  2005/08/04 04:04:19  perev
 * Cleanup
 *
 * Revision 2.60  2005/07/21 21:50:24  perev
 * First/last point of track filled from node now
 *
 * Revision 2.59  2005/07/20 17:34:08  perev
 * MultiVertex
 *
 * Revision 2.58  2005/05/12 18:32:20  perev
 * Temprary hack, save residuals
 *
 * Revision 2.57  2005/04/11 17:42:39  perev
 * Temporary residuals saving added
 *
 * Revision 2.56  2005/03/24 17:51:16  perev
 * print error code added
 *
 * Revision 2.55  2005/03/17 06:33:20  perev
 * TPT like errors implemented
 *
 * Revision 2.54  2005/02/25 17:43:15  perev
 * StTrack::SetKey(...StiKalmanTrack::Id()) now
 *
 * Revision 2.53  2005/02/17 23:19:03  perev
 * NormalRefangle + Error Reseting
 *
 * Revision 2.52  2005/02/07 18:34:16  fisyak
 * Add VMC dead material
 *
 * Revision 2.51  2005/01/17 03:56:56  pruneau
 * change track container to vector
 *
 * Revision 2.50  2005/01/17 01:32:13  perev
 * parameters protected
 *
 * Revision 2.49  2004/12/21 20:46:00  perev
 * Cleanup. All known bugs fixed
 *
 * Revision 2.48  2004/12/02 22:14:53  calderon
 * Only fill the fitTraits.chi2[1] data member for primaries.
 * It holds node->Chi2() from the innerMostHitNode, which will be the
 * vertex for primaries.
 *
 * Revision 2.47  2004/12/02 04:18:06  pruneau
 * chi2[1] now set to incremental chi2 at inner most hit or vertex
 *
 * Revision 2.46  2004/12/01 15:35:46  pruneau
 * removed throw and replaced with continue
 *
 * Revision 2.45  2004/11/08 15:34:16  pruneau
 * fix of the chi2 calculation
 *
 * Revision 2.44  2004/10/27 03:25:54  perev
 * Version V3V
 *
 * Revision 2.43  2004/10/26 06:45:41  perev
 * version V2V
 *
 * Revision 2.42  2004/10/14 02:21:34  calderon
 * Updated code in StTrackDetectorInfo, now only increment the reference count
 * for globals, not for primaries.  So fillTrackDetectorInfo changed to reflect
 * this.
 *
 * Revision 2.41  2004/10/01 01:13:51  calderon
 * Added bug fix from Marco:
 * flag%100 -> flag/100.
 *
 * Revision 2.40  2004/08/17 20:04:28  perev
 * small leak fixed, delete physicalHelix,originD
 *
 * Revision 2.39  2004/08/17 04:53:05  calderon
 * When filling fit traits for primary tracks, set the new flag
 * mPrimaryVertexUsedInFit.
 *
 * Revision 2.38  2004/08/10 14:21:13  calderon
 * Use the firstHit from the dynamic_cast, to avoid a compiler warning
 * for an unused variable.
 *
 * Revision 2.37  2004/08/06 22:23:29  calderon
 * Modified the code to use the setNumberOfxxxPoints(unsigned char,StDetectorId)
 * methods of StTrack, StTrackDetectorInfo, StTrackFitTraits, and to use
 * the maxPointCount(unsigned int detId) method of StiKalmanTrack.
 *
 * Revision 2.36  2004/08/06 02:29:20  andrewar
 * Modifed call to MaxPointCount
 *
 * Revision 2.35  2004/08/05 05:25:25  calderon
 * Fix the assignment of the first point for primaries.  Now,
 * the logic for both globals and primaries is that the first
 * point is the first element of the StHits() vector that
 * can actually be casted to an StHit (the vertex will fail this test,
 * all other hits coming from detectors will satisfy it).
 *
 * Revision 2.34  2004/07/30 18:49:18  calderon
 * For running in production, Yuri's dEdx Maker will fill the Pid Traits,
 * so the filling of Pid Traits in the filler is no longer needed:
 * it actually causes confusion because the V0 finders will loop over
 * the PID traits vector and find the first one, so they won't find
 * the trait created by the dEdx Maker.  It is best to just comment
 * out the filling of the Pid Traits here.
 *
 * Revision 2.33  2004/07/07 19:33:48  calderon
 * Added method fillFlags.  Flags tpc, tpc+svt (globals and primaries) and flags -x02 tracks with less than 5 total fit points
 *
 * Revision 2.32  2004/04/21 21:36:24  calderon
 * Correction in the comments about the encoded method.
 *
 * Revision 2.31  2004/03/31 00:27:29  calderon
 * Modifications for setting the fit points based on the chi2<chi2Max algorithm.
 * -Distinguish between points and fit points, so I added a function for each.
 * -Points is done as it was before, just counting the StHits for a given
 *  detector id.
 * -Fit points is done the same with the additional condition that each
 *  StiKalmanTrackNode has to satisfy the chi2 criterion.
 *
 * Revision 2.30  2004/03/29 00:52:20  andrewar
 * Added key value to StTrack fill. Key is simply the size of the
 * StTrackNode container at the time the track is filled.
 *
 * Revision 2.29  2004/03/23 23:12:36  calderon
 * Added an "accept" function to filter unwanted tracks from Sti into StEvent.
 * The current method just looks for tracks with a negative length, since
 * these were causing problems for the vertex finder (length was nan).  The
 * nan's have been trapped (one hopes!) in StiKalmanTrack, and for these
 * cases the return value is negative, so we can filter them out with a
 * simple length>0 condition.
 *
 * Revision 2.28  2004/03/19 19:33:23  andrewar
 * Restored primary filling logic. Now taking parameters at the
 * vertex for Primary tracks.
 *
 * Revision 2.27  2004/01/27 23:40:46  calderon
 * The filling of the impactParameter() for global tracks is done now
 * only after finding the vertex.  The
 * StPhysicalHelix::distance(StThreeVectorD) method is used for both globals
 * and primaries, the only difference is where the helix is obtained:
 * - globals - helix from StTrack::geometry(), which was filled from the
 *             innermost hit node, which should be a hit at the time.
 * - primaries - helix from innermost hit node, which should be the vertex
 *             at the time it is called.
 *
 * Revision 2.26  2003/12/11 03:44:29  calderon
 * set the length right again, it had dissappeared from the code...
 *
 * Revision 2.25  2003/11/26 04:02:53  calderon
 * track->Chi2() returns the sum of chi2 for all sti nodes.  In StEvent,
 * chi2(0) should be chi2/dof, so we need to divide by
 * dof=track->PointCount()-5;
 *
 * Revision 2.24  2003/09/07 03:49:10  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 2.23  2003/09/02 17:59:59  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.22  2003/08/21 21:21:56  andrewar
 * Added trap for non-finite dEdx. Added logic to fillGeometry so
 * info is for innerMostHitNode on a detector, not vertex (note:
 * Primaries only)
 *
 * Revision 2.21  2003/08/05 18:26:15  andrewar
 * DCA track update logic modified.
 *
 * Revision 2.20  2003/07/01 20:25:28  calderon
 * fillGeometry() - use node->NormalX(), as it should have been since the
 * beginning
 * impactParameter() - always use the innermos hit node, not just for globals
 * removed extra variables which are no longer used.
 *
 * Revision 2.19  2003/05/15 03:50:26  andrewar
 * Disabled call to filldEdxInfo for the SVT. Checks need to be
 * applied to make sure the detector is active before calculator
 * is called, but for the review filling this info is unnecessary.
 *
 * Revision 2.18  2003/05/14 00:04:35  calderon
 * The array of 15 floats containing the covariance matrix has a different
 * order in Sti than in StEvent.  In Sti the array is counted starting from
 * the first row, column go to next column until you hit the diagonal,
 * jump to next row starting from first column. In StEvent the array is
 * counted starting from the first row, column go to the next row until you
 * hit the end, jump to next column starting from diagonal.
 * The filling of the fitTraits was fixed to reflect this.
 *
 * Revision 2.17  2003/05/12 21:21:39  calderon
 * switch back to getting the chi2 from track->Chi2()
 * Covariance matrix is still obtained from node->Get(), and the values
 * are not as expected in StEvent, so this will still need to change.
 *
 * Revision 2.16  2003/05/08 22:23:33  calderon
 * Adding a check for finiteness of node origin and node curvature.  If any
 * of the numbers is not finite, the code will abort().
 *
 * Revision 2.15  2003/04/29 18:48:52  pruneau
 * *** empty log message ***
 *
 * Revision 2.14  2003/04/29 15:28:10  andrewar
 * Removed hacks to get helicity right; switch now done at source
 * (StiKalmanTrackNode).
 *
 * Revision 2.13  2003/04/25 21:42:47  andrewar
 * corrected DCA bug and added temp fix for helicity problem. This will
 * have to be modified when the helicity convention in StiStKalmanTrack
 * is updated.
 *
 * Revision 2.12  2003/04/04 14:48:34  pruneau
 * *** empty log message ***
 *
 * Revision 2.11  2003/03/14 19:02:55  pruneau
 * various updates - DCA is a bitch
 *
 * Revision 2.10  2003/03/13 21:20:10  pruneau
 * bug fix in filler fixed.
 *
 * Revision 2.9  2003/03/13 18:59:44  pruneau
 * various updates
 *
 * Revision 2.8  2003/03/13 16:01:48  pruneau
 * remove various cout
 *
 * Revision 2.7  2003/03/13 15:15:52  pruneau
 * various
 *
 * Revision 2.6  2003/03/12 17:58:05  pruneau
 * fixing stuff
 *
 * Revision 2.5  2003/02/25 16:56:20  pruneau
 * *** empty log message ***
 *
 * Revision 2.4  2003/02/25 14:21:10  pruneau
 * *** empty log message ***
 *
 * Revision 2.3  2003/01/24 06:12:28  pruneau
 * removing centralized io
 *
 * Revision 2.2  2003/01/23 05:26:02  pruneau
 * primaries rec reasonable now
 *
 * Revision 2.1  2003/01/22 21:12:15  calderon
 * Restored encoded method, uses enums but stores the value in constructor
 * as a data member so bit operations are only done once.
 * Fixed warnings.
 *
 * Revision 2.0  2002/12/04 16:50:59  pruneau
 * introducing version 2.0
 *
 * Revision 1.21  2002/09/20 02:19:32  calderon
 * Quick hack for getting code for review:
 * The filler now checks the global Dca for the tracks and only fills
 * primaries when dca<3 cm.
 * Also removed some comments so that the production log files are not swamped
 * with debug info.
 *
 * Revision 1.20  2002/09/12 22:27:15  andrewar
 * Fixed signed curvature -> StHelixModel conversion bug.
 *
 * Revision 1.19  2002/09/05 05:47:36  pruneau
 * Adding Editable Parameters and dynamic StiOptionFrame
 *
 * Revision 1.18  2002/08/29 21:09:22  andrewar
 * Fixed seg violation bug.
 *
 * Revision 1.17  2002/08/22 21:46:00  pruneau
 * Made a fix to StiStEventFiller to remove calls to StHelix and StPhysicalHelix.
 * Currently there is one instance of StHelix used a calculation broker to
 * get helix parameters such as the distance of closest approach to the main
 * vertex.
 *
 * Revision 1.16  2002/08/19 19:33:00  pruneau
 * eliminated cout when unnecessary, made helix member of the EventFiller
 *
 * Revision 1.15  2002/08/12 21:39:56  calderon
 * Introduced fillPidTraits, which uses the values obtained from
 * Andrews brand new dEdxCalculator to create two instances of an
 * StTrackPidTraits object and pass it to the track being filled.
 *
 * Revision 1.14  2002/08/12 15:29:21  andrewar
 * Added dedx calculators
 *
 * Revision 1.13  2002/06/28 23:30:56  calderon
 * Updated with changes debugging for number of primary tracks added.
 * Merged with Claude's latest changes, but restored the tabs, othewise
 * cvs diff will not give useful information: everything will be different.
 *
 * Revision 1.12  2002/06/26 23:05:31  pruneau
 * changed macro
 *
 * Revision 1.11  2002/06/25 15:09:16  pruneau
 * *** empty log message ***
 *
 * Revision 1.10  2002/06/18 18:08:34  pruneau
 * some cout statements removed/added
 *
 * Revision 1.9  2002/06/05 20:31:15  calderon
 * remove some redundant statements, the call to
 * StTrackNode::addTrack()
 * already calls
 * track->SetNode(this), so I don't need to do it again
 *
 * Revision 1.8  2002/05/29 19:14:45  calderon
 * Filling of primaries, in
 * StiStEventFiller::fillEventPrimaries()
 *
 * Revision 1.7  2002/04/16 19:46:44  pruneau
 * must catch exception
 *
 * Revision 1.6  2002/04/16 13:11:30  pruneau
 * *** empty log message ***
 *
 * Revision 1.5  2002/04/09 16:03:13  pruneau
 * Included explicit extension of tracks to the main vertex.
 *
 * Revision 1.4  2002/04/03 16:35:03  calderon
 * Check if primary vertex is available in StiStEventFiller::impactParameter(),
 * if not, return DBL_MAX;
 *
 * Revision 1.3  2002/03/28 04:29:49  calderon
 * First test version of Filler
 * Currently fills only global tracks with the following characteristics
 * -Flag is set to 101, as most current global tracks are.  This is not
 * strictly correct, as this flag is supposed to mean a tpc only track, so
 * really need to check if the track has svt hits and then set it to the
 * appropriate flag (501 or 601).
 * -Encoded method is set with bits 15 and 1 (starting from bit 0).  Bit 1
 * means Kalman fit.
 *  Bit 15 is an as-yet unused track-finding bit, which Thomas said ITTF
 * could grab.
 * -Impact Parameter calculation is done using StHelix and the primary vertex
 * from StEvent
 * -length is set using TrackLength, which might still need tweaking
 * -possible points is currently set from MaxPointCount which returns the
 *  total, and it is not
 *  what we need for StEvent, so this needs to be modified
 * -inner geometry (using the innermostHitNode -> Ben's transformer ->
 *  StPhysicalHelix -> StHelixModel)
 * -outer geometry, needs inside-out pass to obtain good parameters at
 *  outermostHitNode
 * -fit traits, still missing the probability of chi2
 * -topology map, filled from StuFixTopoMap once StDetectorInfo is properly set
 *
 * This version prints out lots of messages for debugging, should be more quiet
 * when we make progress.
 *
 **************************************************************************/
//ROOT
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
//std
#include "Stiostream.h"
#include <algorithm>
#include <stdexcept>
using namespace std;

// SCL
#include "StPhysicalHelix.hh"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StTrackDefinitions.h"
#include "StTrackMethod.h"
#include "StDedxMethod.h"

//StEvent
#include "StPrimaryVertex.h"
#include "StEventTypes.h"
#include "StDetectorId.h"
#include "StHelix.hh"
#include "StDcaGeometry.h"
#include "StHit.h"


#include "StEventUtilities/StEventHelper.h"
#include "StEventUtilities/StuFixTopoMap.cxx"
//Sti
#include "StiVMC/StiKalmanTrackContainer.h"
#include "StiVMC/StiKalmanTrack.h"
#include "StDetectorDbMaker/StiKalmanTrackFitterParameters.h"
//StiVMCMaker
#include "StiStEventFiller.h"

#include "TMath.h"

//_____________________________________________________________________________
StiStEventFiller::StiStEventFiller() : mEvent(0), mTrackStore(0), mTrkNodeMap()
{
   mGloPri = 0;
  originD = new StThreeVectorD(0,0,0);
  physicalHelix = new StPhysicalHelixD(0.,0.,0.,*originD,-1);
 

  //mResMaker.setLimits(-1.5,1.5,-1.5,1.5,-10,10,-10,10);
  //mResMaker.SetDetector(kSvtId);

  // encoded method = 16 bits = 12 finding and 4 fitting, Refer
  // to StTrackMethod.h and StTrackDefinitions.h in pams/global/inc/
  // and StEvent/StEnumerations.h
  // For the IT tracks use:
  // Fitting: kITKalmanFitId     (should be something like 7, but don't hardwire it)
  // Finding: tpcOther           (should be 9th LSB, or shift the "1" 8 places to the left, but also don't hardwire it) 
  // so need this bit pattern:
  // finding 000000010000     
  // fitting             0111 
  //               256  +   7 = 263;
  UShort_t bit = 1 << tpcOther;  // shifting the "1" exactly tpcOther places to the left
  mStiEncoded = kITKalmanFitId + bit; // adding that to the proper fitting Id

}

//_____________________________________________________________________________
StiStEventFiller::~StiStEventFiller()
{
 delete physicalHelix; physicalHelix=0;
 delete originD;       originD      =0;
   cout <<"StiStEventFiller::~StiStEventFiller()"<<endl;
}

//Helper functor, gotta live some place else, just a temp. test of StiKalmanTrack::StHits() method
//_____________________________________________________________________________
struct StreamStHit
{
  void operator()(const StHit* h) 
  {
    //cout << "DetectorId: " << (unsigned long) h->Detector();
    if (const StTpcHit* hit = dynamic_cast<const StTpcHit*>(h)) 
      {
	cout <<hit->position() << " Sector: " << hit->sector() << " Padrow: " << hit->padrow() << endl;
      }
    else if (const StSvtHit* hit = dynamic_cast<const StSvtHit*>(h)) 
      {
	cout << hit->position() << " layer: " << hit->layer() << " ladder: " << hit->ladder()
	     << " wafer: " << hit->wafer() << " barrel: " << hit->barrel() << endl;
      }
    else 
      {	
	cout << hit->position() << endl;
      }
  }
};

//_____________________________________________________________________________
/*! 
  Algorithm:
  Loop over all tracks in the StiKalmanTrackContainer, doing for each track:
  - Create a new global track and associated information (see below)
    and set its data members according to the StiKalmanTrack,
    can be done in a StGlobalTrack constructor
  - Hang the new track to the StTrackNode container in StEvent, this creates a new entry
    in the container, the global track is now owned by it.
    <p>
  In addition to the StGlobalTrack, we need to create the following objects (owned by it):
  StTrackTopologyMap
  StTrackFitTraits
  StTrackGeometry (2 are needed, one at first point, one at last point)
  (note: StHelixModel is implementation of the StTrackGeometry abstract class)
  
  The track also owns a container of PidTraits, this algorithm will not fill this container.
  
  And set up links to:
  StTrackDetectorInfo (owned by StEvent, StSPtrVecTrackDetectorInfo)
  StTrackNode         (owned by StEvent, StSPtrVecTrackNode)
  These links are
  track  -> detector info
  track <-> track node

  Skeleton of the algorithm:
  <code> \n
  StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); \n
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); \n
  for (trackIterator trackIt = mTrackStore->begin(); trackIt != mTrackStore->end(); ++trackIt) { \n
     StiKalmanTrack* kTrack = (*trackIt).second; // the container is a <map>, need second entry of <pair> \n
\n
     StTrackDetectorInfo* detInfo = new StTrackDetectorInfo();\n
     fillDetectorInfo(detInfo,kTrack);\n
     detInfoVec.push_back(detInfo);\n
     \n
     StTrackNode* trackNode = new StTrackNode;\n
     trNodeVec.push_back(trackNode);\n
     \n
     StGlobalTrack* gTrack = new StGlobalTrack();\n
     fillGlobalTrack(gTrack,kTrack);\n
     \n
     // set up relationships between objects\n
     gTrack->SetDetectorInfo(detInfo);\n
     gTrack->setNode(trackNode);\n
     trackNode->AddTrack(gTrack);\n
  }\n
  </code>
  The creation of the various objects needed by StGlobalTrack are taken care of in the methods:
  fillTopologyMap(), fillGeometry(), fillFitTraits(), which are called within fillGlobalTrack().
  
*/
//_____________________________________________________________________________
void StiStEventFiller::fillEvent(StEvent* e, StiKalmanTrackContainer* t)
{
  //cout << "StiStEventFiller::fillEvent() -I- Started"<<endl;
  mGloPri=0;
  if (e==0 || t==0) 
    {
      cout <<"StiStEventFiller::fillEvent(). ERROR:\t"
	   <<"Null StEvent ("<<e<<") || StiKalmanTrackContainer ("<<t<<").  Exit"<<endl;
      return;
    }
  mEvent = e;
  mTrackStore = t;
  mTrkNodeMap.clear();  // need to Reset for this event
  StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); 
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); 
  Int_t errorCount=0; 

  Int_t fillTrackCount1=0;
  Int_t fillTrackCount2=0;
  Int_t fillTrackCountG=0;
  StErrorHelper errh;
  mTrackNumber=0;
  for (vector<StiKalmanTrack*>::iterator trackIt = mTrackStore->begin(); trackIt!=mTrackStore->end();++trackIt) 
    {
      StiKalmanTrack* kTrack = static_cast<StiKalmanTrack*>(*trackIt);
      if (!accept(kTrack)) continue; // get rid of riff-raff
      mTrackNumber++;
      StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
      fillDetectorInfo(detInfo,kTrack,true); //3d argument used to increase/not increase the refCount. MCBS oct 04.
      // track node where the new StTrack will reside
      StTrackNode* trackNode = new StTrackNode;
      // actual filling of StTrack from StiKalmanTrack
      StGlobalTrack* gTrack = new StGlobalTrack;
      try 
	{
	  fillTrackCount1++;
	  fillTrack(gTrack,kTrack,detInfo);
	  // filling successful, set up relationships between objects
	  detInfoVec.push_back(detInfo);
	  //cout <<"Setting key: "<<(UShort_t)(trNodeVec.size())<<endl;
	  gTrack->setKey((UShort_t)kTrack->Id());
	  trackNode->addTrack(gTrack);
	  trNodeVec.push_back(trackNode);
	  // reuse the utility to fill the topology map
	  // this has to be done at the end as it relies on
	  // having the proper track->detectorInfo() relationship
	  // and a valid StDetectorInfo object.
	  //cout<<"Tester: Event Track Node Entries: "<<trackNode->entries()<<endl;
	  mTrkNodeMap.insert(map<StiKalmanTrack*,StTrackNode*>::value_type (kTrack,trNodeVec.back()) );
	  if (trackNode->entries(global)<1)
	    cout << "StiStEventFiller::fillEvent() -E- Track Node has no entries!! -------------------------" << endl;  
          Int_t ibad = gTrack->bad();
	  errh.Add(ibad);
          if (ibad) {
//VP	    printf("GTrack error: %s\n",errh.Say(ibad).Data());
//VP	    throw runtime_error("StiStEventFiller::fillEvent() StTrack::bad() non zero");
          }
	  fillTrackCount2++;
          if (gTrack->numberOfPossiblePoints()<10) continue;
          if (gTrack->geometry()->momentum().mag()<0.1) continue;
	  fillTrackCountG++;
          
	}
      catch (runtime_error & rte ) 
	{
	  cout << "StiStEventFiller::fillEvent() -W- runtime-e filling track"<<rte.what() << endl;
	  delete trackNode;
	  delete detInfo;
	  delete gTrack;
	}
      catch (...) 
	{
	  cout << "StiStEventFiller::fillEvent() -W- Unknown exception filling track."<<endl;
	  delete trackNode;
	  delete detInfo;
	  delete gTrack;
	}
    }
  if (errorCount>4)
    cout << "There were "<<errorCount<<"runtime_error while filling StEvent"<<endl;

  cout <<"StiStEventFiller::fillEvent() -I- Number of filled as global(1):"<< fillTrackCount1<<endl;
  cout <<"StiStEventFiller::fillEvent() -I- Number of filled as global(2):"<< fillTrackCount2<<endl;
  cout <<"StiStEventFiller::fillEvent() -I- Number of filled GOOD globals:"<< fillTrackCountG<<endl;
  errh.Print();

  return;
}
//_____________________________________________________________________________
void StiStEventFiller::fillEventPrimaries() 
{
  //cout <<"StiStEventFiller::fillEventPrimaries() -I- Started"<<endl;
  mGloPri=1;
  if (!mTrkNodeMap.size()) 
    {
      cout <<"StiStEventFiller::fillEventPrimaries(). ERROR:\t"
	   << "Mapping between the StTrackNodes and the StiKalmanTracks is empty.  Exit." << endl;
      return;
    }
  //Added residual maker...aar
  StPrimaryVertex* vertex = 0;
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo();
  cout << "StiStEventFiller::fillEventPrimaries() -I- Tracks in container:" << mTrackStore->size() << endl;
  Int_t mTrackN=0,mVertN=0;
  Int_t noPipe=0;
  Int_t ifcOK=0;
  Int_t fillTrackCount1=0;
  Int_t fillTrackCount2=0;
  Int_t fillTrackCountG=0;
  StErrorHelper errh;
  Int_t nTracks = mTrackStore->size();
  StiKalmanTrack *kTrack = 0;
  StPrimaryTrack *pTrack = 0;
  StGlobalTrack  *gTrack = 0;
  StTrackNode    *nTRack = 0;
  mTrackNumber=0;
  for (mTrackN=0; mTrackN<nTracks;++mTrackN) {
    kTrack = (StiKalmanTrack*)(*mTrackStore)[mTrackN];
    if (!accept(kTrack)) 			continue;
    map<StiKalmanTrack*, StTrackNode*>::iterator itKtrack = mTrkNodeMap.find(kTrack);
    if (itKtrack == mTrkNodeMap.end())  	continue;//Sti global was rejected
    mTrackNumber++;

    nTRack = (*itKtrack).second;
    assert(nTRack->entries()<=10);
    assert(nTRack->entries(global)); 

    //Double_t globalDca = nTRack->track(global)->impactParameter();
    //Even though this is filling of primary tracks, there are certain
    // quantities that need to be filled for global tracks that are only known
    // after the vertex is found, such as dca.  Here we can fill them.
    // 
    gTrack = static_cast<StGlobalTrack*>(nTRack->track(global));
    assert(gTrack->key()==kTrack->Id());
    Float_t minDca = 1e10; //We do not know which primary. Use the smallest one
    
    pTrack = 0;
    for (mVertN=0; (vertex = mEvent->primaryVertex(mVertN));mVertN++) {
      StThreeVectorD vertexPosition = vertex->position();
      Double_t zPrim = vertexPosition.z();
      // loop over StiKalmanTracks
      Float_t globalDca = impactParameter(gTrack,vertexPosition);
      if (fabs(minDca) > fabs(globalDca)) minDca = globalDca;
 
      if (!kTrack->IsPrimary())			continue;
      StiKalmanTrackNode *lastNode = kTrack->LastNode();
      StiHit *pHit = lastNode->Hit();
      if (fabs(pHit->z_g()-zPrim)>0.1)		continue;//not this primary

      fillTrackCount1++;
      // detector info
      StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
      fillDetectorInfo(detInfo,kTrack,false); //3d argument used to increase/not increase the refCount. MCBS oct 04.
      StPrimaryTrack* pTrack = new StPrimaryTrack;
      pTrack->setKey( gTrack->key());

      fillTrack(pTrack,kTrack, detInfo);
      // set up relationships between objects
      detInfoVec.push_back(detInfo);

      nTRack->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
      vertex->addDaughter(pTrack);
      fillTrackCount2++;
      Int_t ibad = pTrack->bad();
      errh.Add(ibad);
      if (ibad) {
//VP	        printf("PTrack error: %s\n",errh.Say(ibad).Data());
//VP	        throw runtime_error("StiStEventFiller::fillEventPrimaries() StTrack::bad() non zero");
      }
      if (pTrack->numberOfPossiblePoints()<10) 		break;
      if (pTrack->geometry()->momentum().mag()<0.1) 	break;
      fillTrackCountG++;
      break;
    } //end of verteces
      kTrack->setDca(minDca);
      gTrack->setImpactParameter(minDca);
      if (pTrack) pTrack->setImpactParameter(minDca);

  } // kalman track loop
  mTrkNodeMap.clear();  // need to Reset for the next event
  cout <<"StiStEventFiller::fillEventPrimaries() -I- Primaries (1):"<< fillTrackCount1<< " (2):"<< fillTrackCount2<< " no pipe node:"<<noPipe<<" with IFC:"<< ifcOK<<endl;
  cout <<"StiStEventFiller::fillEventPrimaries() -I- GOOD:"<< fillTrackCountG <<endl;
  errh.Print();
  return;
}
//_____________________________________________________________________________
/// use the vector of StHits to fill the detector info
/// change: currently point and fit points are the same for StiKalmanTracks,
/// if this gets modified later in ITTF, this must be changed here
/// but maybe use track->PointCount() later?
//_____________________________________________________________________________
void StiStEventFiller::fillDetectorInfo(StTrackDetectorInfo* detInfo, StiKalmanTrack* track, bool refCountIncr) 
{
  //cout << "StiStEventFiller::fillDetectorInfo() -I- Started"<<endl;
  Int_t dets[kMaxDetectorId][3];
  track->AllPointCount(dets,kMaxDetectorId-1);
  for (Int_t i=1;i<kMaxDetectorId;i++) {
    if (!dets[i][1]) continue;
    detInfo->setNumberOfPoints(dets[i][1],static_cast<StDetectorId>(i));
  }
  StiKTNIterator tNode = track->rbegin();
  StiKTNIterator eNode = track->rend();
  StiKalmanTrackNode *lastNode=0,*fistNode=0;
  for (;tNode!=eNode;++tNode) 
  {
      StiKalmanTrackNode *node = &(*tNode);
      if(!node->IsValid()) 	continue;

      StiHit *stiHit = node->Hit();
      if (!stiHit)		continue;

      if (node->Chi2()>1000) continue;
      if (!node->IsFitted()) 	continue;

      const StiDetector *detector = node->Detector();
      assert(detector == stiHit->Detector());
      assert(!detector || stiHit->timesUsed());
      if (!fistNode) fistNode = node;
      lastNode = node;
      StHit *hh = (StHit*)stiHit->stHit();
// 	Fill StHit errors for Gene
      FillStHitErr(hh,node);
      if (!detector) 		continue;
      if (!hh) 			continue;
      assert(detector->GroupId()==hh->detector());
      
      detInfo->addHit(hh,refCountIncr);
      if (!refCountIncr) 	continue;
      hh->setFitFlag(1);
  }
  assert(lastNode && fistNode && (lastNode != fistNode));

  StThreeVectorF posL(lastNode->x_g(),lastNode->y_g(),lastNode->z_g());
  detInfo->setLastPoint (posL);
  StThreeVectorF posF(fistNode->x_g(),fistNode->y_g(),fistNode->z_g());
  detInfo->setFirstPoint(posF);


  //cout << "StiStEventFiller::fillDetectorInfo() -I- Done"<<endl;
}
//_____________________________________________________________________________
void StiStEventFiller::fillGeometry(StTrack* gTrack, StiKalmanTrack* track, bool outer) {
  assert(gTrack);
  assert(track) ;

  StiKalmanTrackNode* node = track->InnOutMostNode(outer,3);
  StTrackGeometry* geometry = new StHelixModel(*((StHelixModel *) node->Helix()));
  if (outer) gTrack->setOuterGeometry(geometry);
  else       gTrack->setGeometry(geometry);
  return;
}
//_____________________________________________________________________________
void StiStEventFiller::fillFitTraits(StTrack* gTrack, StiKalmanTrack* track){
  // mass
  // this makes no sense right now... Double_t massHyp = track->getMass();  // change: perhaps this mass is not set right?
  UShort_t geantIdPidHyp = 9999;
  //if (.13< massHyp<.14) 
  geantIdPidHyp = 9;
  // chi square and covariance matrix, plus other stuff from the
  // innermost track node
  StiKalmanTrackNode* node = track->InnerMostHitNode(3);
  Float_t x[6],covMFloat[15];
  node->GlobalTpt(x,covMFloat);
  Float_t chi2[2];
  //get chi2/dof
  chi2[0] = track->Chi2();  
  chi2[1] = -999; // change: here goes an actual probability, need to calculate?
  // December 04: The second element of the array will now hold the incremental chi2 of adding
  // the vertex for primary tracks
  if (gTrack->type()==primary) {
    assert(node->Detector()==0);
    chi2[1]=node->Chi2();
  }
    
  // setFitTraits uses assignment operator of StTrackFitTraits, which is the default one,
  // which does a memberwise copy.  Therefore, constructing a local instance of 
  // StTrackFitTraits is fine, as it will get properly copied.
  StTrackFitTraits fitTraits(geantIdPidHyp,0,chi2,covMFloat);
  // Now we have to use the new setters that take a detector ID to fix
  // a bug.  There is no encoding anymore.

  Int_t dets[kMaxDetectorId][3]; 
  track->AllPointCount(dets,kMaxDetectorId-1);

  for (Int_t i=1;i<kMaxDetectorId;i++) {
    if (!dets[i][2]) continue;
    fitTraits.setNumberOfFitPoints((unsigned char)dets[i][2],(StDetectorId)i);
  }
  if (gTrack->type()==primary) {
     fitTraits.setPrimaryVertexUsedInFit(true);
  }
  gTrack->setFitTraits(fitTraits);
  return;
}

///_____________________________________________________________________________
/// data members from StEvent/StTrack.h
///  The track flag (mFlag accessed via flag() method) definitions with ITTF 
///(flag definition in EGR era can be found at  http://www.star.bnl.gov/STAR/html/all_l/html/dst_track_flags.html)
///
///  mFlag=zxyy, where  z = 1 for pile up track in TPC (otherwise 0) 
///                     x indicates the detectors included in the fit and 
///                    yy indicates the status of the fit. 
///  Positive mFlag values are good fits, negative values are bad fits. 
///
///  The first digit indicates which detectors were used in the refit: 
///
///      x=1 -> TPC only 
///      x=3 -> TPC       + primary vertex 
///      x=5 -> SVT + TPC 
///      x=6 -> SVT + TPC + primary vertex 
///      x=7 -> FTPC only 
///      x=8 -> FTPC      + primary 
///      x=9 -> TPC beam background tracks            
///
///  The last two digits indicate the status of the refit: 
///       = +x01 -> good track 
///
///       = -x01 -> Bad fit, outlier removal eliminated too many points 
///       = -x02 -> Bad fit, not enough points to fit 
///       = -x03 -> Bad fit, too many fit iterations 
///       = -x04 -> Bad Fit, too many outlier removal iterations 
///       = -x06 -> Bad fit, outlier could not be identified 
///       = -x10 -> Bad fit, not enough points to start 
///
///       = -x11 -> Short track pointing to EEMC

void StiStEventFiller::fillFlags(StTrack* gTrack) {
  if (gTrack->type()==global) {
    gTrack->setFlag(101); //change: make sure flag is ok
  }
  else if (gTrack->type()==primary) {
    gTrack->setFlag(301);
  }
  StTrackFitTraits& fitTrait = gTrack->fitTraits();
  //int tpcFitPoints = fitTrait.numberOfFitPoints(kTpcId);
  Int_t svtFitPoints = fitTrait.numberOfFitPoints(kSvtId);
  Int_t ssdFitPoints = fitTrait.numberOfFitPoints(kSsdId);
  Int_t pxlFitPoints = fitTrait.numberOfFitPoints(kPxlId);
  Int_t istFitPoints = fitTrait.numberOfFitPoints(kIstId);
  //  Int_t totFitPoints = fitTrait.numberOfFitPoints();
  /// In the flagging scheme, I will put in the cases for
  /// TPC only, and TPC+SVT (plus their respective cases with vertex)
  /// Ftpc case has their own code and SSD doesn't have a flag...

  // first case is default above, tpc only = 101 and tpc+vertex = 301
  // next case is:
  // if the track has svt points, it will be an svt+tpc track
  // (we assume that the ittf tracks start from tpc, so we don't
  // use the "svt only" case.)
  if (svtFitPoints+ssdFitPoints+pxlFitPoints+istFitPoints>0) {
      if (gTrack->type()==global) {
	  gTrack->setFlag(501); //svt+tpc
      }
      else if (gTrack->type()==primary) {
	  gTrack->setFlag(601); //svt+tpc+primary
      }
  }
  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
  if (dinfo) {
    Int_t NoTpcFitPoints = dinfo->numberOfPoints(kTpcId);
    Int_t NoFtpcWestId   = dinfo->numberOfPoints(kFtpcWestId);
    Int_t NoFtpcEastId   = dinfo->numberOfPoints(kFtpcEastId);
    // Check that it could be TPC pile-up track, i.e. in the same half TPC (West East) 
    // there are more than 2 hits with wrong Z -position
    Int_t flag = TMath::Abs(gTrack->flag());
    if (NoTpcFitPoints >= 11) {
      const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
      const StPtrVecHit& hits = dinfo->hits(kTpcId);
      Int_t Nhits = hits.size();
      Int_t NoWrongSignZ = 0;
      for (Int_t i = 0; i < Nhits; i++) {
	const StTpcHit *hit = (StTpcHit *) hits[i];
	if ((hit->position().z() < -1.0 && hit->sector() <= 12) ||
	    (hit->position().z() >  1.0 && hit->sector() >  12)) NoWrongSignZ++;
      }
      if (NoWrongSignZ >= 2) 
	gTrack->setFlag((flag%1000) + 1000); // +1000
    }
    if (NoTpcFitPoints < 11 && NoFtpcWestId < 5 && NoFtpcEastId < 5) { 
      // hadrcoded number correspondant to  __MIN_HITS_TPC__ 11 in StMuFilter.cxx
      //keep most sig. digit, set last digit to 2, and set negative sign
      gTrack->setFlag(-(((flag/100)*100)+2)); // -x02 
      if (gTrack->geometry()) {
	const StThreeVectorF &momentum = gTrack->geometry()->momentum();
	if (momentum.pseudoRapidity() > 0.5) {
	  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
	  const StPtrVecHit& hits = dinfo->hits();
	  Int_t Nhits = hits.size();
	  for (Int_t i = 0; i < Nhits; i++) {
	    const StHit *hit = hits[i];
	    if (hit->position().z() > 150.0) {
	      gTrack->setFlag((((flag/100)*100)+11)); // +x11 
	      return;
	    }
	  }
	}
      }
    }
  }
}
//_____________________________________________________________________________
void StiStEventFiller::fillTrack(StTrack* gTrack, StiKalmanTrack* track,StTrackDetectorInfo* detInfo )
{

  //cout << "StiStEventFiller::fillTrack()" << endl;
  // encoded method = 16 bits = 12 fitting and 4 finding, for the moment use:
  // kKalmanFitId
  // bit 15 for finding, (needs to be changed in StEvent).
  // change: make sure bits are ok, are the bits set up one in each position and nothing else?
  // this would mean that the encoded method is wasting space!
  // the problem is that in principle there might be combinations of finders for each tracking detector
  // but the integrated tracker will use only one for all detectors maybe
  // so need this bit pattern:
  // finding 100000000000     
  // fitting             0010 
  //            32768    +    2 = 32770;
  //
  // above is no longer used, instead use kITKalmanfitId as fitter and tpcOther as finding method

  gTrack->setEncodedMethod(mStiEncoded);
  Double_t tlen = track->TrackLength();
  assert(tlen >0.0 && tlen<1000.);
  gTrack->setLength(tlen);// someone removed this, grrrr!!!!
 
  // Follow the StDetectorId.h enumerations...
  // can't include them from here in order not to
  // create a package dependence...
  Int_t dets[kMaxDetectorId][3];
  track->AllPointCount(dets,kMaxDetectorId-1);
  for (Int_t i=1;i<kMaxDetectorId;i++) {
    if(!dets[i][0]) continue;
    gTrack->setNumberOfPossiblePoints((unsigned char)dets[i][0],(StDetectorId)i);
  }

  fillGeometry(gTrack, track, false); // inner geometry
  fillGeometry(gTrack, track, true ); // outer geometry
  fillFitTraits(gTrack, track);
  gTrack->setDetectorInfo(detInfo);
  StuFixTopoMap(gTrack);
  fillFlags(gTrack);
  if (!track->IsPrimary()) fillDca(gTrack,track);
  return;
}
//_____________________________________________________________________________
bool StiStEventFiller::accept(StiKalmanTrack* track)
{
//  Int_t nPossiblePoints = track->MaxPointCount(0);
//  Int_t nMeasuredPoints = track->PointCount   (0);
    Int_t nFittedPoints   = track->FitPointCount(0);
    if (nFittedPoints  <  5 )					return 0;
    if(track->TrackLength()<=0) 				return 0; 
    // insert other filters for riff-raff we don't want in StEvent here.
    

    return 1;
}
//_____________________________________________________________________________
Double_t StiStEventFiller::impactParameter(StiKalmanTrack* track, StThreeVectorD &vertexPosition) {
  return   track->InnerMostNode(2)->Helix()->helix().distance(vertexPosition);
}
//_____________________________________________________________________________
Double_t StiStEventFiller::impactParameter(StTrack* track, StThreeVectorD &vertex) {
  return track->geometry()->helix().distance(vertex);
}
//_____________________________________________________________________________
void StiStEventFiller::fillDca(StTrack* stTrack, StiKalmanTrack* track)
{
  StGlobalTrack *gTrack = dynamic_cast<StGlobalTrack*>(stTrack);
  assert(gTrack);

  StiKalmanTrackNode *tNode = track->InnerMostNode();
  if (!tNode->IsDca()) return;
  const TRVector &pars = tNode->Fitted().P(); 
  const TRSymMatrix &errs = tNode->Fitted().C();
  Float_t setp[7],sete[15];
  TCL::ucopy(pars.GetArray(),setp,7);
  const Double_t *A = errs.GetArray();
  for (Int_t i=1,li=1,jj=0;i< 6;li+=++i) {
    for (Int_t j=1;j<=i;j++) {sete[jj++] = A[li+j];}}
  StDcaGeometry *dca = new StDcaGeometry;
  gTrack->setDcaGeometry(dca);
  dca->set(setp,sete);

}
//_____________________________________________________________________________
void StiStEventFiller::FillStHitErr(StHit *hh,const StiKalmanTrackNode *node) {
#if 0
  Double_t stiErr[6];
  TCL::ucopy(node->V().GetArray(),stiErr,6);
#else
  TRSymMatrix V(node->V());
  Double_t stiErr[6] = {
    0., 
    0., V(0,0),
    0., V(1,0), V(1,1)
  };
#endif
  TRMatrix  R(3,3,node->Detector()->GetMatrix()->GetRotationMatrix());
  TRSymMatrix StiErr(3,stiErr);
  TRSymMatrix StErr(R,TRArray::kAxSxAT,StiErr);
  StThreeVectorF f3(TMath::Sqrt(StErr(0,0)),TMath::Sqrt(StErr(1,1)),TMath::Sqrt(StErr(2,2)));
  hh->setPositionError(f3);
}
