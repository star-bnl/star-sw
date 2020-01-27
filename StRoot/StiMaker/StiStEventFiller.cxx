/***************************************************************************
 *
 * $Id: StiStEventFiller.cxx,v 2.124 2020/01/27 21:27:45 genevb Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Mar 2002
 ***************************************************************************
 *
 * $Log: StiStEventFiller.cxx,v $
 * Revision 2.124  2020/01/27 21:27:45  genevb
 * Add short tracks toward ETOF when there, remove toward EEMC when not there
 *
 * Revision 2.123  2018/06/29 21:46:33  smirnovd
 * Revert iTPC-related changes committed on 2018-06-20 through 2018-06-28
 *
 * Revert "NoDead option added"
 * Revert "Fill mag field more carefully"
 * Revert "Assert commented out"
 * Revert "Merging with TPC group code"
 * Revert "Remove too strong assert"
 * Revert "Restore removed by mistake line"
 * Revert "Remove not used anymore file"
 * Revert "iTPCheckIn"
 *
 * Revision 2.121  2018/04/30 23:18:11  smirnovd
 * [Cosmetic] Minor changes in various files
 *
 * - Renamed data member s/m/mMass/ in StikalmanTrack
 * - Changes in white space
 * - Return STAR code
 *
 * Revision 2.120  2018/04/10 11:32:10  smirnovd
 * Minor corrections across multiple files
 *
 * - Remove ClassImp macro
 * - Change white space
 * - Correct windows newlines to unix
 * - Remove unused debugging
 * - Correct StTpcRTSHitMaker header guard
 * - Remove unused preprocessor directives in StiCA
 * - Minor changes in status and debug print out
 * - Remove using std namespace from StiKalmanTrackFinder
 * - Remove includes for unused headers
 *
 * Revision 2.119  2016/06/30 18:33:48  perev
 * simplifacation
 *
 * Revision 2.117.2.4  2016/06/29 20:09:06  perev
 * Small simplificatoins
 *
 * Revision 2.117.2.3  2016/06/03 16:07:15  smirnovd
 * Sync with MAIN branch as of 2016-05-31
 *
 * Revision 2.118  2016/04/13 23:09:13  perev
 * -opt2 proble solved. Array A[1] removed
 *
 * Revision 2.117  2015/12/28 23:50:27  perev
 * Remove assert temporary
 *
 * Revision 2.116  2015/12/21 19:41:31  perev
 * bug #3166 assert vertex closer to 0,0 <9 removed
 *
 * Revision 2.115  2015/12/20 01:46:56  fisyak
 * Move back commits done by mistake
 *
 * Revision 2.113  2015/12/19 03:40:50  perev
 * assert rxy<4 ==> <9 temporary
 *
 * Revision 2.112  2015/12/18 03:50:06  perev
 * *** empty log message ***
 *
 * Revision 2.111  2015/12/03 19:12:24  perev
 * Remove redundant GTrack error: mFlag: is Negative
 *
 * Revision 2.110  2015/03/27 20:13:43  perev
 * Add printout of good track hits
 *
 * Revision 2.109  2015/03/24 16:37:28  perev
 * fix printout hit: to hits:
 *
 * Revision 2.108  2015/03/21 02:16:53  perev
 * By Lidia request, addet printing number of used hits detector by detector
 * No any modification of any algorithmes
 *
 * Revision 2.107  2015/01/23 20:07:08  perev
 * Debug++
 *
 * Revision 2.106  2014/07/03 00:37:51  perev
 * c++11 fix
 *
 * Revision 2.105  2013/04/10 22:14:20  fisyak
 * Roll back to version 04/04/2013
 *
 * Revision 2.103  2013/01/28 21:51:17  fisyak
 * Correct ranking
 *
 * Revision 2.102  2013/01/18 15:03:37  fisyak
 * Fix TrackData data name clash with StiPPVertexFinder
 *
 * Revision 2.101  2013/01/17 15:57:26  fisyak
 * Add handles for debugging
 *
 * Revision 2.100  2012/11/09 18:28:10  perev
 * fillpull development
 *
 * Revision 2.99  2012/09/16 21:38:42  fisyak
 * use of Tpc West Only and East Only tracks, clean up
 *
 * Revision 2.98  2012/05/07 14:56:14  fisyak
 * Add StKFVertexMaker
 *
 * Revision 2.97  2011/10/17 00:14:34  fisyak
 * Move handles for IdTruth to StEvent
 *
 * Revision 2.96  2011/07/19 19:07:20  perev
 * Remove previous tracks & vertices in StEvrent added
 *
 * Revision 2.95  2011/05/27 18:25:33  genevb
 * Propagate StTrack::key => Int_t to other codes
 *
 * Revision 2.94  2011/04/01 15:52:21  fisyak
 * Enlarge array for possible candidates, add requirement that dominant track should have  > 2 good hits
 *
 * Revision 2.93  2011/03/31 22:11:24  fisyak
 * Propagate IdTruth to StEvent
 *
 * Revision 2.92  2011/01/26 20:11:54  perev
 * track id into StiPull
 *
 * Revision 2.91  2010/09/01 21:26:15  fisyak
 * switch from direct access to public members to methods
 *
 * Revision 2.90  2010/01/27 21:43:49  perev
 * Add _nPrimTracks for case of fiterr
 *
 * Revision 2.89  2009/10/18 22:47:29  perev
 * assert instead of skip
 *
 * Revision 2.88  2009/10/16 14:56:02  fisyak
 * Add check that pHit exists
 *
 * Revision 2.87  2009/10/15 03:29:30  perev
 * Add primary vertex number and charge(GVB)
 *
 * Revision 2.86  2009/08/19 21:27:57  perev
 *  Account time of flight for StiPulls
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
 * StTrack::setKey(...StiTrack::getId()) now
 *
 * Revision 2.53  2005/02/17 23:19:03  perev
 * NormalRefangle + Error reseting
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
 * It holds node->getChi2() from the innerMostHitNode, which will be the
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
 * Modifed call to getMaxPointCount
 *
 * Revision 2.35  2004/08/05 05:25:25  calderon
 * Fix the assignment of the first point for primaries.  Now,
 * the logic for both globals and primaries is that the first
 * point is the first element of the stHits() vector that
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
 * -Points is done as it was before, just counting the stHits for a given
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
 * track->getChi2() returns the sum of chi2 for all sti nodes.  In StEvent,
 * chi2(0) should be chi2/dof, so we need to divide by
 * dof=track->getPointCount()-5;
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
 * fillGeometry() - use node->getX(), as it should have been since the
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
 * switch back to getting the chi2 from track->getChi2()
 * Covariance matrix is still obtained from node->get(), and the values
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
 * -length is set using getTrackLength, which might still need tweaking
 * -possible points is currently set from getMaxPointCount which returns the
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
#include "Sti/StiTrackContainer.h"
#include "Sti/StiKalmanTrack.h"
#include "StDetectorDbMaker/StiKalmanTrackFitterParameters.h"
/////#include "Sti/StiGeometryTransform.h"
#include "StiUtilities/StiDebug.h"
#include "StiUtilities/StiPullEvent.h"

//StiMaker
#include "StiMaker/StiStEventFiller.h"
#include "TMath.h"
#include "StTrack2FastDetectorMatcher.h"
#include "Sti/StiHitTest.h"
#define NICE(angle) StiKalmanTrackNode::nice((angle))
map<StiKalmanTrack*, StTrackNode*> StiStEventFiller::mTrkNodeMap;
map<StTrackNode*, StiKalmanTrack*> StiStEventFiller::mNodeTrkMap;
StiStEventFiller *StiStEventFiller::fgStiStEventFiller = 0;

//_____________________________________________________________________________
StiStEventFiller::StiStEventFiller() : mEvent(0), mTrackStore(0), mFastDetectorMatcher(0)
{
  fgStiStEventFiller = this;
   mUseAux = 0;
   mAux    = 0;
   mGloPri = 0;
   mPullEvent=0;
  
  originD = new StThreeVectorD(0,0,0);
  physicalHelix = new StPhysicalHelixD(0.,0.,0.,*originD,-1);
 

  //mResMaker.setLimits(-1.5,1.5,-1.5,1.5,-10,10,-10,10);
  //mResMaker.setDetector(kSvtId);

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
  unsigned short bit = 1 << tpcOther;  // shifting the "1" exactly tpcOther places to the left
  mStiEncoded = kITKalmanFitId + bit; // adding that to the proper fitting Id
  mFastDetectorMatcher = new StTrack2FastDetectorMatcher();
}
//_____________________________________________________________________________
StiStEventFiller::~StiStEventFiller()
{
 delete physicalHelix; physicalHelix=0;
 delete originD;       originD      =0;
 SafeDelete(mFastDetectorMatcher);
   cout <<"StiStEventFiller::~StiStEventFiller()"<<endl;
}
//_____________________________________________________________________________
/*! 
  Algorithm:
  Loop over all tracks in the StiTrackContainer, doing for each track:
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
     gTrack->setDetectorInfo(detInfo);\n
     gTrack->setNode(trackNode);\n
     trackNode->AddTrack(gTrack);\n
  }\n
  </code>
  The creation of the various objects needed by StGlobalTrack are taken care of in the methods:
  fillTopologyMap(), fillGeometry(), fillFitTraits(), which are called within fillGlobalTrack().
  
*/
//_____________________________________________________________________________
void StiStEventFiller::fillEvent(StEvent* e, StiTrackContainer* t)
{
  mFastDetectorMatcher->Clear();
  mFastDetectorMatcher->fillArrays(e);
  //cout << "StiStEventFiller::fillEvent() -I- Started"<<endl;
  mGloPri=0;
  if (e==0 || t==0) 
    {
      cout <<"StiStEventFiller::fillEvent(). ERROR:\t"
	   <<"Null StEvent ("<<e<<") || StiTrackContainer ("<<t<<").  Exit"<<endl;
      return;
    }
  mEvent = e;
  StEventHelper::Remove(mEvent,"StSPtrVecTrackNode");
  StEventHelper::Remove(mEvent,"StSPtrVecPrimaryVertex");

  if (mUseAux) { mAux = new StiAux; e->Add(mAux);}
  mTrackStore = t;
  memset(mUsedHits,0,sizeof(mUsedHits));
  memset(mUsedGits,0,sizeof(mUsedGits));
  mTrkNodeMap.clear();  // need to reset for this event
  mNodeTrkMap.clear();
  StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); 
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); 
  int errorCount=0; 

  int fillTrackCount1=0;
  int fillTrackCount2=0;
  int fillTrackCountG=0;
  StErrorHelper errh;
  mTrackNumber=0;
  for (int trackIt = 0;trackIt <(int)mTrackStore->size(); trackIt++) 
    {
      StiKalmanTrack* kTrack = static_cast<StiKalmanTrack*>((*mTrackStore)[trackIt]);
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
	  //cout <<"Setting key: "<<(unsigned short)(trNodeVec.size())<<endl;
	  gTrack->setKey(kTrack->getId());
          gTrack->setIdTruth();
	  trackNode->addTrack(gTrack);
	  trNodeVec.push_back(trackNode);
	  // reuse the utility to fill the topology map
	  // this has to be done at the end as it relies on
	  // having the proper track->detectorInfo() relationship
	  // and a valid StDetectorInfo object.
	  //cout<<"Tester: Event Track Node Entries: "<<trackNode->entries()<<endl;
	  StTrackNode* node = trNodeVec.back();
	  mTrkNodeMap.insert(pair<StiKalmanTrack*,StTrackNode*> (kTrack,node) );
	  mNodeTrkMap.insert(pair<StTrackNode*,StiKalmanTrack*> (node,kTrack) );
	  if (trackNode->entries(global)<1)
	    cout << "StiStEventFiller::fillEvent() -E- Track Node has no entries!! -------------------------" << endl;  
          int ibad = gTrack->bad();
          if (ibad) {
	  errh.Add(ibad);
            if (errh.Say(ibad).Contains("Negative")) continue;
	    printf("GTrack error: %s\n",errh.Say(ibad).Data());
            continue;
          }
	  fillTrackCount2++;
if (kTrack->getPointCount(kTpcId)>10)
StiHftHits::hftHist("HFTAfterAll",kTrack);//???????????????????????
          fillPulls(kTrack,gTrack,0);
          if (kTrack->getPointCount()<15) continue;
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
  for (int ij=1; ij<=mUsedHits[0]; ij++) {
    if (!mUsedHits[ij]) continue;
    const char *det =  detectorNameById((StDetectorId)ij);
    cout <<"StiStEventFiller::fillEvent() -I- Number of used hits:"<< det << "(" << ij << ") :"<<mUsedHits[ij]
         << " per track:"<<double(mUsedHits[ij])/fillTrackCount2 <<endl;
  }  
  for (int ij=1; ij<=mUsedGits[0]; ij++) {
    if (!mUsedGits[ij]) continue;
    const char *det =  detectorNameById((StDetectorId)ij);
    cout <<"StiStEventFiller::fillEvent() -I- Number of GOOD hits:"<< det << "(" << ij << ") :"<<mUsedGits[ij]
         << " per track:"<<double(mUsedHits[ij])/fillTrackCountG <<endl;
  }  


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
  int mTrackN=0,mVertN=0;
  int fillTrackCount1=0;
  int fillTrackCount2=0;
  int fillTrackCountG=0;
  StErrorHelper errh;
  int nTracks = mTrackStore->size();
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

    //double globalDca = nTRack->track(global)->impactParameter();
    //Even though this is filling of primary tracks, there are certain
    // quantities that need to be filled for global tracks that are only known
    // after the vertex is found, such as dca.  Here we can fill them.
    // 
    gTrack = static_cast<StGlobalTrack*>(nTRack->track(global));
    assert(gTrack->key()==kTrack->getId());
    float minDca = 1e10; //We do not know which primary. Use the smallest one
    
    pTrack = 0;
    for (mVertN=0; (vertex = mEvent->primaryVertex(mVertN));mVertN++) {
      StThreeVectorD vertexPosition = vertex->position();
      double zPrim = vertexPosition.z();
      // loop over StiKalmanTracks
      float globalDca = impactParameter(gTrack,vertexPosition);
      if (fabs(minDca) > fabs(globalDca)) minDca = globalDca;
 
      if (!kTrack->isPrimary())			continue;
      StiKalmanTrackNode *lastNode = kTrack->getLastNode();
      StiHit *pHit = lastNode->getHit();
      assert (pHit);
      if (fabs(pHit->z_g()-zPrim)>0.1)		continue;//not this primary

      fillTrackCount1++;
      // detector info
      StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
      fillDetectorInfo(detInfo,kTrack,false); //3d argument used to increase/not increase the refCount. MCBS oct 04.
//      double rxy = detInfo->firstPoint().perp();
//      assert(rxy < 9);
      auto myDif = (detInfo->firstPoint()-vertexPosition);
//??      assert(myDif.mag()<0.01);

      StPrimaryTrack* pTrack = new StPrimaryTrack;
      pTrack->setKey( gTrack->key());
      nTRack->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
      fillTrack(pTrack,kTrack, detInfo);
      // set up relationships between objects
      detInfoVec.push_back(detInfo);

      vertex->addDaughter(pTrack);
      fillPulls(kTrack,gTrack,1); 
      int ibad = pTrack->bad();
      errh.Add(ibad);
      if (ibad) {
//VP	        printf("PTrack error: %s\n",errh.Say(ibad).Data());
//VP	        throw runtime_error("StiStEventFiller::fillEventPrimaries() StTrack::bad() non zero");
      continue;
      }
//      rxy = pTrack->geometry()->origin().perp();
//      assert(rxy<9);
      fillTrackCount2++;
      if (kTrack->getPointCount()<15) 		break;
      if (pTrack->geometry()->momentum().mag()<0.1) 	break;
      fillTrackCountG++;
      break;
    } //end of verteces
      kTrack->setDca(minDca);
      gTrack->setImpactParameter(minDca);
      if (pTrack) pTrack->setImpactParameter(minDca);

  } // kalman track loop
  for (mVertN=0; (vertex = mEvent->primaryVertex(mVertN));mVertN++) {vertex->setTrackNumbers();}

  mTrkNodeMap.clear();  // need to reset for the next event
  cout <<"StiStEventFiller::fillEventPrimaries() -I- Primaries (1):"<< fillTrackCount1 <<endl;
  cout <<"StiStEventFiller::fillEventPrimaries() -I- Primaries (2):"<< fillTrackCount2 <<endl;
  cout <<"StiStEventFiller::fillEventPrimaries() -I- GOOD:"<< fillTrackCountG <<endl;
  errh.Print();
  return;
}
//_____________________________________________________________________________
/// use the vector of StHits to fill the detector info
/// change: currently point and fit points are the same for StiKalmanTracks,
/// if this gets modified later in ITTF, this must be changed here
/// but maybe use track->getPointCount() later?
//_____________________________________________________________________________
void StiStEventFiller::fillDetectorInfo(StTrackDetectorInfo* detInfo, StiKalmanTrack* track, bool refCountIncr) 
{
  //cout << "StiStEventFiller::fillDetectorInfo() -I- Started"<<endl;
  int dets[kMaxDetectorId][3];
  track->getAllPointCount(dets,kMaxDetectorId-1);
  int nTotHits = dets[0][2];
  int nTpcHits = dets[kTpcId][2];
  for (int i=1;i<kMaxDetectorId;i++) {
    if (!dets[i][1]) continue;
    detInfo->setNumberOfPoints(dets[i][1],static_cast<StDetectorId>(i));
  }
  StiKTNIterator tNode = track->rbegin();
  StiKTNIterator eNode = track->rend();
  StiKalmanTrackNode *lastNode=0,*fistNode=0;
  for (;tNode!=eNode;++tNode) 
  {
      StiKalmanTrackNode *node = &(*tNode);
      if(!node->isValid()) 	continue;

      StiHit *stiHit = node->getHit();
      if (!stiHit)		continue;

      if (node->getChi2()>1000) continue;
      if (!node->isFitted()) 	continue;

      const StiDetector *detector = node->getDetector();
      assert(detector == stiHit->detector());
      assert(!detector || stiHit->timesUsed());
      if (!fistNode) fistNode = node;
      lastNode = node;
      if (!detector) 		continue;

//		Count used hits for tracks tpc hits >10
      if (nTpcHits > 10) {
	int gid = detector->getGroupId();
	if (mUsedHits[0]<gid) mUsedHits[0]=gid;
	mUsedHits[gid]++;
	if (nTotHits>=15) {
          if (mUsedGits[0]<gid) mUsedGits[0]=gid;
          mUsedGits[gid]++;
        }
      }
      StHit *hh = (StHit*)stiHit->stHit();
      if (!hh) 			continue;
      assert(detector->getGroupId()==hh->detector());
#if 0
// 	Fill StHit errors for Gene
      FillStHitErr(hh,node);
#endif      
      detInfo->addHit(hh,refCountIncr);
      if (!refCountIncr) 	continue;
      hh->setFitFlag(1);
//Kind of HACK, save residials into StiHack 
      fillResHack(hh,stiHit,node);
  }
  assert(lastNode && fistNode && (lastNode != fistNode));

  StThreeVectorF posL(lastNode->x_g(),lastNode->y_g(),lastNode->z_g());
  detInfo->setLastPoint (posL);
  StThreeVectorF posF(fistNode->x_g(),fistNode->y_g(),fistNode->z_g());
  detInfo->setFirstPoint(posF);


  //cout << "StiStEventFiller::fillDetectorInfo() -I- Done"<<endl;
}

//_____________________________________________________________________________
void StiStEventFiller::fillGeometry(StTrack* gTrack, StiKalmanTrack* track, bool outer)
{
  //cout << "StiStEventFiller::fillGeometry() -I- Started"<<endl;
  assert(gTrack);
  assert(track) ;

  StiKalmanTrackNode* node = track->getInnOutMostNode(outer,3);
  StiHit *ihit = node->getHit();
  StThreeVectorF origin(node->x_g(),node->y_g(),node->z_g());
  StThreeVectorF hitpos(ihit->x_g(),ihit->y_g(),ihit->z_g());
  if (node->getDetector()) {
    double dif = (hitpos-origin).mag();
    
    if (dif>3.) {
      dif = node->z_g()-ihit->z_g();
      double nowChi2 = node->evaluateChi2(ihit);
      printf("***Track(%d) DIFF TOO BIG %g chi2 = %g %g\n",track->getId(),dif,node->getChi2(),nowChi2);
      printf("H=%g %g %g N =%g %g %g\n",ihit->x()   ,ihit->y()   ,ihit->z()
	     ,node->getX(),node->getY(),node->getZ());
      const StMeasuredPoint *mp = ihit->stHit();
      printf("H=%g %g %g N =%g %g %g\n",mp->position().x(),mp->position().y(),mp->position().z()
	     ,origin.x(),origin.y(),origin.z());
      
      assert(fabs(dif)<50.);
    }
  }
    // making some checks.  Seems the curvature is infinity sometimes and
  // the origin is sometimes filled with nan's...
  
  int ibad = origin.bad();
  if (ibad) {
      cout << "StiStEventFiller::fillGeometry() Encountered non-finite numbers!!!! Bail out completely!!! " << endl;
      cout << "StThreeVectorF::bad() = " << ibad << endl;
      cout << "Last node had:" << endl;
      cout << "Ref Position  " << node->getRefPosition() << endl;
      cout << "node->getY()  " << node->getY() << endl;
      cout << "node->getZ()  " << node->getZ() << endl;
      cout << "Ref Angle     " << node->getAlpha() << endl;
      cout << "origin        " << origin << endl;
      cout << "curvature     " << node->getCurvature() << endl;
      abort();
  }
  StTrackGeometry* geometry =new StHelixModel(short(track->getCharge()),
					      node->getPsi(),
					      fabs(node->getCurvature()),
					      node->getDipAngle(),
					      origin, 
					      node->getGlobalMomentumF(), 
					      node->getHelicity());

  if (outer)
    gTrack->setOuterGeometry(geometry);
  else
    gTrack->setGeometry(geometry);


  return;
}

//_____________________________________________________________________________
// void StiStEventFiller::fillTopologyMap(StTrack* gTrack, StiKalmanTrack* track){
// 	cout << "StiStEventFiller::fillTopologyMap()" << endl;
//     int map1,map2;
//     map1 = map2 = 0;
//     // change: add code to set the bits appropriately here

//     StTrackTopologyMap topomap(map1,map2);
//     gTrack->setTopologyMap(topomap);
//     return;
// }

//_____________________________________________________________________________
void StiStEventFiller::fillFitTraits(StTrack* gTrack, StiKalmanTrack* track){
  // mass
  // this makes no sense right now... double massHyp = track->getMass();  // change: perhaps this mass is not set right?
  unsigned short geantIdPidHyp = 9999;
  //if (.13< massHyp<.14) 
  geantIdPidHyp = 9;
  // chi square and covariance matrix, plus other stuff from the
  // innermost track node
  StiKalmanTrackNode* node = track->getInnerMostHitNode(3);
  float x[6],covMFloat[15];
  node->getGlobalTpt(x,covMFloat);
  float chi2[2];
  //get chi2/dof
  chi2[0] = track->getChi2();  
  assert(chi2[0]<100);
  chi2[1] = -999; // change: here goes an actual probability, need to calculate?
  // December 04: The second element of the array will now hold the incremental chi2 of adding
  // the vertex for primary tracks
  if (gTrack->type()==primary) {
    assert(node->getDetector()==0);
    chi2[1]=node->getChi2();
  }
    
  // setFitTraits uses assignment operator of StTrackFitTraits, which is the default one,
  // which does a memberwise copy.  Therefore, constructing a local instance of 
  // StTrackFitTraits is fine, as it will get properly copied.
  StTrackFitTraits fitTraits(geantIdPidHyp,0,chi2,covMFloat);
  // Now we have to use the new setters that take a detector ID to fix
  // a bug.  There is no encoding anymore.

  int dets[kMaxDetectorId][3]; 
  track->getAllPointCount(dets,kMaxDetectorId-1);

  for (int i=1;i<kMaxDetectorId;i++) {
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
///       = -x12 -> Short track pointing to ETOF

void StiStEventFiller::fillFlags(StTrack* gTrack) 
{
  Int_t flag = 0;
  if (gTrack->type()==global) {
    flag = 101; //change: make sure flag is ok
  }
  else if (gTrack->type()==primary) {
    flag = 301;
  }
  StTrackFitTraits& fitTrait = gTrack->fitTraits();
  //int tpcFitPoints = fitTrait.numberOfFitPoints(kTpcId);
  int svtFitPoints = fitTrait.numberOfFitPoints(kSvtId);
  int ssdFitPoints = fitTrait.numberOfFitPoints(kSsdId);
  int pxlFitPoints = fitTrait.numberOfFitPoints(kPxlId);
  int istFitPoints = fitTrait.numberOfFitPoints(kIstId);
  //  int totFitPoints = fitTrait.numberOfFitPoints();
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
	flag = 501; //svt+tpc
      }
      else if (gTrack->type()==primary) {
	flag = 601;  //svt+tpc+primary
      }
  }
  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
  if (dinfo) {
    Int_t NoTpcFitPoints = dinfo->numberOfPoints(kTpcId);
    Int_t NoFtpcWestId   = dinfo->numberOfPoints(kFtpcWestId);
    Int_t NoFtpcEastId   = dinfo->numberOfPoints(kFtpcEastId);
    // Check that it could be TPC pile-up track, i.e. in the same half TPC (West East) 
    // there are more than 2 hits with wrong Z -position
    if (NoTpcFitPoints >= 11) {
      const StPtrVecHit& hits = dinfo->hits(kTpcId);
      Int_t Nhits = hits.size();
      Int_t NoWrongSignZ = 0;
      Int_t NoPositiveSignZ = 0;
      Int_t NoNegativeSignZ = 0;
      Int_t NoPromptHits = 0;
      Double_t zE = -200, zW = 200;
      Int_t    rE = 0, rW = 0;
      Int_t   nW = 0, nE = 0;
      for (Int_t i = 0; i < Nhits; i++) {
	const StTpcHit *hit = (StTpcHit *) hits[i];
	Double_t z = hit->position().z();
	Int_t sector = hit->sector();
	if (sector <= 12) nW++;
	else              nE++;
	Int_t row    = hit->padrow();
	if ((z < -1.0 && sector <= 12) ||
	    (z >  1.0 && sector >  12)) NoWrongSignZ++;
	else {
	  if (z < -1.0) {NoNegativeSignZ++; if (z > zE) {zE = z; rE = row;}}
	  if (z >  1.0) {NoPositiveSignZ++; if (z < zW) {zW = z; rW = row;}}
	}
	if (TMath::Abs(209.4 - TMath::Abs(z)) < 3.0) NoPromptHits++;
      }
      if (NoWrongSignZ >= 2)                             gTrack->setPostCrossingTrack();
      else {
	if (NoPromptHits == 1)                           gTrack->setPromptTrack();
	if (NoPositiveSignZ >= 2 && NoNegativeSignZ >=2) {
	  if (zW - zE < 10 ||
	      TMath::Abs(rW - rE) < 3) 
	    gTrack->setMembraneCrossingTrack();
	}
      }
      if (nW >  0 && nE == 0) gTrack->setWestTpcOnly();
      if (nW == 0 && nE >  0) gTrack->setEastTpcOnly();
    }
    if (NoTpcFitPoints < 11 && NoFtpcWestId < 5 && NoFtpcEastId < 5) { 
      // hardcoded number correspondant to  __MIN_HITS_TPC__ 11 in StMuFilter.cxx
      //keep most sig. digit, set last digit to 2, and set negative sign
      gTrack->setRejected();
      flag = - ((flag/100)*100 + 2); // -x02 

      // Deciding which short tracks to keep based on event time.
      // Hardcoded times are not optimal, and will need revisiting
      // when EEMC is turned back on after BES-II, eTOF stays or goes?
      int evtTime = mEvent->time();
      bool doShort2EMC = (evtTime < 1538352000 || evtTime > 1633046400); // t < 2018-10-01 or t > 2021-10-01
      bool doShort2ETOF = (evtTime > 1525910400 && evtTime < 1633046400); // 2018-05-10 < t < 2021-10-01

      if ((doShort2EMC || doShort2ETOF) && gTrack->geometry()) {
	const StThreeVectorF &momentum = gTrack->geometry()->momentum();
	const float eta = momentum.pseudoRapidity();
	if (TMath::Abs(eta) > 0.5) {
	  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
	  const StPtrVecHit& hits = dinfo->hits();
	  Int_t Nhits = hits.size();
	  Bool_t ShortTrack2EMC = kFALSE;
	  Bool_t ShortTrack2ETOF = kFALSE;
	  for (Int_t i = 0; i < Nhits; i++) {
	    const StHit *hit = hits[i];
	    if (doShort2EMC && eta > 0.5 && hit->position().z() > 150.0) {
	      ShortTrack2EMC = kTRUE;
	      break;
	    }
	    if (doShort2ETOF && eta < -0.5 && hit->position().z() < -150.0) {
	      ShortTrack2ETOF = kTRUE;
	      break;
	    }
	  }
	  if (ShortTrack2EMC) {
	    gTrack->setShortTrack2EMC();
	    flag = (TMath::Abs(flag)/100)*100+11; // +x11 
	  } else if (ShortTrack2ETOF) {
	    gTrack->setShortTrack2ETOF();
	    flag = (TMath::Abs(flag)/100)*100+12; // +x12 
	  }
	}
      }
    }
  }
  
  gTrack->setFlag( flag);
  if (gTrack->type()==global) {
    // Match with fast detectors
    StPhysicalHelixD hlx = gTrack->outerGeometry()->helix();
    StiTrack2FastDetector t;
    mFastDetectorMatcher->matchTrack2FastDetectors(&hlx,&t);
    if (t.btofBin > 0) {
      if (t.mBtof > 0) gTrack->setToFMatched();
      else             gTrack->setToFNotMatched();
    }
    if (t.ctbBin > 0) {
      if (t.mCtb  > 0) gTrack->setCtbMatched();
      else             gTrack->setCtbNotMatched();
    }
    if (t.bemcBin > 0 || t.eemcBin > 0) {
      Int_t W = 0;
      if (t.bemcBin > 0) {
	W = StBemcHitList::instance()->getFired(t.bemcBin);
	if (W > 0) gTrack->setBemcMatched();
	else      gTrack->setBemcNotMatched();
      } else if (t.eemcBin > 0) {
	W = StEemcHitList::instance()->getFired(t.eemcBin);
	if (W > 0) gTrack->setEemcMatched();
	else      gTrack->setEemcNotMatched();
      }
      if (W > 0) {
	UInt_t fext = gTrack->flagExtension();
	if (W > 7) W = 7;
	fext &= ~7;
	fext += W;
	gTrack->setFlagExtension(fext);
      }
    }
  } else if (gTrack->type()==primary) {
    StTrackNode *n = gTrack->node();
    assert(n);
    StTrack *t = n->track(global);
    assert(t);
    gTrack->setFlagExtension(t->flagExtension());
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
  double tlen = track->getTrackLength();
  assert(tlen >0.0 && tlen<1000.);
  gTrack->setLength(tlen);// someone removed this, grrrr!!!!
  gTrack->setSeedQuality(track->getSeedHitCount());
  // Follow the StDetectorId.h enumerations...
  // can't include them from here in order not to
  // create a package dependence...
  int dets[kMaxDetectorId][3];
  track->getAllPointCount(dets,kMaxDetectorId-1);
  for (int i=1;i<kMaxDetectorId;i++) {
    if(!dets[i][0]) continue;
    gTrack->setNumberOfPossiblePoints((unsigned char)dets[i][0],(StDetectorId)i);
  }
  fillGeometry(gTrack, track, false); // inner geometry
  fillGeometry(gTrack, track, true ); // outer geometry
  fillFitTraits(gTrack, track);
  gTrack->setDetectorInfo(detInfo);
  StuFixTopoMap(gTrack);
  fillFlags(gTrack);
  if (!track->isPrimary()) fillDca(gTrack,track);
  return;
}
//_____________________________________________________________________________
bool StiStEventFiller::accept(StiKalmanTrack* track)
{
//  int nPossiblePoints = track->getMaxPointCount(0);
//  int nMeasuredPoints = track->getPointCount   (0);
    int nFittedPoints   = track->getFitPointCount(0);
    if (nFittedPoints  <  5 )					return 0;
#if 0
    if (nFittedPoints < 10 && nFittedPoints*2 < nPossiblePoints)return 0;
    if(track->getPt()<=0.1) 					return 0;
#endif
    if(track->getTrackLength()<=0) 				return 0; 
    if(track->getChi2()>100) 					return 0; 
    // insert other filters for riff-raff we don't want in StEvent here.
    

    return 1;
}
//_____________________________________________________________________________
double StiStEventFiller::impactParameter(StiKalmanTrack* track
	                                ,StThreeVectorD &vertexPosition) 
{
  StiKalmanTrackNode*	node;

  node = track->getInnerMostNode(2); // ...
  

  originD->setX(node->x_g());
  originD->setY(node->y_g());
  originD->setZ(node->z_g());


  physicalHelix->setParameters(fabs(node->getCurvature()),
			       node->getDipAngle(),
			       node->getPhase(),
			       *originD,
			       node->getHelicity());
  

  //cout <<"PHelix: "<<*physicalHelix<<endl;
  return physicalHelix->distance(vertexPosition);
}
//_____________________________________________________________________________
double StiStEventFiller::impactParameter(StTrack* track, StThreeVectorD &vertex) 
{
  StPhysicalHelixD helix = track->geometry()->helix();

  //cout <<"PHelix: "<<helix<<endl;
  return helix.distance(vertex);
}
//_____________________________________________________________________________
 void StiStEventFiller::fillResHack(StHit *hh,const StiHit *stiHit, const StiKalmanTrackNode *node)
 {
 
  if (!mAux) return;
      StiAux_t aux;
// local frame
  aux.xnl[0] = node->getX();
  aux.xnl[1] = node->getY();
  aux.xnl[2] = node->getZ();

  aux.xhl[0] = stiHit->x();
  aux.xhl[1] = stiHit->y();
  aux.xhl[2] = stiHit->z();

  aux.ca     = node->getEta();
  aux.rho    = node->getCurvature();
  aux.nYY    = node->getCyy();
  aux.nZZ    = node->getCzz();
  aux.hYY    = node->getEyy();
  aux.hZZ    = node->getEzz();

  aux.unl[0] = node->getX();
  aux.unl[1] = node->unTouched().mPar[0];
  aux.unl[2] = node->unTouched().mPar[1];
  aux.uYY    = sqrt(node->unTouched().mErr[0]);
  aux.uZZ    = sqrt(node->unTouched().mErr[2]);


  // global frame
  aux.xng[0] = node->x_g();
  aux.xng[1] = node->y_g();
  aux.xng[2] = node->z_g();

  aux.xhg[0] = stiHit->x_g();
  aux.xhg[1] = stiHit->y_g();
  aux.xhg[2] = stiHit->z_g();
  aux.psi    = node->getPsi();
  aux.dip    = node->getDipAngle();
  // invariant
  double chi2 = node->getChi2();if (chi2>1000) chi2=1000;
  aux.chi2   = chi2;
  int id = mAux->AddIt(&aux);
  assert(id);
  hh->setId(id);
  assert(hh->id());
//mAux->PrintIt(id);
  

}
//_____________________________________________________________________________
void StiStEventFiller::fillDca(StTrack* stTrack, StiKalmanTrack* track)
{
  StGlobalTrack *gTrack = dynamic_cast<StGlobalTrack*>(stTrack);
  assert(gTrack);

  StiKalmanTrackNode *tNode = track->getInnerMostNode();
  if (!tNode->isDca()) return;
  const StiNodePars &pars = tNode->fitPars(); 
  const StiNodeErrs &errs = tNode->fitErrs();
  float alfa = tNode->getAlpha();
  Float_t setp[7] = {(float)pars.y(),    (float)pars.z(),    (float)pars.eta()
                    ,(float)pars.ptin(), (float)pars.tanl(), (float)pars.curv(), (float)pars.hz()};
  setp[2]+= alfa;  
  Float_t sete[15];
  for (int i=1,li=1,jj=0;i< kNPars;li+=++i) {
    for (int j=1;j<=i;j++) {sete[jj++]=errs.G()[li+j];}}
  StDcaGeometry *dca = new StDcaGeometry;
  gTrack->setDcaGeometry(dca);
  dca->set(setp,sete);

}
//_____________________________________________________________________________
void StiStEventFiller::FillStHitErr(StHit *hh,const StiKalmanTrackNode *node)
{
  double stiErr[6],stErr[6];
  memcpy(stiErr,node->hitErrs(),sizeof(stiErr));
  double alfa = node->getAlpha();
  double c = cos(alfa);
  double s = sin(alfa);
  double T[3][3]={{c,-s, 0}
                 ,{s, c, 0}
		 ,{0, 0, 1}};
  
  TCL::trasat(T[0],stiErr,stErr,3,3);
  StThreeVectorF f3(sqrt(stErr[0]),sqrt(stErr[2]),sqrt(stErr[5]));
  hh->setPositionError(f3);
}
//_____________________________________________________________________________
void StiStEventFiller::fillPulls(StiKalmanTrack* track,const StGlobalTrack *gTrack, int gloPri) 
{
enum dcaEmx {kImpImp,
	     kZImp, kZZ,
	     kPsiImp, kPsiZ, kPsiPsi,
	     kPtiImp, kPtiZ, kPtiPsi, kPtiPti,
	     kTanImp, kTanZ, kTanPsi, kTanPti, kTanTan};

  //cout << "StiStEventFiller::fillDetectorInfo() -I- Started"<<endl;
  if (!mPullEvent) 	return;
  if (gloPri && track->isPrimary()!=1) return;
  const StDcaGeometry *myDca = gTrack->dcaGeometry();
  if (!myDca)		return;

  int dets[kMaxDetectorId][3];
  track->getAllPointCount(dets,kMaxDetectorId-1);
  StiPullTrk aux;
  aux.mVertex = (unsigned char)track->isPrimary();
  aux.mTrackNumber=track->getId();
  aux.nAllHits = dets[0][2];
  aux.nTpcHits = dets[kTpcId][2];
  aux.nSvtHits = dets[kSvtId][2];
  aux.nSsdHits = dets[kSsdId][2];
  aux.nPxlHits = dets[kPxlId][2];
  aux.nIstHits = dets[kIstId][2];
  aux.mL       = (unsigned char)track->getTrackLength();
  aux.mChi2    = track->getChi2();
  aux.mCurv    = myDca->curvature();
  aux.mPt      = myDca->pt();
  aux.mPsi     = myDca->psi();
  aux.mDip     = myDca->dipAngle();
  StThreeVectorF v3 = myDca->origin();
  aux.mRxy     = v3.perp();
  aux.mPhi     = v3.phi();
  aux.mZ       = v3.z();
  
  const float *errMx = myDca->errMatrix();
  aux.mPtErr   = sqrt(errMx[kPtiPti])*aux.mPt*aux.mPt;
  double c2dip = myDca->tanDip(); c2dip = 1./(1.+c2dip*c2dip);
  aux.mPsiErr  = sqrt(errMx[kPsiPsi]);
  aux.mDipErr  = sqrt(errMx[kTanTan])*c2dip;
  aux.mRxyErr  = sqrt(errMx[kImpImp]);
  aux.mZErr    = sqrt(errMx[kZZ]);

  aux.mIdTruTk = gTrack->idTruth();
  aux.mQaTruTk = gTrack->qaTruth();
  mPullEvent->Add(aux,gloPri);


  StiKTNIterator tNode = track->rbegin();
  StiKTNIterator eNode = track->rend();
  for (;tNode!=eNode;++tNode) 
  {
      StiKalmanTrackNode *node = &(*tNode);
      if(!node->isValid()) 	continue;

      StiHit *stiHit = node->getHit();
      if (!stiHit)		continue;

      if (node->getChi2()>1000) continue;
      if (!node->isFitted()) 	continue;

      const StiDetector *detector = node->getDetector();
      assert(detector == stiHit->detector());
      assert(!detector || stiHit->timesUsed());
      StHit *hh = (StHit*)stiHit->stHit();
      fillPulls(hh,stiHit,node,track,dets,gloPri);
      if (gloPri) continue;
      fillPulls(hh,stiHit,node,track,dets,2);
  }
}
//_____________________________________________________________________________
 void StiStEventFiller::fillPulls(StHit *stHit,const StiHit *stiHit
                                 ,const StiKalmanTrackNode *node
				 ,const StiKalmanTrack     *track
                                 ,int dets[1][3],int gloPriRnd)
{
  double x,y,z,r,xp,yp,zp,rp;
  float  untErrs[3];

  const StiNodeInf *inf = 0;
  if (gloPriRnd==2) {
    inf = node->getInfo();
    if (!inf)	return;
  }
  double timeFlight = node->getTime();
  const StiNodeErrs &mFE = (inf)? inf->mPE : node->fitErrs();
  const StiNodePars &mFP = (inf)? inf->mPP : node->fitPars(); 
  StiHitErrs  mHrr;
  memcpy(mHrr.G(), (inf)? inf->mHrr.G() : node->hitErrs(),sizeof(StiHitErrs));

  StiPullHit aux;
// local frame
// local HIT
  aux.mIdTruth = stHit->idTruth();
  aux.mQaTruth = stHit->qaTruth();

  aux.mVertex = (unsigned char)track->isPrimary();
  aux.nHitCand = node->getHitCand();
  aux.iHitCand = node->getIHitCand();
  if (!aux.nHitCand)  aux.nHitCand=1;
  aux.lXHit = stiHit->x();
  aux.lYHit = stiHit->y(timeFlight);
  aux.lZHit = stiHit->z(timeFlight);
  aux.lYHitErr = sqrt(mHrr.hYY);
  aux.lZHitErr = sqrt(mHrr.hZZ);
  aux.lHitEmx[0] = mHrr.hYY;
  aux.lHitEmx[1] = mHrr.hZY;
  aux.lHitEmx[2] = mHrr.hZZ;

// local FIT
  aux.lXFit = mFP.x();
  aux.lYFit = mFP.y();
  aux.lZFit = mFP.z();
  aux.lYFitErr = sqrt(mFE._cYY);
  aux.lZFitErr = sqrt(mFE._cZZ);
  aux.lFitEmx[0] = mFE._cYY;
  aux.lFitEmx[1] = mFE._cZY;
  aux.lFitEmx[2] = mFE._cZZ;
//  assert(fabs(aux.lYFit-aux.lYHit)>1e-10 || fabs(aux.lXHit)<4);

// local Pull
  xp = aux.lXHit;
  yp = (inf)? mFP.y(): (double)node->unTouched().mPar[0];
  zp = (inf)? mFP.z(): (double)node->unTouched().mPar[1];
  aux.lYPul = aux.lYHit-yp;
  aux.lZPul = aux.lZHit-zp;
  if (fabs(aux.lYPul)>10) StiDebug::Break(-1);
  if (fabs(aux.lZPul)>10) StiDebug::Break(-1);
  if (!inf) {TCL::ucopy(node->unTouched().mErr,untErrs,3);}
  else      {TCL::ucopy(aux.lFitEmx           ,untErrs,3);}
  //#ifdef YF /* I don't know why these parameters are not set */
  assert(untErrs[0]>=0);
  assert(untErrs[2]>=0);
  //#endif 
  TCL::vadd(untErrs,aux.lHitEmx,aux.lPulEmx,3);
  aux.lYPulErr = sqrt(aux.lPulEmx[0]);
  aux.lZPulErr = sqrt(aux.lPulEmx[2]);

  aux.lPsi  = mFP.eta();
  aux.lDip  = atan(mFP.tanl());

// global frame
  double alfa = node->getAlpha();
  float F[2][2];

//		global Hit
  x = stiHit->x(); y = stiHit->y(timeFlight); z = stiHit->z(timeFlight);
  r = sqrt(x*x+y*y);

  aux.gRHit = r;
  aux.gPHit = atan2(stiHit->y_g(),stiHit->x_g());
  aux.gZHit = stiHit->z_g();
  memset(F[0],0,sizeof(F));
  F[0][0]=  x/(r*r);
  F[1][1]= 1;
  TCL::trasat(F[0],aux.lHitEmx,aux.gHitEmx,2,2);
  aux.gPHitErr = sqrt(aux.gHitEmx[0]);
  aux.gZHitErr = sqrt(aux.gHitEmx[2]);


//		global Fit
  x = mFP.x(); y = mFP.y();z = mFP.z();
  r = sqrt(x*x+y*y);
  aux.gRFit = r;
  aux.gPFit = NICE(atan2(y,x)+alfa);
  aux.gZFit = z;

  memset(F[0],0,sizeof(F));
  F[0][0]=  x/(r*r);
  F[1][1]= 1;
  TCL::trasat(F[0],aux.lFitEmx,aux.gFitEmx,2,2);
  aux.gPFitErr = sqrt(aux.gFitEmx[0]);
  aux.gZFitErr = sqrt(aux.gFitEmx[2]);
  
//		global Pull
  rp = sqrt(xp*xp+yp*yp);
  aux.gPPul = ((aux.gPHit-alfa)-atan2(yp,xp))*rp;
  aux.gZPul = aux.lZHit-zp;
  memset(F[0],0,sizeof(F));
  F[0][0]=  xp/(rp*rp);
  F[1][1]= 1;
  TCL::trasat(F[0],untErrs,aux.gPulEmx,2,2);
  TCL::vadd(aux.gHitEmx,aux.gPulEmx,aux.gPulEmx,3);
// 	Now account that Phi ==> R*Phi  
  aux.gPulEmx[0]*= rp*rp;
  aux.gPulEmx[1]*= rp;
  aux.gPPulErr = sqrt(aux.gPulEmx[0]);
  aux.gZPulErr = sqrt(aux.gPulEmx[2]);

  aux.gPsi  = node->getPsi();
  aux.gDip  = node->getDipAngle();

  // invariant
  aux.mCurv   = mFP.curv();
  aux.mPt     = fabs(1./mFP.ptin());
  aux.mCharge = stHit->charge();
  aux.mChi2   = node->getChi2();
  aux.mNormalRefAngle = alfa;
  aux.mHardwarePosition=0;
  aux.mDetector=0;
  aux.mTrackNumber=track->getId();
  aux.nAllHits  = dets[0][2];
  aux.nTpcHits  = dets[kTpcId][2];
  aux.nSvtHits  = dets[kSvtId][2];
  aux.nSsdHits  = dets[kSsdId][2];
  aux.nPxlHits  = dets[kPxlId][2];
  aux.nIstHits  = dets[kIstId][2];
  const StiDetector *stiDet = stiHit->detector();
  if (stiDet) 		{
    aux.mHardwarePosition=stHit->hardwarePosition();
    aux.mDetector=stHit->detector();
    const StiPlacement *place = stiDet->getPlacement();
    aux.mNormalRadius   = place->getNormalRadius();
    aux.mNormalYOffset  = place->getNormalYoffset();
    aux.mZCenter        = 0;
  }
  mPullEvent->Add(aux,gloPriRnd);

}
