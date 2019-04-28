//StiKalmanTrack.cxx
/*
 * $Id: StiKalmanTrack.cxx,v 2.167 2019/04/28 02:36:42 genevb Exp $
 * $Id: StiKalmanTrack.cxx,v 2.167 2019/04/28 02:36:42 genevb Exp $
 *
 * /author Claude Pruneau
 *
 * $Log: StiKalmanTrack.cxx,v $
 * Revision 2.167  2019/04/28 02:36:42  genevb
 * Restore NHitsPossible by swimming through other layers in getAllPointCount() after removing from initalize0()
 *
 * Revision 2.166  2019/04/13 02:11:27  genevb
 * Remove swimming through hit-less TPC layers in initialize0 importing StiCA seeds (resolves RT3388)
 *
 * Revision 2.165  2018/11/27 20:21:57  smirnovd
 * Correct indentation, white space, and comments
 *
 * Revision 2.164  2018/11/27 20:21:51  smirnovd
 * Use bitwise AND operator instead of logical one
 *
 * This is the correct way to check a bit set in 'mode'
 *
 * Revision 2.163  2018/11/27 20:21:44  smirnovd
 * Properly set default mode for StiKalmanTrack::approx()
 *
 * Revision 2.162  2018/11/27 20:21:38  smirnovd
 * Remove unused local variables
 *
 * Revision 2.161  2018/11/27 20:21:32  smirnovd
 * Remove unused function argument
 *
 * Revision 2.160  2018/11/27 20:21:24  smirnovd
 * Remove debug code
 *
 * Revision 2.159  2018/11/27 20:21:16  smirnovd
 * Remove commented out code
 *
 * Revision 2.158  2018/11/13 18:40:20  perev
 * Put back constant for approx()
 *
 * Revision 2.157  2018/11/10 03:15:24  perev
 * JetCorr fix +tuning
 *
 * Revision 2.156  2018/11/10 00:45:31  perev
 * 1. for approx() added options:
 *    kAppGud - errors are correct
 *    kAppRR - fit with errors
 *    kAppUpd - update only one node
 *    kAppUPD - update all nodes
 * 2. Fit in direction inner ==> outer
 *
 * Revision 2.155  2018/07/06 22:13:16  smirnovd
 * [Cosmetic] Remove unused variables and commented code
 *
 * Revision 2.154  2018/06/29 21:46:27  smirnovd
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
 * Revision 2.152  2018/04/30 23:18:11  smirnovd
 * [Cosmetic] Minor changes in various files
 *
 * - Renamed data member s/m/mMass/ in StikalmanTrack
 * - Changes in white space
 * - Return STAR code
 *
 * Revision 2.151  2018/04/11 02:41:08  smirnovd
 * Remove deprecated methods in StikalmanTrack
 *
 * Revision 2.150  2018/04/11 02:40:55  smirnovd
 * Add new method StikalmanTrack::getInnerMostDetHitNode()
 *
 * Use it in StiCA to replace getInnerMostTPCHitNode()
 *
 * Revision 2.149  2018/04/10 11:38:34  smirnovd
 * Replace thrown exceptions with runtime asserts
 *
 * Revision 2.148  2018/01/12 23:17:09  smirnovd
 * Removed declared but undefined functions
 *
 * Revision 2.147  2017/01/26 21:32:41  perev
 * 1. Method removeNode added. It removes node from the track.
 *    It becames important for the case with reuse hits when old Dca node
 *    is not more correct and must be removed and new one created
 * 2. Method  getChi2Max() added to calculate maximal bad node.
 *    It could (but not yet used) for filtering with reuse hits ON.
 * 3. Method idTruth added. It uses idTruth's of hits and calculates
 *    dominant contrubutor
 *
 * Revision 2.146  2016/11/07 22:42:13  perev
 * 1. Workaround for the bug in CA #3233 moved into StiCA
 * 2. Bug #3231, Layer radius replaced by Normal one
 * 3. Workaround for bug #3232 modified. Now node parameters for the case
 *    wrong ones from CA recalculated for both, first and last node.
 *    This must help to refit().
 * 4. Simplified code in refitL()
 *
 * Revision 2.145  2016/07/08 16:16:55  perev
 * It is workaround for bug in CA (#3230). CA sometimes gives the same hit twice
 * Here I test all hits to be in decreasing order of x() (local x).
 *  If not, bad hits skipped.
 * It is not the best way, it is better to fix it inside CA.
 *  but temporary it will work(VP)
 *
 * Revision 2.144  2016/07/07 01:15:00  perev
 * Removed changing of timesUsed in releaseHits.
 * This method is called inside refit when hits is not yet marked as used
 *
 * Revision 2.143  2016/06/30 19:51:52  perev
 * WarnOff
 *
 * Revision 2.142  2016/06/29 18:18:30  perev
 * See comments in StiKalmanTrack.h
 *
 * Revision 2.139.4.5  2016/06/02 16:50:03  smirnovd
 * StiKalmanTrack: Refactored public refit() to use protected refit(int&)
 *
 * Two return values from protected refit(int&) can be used in different context.
 * For example, derived class StiCAKalmanTrack return a value different from the
 * base class.
 *
 * Revision 2.139.4.4  2016/06/02 16:45:42  smirnovd
 * Squashed changes on MAIN branch after StiCA_2016 was brached off
 *
 * commit 0b534582b5bf40a64870088f6864387a7941a9be
 * Author: perev <perev>
 * Date:   Tue May 31 17:11:46 2016 +0000
 *
 *     Coverity
 *
 * commit cbfeeef5e8f9a6e24ddd7329ff5770086e535493
 * Author: perev <perev>
 * Date:   Tue Apr 19 01:58:39 2016 +0000
 *
 *     Assignment out of array boundary removed(J.Lauret)
 *
 * commit a49f5f23dc613c1ee8ab61c543e713f776d3c7fe
 * Author: perev <perev>
 * Date:   Tue Apr 19 01:37:22 2016 +0000
 *
 *     WarnOff
 *
 * commit 48ca225cc052db66cd8a3934f15c46345c9862c6
 * Author: perev <perev>
 * Date:   Fri Apr 15 20:47:42 2016 +0000
 *
 *     Warnoff
 *
 * commit b1b0f73cef0f5675bd84106241067329e0221079
 * Author: perev <perev>
 * Date:   Fri Apr 15 20:13:06 2016 +0000
 *
 *     Warnoff
 *
 * commit 393adde57febc06a90d054f71e621e8efd082e10
 * Author: perev <perev>
 * Date:   Wed Apr 13 23:08:44 2016 +0000
 *
 *     -opt2 proble solved. Array A[1] removed
 *
 * commit 1c105bdc0cbde40ccec63fdbf40e79dfb3e7f0e0
 * Author: perev <perev>
 * Date:   Mon Mar 28 00:17:55 2016 +0000
 *
 *     1st hit must be not used at all
 *
 * commit 1eca42192ef93788d149625ecebc8390f8b0bc3a
 * Author: perev <perev>
 * Date:   Mon Mar 28 00:15:53 2016 +0000
 *
 *     Add max number of tracks assigned to one hit
 *
 * commit b349ba99342bc38eaa82f3d2a8d25aa29ba73c29
 * Author: genevb <genevb>
 * Date:   Thu Feb 25 23:04:50 2016 +0000
 *
 *     kSsdId => kSstId
 *
 * commit a06d8162931b223b4a405ea5714e703b1cad14e3
 * Author: perev <perev>
 * Date:   Mon Dec 28 23:50:27 2015 +0000
 *
 *     Remove assert temporary
 *
 * commit f8646d17ed86b9be5b5fa940691f9871346a5ee2
 * Author: perev <perev>
 * Date:   Mon Dec 21 19:41:31 2015 +0000
 *
 *     bug #3166 assert vertex closer to 0,0 <9 removed
 *
 * commit 48a6813db30f593a90a79beb688c27d0e8946bfa
 * Author: perev <perev>
 * Date:   Sat Dec 19 03:40:50 2015 +0000
 *
 *     assert rxy<4 ==> <9 temporary
 *
 * commit d49576f25ba887ba4ff82c3bf1ffcc760c8da6b2
 * Author: perev <perev>
 * Date:   Fri Dec 18 03:50:06 2015 +0000
 *
 *     *** empty log message ***
 *
 * commit 23e9c0447bd41151e45728a6f4dd3cc554be1cfb
 * Author: perev <perev>
 * Date:   Thu Dec 3 19:12:24 2015 +0000
 *
 *     Remove redundant GTrack error: mFlag: is Negative
 *
 * Revision 2.140  2016/04/13 23:08:44  perev
 * -opt2 proble solved. Array A[1] removed
 *
 * Revision 2.139  2015/04/02 16:29:16  perev
 * Member mCombUsed introdused to memorize combination of hits selected
 * Enum keepHit and kGoodHir added instead of using 1 & 2.
 * StiKalmanTrack::add added new parameter StiTrackNode *near. It allows
 * to add node in the middle. It is important for combinatorics.
 * It is not clear how it was working before, but there is no time to investigate
 *
 * Revision 2.138  2015/02/09 15:47:59  genevb
 * Restore inversion of hh because it is used in multiple places
 *
 * Revision 2.137  2015/02/09 04:14:52  perev
 * Remove redundant hit->subTimesUsed() + Cleanup
 *
 * Revision 2.136  2015/02/07 04:21:05  perev
 * More accurate zero field accounting
 *
 * Revision 2.135  2015/02/02 04:37:19  perev
 * replacemens of names *TimesUsed to new versions
 *
 * Revision 2.134  2015/01/15 19:10:19  perev
 * Added mthod test() for debug only
 *
 * Revision 2.133  2014/11/10 21:45:09  perev
 * In approx more carefully accounted case mag field == 0
 * To deside that field == 0 method isZeroH() is used
 *
 * Revision 2.132  2014/10/30 15:03:54  jeromel
 * Reverted to Oct 2nd
 *
 * Revision 2.127  2014/09/29 21:44:55  perev
 * Check cos>=1 replaced to cos>=.99
 *
 * Revision 2.126  2014/07/09 00:15:45  perev
 * Fix wrong Xi2 for 5hits track
 *
 * Revision 2.125  2013/04/10 22:09:01  fisyak
 * Roll back to version 04/04/2013
 *
 * Revision 2.123  2012/03/12 23:17:12  fisyak
 * Correct detectorOld cast for StiCA
 *
 * Revision 2.122  2012/03/12 20:51:42  fisyak
 * Restrict new No. possible point calculation to StiCA only
 *
 * Revision 2.121  2011/11/21 17:05:26  fisyak
 * Correct no. of possible point for CA case
 *
 * Revision 2.120  2010/09/07 18:37:31  fisyak
 * Restore Sti logic before TPCCATracker
 *
 * Revision 2.119  2010/09/06 18:20:48  fisyak
 * Add TPCCATracker
 *
 * Revision 1.8  2010/08/25 21:34:05  ikulakov
 * Warnings fix.
 *
 * Revision 1.7  2010/08/23 21:56:28  ikulakov
 * Fix - alinghment bag.
 *
 * Revision 1.6  2010/08/09 17:51:14  mzyzak
 * StiPerformance is added; bug with cov matrix of the seed parameters is fixed; bug with the q/p sign of the seed parameters is fixed; functionality of the performance is extended
 *
 * Revision 1.5  2010/08/05 21:16:53  ikulakov
 * Add fit status statistic.
 *
 * Revision 1.4  2010/08/04 13:45:46  ikulakov
 * Fix - hz & sign pt.
 *
 * Revision 1.3  2010/08/02 16:45:27  ikulakov
 * Use tracks params obtained from CATracker for StRoot KF fitter initialization.
 *
 * Revision 1.2  2010/07/29 16:19:11  fisyak
 * GSI CA tracker
 *
 * Revision 2.118  2010/04/03 04:04:57  perev
 * Account field=0
 *
 * Revision 2.117  2010/02/17 14:28:07  fisyak
 * Add seed quality information
 *
 * Revision 2.116  2009/10/18 22:49:52  perev
 * remove STAR LOG in print()
 *
 * Revision 2.115  2009/10/15 03:30:20  perev
 * Add primary vertex number
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
 * No any cuts in isPrimary()
 *
 * Revision 2.103  2008/04/08 14:21:17  fisyak
 * 2 cm => StiKalmanTrackFinderParameters::instance()->maxDca3dVertex() in StiKalmanTrack::isPrimary()
 *
 * Revision 2.102  2008/04/07 19:20:53  perev
 * More clear isPrimary()
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
 * method getPoint added
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
 * getAllPointCount(...) added
 *
 * Revision 2.70  2005/08/16 20:11:10  perev
 * Typo corrected
 *
 * Revision 2.69  2005/08/14 01:10:55  perev
 * Non empty unset(). Free all nodes when track is freed
 *
 * Revision 2.68  2005/08/09 14:51:25  perev
 * Add reduce method, reducing all the nodes
 *
 * Revision 2.67  2005/08/04 03:50:31  perev
 * removeLastNode() added
 *
 * Revision 2.66  2005/07/20 17:21:44  perev
 * MultiVertex
 *
 * Revision 2.65  2005/06/09 03:12:39  perev
 * Fix typo in getNodes()
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
 * getMaxPointCount() fixed(thank to Jan)
 *
 * Revision 2.59  2005/03/31 17:25:57  perev
 * getMaxPointCount() fixed(thank to Jan)
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
 * Add debug print out for extendToVertex
 *
 * Revision 2.53  2005/02/17 23:19:02  perev
 * NormalRefangle + Error reseting
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
 * use _alpha instead of getRefAngle while extending to vertex
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
 * (2) Changed the StiKalmanTrackFinder::find(StiTrack*) function's algorithm for the navigation of the
 * detector volumes. The new code uses an iterator to visit all relevant volumes. The code is now more robust and compact
 * as well as much easier to read and maintain.
 * (3) Changed the chi2 calculation in StiKalmanTrack::getChi2 and propagated the effects of this change
 * in both StiTrackingPlots and StiStEventFiller classes.
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
 * Added getMaxPointCount(int detectorId)< where detectorId corresponds to the
 * StDetectorId value.
 *
 * Revision 2.33  2004/04/04 23:19:28  jeromel
 * isfinite() -> finite()
 *
 * Revision 2.32  2004/03/31 00:23:41  calderon
 * -Fixed memory leak in StiDetectorTreeBuilder::hangWhere (100 chars were lost
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
 * -StiTrackKalmanTrackFitter now also sets a pointer to itself in
 *  static StiKalmanTrack::setFitParameters()
 * -Removed some print outs from VectorizedFactory to reduce the size of the log
 *  files.
 *
 * Revision 2.31  2004/03/23 23:10:37  calderon
 * Check for nan's in getTrackLength() calculation.  When the argument for the
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
 * inside the add(stiHit*...) method used while initializing tracks from the
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
 * and modified getNodes of the StiKalmanTrack class to use it. This eliminates explicit
 * references to Tpc and Svt within StiKalmanTrack...
 *
 * Revision 2.17  2003/03/14 19:02:20  pruneau
 * various minor updates
 *
 * Revision 2.16  2003/03/13 21:21:26  pruneau
 * getPhase() fixed. MUST inclde -helicity()*pi/2
 *
 * Revision 2.15  2003/03/13 18:59:08  pruneau
 * various updates
 *
 * Revision 2.14  2003/03/13 16:38:11  andrewar
 * Made use of X0() calls in getTrackRadLength()
 *
 * Revision 2.13  2003/03/13 15:16:41  pruneau
 * fixed getPhi, getPseudoRapdity, getPhase methods
 *
 * Revision 2.12  2003/03/12 17:57:29  pruneau
 * Elss calc updated.
 *
 * Revision 2.11  2003/03/04 15:16:22  andrewar
 * Added getTrackRadLength function to return radiation thickness along track (%).
 *
 */


#include <cassert>
//Std
#include <stdexcept>
#include <cmath>

//SCL
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"

#include "StHit.h"

//Sti
#include "StiKalmanTrack.h"
#include "StiKalmanTrackFinder.h"
#include "StiToolkit.h"
#include "StiDetectorContainer.h"
#include "StiHit.h"
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiDetectorGroups.h"
#include "StiDetectorBuilder.h"
#include "StiPlacement.h"
#include "StiMaterial.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StPhysicalHelixD.hh"
#include "StHelix.hh"
#include "StDetectorDbMaker/StiKalmanTrackFitterParameters.h"
#include "StDetectorDbMaker/StiKalmanTrackFinderParameters.h"
#include "StiHitContainer.h"
#include "StiUtilities/StiDebug.h"
#include "TCernLib.h"
#include "StMessMgr.h"
ostream& operator<<(ostream&, const StiHit&);

Factory<StiKalmanTrackNode>* StiKalmanTrack::trackNodeFactory = 0;
int StiKalmanTrack::mgMaxRefiter = 100;
int StiKalmanTrack::_debug = 0;
int debugCount=0;

StiTrackNodeHelper StiKalmanTrack::sTNH;


/*! 
   Reset the class members to their default state.
   This method is called by the ctor of the class to initialize the
   members of the class to an "empty" or null track state. The
   method must also be called everytime an instance of this class is
   retrieved from its factory in order to set the first and last
   nodes to "null" thus guaranteeing that the track object is empty
   i.e. does not represent any track and is thus ready for a new
   search and reconstruction.  
 */
//_____________________________________________________________________________
void StiKalmanTrack::reset()
{
static int mIdCount = 0;
  if ((++mIdCount) >= 1<<16) mIdCount = 1;
  mIdDb = mIdCount; 
  firstNode = 0;
  lastNode  = 0;
  mSeedHitCount = 0;
  mCombUsed = 0;
  mVertex = 0;
  mMass  = -1.;
  mFlag  = 0;
  _dca   = 0;
  _vChi2=-2;
  StiDebug::Break(mIdDb);
}
//_____________________________________________________________________________
/*! 
  Set the factory used for the creation of kalman track nodes.
  \see StiKalmanTrackNodeFactory
*/
//_____________________________________________________________________________
void StiKalmanTrack::setKalmanTrackNodeFactory(Factory<StiKalmanTrackNode>* val)
{
  trackNodeFactory = val;
}



//_____________________________________________________________________________
/*! Initialization of this kalman track from external parameters.
  <p>
  This track object is initialized on the basis of parameters determined externally. The
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
int StiKalmanTrack::initialize(const std::vector<StiHit*> &hits)
{
  reset();
  const StiDetector* detector=0;
  UInt_t nhits = hits.size();
  setSeedHitCount(nhits);

  for (UInt_t ihit=0;ihit<nhits;ihit++)
  {
    StiHit *hit = hits[ihit];	//loop from in to out to keep sign of dir
    detector = hit->detector();
    assert(detector);
    StiKalmanTrackNode * n = trackNodeFactory->getInstance();
    n->initialize(hit);
    add(n,kOutsideIn);
  }

  int ierr = approx(kAppRR|kAppUPD);
  if (!ierr) return 0;
  BFactory::Free(this);
  return 1;  
}
//_____________________________________________________________________________
int StiKalmanTrack::initialize0(const std::vector<StiHit*> &hits, StiNodePars *firstPars, StiNodePars *lastPars, StiNodeErrs *firstErrs, StiNodeErrs *lastErrs)
{
  reset();
  const StiDetector* detector=0;
  UInt_t nhits = hits.size();
  setSeedHitCount(nhits);

  for (UInt_t ihit = 0; ihit < nhits; ihit++)  {
    StiHit *hit = hits[ihit];
    detector = hit->detector();
    assert(detector);
    StiKalmanTrackNode * n = trackNodeFactory->getInstance();
    n->initialize(hit);
    add(n,kOutsideIn);
  }  
  if (!firstPars)	{approx(); return 0;}
  else 			{firstNode->fitPars() = *firstPars;}

  if (firstErrs)	{firstNode->fitErrs() = *firstErrs;}
  if (lastPars)		{lastNode ->fitPars() = *lastPars ;}
  if (lastErrs)		{lastNode->fitErrs()  = *lastErrs ;}

  return 0;  
}



//_____________________________________________________________________________
/*! Return the track sign
   <h3>Notes</h3> 
   <ol>
   <li>Use the last node and the field.</li>
   </ol>
*/
//_____________________________________________________________________________
int StiKalmanTrack::getCharge() const
{
  StiKalmanTrackNode *node = getInnerMostNode();
  if (!node) return 0;
  return  node->getCharge();
}

//_____________________________________________________________________________
/// Return the track chi2 per dof
/// <p>
/// The track chi2 is calculated from the incremental chi2 of all nodes carrying a hit that contributed to the fit of the track. 
/// Note that a hit is not counted as contributing to the fit if its chi2 exceeds "StiKalmanTrackFitterParameters::instance()->getMaxChi2()"
/// Note that this function returns "-1" if the number of fit points is smaller than 6
double  StiKalmanTrack::getChi2() const
{
  double fitHits   = 0;
  double trackChi2 = 0;
  double maxChi2   = StiKalmanTrackFitterParameters::instance()->getMaxChi2();
  if (!firstNode) return 1.e+60;
  StiKTNIterator it;
  for (it=begin();it!=end();++it)  {
    StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) 	continue;
    if (!node->getHit() ) 	continue;
    if (!node->getDetector())	continue;
    double nodeChi2 = node->getChi2();
    if (nodeChi2>maxChi2) 	continue;
    trackChi2 += nodeChi2;
    ++fitHits;
  }
  return (fitHits>3)?trackChi2/(2.*fitHits-5.):1e30;
}
//_____________________________________________________________________________
/// Return the maximal node chi2 
double  StiKalmanTrack::getChi2Max() const
{
  double trackChi2 = 0;
  double maxChi2   = StiKalmanTrackFitterParameters::instance()->getMaxChi2();
  if (!firstNode) return 1e11;
  for (auto it=begin();it!=end();++it)  {
    StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) 	continue;
    if (!node->getHit() ) 	continue;
    if (!node->getDetector())	continue;
    double nodeChi2 = node->getChi2();
    if (nodeChi2>maxChi2) 	continue;
    if (trackChi2<nodeChi2) trackChi2=nodeChi2;
  }
  return trackChi2;
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
int StiKalmanTrack::getPointCount(int detectorId) const
{
  const StiDetector *detector=0;   
  int nPts = 0;
  StiKTNIterator it;
  for (it=begin();it!=end();it++) {
    StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) 	continue;
    if (!node->getHit())	continue;
    detector = node->getDetector();  
    if (!detector) 		continue;
    if (node->getChi2()>=1000)  continue;
    if (detectorId && detector->getGroupId() != detectorId) 	continue;
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
int StiKalmanTrack::getMaxPointCount(int detectorId) const
{
  int nPts = 0;
  StiKTNIterator it;

  for (it=begin();it!=end();it++){
    const StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) 					continue;
    const StiDetector *detector = node->getDetector();
    if (!detector)						continue;
    StiHit* h = node->getHit();
    if (!h && !detector->isActive(node->getY(),node->getZ()))	continue;
    if (detectorId && detector->getGroupId() != detectorId) 	continue;
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
int    StiKalmanTrack::getGapCount()    const  
{
  int gaps = 0;
  if (firstNode)
    {
      StiKTNIterator it;
      bool inGap = false;
      for (it=begin();it!=end();it++)
	{
	  const StiDetector * detector = (*it).getDetector();
	  if (detector && detector->isActive())
	    {
	      if ((*it).getHit())
		{
		  if (inGap) 
		    inGap = false;
		}
	      else
		{
		  if (!inGap)
		    {
		      inGap = true;
		      gaps++;
		    }										
		}
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
  <li>Call "getPointCount()" to get the count.</li>
  </ol>
  \return number of hits on this track.
*/
///Get number of fit points in given detector
//_____________________________________________________________________________
int StiKalmanTrack::getFitPointCount(int detectorId)    const  
{
  int fitPointCount  = 0;
  StiKTNIterator it;
  for (it=begin();it!=end();it++)  {
    StiKalmanTrackNode* node = &(*it); 
    if(!node->isValid())		continue;
    StiHit* hit = node->getHit();
    if (!hit)				continue;
    if (!node->isFitted())		continue;
    const StiDetector *det = hit->detector();
    if (!det)				continue;  
    if (detectorId && detectorId!=det->getGroupId())continue;
    fitPointCount++;
  }
  return fitPointCount;
}
//_____________________________________________________________________________
void StiKalmanTrack::getAllPointCount(int count[1][3],int maxDetId) const
{
//  output array actually is count[maxDetId+1][3] 
//  count[0] all detectors
//  count[detId] for particular detector
//  count[detId][0] == number of possible points
//  count[detId][1] == number of measured points
//  count[detId][2] == number of fitted   points
enum {kPP=0,kMP=1,kFP=2};

  StiDetectorContainer    *detectorContainer = StiToolkit::instance()->getDetectorContainer();	 
  const StiDetector* detectorOld = 0;	 
 
  memset(count[0],0,(maxDetId+1)*3*sizeof(int));
  StiKTNIterator it;

  for (it=begin();it!=end();it++){
    const StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) 	continue;
    const StiDetector *detector = node->getDetector();
    if (!detector)		continue;
    int detId = detector->getGroupId();
    StiHit* h = node->getHit();
    if (detectorOld && detId == kTpcId) {
      Double_t R = detector->getPlacement()->getNormalRadius();
      Double_t angle = detector->getPlacement()->getNormalRefAngle();
      Double_t R_OLD = detectorOld->getPlacement()->getNormalRadius();
      while (R < R_OLD) {
        detectorContainer->setToDetector( detectorOld );
        if ( detectorContainer->moveIn(TMath::DegToRad()*5,100,R)) {
          StiDetector* d = detectorContainer->getCurrentDetector(); //**detectorContainer;	 
          if (d == detector) break;
          detectorOld = d;
          R_OLD = detectorOld->getPlacement()->getNormalRadius();
          Double_t angle_OLD = detectorOld->getPlacement()->getNormalRefAngle();
          if (detectorOld->isActive() && R < R_OLD && TMath::Abs(angle - angle_OLD) < TMath::DegToRad()*5) { // the same sector
            count[0][kPP]++; count[detId][kPP]++;
          }
        } else break;
      }
    }

//fill possible points
    if (h || detector->isActive(node->getY(),node->getZ())) {
       count[0][kPP]++; count[detId][kPP]++;
    }
    detectorOld = detector;
    
    if (!h ) 			continue;
//fill measured points
    count[0][kMP]++; count[detId][kMP]++;
    if (!node->isFitted()) 	continue;
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
double StiKalmanTrack::getTrackLength() const
{
  double x[2][4];
  for (int i = 0; i < 4; i++){ x[0][i] = 0; }
  double len=0;
  int iready=0;
  StiKalmanTrackNode *node;
  StiKTNIterator it = begin();
  for (;(node=it());it++){
    if (!node->isValid()) 	continue;
    if (!node->getHit()) 	continue;
    if ( node->getChi2()>10000.)continue;
    x[1][0]=node->x_g();
    x[1][1]=node->y_g();
    x[1][2]=node->z_g();
    x[1][3]=node->getCurvature();
    if (iready) {
      double dlen = sqrt(pow(x[1][0]-x[0][0],2) + pow(x[1][1]-x[0][1],2));
      double curv = fabs(0.5*(x[0][3]+x[1][3]));
      double dsin = (0.5*dlen*curv);
      if (dsin>0.9) {
         LOG_DEBUG <<
           Form("StiKalmanTrack::getTrackLength ***ERROR*** dsin %g >.9",dsin)
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


/*! Calculates the radiation length of material crossed by the track.

 */
//_____________________________________________________________________________
double StiKalmanTrack::getTrackRadLength() const
{
  double x1, x2, x3;  //lengths in different media
  double totalR=0.;
  //Are we going in or out? Makes a difference which material to call

  StiKTNIterator tNode = begin();

  //set initial conditions for tNode, the 'current' node;
  //will also need 'nextNode', ie node which is next 
  StiKalmanTrackNode *thisNode = &(*tNode);

  x1=thisNode->pathlength()/2.; 
  x3=0.;


  //  while ((++tNode)!=end() && (*tNode).getDetector())
  while ((tNode++)!=end() && (*tNode).getDetector())
    {

      StiKalmanTrackNode *nextNode = &(*(tNode)); //incrimented tNode
    

      x2=thisNode->pathLToNode(nextNode); 
      x3=nextNode->pathlength()/2.;

      if(x3==-1.) continue;  
      //if there is an error with "next" node, proceed along track
      //without updating current node. This should provide
      //function thisNode=thisNode, nextNode=new node

      if (x2> (x1+x3)) x2 = x2 - x1 - x3; //x2 is now the gap distance
      else x2=0.;

      cout
	   <<"getTrackRadLength:"
	   <<"\n\tIn Detector: "<<thisNode->getDetector()->getName()
	   <<"\n\t\tMaterial: "<<thisNode->getDetector()->getMaterial()
	   <<"\n\t\tLength: "<<x1
	   <<"\t\tGap Length: "<<x2
           <<"\n\tNext Detector: "<<nextNode->getDetector()->getName()
	   <<"\n\t\tMaterial: "<<nextNode->getDetector()->getMaterial()
	   <<"\n\t\tLength: "<<x3
           << endl;
	{
	  if (thisNode->getX0()>0)    totalR += x1/thisNode->getX0();
	  if (nextNode->getGasX0()>0) totalR += x2/nextNode->getGasX0();
	  if (nextNode->getX0()>0)    totalR += x3/nextNode->getX0();
	}
      //cache nextNode for next iteration...
      thisNode = nextNode;
      x1       = x3;
    }
  if (totalR>200.)
    cout <<"StiKalmanTrack::getTrackRadLength() -W- Total Rad Length Error: "<<totalR;
  return totalR;
}
//_____________________________________________________________________________
double StiKalmanTrack::getNearBeam(StThreeVectorD *pnt,StThreeVectorD *dir) const
{
  StiKalmanTrackNode * inNode = lastNode;
  StThreeVectorD in(inNode->x_g(),inNode->y_g(),inNode->z_g());

  StPhysicalHelixD hlx(fabs(inNode->getCurvature()),
		            inNode->getDipAngle(),
		            inNode->getPhase(),
		            in,
		            inNode->getHelicity());
  double per = hlx.period();
  double len = hlx.pathLength(0.,0.);
//  StHelix can return negative length if -ve path is shorter then +ve one
//  period ia added in this case;
  if (fabs(len) > fabs(len+per)) len+=per;
  if (fabs(len) > fabs(len-per)) len-=per;

  hlx.moveOrigin(len);
  if (pnt) (*pnt) = hlx.at(0);

  if (dir) {
    double phase = hlx.phase();
    double dip   = hlx.dipAngle();
    int h        = hlx.h();

    (*dir)[0]= -sin(phase)*cos(dip)*h;	
    (*dir)[1]=  cos(phase)*cos(dip)*h;
    (*dir)[2]=             sin(dip)*h;}

  return fabs(len);
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
StiKalmanTrackNode * StiKalmanTrack::getInnOutMostNode(int inot,int qua)  const
{
  assert(firstNode && lastNode);
  
  StiKalmanTrackNode *node;
  StiKTNIterator it =(inot) ? begin():rbegin();
  for (;(node=it());it++){
    if (!node->isValid()) 				continue;
    StiHit *hit = node->getHit();
    if (qua&kKeepHit) {if (!hit) continue;}
    if (qua&kGoodHit) {if (!hit || node->getChi2()>10000.)continue;}
    return node;
  }
  cout << "StiKalmanTrack::getInnOutMostNode() -E- No requested nodes " << endl;
  return 0;
}
//_____________________________________________________________________________
StiKalmanTrackNode * StiKalmanTrack::getOuterMostHitNode(int qua)  const
{
  return getInnOutMostNode(1,qua|1);
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

StiKalmanTrackNode * StiKalmanTrack::getInnerMostHitNode(int qua)   const
{
  return getInnOutMostNode(0,qua|1);
}
//_____________________________________________________________________________
StiKalmanTrackNode * StiKalmanTrack::getInnerMostDetHitNode(int detId)   const
{
  assert(firstNode && lastNode);
  StiKalmanTrackNode *node = 0;
  for (auto it=begin();(node=it());++it) 
  {
    if (!node->isValid())		continue;
    if (node->getChi2()>10000.) 	continue;
    StiHit* hit = node->getHit();
    if (!hit) 				continue;
    auto *det = hit->detector();
    if (!det) 				continue;
    if (detId!=det->getGroupId())	continue;
    return node;
  }
  return 0;
}
//_____________________________________________________________________________
int StiKalmanTrack::getNNodes(int qua)  const
{
  StiKalmanTrackNode *node;
  StiKTNIterator it = begin();
  int nn=0;
  for (;(node=it());it++){
    if (!node->isValid()) 				continue;
    StiHit *hit = node->getHit();
    if (qua&kKeepHit) { if (!hit) continue;} 			
    if (qua&kGoodHit) { if (!hit || node->getChi2()>10000.) continue;}
    nn++;
  }
  return nn;
}

//_____________________________________________________________________________
///return vector of nodes with hits
vector<StiKalmanTrackNode*> StiKalmanTrack::getNodes(int detectorId) const
{
  StiKTNIterator it;
  vector<StiKalmanTrackNode*> nodeVec;
  for (it=begin();it!=end();++it) {
          StiKalmanTrackNode* node = &(*it);
    const StiHit* hit = node->getHit();
    if(!hit) 				continue;
    const StiDetector *det = hit->detector();
    if (!det) 				continue;
    if (node->getDedx()<=0.)		continue;   
    if (detectorId!=det->getGroupId())	continue;
    nodeVec.push_back(node);
  }
  return nodeVec;
}

//_____________________________________________________________________________
///return hits;
vector<const StMeasuredPoint*> StiKalmanTrack::stHits() const
{
  StiKTNIterator it;
  vector<const StMeasuredPoint*> hits;
  for (it=begin();it!=end();++it) {
    const StiKalmanTrackNode* node = &(*it);
    if (!node->isValid()) 	continue;
    if (node->getChi2()>10000.) continue;
    const StiHit* hit = node->getHit();
    if (!hit) 			continue;
    if (!hit->detector())	continue;
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
void StiKalmanTrack::reserveHits(int yes)
{
  if (yes) {
   StiKTNForwardIterator it(lastNode);
   for_each( it, it.end(), SetHitUsed()   );
  } else {
   StiKTNForwardIterator it(lastNode);
   for_each( it, it.end(), SetHitUnused() );
  } 

}

/*! Extend track to the given vertex.
  <p>
  Attempt an extension of the track  the given vertex. 
  <p>
  <ol>
  <li>Get node from node factory.</li>
  <li>Reset node.</li>
  <li>Propagate the node from given parent node "sNode", to the given vertex using a 
  call to "propagate".</li>
  <li>Evaluate the chi2 of the extrapolated if the vertex is added to the track. Done
  using a call to "evaluateChi2".</li>
  <li>If chi2 is less than max allowed "maxChi2ForSelection", update track parameters
  using the vertex as a measurement and add the vertex to the track as the last node.</li>
  </ol>
  <h3>Notes</h3>
  <ul>
  <li>Throws logic_error if no node can be obtained from the node factory.</li>
  <li>The methods "propagate", "evaluateChi2", and "updateNode" may throw 
  runtime_error exceptions which are NOT caught here...</li>
  </ul>
*/

//_____________________________________________________________________________
StiTrackNode *StiKalmanTrack::extendToVertex(StiHit* vertex)
{
static int nCall=0; nCall++;
  double chi2;
  int dcaHit = vertex->isDca();
  StiKalmanTrackNode * sNode=0;
  StiKalmanTrackNode * tNode=0;
  bool trackExtended = false;

  StiKalmanTrackNode * innerMostHitNode = getInnerMostHitNode();
  if (!innerMostHitNode) 		return 0;
		
  StiHit localVertex = *vertex;
  sNode = getInnerMostNode();
  if (sNode->isDca()) {//it is fake node. Remove it
    removeLastNode();
    sNode = getInnerMostNode();
  }


  localVertex.rotate(sNode->getAlpha());
  tNode = trackNodeFactory->getInstance();
  StiHit *myHit;
  //cout << "SKT::extendToVertex() -I- x,y,z:"<< localVertex.x() 
  //     << " " <<  localVertex.y() << " " << localVertex.z() << endl;
  //cout << "SKT::extendToVertex() -I- sNode->getX():"<<sNode->getX()<<endl;
  //cout << "SKT::extendToVertex() -I-0 tNode->getX():"<< tNode->getX()<<endl;
  if (tNode->propagate(sNode, &localVertex,kOutsideIn))
    { 
      //cout << " on vertex plane:";
      double dy=0,dz=0,d=0;
      if (dcaHit) {		//Fake DCA vertex
        tNode->setChi2(0); 
	tNode->setHit(0);
	tNode->setDetector(0);
        return tNode;
      } else {			//Normal vertex 
	tNode->setChi2(3e33);
	chi2 = tNode->evaluateChi2(&localVertex); 
	dy=tNode->getY()- localVertex.y();
	dz=tNode->getZ()- localVertex.z();
	d = ::sqrt(dy*dy+dz*dz);
	_vChi2= chi2; _dca = d;
      }


//    if (chi2<StiKalmanTrackFinderParameters::instance()->maxChi2Vertex  && d<4.)
//    if (                             d<4.)
	if (chi2<StiKalmanTrackFinderParameters::instance()->maxChi2Vertex())
	{
	  myHit = StiToolkit::instance()->getHitFactory()->getInstance();
	  *myHit = localVertex;
	  tNode->setHit(myHit);
	  tNode->setChi2(chi2);
	  tNode->setDetector(0);
          trackExtended = (tNode->updateNode()==0);
          
	  if (trackExtended) return tNode;
          trackNodeFactory->free(tNode);             
	}
      else if (d < 4) {
        LOG_DEBUG <<
          Form("Primary(%d) not accepted BUT d = %g chi2 = %g",nCall,d,chi2)
        << endm;
      }
    }
  //else
  //  cout <<" TRACK NOT REACHING THE VERTEX PLANE!"<<endl;
  return 0;
}
///Return all the hits associated with this track, including those with a large incremental
///chi2 that may not contribute to the fit.
//_____________________________________________________________________________
vector<StiHit*> StiKalmanTrack::getHits()
{
  vector<StiHit*> hits;
  StiKalmanTrackNode* leaf = getLastNode();
  StiKTNForwardIterator it(leaf);
  StiKTNForwardIterator end = it.end();
  for (;it!=end;++it) 
    {
      const StiKalmanTrackNode& node = *it;
      if (!node.isValid())		continue;
      if (node.getChi2()>10000.) 	continue;
      StiHit* hit = node.getHit();
      if (!hit) 			continue;
      hits.push_back(hit);
    }
  return hits;
}

//_____________________________________________________________________________
/// Return global dca of the track relative to given vertex or point.
double  StiKalmanTrack::getDca(const StiHit * vertex)    const
{
  StiKalmanTrackNode*	node;

  node = getInnerMostHitNode(2); 
  StThreeVectorD originD(node->x_g(),node->y_g(),node->z_g());
  StThreeVectorD vxDD(vertex->x_g(), vertex->y_g(),vertex->z_g());
  StPhysicalHelixD physicalHelix(0.,0.,0.,originD,-1);
  physicalHelix.setParameters(fabs(node->getCurvature()),
			       node->getDipAngle(),
			       node->getPhase(),
			       originD,
			       node->getHelicity());
  double dca = physicalHelix.distance(vxDD);
  return dca;
}
//_____________________________________________________________________________
ostream& operator<<(ostream& os, const StiKalmanTrack& track)
{
  try 
    {
      os << *((StiTrack *) &track);
      os <<"List of nodes" << endl;
      StiKTNIterator tNode = track.begin();
      StiKTNIterator eNode = track.end();
      //set initial conditions for tNode, the 'current' node;
      //will also need 'nextNode', ie node which is next 
      while (tNode != eNode) {
	StiKalmanTrackNode *thisNode = &(*tNode);
	if (thisNode) os << *thisNode;
	tNode++;
      }
    }
  catch (runtime_error & rte)
    {
      os << " Run-time Error while accessing track parameters: " << rte.what() << endl;
    }
  catch (logic_error & le)
    {
      os << " Logic Error while accessing track parameters: " << le.what() << endl;
    }
  return os;
}

//_____________________________________________________________________________
///Extrapolate this track to the beam axis (x==0) to provide an estimate of the
///track location at the beam axis.
///Returns a null pointer is the operation cannot be completed i.e. the track does not reach
///the beam axis plane.
StiKalmanTrackNode * StiKalmanTrack::extrapolateToBeam()
{
  StiKalmanTrackNode * innerMostNode = getInnerMostNode();
  //return null if there is no node to extrapolate from.
  if (!innerMostNode) return 0;
  StiKalmanTrackNode * n = trackNodeFactory->getInstance();
  if (n->propagateToBeam(innerMostNode,kOutsideIn)) return n;
  trackNodeFactory->free(n);
  return 0;
}

//_____________________________________________________________________________
StiKalmanTrackNode * StiKalmanTrack::extrapolateToRadius(double radius)
{
  StiKalmanTrackNode * outerMostNode = getOuterMostNode();
  //return null if there is no node to extrapolate from.
  if (!outerMostNode) return 0;
  StiKalmanTrackNode *n = trackNodeFactory->getInstance();
  if (n->propagateToRadius(outerMostNode,radius,kOutsideIn)) return n;
  trackNodeFactory->free(n);
  return 0;
}

//_____________________________________________________________________________
void StiKalmanTrack::add(StiTrackNode * node,int direction,StiTrackNode *near)
{
   
   StiKalmanTrackNode *Node = (StiKalmanTrackNode*)node;
   if (lastNode==0) {
     lastNode = firstNode = Node; return;
   }
   if (direction==0) {
     if (!near) near = lastNode;
     near->add(Node,direction);
     lastNode = Node;
  } else {
     if (!near) near = firstNode;
     near->add(Node,direction);
     firstNode = Node;
  }
}
//_____________________________________________________________________________
void StiKalmanTrack::setFirstLastNode(StiKalmanTrackNode * node)
{
 firstNode = (StiKalmanTrackNode*)node->getFirstNode();
  lastNode = (StiKalmanTrackNode*)node->getLastNode ();
}
//_____________________________________________________________________________
void StiKalmanTrack::removeLastNode()
{
  StiKalmanTrackNode *node = lastNode;
  lastNode = (StiKalmanTrackNode*)node->disconnect();
  BFactory::Free(node);
}
//_____________________________________________________________________________


/**
 * Public interface to protected method capable of returning two return values
 * used in this and derived StiCAKalmanTrack classes.
 */
int StiKalmanTrack::refit()
{
  int errType = kNoErrors;
  
  enum {kMaxIter=30,kPctLoss=10,kHitLoss=3};
  static double defConfidence = StiDebug::dFlag("StiConfidence",0.01);
  int nNBeg = getNNodes(3), nNEnd = nNBeg;
  if (nNBeg<=3) 	return 1;
  if (!mgMaxRefiter) 	return 0;
  StiKalmanTrackNode *inn= getInnerMostNode(3);
  int fail=0,status=0;

  StiNodePars pPrev;
  StiNodeErrs ePrev;
  int iter=0,igor=0;
  double qA;
  double errConfidence = defConfidence;
  for (int ITER=0;ITER<mgMaxRefiter;ITER++) {
    for (iter=0;iter<kMaxIter;iter++) {
      fail = 0;
      errType = kNoErrors;
      sTNH.set(StiKalmanTrackFitterParameters::instance()->getMaxChi2()*10,StiKalmanTrackFitterParameters::instance()->getMaxChi2Vtx()*100,errConfidence,iter);
      pPrev = inn->fitPars();
      ePrev = inn->fitErrs(); 
      
      status = refitL();  
      if (status) 	{fail= 1; errType = kRefitFail; break;}
      nNEnd = sTNH.getUsed();
      if ((nNEnd <=3))	{fail= 2; errType = kNotEnoughUsed; break;}
      if (!inn->isValid() || inn->getChi2()>1000) {
        inn = getInnerMostNode(3); fail=-1; errType = kInNodeNotValid; continue;}	
      qA = StiKalmanTrack::diff(pPrev,ePrev,inn->fitPars(),inn->fitErrs(),igor);
      static int oldRefit = StiDebug::iFlag("StiOldRefit");
      if (oldRefit) {
        if (qA>0.5)		{fail=-2; errType = kBadQA; continue;} 
      } else {
        if (qA <1 && errConfidence>0.1) errConfidence = 0.1;
        if (qA>0.01)		{fail=-2; errType = kBadQA; continue;} 
        if (sTNH.isCutStep())	{fail=-2; errType = kBadQA; continue;} 
      }
      double info[2][8];
      sTNH.mCurvQa.getInfo(info[0]);
      sTNH.mTanlQa.getInfo(info[1]);
      break;
    }
    if (fail>0) 						break;
      //		
    StiKalmanTrackNode *worstNode= sTNH.getWorst();
    if (worstNode && worstNode->getChi2()>StiKalmanTrackFitterParameters::instance()->getMaxChi2())     
    { //worstNode->getHit()->subTimesUsed();
      worstNode->setHit(0); worstNode->setChi2(3e33); continue;}
    if (rejectByHitSet()) { releaseHits()            ;continue;}
    
    if (!fail) 							break;
    
    StiKalmanTrackNode *flipFlopNode= sTNH.getFlipFlop();
    if (flipFlopNode && flipFlopNode->getFlipFlop()>kMaxIter/3)     
    { //flipFlopNode->getHit()->subTimesUsed();
      flipFlopNode->setHit(0); flipFlopNode->setChi2(3e33); 	continue;}
    break;
      //	The last resource
      //    errConfidence = 0.5*(errConfidence+1);
      //    if (errConfidence>0.99) 				break;
  }
  StiKalmanTrackNode *vertexNode= sTNH.getVertexNode();

    //		Test for primary 
  while (!fail && vertexNode) {
    fail = 13;			//prim node invalid
    errType = kVertexNodeInvalid;
    if (!vertexNode->isValid()) 			break;
    fail = 99;			//prim node Chi2 too big
    errType = kNodeNotValid;
    if ( vertexNode->getChi2()>StiKalmanTrackFitterParameters::instance()->getMaxChi2Vtx())	break;
    fail = 98;			//too many dropped nodes
    errType = kTooManyDroppedNodes;    
    if (nNBeg*kPctLoss/100 < nNBeg-nNEnd
        &&  nNEnd+kHitLoss < nNBeg)			break;
    fail = 0;
    errType = kNoErrors;
    break;    
  }
  if (!fail) { //Cleanup. Hits of bad nodes set to zero
    StiKalmanTrackNode *node;
    StiKTNIterator it = begin();
    for (;(node=it());it++){
      if (node == vertexNode)				continue;
      StiHit *hit = node->getHit();
      if(!hit) 						continue;
      if (node->isValid() && node->getChi2()<10000. ) 	continue;
      node->setHit(0);
    }
  }

  if (fail) setFlag(-1);
  return errType;
}
//_____________________________________________________________________________
int StiKalmanTrack::refitL() 
{
static int nCall=0;nCall++;
  StiDebug::Break(nCall);

  StiKTNIterator source;
  StiKalmanTrackNode *pNode = 0,*targetNode;
  int iNode=0, status = 0,isStarted=0;
  sTNH.setDir(1);
  for (source=rbegin();source!=rend();source++) {
    iNode++;
    targetNode = &(*source);

    if (!isStarted) {
      if (!targetNode->getHit()) 	targetNode->setInvalid();		
      if ( targetNode->getChi2()>1000) 	targetNode->setInvalid();
      if (!targetNode->isValid()) 	continue;
    }
    isStarted++;
    sTNH.set(pNode,targetNode);
    status = sTNH.makeFit(0);
    if (status) continue;
    if (!targetNode->isValid()) 	continue;
    pNode = targetNode;
  }//end for of nodes

    pNode = 0; iNode=0;isStarted=0;
  sTNH.setDir(0);
  for (source=begin();source!=end();source++) {
    iNode++;
    targetNode = &(*source);
    if (!isStarted) {
      if (!targetNode->getHit()) 	targetNode->setInvalid();		
      if ( targetNode->getChi2()>1000) 	targetNode->setInvalid();
      if (!targetNode->isValid()) 	continue;
    }
    isStarted++;
    sTNH.set(pNode,targetNode);
    status = sTNH.makeFit(1);
    if (status) 			continue;
    if (!targetNode->isValid()) 	continue;
    pNode = targetNode;
  }//end for of nodes
  return 0;
}
//_____________________________________________________________________________
void StiKalmanTrack::reduce() 
{
  StiKTNIterator source;
  for (source=begin();source!=end();source++) {(*source).reduce();}
}
//_____________________________________________________________________________
void StiKalmanTrack::unset() 
{
  if (!lastNode) return;
  StiKTNIterator source;
  for (source=begin();source!=end();source++) {BFactory::Free(&(*source));}
  lastNode=0; firstNode=0;
}
//_____________________________________________________________________________
void StiKalmanTrack::print(const char *opt) const
{
  printf("Track %p\n",(void*)this);

  StiKTNIterator it;
  int n=0;
  for (it=begin();it!=end();++it) {
    StiKalmanTrackNode *node = &(*it);
    StiHit *hit = node->getHit();
    if (!hit && strchr(opt,'h')) continue;
    if (!hit && strchr(opt,'H')) continue;
    n++;
    printf("%3d - ",n);
    node->print(opt);
  }
}

//_____________________________________________________________________________
int StiKalmanTrack::approx(int mode)
{
  //const double BAD_XI2[2] = {70,5}, XI2_FACT = 1; // Tuned constants
  const double BAD_XI2[2] = {99,22}, XI2_FACT = 9;
  mXi2=0;
  StiHitErrs hr;
  // Loop over nodes and collect global xyz

  StiKTNIterator source;
  StiKalmanTrackNode *targetNode;
  int nNode=0;
  THelixFitter circ;
  THelixTrack  cirl;
  int zeroH = -1;
  for (source = rbegin(); (targetNode = source()); ++source) {
    if (!targetNode->isValid()) 	continue;
    const StiHit * hit = targetNode->getHit();
    if (!hit) 				continue;
    if (targetNode->getChi2()>1000)	continue;
    if (zeroH<0) {//What kind of mag field ?
      double hz = targetNode->getHz();
      zeroH = fabs(hz)<=kZEROHZ;
    }
    circ.Add(hit->x_g(),hit->y_g(),hit->z_g());
    if (mode & kAppRR) {
      hr = targetNode->getGlobalHitErrs(hit);
      circ.AddErr(hr.G(),hr.hZZ);
    }
    nNode++;
  }
  if (!nNode) 				return 1; 
  
  mXi2 =circ.Fit();
  if (mXi2 > BAD_XI2[mode & kAppGud]) return 2; //Xi2 too bad, no updates
  if (zeroH) circ.Set(kZEROCURV);
  if (mode & kAppRR) circ.MakeErrs();

  double s=0,xyz[3]; 
  double curv = circ.GetRho();
  for (source = rbegin(); (targetNode = source()); ++source) {
    if (!targetNode->isValid()) 	continue;
    const StiHit *hit = targetNode->getHit();
    if (hit) {
      xyz[0] = hit->x_g();
      xyz[1] = hit->y_g();
      xyz[2] = hit->z_g();
    } else {
      xyz[0] = targetNode->x_g();
      xyz[1] = targetNode->y_g();
      xyz[2] = targetNode->z_g();
    }
    double ds = circ.Path(xyz[0],xyz[1]);
    circ.Move(ds);
    s+=ds;
    int upd = (mode & kAppUPD);
    upd |= ((mode & kAppUpd) && (targetNode == firstNode));
    if (!upd) continue;
    cirl = circ;
    double alfa = targetNode->getAlpha();
    cirl.Rot(-alfa);
    StiNodePars P = targetNode->fitPars();
    P.x()  =  cirl.Pos()[0];
    P.y()  =  cirl.Pos()[1];
    P.z()  =  cirl.Pos()[2];
    P.eta()  = atan2(cirl.Dir()[1],cirl.Dir()[0]);
    P.curv() = curv;
    double hh = P.hz();
    hh = (fabs(hh)<1e-10)? 0:1./hh;
    P.ptin() = (hh)? curv*hh:1e-3;

    P.tanl() = cirl.GetSin()/cirl.GetCos();
    P._cosCA = cirl.Dir()[0]/cirl.GetCos();
    P._sinCA = cirl.Dir()[1]/cirl.GetCos();
    if (fabs(P._cosCA)>0.99 || fabs(P._sinCA)>0.99) P.ready();

    targetNode->fitPars() = P;
    int ians = targetNode->nudge();
    if(ians) {nNode--; targetNode->setInvalid();continue;}
    if (mode & kAppRR) {
      P = targetNode->fitPars();
      StiNodeErrs &E = targetNode->fitErrs();
      cirl.StiEmx(E.G());
      TCL::vscale(&(E._cPX),hh,&(E._cPX),5);
      E._cPP*=hh; E._cTP*=hh;
      if ((mode & kAppGud) == 0 && mXi2 > XI2_FACT) E*=mXi2/XI2_FACT;
      E.check("In aprox");
    }
  }
  return 0;
}
//_____________________________________________________________________________
double StiKalmanTrack::diff(const StiNodePars &p1,const StiNodeErrs &e1
                           ,const StiNodePars &p2,const StiNodeErrs &e2,int &igor) 
{
  double est=0;
  for (int i=0;i<kNPars;i++) {
    double err = 0.5*(e1(i,i)+e2(i,i));
    if (err<1e-10) continue;
    double dif = pow(p1[i]-p2[i],2)/err;
    if (est<dif) {est = dif; igor = i;}
  }
  return est;
}
//_____________________________________________________________________________
StiKalmanTrack &StiKalmanTrack::operator=(const StiKalmanTrack &tk)
{
  StiTrack::operator=(tk);
  firstNode=0;
  lastNode=0;

  mSeedHitCount=tk.mSeedHitCount; 	//number of points used to seed the track
  mFlag        =tk.mFlag;         	//A flag to pack w/ topo info
  mMass        =tk.mMass;             	// mass hypothesis
  _dca	       =tk._dca;
  _vChi2       =tk._vChi2;		//
  mVertex      =tk.mVertex;
  StiKTNIterator it;
  for (it=tk.begin();it!=tk.end();it++){
    const StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) continue;
    StiKalmanTrackNode *myNode=trackNodeFactory->getInstance();
    *myNode=*node;
    add(myNode,kOutsideIn);
  }
  return *this;
}

//_____________________________________________________________________________
void StiKalmanTrack::setMaxRefiter(int maxRefiter) 
{
  mgMaxRefiter = maxRefiter;
}
//_____________________________________________________________________________
int StiKalmanTrack::rejectByHitSet()  const
{
  StiKalmanTrackNode *node;
  int sum=0;
  for (StiKTNIterator it = rbegin();(node=it());it++){
    if (node->x()>50)		break;
    if (!node->isValid()) 	continue;
    StiHit *hit = node->getHit();
    if (!hit) 			continue;
    if (!hit->detector())	continue;
    if (node->getChi2()>1000) 	continue;
    sum+= StiKalmanTrackFinderParameters::instance()->hitWeight(int(hit->x()));
  }
  if (!sum) return 0;
  return sum < StiKalmanTrackFinderParameters::instance()->sumWeight();
}
//_____________________________________________________________________________
int StiKalmanTrack::releaseHits(double rMin,double rMax)
{
  StiKalmanTrackNode *node;
  int sum=0;
  for (StiKTNIterator it = rbegin();(node=it());it++){
    StiHit *hit = node->getHit();
    if (!hit) 			continue;
    if (!hit->detector())	continue;
    if (hit->x()<rMin)		continue;
    if (hit->x()>rMax)		break;
    sum++;
    node->setHit(0);
  }
  return sum;
}
//_____________________________________________________________________________
void StiKalmanTrack::test(const char *txt) const
{

  for (auto it=begin();it!=end();it++)  {
    StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) continue;
    const StiDetector *det = node->getDetector();
    if  (!det) continue;
    const auto &P = node->fitPars();
    double tst = P[0]*P._cosCA+P[1]*P._sinCA;
    if (tst>=0) continue;
    tst /= sqrt(P[0]*P[0]+P[1]*P[1]);
//    assert (tst>=-1e-5);
StiDebug::Count("OverKill",tst);
  }
}
//_____________________________________________________________________________
//_____________________________________________________________________________
#include "StarRoot/TIdTruUtil.h"
//_____________________________________________________________________________
int StiKalmanTrack::idTruth(int *qa) const
{
  TIdTruUtil ut;
  for (auto it=begin();it!=end();it++)  {
    StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) 	continue;
    const StiHit *hit = node->getHit();
    if (!hit) 			continue;
    if (!node->getDetector())	continue;
    if ( node->getChi2()>100)	continue;
    ut.Add(hit->idTruth(),hit->qaTruth());
  }
  if (qa) *qa = 100*ut.GetQua();  
  return ut.GetIdTru();
}
