#ifndef TrackData_h
#define TrackData_h
/***********************************************
 * $Id: TrackData.h,v 1.11 2017/05/09 12:29:41 smirnovd Exp $
 ******************************************************
 */
#include <TVector3.h>
#include <Sti/StiTrackNode.h>
#include "StMuDSTMaker/COMMON/StMuTrack.h"


#include <vector>
using namespace std;  // for vector

class VertexData ;
class StiKalmanTrack;
class StDcaGeometry;


/// approximtion of track as stright line @ DCA to beamLine=0,0
class DcaTrack
{
public:

  TVector3 R,gP; // position (3*cm), global momentum (3*GeV/c)
  float sigYloc, sigZ;//  error of position(2*cm), local sector ref frame
  StiNodeErrs fitErr; // covariance matrix
  float gChi2; // global track, from Kalman 
  int nFitPoint; 

  void print() {
    printf("#track@DCA(0,0) R/cm=[%5.2f %5.2f %.1f], errYloc=%.2fcm , errZ=%.1fcm,  glob P=[%6.2f %6.2f %6.1f]GeV/c, PT=%.2f\n",R.x(),R.y(),R.z(),sigYloc, sigZ,gP.x(),gP.y(),gP.z(), gP.Pt() );
    printf("   chi2=%f, nFitP=%d,  fitErr: cXX=%f cYX=%f cYY=%f cZX=%f cZY=%f cZZ=%f\n",gChi2, nFitPoint,fitErr._cXX,fitErr._cYX,fitErr._cYY,fitErr._cZX,fitErr._cZY,fitErr._cZZ);
  }

};


class TrackData
{
public:

  /// > 0 if assigned to a good vertex;
  /// = 0 free, not used for any vertex
  int vertexID;

  /// Pointer to original track
  const void* mother;

  const StDcaGeometry* dca;

  short mIdTruth;
  short mQuality;
  int   mIdParentVx;
  DcaTrack dcaTrack; // for 3D vertex reco
  float zDca, ezDca; // (cm) Z of track @ DCA to beam
  float rxyDca;
  float gPt; // (GeV) global
  // 3-stat logic: 1=match, -1=veto, 0=dunno
  int mBtof,mCtb,mBemc,mEemc,mTpc; 
  bool anyMatch,anyVeto;
  float weight; // compound from all maching tests
  int btofBin; // >=0 if track passed through BTOF cell
  int ctbBin;  // >=0 if track passed through CTB slat
  int bemcBin; // >=0 if track passed through BTOW tower
  int eemcBin; // >=0 if track passed through ETOW tower
  // ........................methods

  TrackData() : TrackData(nullptr, nullptr) { }

  TrackData(const void* motherTrack, const StDcaGeometry* motherDca);

  template<class OriginalTrack_t>
  const OriginalTrack_t* getMother() const { return static_cast<const OriginalTrack_t*>(mother); }

  void scanNodes( vector<int> & hitPatt, int jz0);
  bool matchVertex(VertexData &V, float kSig) ;

  /// Calculates chi^2 at track DCA w.r.t. the vertex
  double calcChi2DCA(const VertexData &V) const;

  float getTpcWeight();
  void updateAnyMatch(bool match, bool vet,int & mXXX);
  void print(ostream& os) const;
};


template<class OriginalTrack_t>
class TrackDataT : public TrackData
{
public:

  TrackDataT(const OriginalTrack_t &motherTrack, const StDcaGeometry* trackDca=nullptr) :
    TrackData(&motherTrack, trackDca) { }

  const OriginalTrack_t* getMother() const { return static_cast<const OriginalTrack_t*>(mother); }
};


template<>
TrackDataT<StMuTrack>::TrackDataT(const StMuTrack &motherTrack, const StDcaGeometry* trackDca);


#endif


/*
 * $Log: TrackData.h,v $
 * Revision 1.11  2017/05/09 12:29:41  smirnovd
 * [Cosmetic] Squashed commit of the following:
 *
 * - StPPVertexFinder: Variables renamed for readability
 *    - s/trk/track/
 *    - s/V/vertex/
 *    - s/{nmAny,n1}/nTracksMatchingAnyFastDetector/
 * - VertexData: Changed print out format
 * - Removed some not very helpful debug output
 * - Whitespace and other minor adjustments
 * - Updated doxygen, whitespace, resolved ambiguity
 *    - std::fabs, std::sqrt
 *    - StPPVertexFinder: s/trk/track/
 * - StGenericVertexFinder: Minor changes in InitRun()
 * - StGenericVertexFinder::Fit(...) -> fit(...)
 *
 * Revision 1.10  2017/05/03 20:14:27  smirnovd
 * Added overlooked TrackDataT<StMuTrack> constructor definition
 *
 * Revision 1.9  2017/03/21 15:15:01  smirnovd
 * Revert "TrackDataT: Removed erroneous inlined definition for constructor"
 *
 * This reverts commit 96e7f20e48eed3c52eb272b27740ff190ce1b9fc.
 *
 * Revision 1.7  2017/03/15 22:56:50  smirnovd
 * TrackDataT: Added specialization for template constructor
 *
 * Revision 1.6  2017/03/15 22:56:44  smirnovd
 * TrackData: Introduce deligating constructor
 *
 * Revision 1.5  2017/03/02 19:11:19  smirnovd
 * Squashed commit of many assorted changes
 *
 * See a704cb55..cb1f57fa on master for details
 *
 * Revision 1.4  2017/02/21 21:34:22  smirnovd
 * Enhanced proxy data structures for track and vertex
 *
 * For details see commits on master branch cdc758df..49016672
 *
 * - Update comments & doxygen, removed commented code
 * - TrackDataT: New helper for TrackData to manipulate original mother track
 * - TrackData: Don't limit track's mother to specific type
 *   - This proxy track can be created from other than StiKalmanTrack type, e.g.  MuDstTrack
 * - VertexData: Added member to flag triggered vertex
 * - TrackData: Added method to calculate chi2 w.r.t. a vertex
 * - TrackData: Added member pointer to DCA geometry
 * - TrackData: Added print() method
 * - TrackData & VertexData: Added fields with simulation data
 *
 * Revision 1.3  2010/09/10 21:08:35  rjreed
 * Added function UseBOTF and bool mUseBtof to switch the use of the TOF on and off in vertex finding.  Default value is off (false).
 * Added functions, and variables necessary to use the TOF in PPV for vertex finding.  Includes matching tracks to the TOF and changing the track weight based on its matched status with the TOF.
 *
 * Revision 1.2  2009/07/09 21:29:03  balewski
 * allow export of prim tracks for 3D beam line fit (use VtxSeedCalG option),
 * oneTrack vertex thresholds was lowered form 15 to 10 GeV/c
 *
 * Revision 1.1  2005/07/11 20:38:12  balewski
 * PPV added for real
 *

 *
 *
 *********************************************************************/
