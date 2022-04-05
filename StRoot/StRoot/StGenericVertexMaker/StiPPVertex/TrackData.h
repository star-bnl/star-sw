#ifndef TrackData_h
#define TrackData_h

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
