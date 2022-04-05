#ifndef TrackData_h
#define TrackData_h
/***********************************************
 * $Id: TrackData.h,v 1.1 2013/08/16 22:19:56 perev Exp $
 ******************************************************
 */
#include <TVector3.h>
#include <StEvent/StTrackNode.h>


#include <vector>
class StGlobalTrack;
namespace StEvPPV {

class MyNodeErrs
{
public:
union{double A[1];double _cXX;};
  double _cYX,_cYY;                       
  double _cZX,_cZY, _cZZ;                 
  double _cEX,_cEY, _cEZ, _cEE;           
  double _cPX,_cPY, _cPZ, _cPE, _cPP;     
  double _cTX,_cTY, _cTZ, _cTE, _cTP, _cTT;
};  

class VertexData ;

class DcaTrack 
{ // approximtion of track as stright line @ DCA to beamLine=0,0
 public:
  TVector3 R,gP; // position (3*cm), global momentum (3*GeV/c)
  float sigYloc, sigZ;//  error of position(2*cm), local sector ref frame
//VP  MyNodeErrs fitErr; // covariance matrix
  float gChi2; // global track, from Kalman 
  int nFitPoint; 
//   void print() { printf("#track@DCA(0,0) R/cm=[%5.2f %5.2f %.1f], errYloc=%.2fcm , errZ=%.1fcm,  glob P=[%6.2f %6.2f %6.1f]GeV/c, PT=%.2f\n",R.x(),R.y(),R.z(),sigYloc, sigZ,gP.x(),gP.y(),gP.z(), gP.Pt() );
//                  printf("   chi2=%f, nFitP=%d,  fitErr: cXX=%f cYX=%f cYY=%f cZX=%f cZY=%f cZZ=%f\n",gChi2, nFitPoint,fitErr._cXX,fitErr._cYX,fitErr._cYY,fitErr._cZX,fitErr._cZY,fitErr._cZZ); }
  
};


class TrackData {
 public: 
  int vertexID; /* >0 if assigned to a good vertex; 
		   =0 free, not used for any vertex
		*/
  const StGlobalTrack* mother; // oryginal track
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
  TrackData();
  void scanNodes( vector<int> & hitPatt, int jz0);
  bool matchVertex(VertexData &V, float kSig) ;
  float getTpcWeight();
  void updateAnyMatch(bool match, bool vet,int & mXXX);
};
}// end namespace StEvPPV

#endif


/*
 * $Log: TrackData.h,v $
 * Revision 1.1  2013/08/16 22:19:56  perev
 * PPV with only StEvent dependency
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
