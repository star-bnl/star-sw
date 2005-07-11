#ifndef TrackData_h
#define TrackData_h
/***********************************************
 * $Id: TrackData.h,v 1.1 2005/07/11 20:38:12 balewski Exp $
 ******************************************************
 */

#include <vector>
using namespace std;  // for vector

class VertexData ;

class TrackData {
 public: 
  int vertexID; /* >0 if assigned to a good vertex; 
		   <0 if to pileup vertex
		   =0 free, not used for any vertex
		*/
  
  float zDca, ezDca; // (cm) Z of track @ DCA to beam
  float rxyDca;
  float gPt; // (GeV) global
  // 3-stat logic: 1=match, -1=veto, 0=dunno
  int mCtb,mBemc,mEemc,mTpc; 
  bool anyMatch,anyVeto;
  float weight; // compound from all maching tests
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
#endif


/*
 * $Log: TrackData.h,v $
 * Revision 1.1  2005/07/11 20:38:12  balewski
 * PPV added for real
 *

 *
 *
 *********************************************************************/
