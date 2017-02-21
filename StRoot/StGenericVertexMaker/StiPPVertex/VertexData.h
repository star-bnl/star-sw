#ifndef VertexData_h
#define VertexData_h
/*********************************************************************
 * $Id: VertexData.h,v 1.7 2017/02/21 21:34:22 smirnovd Exp $
 *********************************************************************
 * full description of found vertex
 */

#include <TVector3.h>

class VertexData {
 public:
  int id; // vertex ID assigned by PPV
  bool isTriggered; ///< Indicates whether the vertex potentially belongs to triggered event
  short mIdTruth;
  TVector3 r,er; // vertex position and its error
  int nUsedTrack; // # of tracks used to identify the vertex
  float Lmax; // maximum of the likelhood function.
  float gPtSum; // total tranverse momentum of used tracks.
  int nBtof,nCtb,nBemc,nEemc,nTpc,nAnyMatch; // number of matched tracks
  int nBtofV,nCtbV,nBemcV,nEemcV,nTpcV,nAnyVeto; // number of vetoed tracks
  
  // methods
  VertexData(int vertexId=0);
  VertexData(const TVector3& position);
  void print(ostream& os) const;
};
#endif


/*
 * $Log: VertexData.h,v $
 * Revision 1.7  2017/02/21 21:34:22  smirnovd
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
 * Revision 1.6  2017/01/06 21:01:58  smirnovd
 * VertexData: Initialize vertexID in constructor
 *
 * Revision 1.5  2016/11/07 21:19:47  smirnovd
 * VertexData: Added constructor to build vertex with coordinates
 *
 * Revision 1.4  2016/11/07 21:19:27  smirnovd
 * Added and reworded some doxygen and other comments
 *
 * Also cleaned up not-so-useful comments
 *
 * Revision 1.3  2010/09/10 21:08:35  rjreed
 * Added function UseBOTF and bool mUseBtof to switch the use of the TOF on and off in vertex finding.  Default value is off (false).
 * Added functions, and variables necessary to use the TOF in PPV for vertex finding.  Includes matching tracks to the TOF and changing the track weight based on its matched status with the TOF.
 *
 * Revision 1.2  2005/08/30 22:08:43  balewski
 * drop '*' from declaration of   mTrackData &  mVertexData
 *
 * Revision 1.1  2005/07/11 20:38:13  balewski
 * PPV added for real
 *

 *
 *
 *********************************************************************/
