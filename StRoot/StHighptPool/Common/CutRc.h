#ifndef CutRc_H
#define CutRc_H

#include "Cut.h"
#include "Centrality.h"
#include "StHighptPool/StHiMicroEvent/StHiMicroEvent.h"

class CutRc :public Cut {
 public:
  
  static bool Accept(StHiMicroEvent*);
  static bool AcceptTrgWord(StHiMicroEvent*);
  static bool AcceptCent(StHiMicroEvent*);
  static bool AcceptFlowCent(StHiMicroEvent*);
  static bool AcceptZDCCent(StHiMicroEvent *);
  static bool AcceptVertexZ(StHiMicroEvent*);
  static bool AcceptEastSideVertexZ(StHiMicroEvent*);
  static bool AcceptWestSideVertexZ(StHiMicroEvent*);

  static bool Accept(StHiMicroTrack*);
  static bool AcceptNoEta(StHiMicroTrack*);
  static bool AcceptNoDca(StHiMicroTrack*);
  static bool AcceptFitPts(StHiMicroTrack*);
  static bool AcceptEta(StHiMicroTrack*);
  static bool AcceptSDcaGl(StHiMicroTrack*);
  static bool AcceptFirstPadrow(StHiMicroTrack*);
  static bool AcceptSameSector(StHiMicroTrack*);
  static bool AcceptEastSideTrack(StHiMicroTrack*);
  static bool AcceptWestSideTrack(StHiMicroTrack*);

  // for east/west require the vertex to be on the same side
  // for same side, just require the last possible sector	--this is just a central membrane cut
  // and the first possible sector to be on the same side
  static bool AcceptTrackVtxZHalf(StHiMicroTrack*,Float_t);	//To determine if track and vertex are on same side
  static bool AcceptTrackHalf(StHiMicroTrack*);	//To determine if track is only on one side or the other

};

#endif
