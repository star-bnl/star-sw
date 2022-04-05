#ifndef CutRc_H
#define CutRc_H

#include "Cut.h"
#include "StHiMicroEvent/StHiMicroEvent.h"

class CutRc :public Cut {
 public:
  
  static bool AcceptTrackHalf(StHiMicroTrack*,float);

  //  static bool AcceptFlow(StHiMicroEvent*);
  // static bool AcceptZdc(StHiMicroEvent*);
  
  static bool Accept(StHiMicroEvent*);
  static bool AcceptCent(StHiMicroEvent*);
  static bool AcceptFlowCent(StHiMicroEvent*);
  static bool AcceptZdcCtbCent(StHiMicroEvent*);
  static bool AcceptHMinusCent(StHiMicroEvent*);
  static bool AcceptNchCent(StHiMicroEvent*);

  static bool AcceptVertexZ(StHiMicroEvent*);

  static bool Accept(StHiMicroTrack*);
  static bool AcceptNoEta(StHiMicroTrack*);
  static bool AcceptFitPts(StHiMicroTrack*);
  static bool AcceptEta(StHiMicroTrack*);
  static bool AcceptSDcaGl(StHiMicroTrack*);

};



#endif
