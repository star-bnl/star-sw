#ifndef CutMc_H
#define CutMc_H

#include "Cut.h"

class StMiniMcEvent;
class StTinyRcTrack;
class StTinyMcTrack;
class StMiniMcPair;

class CutMc : public Cut{
 public:

  static bool AcceptTrackHalf(StTinyRcTrack*,float);

  //static bool AcceptFlow(StMiniMcEvent*);
  //static bool AcceptZdc(StMiniMcEvent*);

  static bool Accept(StMiniMcEvent*);
  static bool AcceptCent(StMiniMcEvent*);
  static bool AcceptFlowCent(StMiniMcEvent*);
  static bool AcceptZdcCtbCent(StMiniMcEvent*);
  static bool AcceptHMinusCent(StMiniMcEvent*);
  static bool AcceptNchCent(StMiniMcEvent*);

  static bool AcceptVertexZ(StMiniMcEvent*);

  static bool Accept(StMiniMcPair*);
  static bool AcceptNoEta(StTinyRcTrack*);
  static bool AcceptFitPts(StTinyRcTrack*);
  static bool AcceptEtaPr(StTinyRcTrack*);
  static bool AcceptSDcaGl(StTinyRcTrack*);
  
  static bool AcceptEtaMc(StTinyMcTrack*);
  static bool AcceptEtaMcTight(StTinyMcTrack*);
  static bool AcceptMcPts(StTinyMcTrack*);

};  



#endif
