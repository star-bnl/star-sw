/***************************************************************************  
 *  
 * $Id: CutRc.h,v 1.1 2002/04/02 20:05:17 jklay Exp $  
 *  
 * Author: Bum Choi, UT Austin, Apr 2002  
 *  
 ***************************************************************************  
 *  
 * Description:  Class for making standardized cuts in highpt analysis 
 *               
 *               
 ***************************************************************************
 *
 * $Log: CutRc.h,v $
 * Revision 1.1  2002/04/02 20:05:17  jklay
 * Bums analysis tools for highpt uDSTs
 *
 * 
 **************************************************************************/
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

  static bool AcceptVertexZ(StHiMicroEvent*);

  static bool Accept(StHiMicroTrack*);
  static bool AcceptNoEta(StHiMicroTrack*);
  static bool AcceptFitPts(StHiMicroTrack*);
  static bool AcceptEta(StHiMicroTrack*);
  static bool AcceptSDcaGl(StHiMicroTrack*);

};



#endif
