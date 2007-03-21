/***************************************************************************
 *
 * $Id: StSvtHybridObject.hh,v 1.2 2007/03/21 17:22:20 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridObject.hh,v $
 * Revision 1.2  2007/03/21 17:22:20  fisyak
 * Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
 *
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDOBJECT_HH
#define STSVTHYBRIDOBJECT_HH

#include "StObject.h"
#include "StSvtHybrid.h"

class StSvtHybridObject: public StObject, public StSvtHybrid {
 public:
  StSvtHybridObject(int barrel = 0, int ladder = 0, int wafer = 0, int hybrid = 0) : StObject(), StSvtHybrid(barrel, ladder, wafer, hybrid) {}
  void SetName();
  virtual ~StSvtHybridObject() {}
  ClassDef(StSvtHybridObject,1)
};
#endif
