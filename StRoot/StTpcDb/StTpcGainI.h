/***************************************************************************
 *
 * $Id: StTpcGainI.h,v 1.5 1999/12/16 22:00:54 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for TPC gains  
 *
 ***************************************************************************
 *
 * $Log: StTpcGainI.h,v $
 * Revision 1.5  1999/12/16 22:00:54  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STTPCGAINI__
#define __STTPCGAINI__
#include <TObject.h>
#include "StTpcPadPlaneI.h"

class StTpcGainI : public TObject {

  //Abstract base class defining accessors
public:

  virtual float getGain(int row, int pad)   const = 0;
  virtual float getOnlineGain(int row, int pad) const = 0;
  virtual float getNominalGain(int row, int pad) const = 0;
  virtual float getRelativeGain(int row, int pad) const = 0;
  virtual float getAverageGainInner(int sector) const = 0;
  virtual float getAverageGainOuter(int sector) const = 0;
  virtual void SetPadPlanePointer(StTpcPadPlaneI* ppin) = 0;

  ClassDef(StTpcGainI,0)

};

#endif














