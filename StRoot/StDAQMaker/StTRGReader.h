/***************************************************************************
 *
 * $Id: StTRGReader.h,v 1.2 2000/06/12 15:04:02 perev Exp $
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for TRG reader class
 *
 ***************************************************************************
 *
 * $Log: StTRGReader.h,v $
 * Revision 1.2  2000/06/12 15:04:02  perev
 * SVT + cleanup
 *
 * Revision 1.1  2000/01/24 20:35:37  ward
 * Access trigger data.
 *
 *
 **************************************************************************/
#ifndef _StTRGReader_
#define _StTRGReader_

#ifndef __CINT__
#include "StDaqLib/GENERIC/EventReader.hh"
#endif /*__CINT__*/

#include "StDaqLib/TRG/TRG_Reader.hh"

class  EventReader;
class StDAQReader;

class  StTRGReader {
  public:

  StTRGReader(StDAQReader *rd);
  char thereIsTriggerData(); // returns FALSE if there is no trigger data in the .daq file
  virtual ~StTRGReader();
  virtual  int close();
  TRG_Reader  *fTRGImpReader; // Making this public saves a double layer of accessor functions.

  virtual void Update();
protected:

  StDAQReader *fDAQReader;
};
#endif
