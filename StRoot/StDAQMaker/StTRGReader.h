/***************************************************************************
 *
 * $Id: StTRGReader.h,v 1.1 2000/01/24 20:35:37 ward Exp $
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for TRG reader class
 *
 ***************************************************************************
 *
 * $Log: StTRGReader.h,v $
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

  friend class StDAQReader;  // Not sure what good this does.
  StTRGReader(StDAQReader *rd);
  char thereIsTriggerData(); // returns FALSE if there is no trigger data in the .daq file
  virtual ~StTRGReader();
  virtual  int close();
  TRG_Reader  *fTRGImpReader; // Making this public saves a double layer of accessor functions.

protected:
  virtual void Update();

  StDAQReader *fDAQReader;
};
#endif
