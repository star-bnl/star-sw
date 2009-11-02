/***************************************************************************
 *
 * $Id: StTRGReader.h,v 1.8 2009/11/02 21:42:52 fine Exp $
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for TRG reader class
 *
 ***************************************************************************
 *
 * $Log: StTRGReader.h,v $
 * Revision 1.8  2009/11/02 21:42:52  fine
 * allow the Akio DAT file to be read in 1999-2008 format
 *
 * Revision 1.7  2007/11/19 19:40:11  akio
 * Change fro run8
 *
 * Revision 1.6  2007/02/22 22:18:41  akio
 * Update for 2007
 *
 * Revision 1.5  2004/11/03 17:55:28  akio
 * update for fy05 run
 *
 * Revision 1.4  2003/12/23 23:17:10  akio
 * Update for Fy04 run
 *
 * Revision 1.3  2003/07/16 19:58:29  perev
 * Cleanup of StTriggerData2003 at all
 *
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
#include "StDaqLib/TRG/trgStructures2000.h"
#include "StDaqLib/TRG/trgStructures2003.h"
#include "StDaqLib/TRG/trgStructures2004.h"
#include "StDaqLib/TRG/trgStructures2005.h"
#include "StDaqLib/TRG/trgStructures2007.h"
#include "StDaqLib/TRG/trgStructures2008.h"
#endif /*__CINT__*/

#include "StDaqLib/TRG/TRG_Reader.hh"

class EventReader;
class StDAQReader;

class  StTRGReader {
  public:

  StTRGReader(StDAQReader *rd);
  char thereIsTriggerData(); // returns FALSE if there is no trigger data in the .daq file
  virtual ~StTRGReader();
  virtual  int close();
  int   getYear() const;

  const char            *getData()         const;
  const TrgDataType2000 *getDataType2000() const;
  const TrgDataType2003 *getDataType2003() const;
  const TrgDataType2004 *getDataType2004() const;
  const TrgDataType2005 *getDataType2005() const;
  const TrgDataType2007 *getDataType2007() const;
  const TrgDataType2008 *getDataType2008() const;

  virtual void Update();
  enum { kTrgOldVersionFormat=0};
  static bool OldFormat(int version) { return (version <= kTrgOldVersionFormat); }
protected:
  TRG_Reader  *fTRGImpReader; // Making this protected hide implementation from the user

  StDAQReader *fDAQReader;
};
#endif
