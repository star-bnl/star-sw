/**********************************************************************
 *
 * $Id: StEStructBuffer.h,v 1.3 2006/04/04 22:10:12 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Data buffer to hold events for mixing per z-vertex
 *
 *
 ***********************************************************************/
#ifndef STEBYEBUFFER_H
#define STEBYEBUFFER_H

#include "StEStructBinning.h"

#define MAXBUFFERSIZE 2  // now the actual buffer size
#define DELTANMAX 100     // max multiplicity difference in mixed events

class TFile;
class StEStructEvent;
class TH1F;

class StEStructBuffer {

  StEStructEvent** mEvent;
  int mnumEvents;  // total number of events in buffer
  int mcurEvent;   // index to current event 
  int mnumMixed;   // number of successfully returned events
  int mnumDeleted;
  int mnumInput; 

 public:

  StEStructBuffer();
  virtual ~StEStructBuffer();

  StEStructEvent* nextEvent(int mult);
  void addEvent(StEStructEvent* event);
  void resetCounter();
  int  numEventsIn();
  int  numEventsDeleted();

  void Print();  // useful for testing and debugging

};


inline void StEStructBuffer::resetCounter(){ mcurEvent=-1; mnumMixed=0; };

inline int StEStructBuffer::numEventsIn(){ return mnumInput; };
inline int StEStructBuffer::numEventsDeleted(){ return mnumDeleted; };


#endif
/***********************************************************************
 *
 * $Log: StEStructBuffer.h,v $
 * Revision 1.3  2006/04/04 22:10:12  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.2  2005/09/14 17:14:23  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

