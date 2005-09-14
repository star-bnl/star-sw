/**********************************************************************
 *
 * $Id: StEStructBuffer.h,v 1.2 2005/09/14 17:14:23 msd Exp $
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

#define MAXBUFFERSIZE 10  // now the actual buffer size
#define DELTANMAX 100     // max multiplicity difference in mixed events

class TFile;
class StEStructEvent;
class TH1F;

class StEStructBuffer {

  StEStructEvent** mEvent;
  int mnumEvents;  // total number of events in buffer
  int mcurEvent;   // index to current event 
  int mnumMixed;   // number of successfully returned events

 public:

  StEStructBuffer();
  virtual ~StEStructBuffer();

  StEStructEvent* nextEvent(int mult);
  void addEvent(StEStructEvent* event);
  void resetCounter();

  void Print();  // useful for testing and debugging

};


inline void StEStructBuffer::resetCounter(){ mcurEvent=-1; mnumMixed=0; };

#endif
/***********************************************************************
 *
 * $Log: StEStructBuffer.h,v $
 * Revision 1.2  2005/09/14 17:14:23  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

