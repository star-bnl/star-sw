/**********************************************************************
 *
 * $Id: StEStructBuffer.h,v 1.1 2003/10/15 18:20:46 porter Exp $
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


class TFile;
class StEStructEvent;
class TH1F;

class StEStructBuffer {

  StEStructEvent** mEvent;
  int mnumEvents;
  int mcurEvent;
  
 public:

  StEStructBuffer();
  virtual ~StEStructBuffer();

  StEStructEvent* nextEvent();
  void addEvent(StEStructEvent* event);
  void resetCounter();

};

inline StEStructEvent* StEStructBuffer::nextEvent(){
  mcurEvent++;
  return mEvent[mcurEvent];
}

inline void StEStructBuffer::resetCounter(){ mcurEvent=-1; };

#endif
/***********************************************************************
 *
 * $Log: StEStructBuffer.h,v $
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

