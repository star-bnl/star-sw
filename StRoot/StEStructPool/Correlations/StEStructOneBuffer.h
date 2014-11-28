/**********************************************************************
 *
 * $Id: StEStructOneBuffer.h,v 1.2 2011/08/02 20:34:03 prindle Exp $
 *
 * Author: Duncan Prindle 
 *
 **********************************************************************
 *
 * Description:  Data buffer to hold events for mixing.
 *                   Expect events to be pre-sorted in (multiplicity,z_vertex) so we 
 *
 *
 ***********************************************************************/
#ifndef STESTRUCTONEBUFFER__H
#define STESTRUCTONEBUFFER__H

#include "TROOT.h"
class StEStructEvent;

class StEStructOneBuffer {
  public:
    StEStructOneBuffer(int nMix, int deltaMultMax, float deltaZMax, float deltaRateMax);
    virtual ~StEStructOneBuffer();

    void resetCounter();
    void addEvent(StEStructEvent* event);
    StEStructEvent* nextEvent(int mult, float vz, float coinc);

    StEStructEvent** mEvent;
    int mNumMixed;  // number of events to mix
    int mcurEvent;   // index to current event 
    int mDeltaMultMax;
    float mDeltaZMax;
    float mDeltaRateMax;

    ClassDef(StEStructOneBuffer,1)
};


inline void StEStructOneBuffer::resetCounter() {
    mcurEvent=-1;
};


#endif
/***********************************************************************
 *
 *
 *********************************************************************/

