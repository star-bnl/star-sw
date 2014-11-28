/********************************************************************** 
 *
 * $Id: StEStructOneBuffer.cxx,v 1.2 2011/08/02 20:34:03 prindle Exp $
 *
 * Author: Duncan Prindle
 *
 **********************************************************************
 *
 * Description:  Data buffer to hold events for mixing in case where events are presorted.
 *
 *  The algorithm is such that looping over events eventually returns the element,
 *        mEvent[MAXBUFFERSIZE]=NULL  
 *
 *  The buffer is filled from [0] to [MAX.. -1] and numEvent counter
 *  then stays and MAX-1 (so it should be called 'numEventIndex').
 *  Once filled, event in [MAX-1] is deleted, all events are moved
 *  'down' in the array and current event is added at [0].
 *
 *  To include a delta-N cut (mixed event multiplicity difference), I needed
 *  to increase the buffer size.  So now the actual size is defined by MAXBUFFERSIZE,
 *  and _MAXEBYEBUFFER_ is the number of mixed events we will try to make.
 *
 *  The delta-N cut is currently hard-coded to 100.
 *
 ***********************************************************************/
#include "StEStructOneBuffer.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructMaxB.h"

ClassImp(StEStructOneBuffer);

StEStructOneBuffer::StEStructOneBuffer(int nMix, int deltaMultMax, float deltaZMax, float deltaRateMax) {
    mNumMixed = nMix;
    mcurEvent = -1;
    mDeltaMultMax = deltaMultMax;
    mDeltaZMax = deltaZMax;
    mDeltaRateMax = deltaRateMax;
    mEvent=new StEStructEvent*[mNumMixed+1];
    for (int i=0;i<=mNumMixed;i++) {
        mEvent[i] = NULL;
    }
}


StEStructOneBuffer::~StEStructOneBuffer() {
    for (int i=0;i<=mNumMixed;i++) {
        if (mEvent[i]) {
            delete mEvent[i];
        }
    }
    delete [] mEvent;
}

void StEStructOneBuffer::addEvent(StEStructEvent* event) {
    if (!event) {
        return;
    }
    if (mEvent[mNumMixed-1]) {
        delete mEvent[mNumMixed-1];
    }
    for (int i=mNumMixed-1;i>0;i--) {
        mEvent[i] = mEvent[i-1];
    }
    mEvent[0] = event;
}

StEStructEvent* StEStructOneBuffer::nextEvent(int mult, float vz, float coinc) {
    // Return next event within mDeltaMultMax and mDeltaZMax of input.
    mcurEvent++;
    while(mEvent[mcurEvent]) {
        if(abs(mult - mEvent[mcurEvent]->Ntrack()) <= mDeltaMultMax &&
           fabs(vz-mEvent[mcurEvent]->VertexZ()) <= mDeltaZMax &&
           fabs(coinc-mEvent[mcurEvent]->ZDCCoincidence()) <= mDeltaRateMax) {
            break;
        }
        mcurEvent++;
        if(mcurEvent > mNumMixed) {
            // This shouldn't happen, since mEvent[mNumMixed] == 0.
            return 0;
        }
    }
    return mEvent[mcurEvent];
}


/***********************************************************************
 *
 * $Log: StEStructOneBuffer.cxx,v $
 * Revision 1.2  2011/08/02 20:34:03  prindle
 *   More detailed histograms for event mixing.
 *   Buffer: increased mixed events to 4 (from 2)
 *   CutBin: added mode 9 for exploration of p_t space, fixed place in mode 5 where
 *           histogram was written before checking it existed.
 *   OneBuffer: added ZDC coincidence rate to event sorting space.
 *
 * Revision 1.1  2010/09/02 21:54:03  prindle
 *   OneBuffer: When we sort on z-vertex and multiplicity we can use a single event mixing buffer.
 *
 * Revision 1.5  2006/04/04 22:10:11  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.4  2005/09/14 17:14:22  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.3  2005/03/28 22:59:08  porter
 * I opened a memory leak on last ci due to forgetting how StEStructBuffer
 * actually worked! This is now fixed with explaination in description.
 *
 * Revision 1.2  2005/03/03 01:30:43  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/





