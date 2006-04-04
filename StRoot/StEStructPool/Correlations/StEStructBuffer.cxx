/**********************************************************************
 *
 * $Id: StEStructBuffer.cxx,v 1.5 2006/04/04 22:10:11 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Data buffer to hold events for mixing per z-vertex
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
#include "StEStructBuffer.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructMaxB.h"


StEStructBuffer::StEStructBuffer(){

  mnumEvents=mnumDeleted=mnumInput=0;
  mEvent=new StEStructEvent*[MAXBUFFERSIZE+1];
  for(int i=0;i<=MAXBUFFERSIZE;i++)mEvent[i]=NULL;
  resetCounter();

}


StEStructBuffer::~StEStructBuffer(){
    
  for(int i=0;i<=mnumEvents;i++)delete mEvent[i];
  delete [] mEvent;

}

void StEStructBuffer::addEvent(StEStructEvent* event){

  if(!event)return;
  if(mnumEvents==MAXBUFFERSIZE-1){
    if(mEvent[mnumEvents]){ 
      delete mEvent[mnumEvents];
      mEvent[mnumEvents]=NULL;
      mnumDeleted++;
    }
  } else {
    mnumEvents++;
  }
  //for(int i=mnumEvents-1;i>0;i--)mEvent[i]=mEvent[i-1];
  for(int i=mnumEvents;i>0;i--)mEvent[i]=mEvent[i-1];
  mEvent[0]=event;
  mnumInput++;

}

StEStructEvent* StEStructBuffer::nextEvent(int mult){
  // Searches for an event with multiplicity difference from mult less than DELTANMAX
  // Finish if number of returned events = _MAXEBYEBUFFER_ or we run out of buffer

  if(mnumMixed==_MAXEBYEBUFFER_) return NULL;  // are we already done?
  mcurEvent++;
  while(mEvent[mcurEvent]) {
    if(abs(mult - mEvent[mcurEvent]->Ntrack()) <= DELTANMAX) break;  
    mcurEvent++;
    if(mcurEvent>MAXBUFFERSIZE) cout << "   *** ERROR ***: BUFFER OVERFLOW" << endl;  // should never happen
  }
  mnumMixed++;
  return mEvent[mcurEvent];

}


void StEStructBuffer::Print(){
  for(int i=0;i<=MAXBUFFERSIZE;i++) {
    if (mEvent[i]) cout << i <<": " << mEvent[i]->Ntrack() << "\t";
    else cout <<  i <<": NULL" << "\t";
  }
  cout << endl;

}

/***********************************************************************
 *
 * $Log: StEStructBuffer.cxx,v $
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





