/**********************************************************************
 *
 * $Id: StEStructBuffer.cxx,v 1.3 2005/03/28 22:59:08 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Data buffer to hold events for mixing per z-vertex
 *
 *  The algorithm is such that looping over events ceases cause the element,
 *        mEvent[_MAXEBYEBUFFER_]=NULL  
 *
 *  The buffer is filled from [0] to [_MAX.._ -1] and numEvent counter
 *  then stays and _MAX_-1 (so it should be called 'numEventIndex').
 *  Once filled, event in [_MAX_-1] is deleted, all events are moved
 *  'down' in the array and current event is added at [0].
 *
 *
 ***********************************************************************/
#include "StEStructBuffer.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructMaxB.h"


StEStructBuffer::StEStructBuffer(){

  mnumEvents=0;
  mEvent=new StEStructEvent*[_MAXEBYEBUFFER_+1];
  for(int i=0;i<=_MAXEBYEBUFFER_;i++)mEvent[i]=NULL;
  resetCounter();

}


StEStructBuffer::~StEStructBuffer(){
    
  for(int i=0;i<mnumEvents;i++)delete mEvent[i];
  delete [] mEvent;

}

void StEStructBuffer::addEvent(StEStructEvent* event){

  if(!event)return;
  if(mnumEvents==_MAXEBYEBUFFER_-1){
    if(mEvent[mnumEvents]){ 
      delete mEvent[mnumEvents];
      mEvent[mnumEvents]=NULL;
    }
  } else {
    mnumEvents++;
  }
  for(int i=mnumEvents-1;i>0;i--)mEvent[i]=mEvent[i-1];
  mEvent[0]=event;

}

/***********************************************************************
 *
 * $Log: StEStructBuffer.cxx,v $
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





