/**********************************************************************
 *
 * $Id: StEStructBuffer.cxx,v 1.1 2003/10/15 18:20:46 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Data buffer to hold events for mixing per z-vertex
 *
 *
 ***********************************************************************/
#include "StEStructBuffer.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"

#define _MAXEBYEBUFFER_ 3

StEStructBuffer::StEStructBuffer(){

  mnumEvents=0;
  mEvent=new StEStructEvent*[_MAXEBYEBUFFER_];
  for(int i=0;i<_MAXEBYEBUFFER_;i++)mEvent[i]=NULL;
  resetCounter();

}


StEStructBuffer::~StEStructBuffer(){
    
  for(int i=0;i<mnumEvents;i++)delete mEvent[i];
  delete [] mEvent;

}

void StEStructBuffer::addEvent(StEStructEvent* event){

  if(!event)return;
  if(mnumEvents==_MAXEBYEBUFFER_-1){
    if(mEvent[mnumEvents-1])delete mEvent[mnumEvents-1];
  } else {
    mnumEvents++;
  }
  for(int i=mnumEvents-1;i>0;i--)mEvent[i]=mEvent[i-1];
  mEvent[0]=event;

}

/***********************************************************************
 *
 * $Log: StEStructBuffer.cxx,v $
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/





