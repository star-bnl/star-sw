/**********************************************************************
 *
 * $Id: StEStructEventWriter.cxx,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  StEStructAnalysis implementation which writes EStruct
 *               MuDsts using the StEStructEventMaker class
 *
 ***********************************************************************/
#include "StEStructEventWriter.h"

#include "StEStructPool/EventMaker/StEStructEventMaker.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"


void StEStructEventWriter::setOutputFileName(const char* fname){
  if(!myMaker) return;
  myMaker->setOutputFile(fname);
}

void StEStructEventWriter::finish(){

  if(myMaker) myMaker->Finish();

};


bool StEStructEventWriter::doEvent(StEStructEvent* e){
  return myMaker->writeEvent(e);
}

/***********************************************************************
 *
 * $Log: StEStructEventWriter.cxx,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
