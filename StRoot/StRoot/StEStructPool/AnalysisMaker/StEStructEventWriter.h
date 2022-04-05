/**********************************************************************
 *
 * $Id: StEStructEventWriter.h,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  StEStructAnalysis implementation which writes EStruct
 *               MuDsts using the StEStructEventMaker class
 *
 ***********************************************************************/
#ifndef _STESTRUCTEVENTWRITER_H
#define _STESTRUCTEVENTWRITER_H


#include "StEStructAnalysis.h"

class StEStructEventMaker;

class StEStructEventWriter : public StEStructAnalysis {

 protected:

  StEStructEventMaker* myMaker;

 public:
   
  StEStructEventWriter(StEStructEventMaker* maker){ myMaker=maker; };
  virtual ~StEStructEventWriter(){};

  virtual void setOutputFileName(const char* fname);
  virtual bool doEvent(StEStructEvent* e);
  virtual void finish();

  ClassDef(StEStructEventWriter,1)

};


#endif
/***********************************************************************
 *
 * $Log: StEStructEventWriter.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
