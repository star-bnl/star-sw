/**********************************************************************
 *
 * $Id: StEStructAnalysis.h,v 1.1 2003/10/15 18:20:31 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Abstract analysis class
 *
 ***********************************************************************/
#ifndef _STEBYEANALYSIS_H
#define _STEBYEANALYSIS_H

#include "TROOT.h"

class StEStructEvent;

class StEStructAnalysis {

 public:

  StEStructAnalysis();
  virtual ~StEStructAnalysis() {};

  virtual void setOutputFileName(const char* outFileName) = 0;
  virtual bool doEvent(StEStructEvent* event) = 0;
  virtual void finish()  = 0;

  ClassDef(StEStructAnalysis,1)
};


#endif

/***********************************************************************
 *
 * $Log: StEStructAnalysis.h,v $
 * Revision 1.1  2003/10/15 18:20:31  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

