/**********************************************************************
 *
 * $Id: StEStructAnalysis.h,v 1.2 2004/06/25 03:10:22 porter Exp $
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
#include "Stiostream.h"

class StEStructEvent;

class StEStructAnalysis {

 public:

  StEStructAnalysis();
  virtual ~StEStructAnalysis() {};

  virtual void setOutputFileName(const char* outFileName) = 0;
  virtual bool doEvent(StEStructEvent* event) = 0;
  virtual void finish()  = 0;

  // new method; should be pure-virtual but make it 'no-opt' so older codes
  // won't be required to implement it 
  virtual void logStats(ostream& os){ /* no opt */ }; 

  ClassDef(StEStructAnalysis,1)
};


#endif

/***********************************************************************
 *
 * $Log: StEStructAnalysis.h,v $
 * Revision 1.2  2004/06/25 03:10:22  porter
 * added a new common statistics output and added electron cut with momentum slices
 *
 * Revision 1.1  2003/10/15 18:20:31  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

