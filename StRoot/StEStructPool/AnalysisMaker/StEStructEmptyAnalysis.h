/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.h,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Empty analysis code for testing
 *
 **********************************************************************/
#ifndef _STESTRUCT_EMPTY_HH
#define _STESTRUCT_EMPTY_HH


#include "StEStructAnalysis.h"

class StEStructEmptyAnalysis : public StEStructAnalysis {

 public:

  StEStructEmptyAnalysis();
  ~StEStructEmptyAnalysis(){};

  virtual void setOutputFileName(const char* outFileName) {};
  virtual bool doEvent(StEStructEvent* event); 
  virtual void finish()  {};

  ClassDef(StEStructEmptyAnalysis,1)
};


#endif
/**********************************************************************
 *
 * $Log: StEStructEmptyAnalysis.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

