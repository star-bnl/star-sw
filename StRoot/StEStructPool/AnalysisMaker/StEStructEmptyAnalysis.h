/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.h,v 1.2 2004/06/25 03:10:29 porter Exp $
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


#include "TROOT.h"
#include "StEStructAnalysis.h"
class TH1F;

class StEStructEmptyAnalysis : public StEStructAnalysis {

  char* moutFileName;
  
  int mhm[14];

  TH1F** etaMean[3];
  TH1F** phiMean[3];
  TH1F** ytMean[3];

 public:

  StEStructEmptyAnalysis();
  ~StEStructEmptyAnalysis(){};

  virtual void setOutputFileName(const char* outFileName);
  virtual bool doEvent(StEStructEvent* event); 
  virtual void finish();

  ClassDef(StEStructEmptyAnalysis,1)
};

inline void StEStructEmptyAnalysis::setOutputFileName(const char* fName){
  if(!fName) return;
  moutFileName=new char[strlen(fName)+1];
  strcpy(moutFileName,fName);
}

#endif
/**********************************************************************
 *
 * $Log: StEStructEmptyAnalysis.h,v $
 * Revision 1.2  2004/06/25 03:10:29  porter
 * added a new common statistics output and added electron cut with momentum slices
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

