/**********************************************************************
 *
 * $Id: StEStructEventReader.h,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Abstract event reader class
 *
 ***********************************************************************/
#ifndef __STEBYEEVENTREADER__H
#define __STEBYEEVENTREADER__H

// -> for rootcint preprocessing
#include "TROOT.h"

//-> forward declaration
class StEStructEvent;

class StEStructEventReader {


public:

  virtual ~StEStructEventReader();
  virtual StEStructEvent* next()  = 0;
  virtual bool done()          = 0;

  ClassDef(StEStructEventReader,1)

};

#endif

/***********************************************************************
 *
 * $Log: StEStructEventReader.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




