/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.cxx,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Empty analysis code for testing
 *
 **********************************************************************/
#include "StEStructEmptyAnalysis.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "Stiostream.h"


ClassImp(StEStructEmptyAnalysis)

StEStructEmptyAnalysis::StEStructEmptyAnalysis(){};

bool StEStructEmptyAnalysis::doEvent(StEStructEvent* event){
  cout<<" Deleting Event from EmptyAnalysis "<<endl;
  delete event;
  return true;
}

/**********************************************************************
 *
 * $Log: StEStructEmptyAnalysis.cxx,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
