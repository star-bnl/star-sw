/***************************************************************************
 *
 * $Id: StDbCondFactory.hh,v 1.2 1999/09/30 02:06:02 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Conditions tables
 *
 ***************************************************************************
 *
 * $Log: StDbCondFactory.hh,v $
 * Revision 1.2  1999/09/30 02:06:02  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBCondFACTORY_HH
#define STDBCondFACTORY_HH

#include "StDbFactoryI.hh"

class StDbCondFactory : public StDbFactoryI {

protected:

  StDbCondFactory(){ mdbType = Conditions; };
  virtual void initIDList();

static StDbCondFactory* mInstance;

public:

  static StDbCondFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbCondFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbCondFactory(){ deleteIDList(); };

};

#endif









