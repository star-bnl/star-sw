/***************************************************************************
 *
 * $Id: StDbGeomFactory.hh,v 1.2 1999/09/30 02:06:06 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Geometry tables
 *
 ***************************************************************************
 *
 * $Log: StDbGeomFactory.hh,v $
 * Revision 1.2  1999/09/30 02:06:06  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBGeomFACTORY_HH
#define STDBGeomFACTORY_HH

#include "StDbFactoryI.hh"

class StDbGeomFactory : public StDbFactoryI {

protected:

  StDbGeomFactory(){ mdbType = Geometry; };
  virtual void initIDList();

static StDbGeomFactory* mInstance;

public:

  static StDbGeomFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbGeomFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbGeomFactory(){ deleteIDList(); };

};

#endif









