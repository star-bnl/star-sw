/***************************************************************************
 *
 * $Id: StDbGeomFactory.hh,v 1.3 2000/01/10 20:37:54 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Geometry tables
 *
 ***************************************************************************
 *
 * $Log: StDbGeomFactory.hh,v $
 * Revision 1.3  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
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

  StDbGeomFactory(){ mdbType = dbGeometry; };
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









