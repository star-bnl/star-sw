/***************************************************************************
 *
 * $Id: StDbRunParamsFactory.hh,v 1.3 2000/01/10 20:37:54 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Run Params tables
 *
 ***************************************************************************
 *
 * $Log: StDbRunParamsFactory.hh,v $
 * Revision 1.3  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.2  1999/09/30 02:06:08  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBRunParamsFACTORY_HH
#define STDBRunParamsFACTORY_HH

#include "StDbFactoryI.hh"

class StDbRunParamsFactory : public StDbFactoryI {

protected:

  StDbRunParamsFactory(){ mdbType = dbRunParams; };
  void initIDList();

static StDbRunParamsFactory* mInstance;

public:

  static StDbRunParamsFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbRunParamsFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbRunParamsFactory(){ deleteIDList(); };

};

#endif









