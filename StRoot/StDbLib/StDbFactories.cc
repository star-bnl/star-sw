/***************************************************************************
 *
 * $Id: StDbFactories.cc,v 1.6 2000/01/10 20:37:53 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  locates a Table factory of given Db-Type & Db-Domain
 *
 ***************************************************************************
 *
 * $Log: StDbFactories.cc,v $
 * Revision 1.6  2000/01/10 20:37:53  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.5  1999/10/19 14:30:38  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.4  1999/09/30 02:06:04  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbFactories.hh"
#include "StDbCalibFactory.hh"
#include "StDbGeomFactory.hh"
#include "StDbCondFactory.hh"
#include "StDbRunParamsFactory.hh"

StDbFactories* StDbFactories::mInstance=0;

StDbFactoryI*
StDbFactories::getFactory(StDbType type){


StDbFactoryI* factory = 0;

for(Factories::iterator itr = mfactories.begin();
    itr != mfactories.end(); ++itr){
  if( (*itr)->getDbType()==type){
     factory = *itr;
     break;
  }
}
 if(!factory)factory = getFactory(dbStDb);

return factory;
}

void
StDbFactories::initList(){

mfactories.push_back(StDbCalibFactory::Instance());
mfactories.push_back(StDbCondFactory::Instance());
mfactories.push_back(StDbGeomFactory::Instance());
mfactories.push_back(StDbRunParamsFactory::Instance());
mfactories.push_back(new StDbFactoryI);

}





