/***************************************************************************
 *
 * $Id: StDbFactories.hh,v 1.3 1999/09/30 02:06:04 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  locates a Table factory of given Db-Type & Db-Domain
 *
 ***************************************************************************
 *
 * $Log: StDbFactories.hh,v $
 * Revision 1.3  1999/09/30 02:06:04  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBFACTORIESI_HH
#define STDBFACTORIESI_HH

#include "StDbDefs.hh"
#include "StDbTable.h"
#include "StDbFactoryI.hh"


#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDbFactoryI*, allocator<StDbFactoryI*> > Factories;
#else
typedef list<StDbFactoryI*> Factories;
#endif


class StDbFactories {

protected:

  Factories mfactories;

  StDbFactories() { initList(); };
  void initList();
  static StDbFactories* mInstance;

public:

  static StDbFactories* Instance() {
    if(!mInstance){
      mInstance = new StDbFactories();
    }
  return mInstance;
  }

  StDbFactoryI* getFactory(StDbType type);
 
};


#endif






