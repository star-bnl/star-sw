/***************************************************************************
 *
 * $Id: StDbFactoryI.hh,v 1.4 1999/10/19 14:30:38 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for generic tables
 *
 ***************************************************************************
 *
 * $Log: StDbFactoryI.hh,v $
 * Revision 1.4  1999/10/19 14:30:38  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.3  1999/09/30 02:06:05  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBFACTORYI_HH
#define STDBFACTORYI_HH

#include "StDbDefs.hh"
#include "StDbTableID.h"
#include <fstream.h>

class StDbTable;

#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDbTableID*, allocator<StDbTableID*> > IDList;
#else
typedef list<StDbTableID*> IDList;
#endif


class StDbFactoryI {

protected:

  bool isloaded;
  StDbType mdbType;
  IDList mIDList;

  virtual void initIDList(); // Here's all the work in derived classes

public:

  StDbFactoryI() : isloaded(false), mdbType(StarDb) {};
  virtual ~StDbFactoryI(){ deleteIDList();};
  virtual StDbType getDbType() { return mdbType; };

  virtual int findTableID(const char* tableName);
  virtual StDbTable* getDbTable(const char* tableName, int option);
  virtual void deleteIDList();
  virtual void initIDList(ifstream& is);

};

inline void
StDbFactoryI::initIDList() {
isloaded=true;
// cerr<< "StDbFactoryI:: Has No default Tables in Base Class"<< endl;
}


#endif







