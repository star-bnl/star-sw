#ifndef TABLEITR_HH
#define TABLEITR_HH

#ifndef __CINT__
#include "StDbConfigNode.hh" // also includes StDbTable.h
typedef TableList::iterator ListIter;
#else
class ListIter;
#endif

#include "StDbTableI.h"

class TableIter {

  ListIter itr;
  StDbConfigNode* mnode;

public:

  TableIter() : mnode(0){};
  TableIter(StDbConfigNode* node){ init(node);};
  ~TableIter(){};

  void init(StDbConfigNode* node);

  StDbTableI* next();
  StDbTableI* operator++();
  bool done();

  // will also want a method nextAndRelease() which does the
  // same as next() but removes the table from the StDbConfigNode

  //ClassDef(TableIter,0)

};


#endif






