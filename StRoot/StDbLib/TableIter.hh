#ifndef TABLEITR_HH
#define TABLEITR_HH

#include "StDbConfigNode.hh" // also includes StDbTable.h
#include "StDbTableI.h"

class TableIter {

  TableList::iterator itr;
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

};


#endif






