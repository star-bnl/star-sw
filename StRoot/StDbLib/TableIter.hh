#ifndef TABLEITR_HH
#define TABLEITR_HH


#include "StDbConfigNode.hh"
//#include "StDbTableComponent.h"


class TableIter {

  TableList::iterator itr;

public:

  TableIter(){};
  ~TableIter(){};

  void init(StDbConfigNode* node);
  StDbTableComponent* next(char*& tableName);
  bool done(StDbConfigNode* node);

};


#endif






