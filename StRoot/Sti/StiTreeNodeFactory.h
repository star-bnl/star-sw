#ifndef StiTreeNodeFactory_H
#define StiTreeNodeFactory_H 1

#include "StiObjectFactory.h"
#include "StiDefaultMutableTreeNode.h"


class StiTreeNodeFactory : public StiObjectFactory
{
 public:

  StiTreeNodeFactory(int originalSize, 
		     int incrementalSize=-1, 
		     int maxIncrementCount=-1,
		     int childCount=1);
  virtual ~StiTreeNodeFactory();

  StiTreeNode * getTreeNode();
  
 protected:

  void createObjects(int n); 

  int defaultChildCount;

  ClassDef(StiTreeNodeFactory, 1)
};

#endif
