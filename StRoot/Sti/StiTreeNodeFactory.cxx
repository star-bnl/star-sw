#include "StiTreeNodeFactory.h"

ClassImp(StiTreeNodeFactory) 


StiTreeNodeFactory::StiTreeNodeFactory(int originalSize, 
				       int incrementalSize=-1, 
				       int maxIncrementCount=-1,
				       int childCount=1)
  : StiObjectFactory("TreeNode",originalSize,incrementalSize,maxIncrementCount)
{
  defaultChildCount = childCount;
}

StiTreeNodeFactory::~StiTreeNodeFactory()
{  
  for (int i=0;i<getCurrentSize();i++)
    {
      delete (*container)[i];
    }
}

StiTreeNode * StiTreeNodeFactory::getTreeNode()
{
  return (StiTreeNode *) getObject();
}
  
void StiTreeNodeFactory::createObjects(int n)
{
  for (int i=0;i<n;i++)
    {
      // add instance of tree node with null user object
      container->Add(new StiDefaultMutableTreeNode(0, defaultChildCount));
    }
}
