#ifndef StiTreeNode_H
#define StiTreeNode_H 1
#include <Stiostream.h>
#include <stdlib.h>

class StiTreeNode 
{
 public:
  //
  // Returns the child <code>StiTreeNode</code> at index 
  // <code>childIndex</code>.
  ///
  virtual StiTreeNode * getChildAt(int childIndex) const =0;
  
  //
  // Returns the number of children <code>StiTreeNode</code>s the receiver
  // contains.
  ///
  virtual int getChildCount() const =0;
  
  //
  // Returns the parent <code>StiTreeNode</code> of the receiver.
  ///
  virtual StiTreeNode * getParent()=0;
  
  //
  // Returns the index of <code>node</code> in the receivers children.
  // If the receiver does not contain <code>node</code>, -1 will be
  // returned.
  ///
  virtual int getIndex(StiTreeNode * node)=0;
  
  //
  // Returns true if the receiver allows children.
  ///
  virtual bool getAllowsChildren()=0;
  
  //
  // Returns true if the receiver is a leaf.
  ///
  virtual bool isLeaf()=0;


  virtual void setParent(StiTreeNode * newParent)=0;

  //virtual void           setDepth(int depth)=0;
  //virtual int            getDepth() const =0;

  
};

#endif



