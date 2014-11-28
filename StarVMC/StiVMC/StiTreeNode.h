#ifndef StiDefaultMutableTreeNode_H
#define StiDefaultMutableTreeNode_H 1
#include "Rtypes.h"
/*! \class StiTreeNode
  A <code>StiTreeNode</code> is a general-purpose
  node in a tree data
  structure. A tree node may have at most one parent and 0 or more children.
  <code>StiTreeNode</code> provides operations for examining
  and modifying a
  node's parent and children and also operations for examining the tree that
  the node is a part of.  A node's tree is the set of all nodes that can be
  reached by starting at the node and following all the possible links to
  parents and children.  A node with no parent is the Root of its tree; a
  node with no children is a leaf.  A tree may consist of many subtrees,
  each node acting as the Root for its own subtree.
  <p>
  This class provides enumerations for efficiently traversing a tree or
  subtree in various orders or for following the path between two nodes.
  <p>
  <b>This is not a thread safe class.</b>If you intend to use
  a StiDefaultMutableTreeNode (or a tree of TreeNodes) in more than one
  thread, you
  need to do your own synchronizing. A good convention to adopt is
  synchronizing on the Root node of a tree.
  <p>
  While StiDefaultMutableTreeNode implements the MutableTreeNode interface and
  will allow you to add in any implementation of MutableTreeNode not all
  of the methods in StiDefaultMutableTreeNode will be applicable to all
  StiTreeNodes implementations. Especially with some of the enumerations
  that are provided, using some of these methods assumes the
  StiDefaultMutableTreeNode contains only StiDefaultMutableNode instances. All
  of the TreeNode/MutableTreeNode methods will behave as defined no
  matter what implementations are added.
  <p>
  
  @see StiTreeNode

  \author Claude Pruneau

*/
class StiTreeNode {
 public:

  virtual ~StiTreeNode(){};
  StiTreeNode() {Reset();}
  const StiTreeNode& operator=(const StiTreeNode& node);  

  void         Reset() {parent = children[0] = children[1]=0;}
  void         Unset() {}
  void         Remove(Int_t childIndex) ;
  StiTreeNode *Disconnect(Int_t all=0); 
  void         CutTail(Int_t direction) ;
  void         SetParent(StiTreeNode *  newParent) {parent = newParent;}
  StiTreeNode *Parent() 	    const          {return parent;}
  StiTreeNode *ChildAt(Int_t index) const          {return children[index];}
  Int_t        ChildCount() 	    const;
  void         Add(StiTreeNode *newChild,Int_t direction);
  StiTreeNode *NextNode() 	const {return children[0];}
  StiTreeNode *PrevNode() 	const {return parent;}
  StiTreeNode *FirstNode()	const;
  StiTreeNode *LastNode() 	const;
 protected:
  StiTreeNode *parent;
  StiTreeNode *children[2];
};
#endif
