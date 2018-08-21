#ifndef StxDefaultMutableTreeNode_H
#define StxDefaultMutableTreeNode_H 1
#include "Rtypes.h"
/*! \class StxTreeNode
  A <code>StxTreeNode</code> is a general-purpose
  node in a tree data
  structure. A tree node may have at most one parent and 0 or more children.
  <code>StxTreeNode</code> provides operations for examining
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
  a StxDefaultMutableTreeNode (or a tree of TreeNodes) in more than one
  thread, you
  need to do your own synchronizing. A good convention to adopt is
  synchronizing on the Root node of a tree.
  <p>
  While StxDefaultMutableTreeNode implements the MutableTreeNode interface and
  will allow you to add in any implementation of MutableTreeNode not all
  of the methods in StxDefaultMutableTreeNode will be applicable to all
  StxTreeNodes implementations. Especially with some of the enumerations
  that are provided, using some of these methods assumes the
  StxDefaultMutableTreeNode contains only StxDefaultMutableNode instances. All
  of the TreeNode/MutableTreeNode methods will behave as defined no
  matter what implementations are added.
  <p>
  
  @see StxTreeNode

  \author Claude Pruneau

*/
class StxTreeNode {
 public:

  virtual ~StxTreeNode(){};
  StxTreeNode() {Reset();}
  const StxTreeNode& operator=(const StxTreeNode& node);  

  void         Reset() {parent = children[0] = children[1]=0;}
  void         Unset() {}
  void         Remove(Int_t childIndex) ;
  StxTreeNode *Disconnect(Int_t all=0); 
  void         CutTail(Int_t direction) ;
  void         SetParent(StxTreeNode *  newParent) {parent = newParent;}
  StxTreeNode *Parent() 	    const          {return parent;}
  StxTreeNode *ChildAt(Int_t index) const          {return children[index];}
  Int_t        ChildCount() 	    const;
  void         Add(StxTreeNode *newChild,Int_t direction);
  StxTreeNode *NextNode() 	const {return children[0];}
  StxTreeNode *PrevNode() 	const {return parent;}
  StxTreeNode *FirstNode()	const;
  StxTreeNode *LastNode() 	const;
 protected:
  StxTreeNode *parent;
  StxTreeNode *children[2];
};
#endif
