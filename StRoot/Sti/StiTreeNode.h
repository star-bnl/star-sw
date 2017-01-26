#ifndef StiDefaultMutableTreeNode_H
#define StiDefaultMutableTreeNode_H 1

#include <Stiostream.h>
#include <stdlib.h>


/*! \class StiTreeNode
  A <code>StiTreeNode</code> is a general-purpose
  node in a tree data
  structure. A tree node may have at most one parent and 0 or more children.
  <code>StiTreeNode</code> provides operations for examining
  and modifying a
  node's parent and children and also operations for examining the tree that
  the node is a part of.  A node's tree is the set of all nodes that can be
  reached by starting at the node and following all the possible links to
  parents and children.  A node with no parent is the root of its tree; a
  node with no children is a leaf.  A tree may consist of many subtrees,
  each node acting as the root for its own subtree.
  <p>
  This class provides enumerations for efficiently traversing a tree or
  subtree in various orders or for following the path between two nodes.
  <p>
  <b>This is not a thread safe class.</b>If you intend to use
  a StiDefaultMutableTreeNode (or a tree of TreeNodes) in more than one
  thread, you
  need to do your own synchronizing. A good convention to adopt is
  synchronizing on the root node of a tree.
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


class StiTreeNode 
{
 public:

  virtual ~StiTreeNode(){};
  StiTreeNode();
  const StiTreeNode& operator=(const StiTreeNode& node);  

  void reset();
  void unset(){;}
  void remove(int childIndex) ;
  StiTreeNode *disconnect(int all=0); 
  void cutTail(int direction) ;
  void setParent(StiTreeNode *  newParent) ;
  StiTreeNode *getParent() 	const;
  StiTreeNode *getChildAt(int index) const;
  int getChildCount() 		const;
  void add(StiTreeNode *newChild,int direction);
  StiTreeNode *getNextNode() 	const;
  StiTreeNode *getPrevNode() 	const;
  StiTreeNode *getFirstNode()	const;
  StiTreeNode *getLastNode() 	const;
         void  remove(StiTreeNode **fstNode,StiTreeNode **lstNode);
 protected:
  StiTreeNode *parent;
  StiTreeNode *children[2];

};


#endif


