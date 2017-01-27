#include <assert.h>
#include "StiTreeNode.h"
#include "Factory.h"


//______________________________________________________________________________
/// Creates a tree node that has no parent and no children, but which
/// allows children.
StiTreeNode::StiTreeNode()
{
	reset();
}
//______________________________________________________________________________
void StiTreeNode::reset()
{
  parent = 0; children[0]=0; children[1]=0;
}
//______________________________________________________________________________  
/// Removes the child at the specified index from this node's children
/// and sets that node's parent to 0. The child node to remove
/// must be a <code>StiTreeNode *</code>.
///
/// @param	childIndex	the index in this node's child array
///				of the child to remove
/// @exception	ArrayIndexOutOfBoundsException	if
///				<code>childIndex</code> is out of bounds
//______________________________________________________________________________
void StiTreeNode::remove(int childIndex) 
{
  StiTreeNode *node =0;
  if (childIndex>=0) 	{ //remove children
    node =children[childIndex];children[childIndex]=0;
    if (!childIndex) 	{children[0]=children[1]; children[1]=0;}
    if (!node) 			return;
    if (node->parent!=this)	return;
    node->remove(0);
    
  } else 		{//remove parent
    node = parent; parent=0;
    if (!node) 			return;
    node->remove(0);
    node->remove(0);
    node->remove(-1);
  }
  node->reset();
  BFactory::Free(node);
}
//______________________________________________________________________________
StiTreeNode *StiTreeNode::disconnect(int all) 
{
  StiTreeNode *node =0;
  node = parent; parent=0;
  if (!node) 			return 0;
  assert(node->children[0]==this);
  assert(!children[1]);
  node->children[0]=0;
  if (all) return node;
  node->children[0]=children[0];
  if (children[0]) children[0]->parent=node;
  children[0]=0; 
  return node;
}

//______________________________________________________________________________
/// Sets this node's parent to <code>newParent</code> but does not 
/// change the parent's child array.  This method is called from
/// <code>insert()</code> and <code>remove()</code> to
/// reassign a child's parent, it should not be messaged from anywhere
/// else.
///
/// @param	newParent	this node's new parent
//______________________________________________________________________________
void StiTreeNode::setParent(StiTreeNode *  newParent) 
{
    parent = newParent;
}

//______________________________________________________________________________
/// Returns this node's parent or 0 if this node has no parent.
///
/// @return	this node's parent TreeNode, or 0 if this node has no parent
//______________________________________________________________________________
StiTreeNode *  StiTreeNode::getParent() const
{
  return parent;
}

/// Returns the child at the specified index in this node's child array.
///
/// @param	index	an index into this node's child array
///						is out of bounds
/// @return	the StiTreeNode in this node's child array at  the specified index
StiTreeNode *  StiTreeNode::getChildAt(int index) const
{
  return children[index];
}

//______________________________________________________________________________
/// Returns the number of children of this node.
///
/// @return	an int giving the number of children of this node
//______________________________________________________________________________
int StiTreeNode::getChildCount() const
{
  int n=0; 
  if(children[0]) n++;
  if(children[1]) n++;
  return n;
}

//______________________________________________________________________________
/// Returns the node that follows this node in a preorder traversal of this
// node's tree.  Returns 0 if this node is the last node of the
// traversal.  This is an inefficient way to traverse the entire tree; use
// an enumeration, instead.
//
// @see	#preorderEnumeration
// @return	the node that follows this node in a preorder traversal, or
//		0 if this node is last
//______________________________________________________________________________
StiTreeNode *  StiTreeNode::getNextNode() const
{
  return children[0];
}


//______________________________________________________________________________// 
//Returns the node that precedes this node in a preorder traversal of
// this node's tree.  Returns 0 if this node is the first node of the
// traveral -- the root of the tree.  This is an inefficient way to
// traverse the entire tree; use an enumeration, instead.
//
// @see	#preorderEnumeration
// @return	the node that precedes this node in a preorder traversal, or
//		0 if this node is the first
//______________________________________________________________________________
StiTreeNode *  StiTreeNode::getPrevNode() const
{
    return parent;
}
//______________________________________________________________________________
const StiTreeNode& StiTreeNode::operator=(const StiTreeNode& node)  
{
  reset();
  parent = node.parent;	
  return *this;
}
//______________________________________________________________________________
void StiTreeNode::add(StiTreeNode *  newChild,int direction)
{
  if (direction==0) {
    assert(!children[1]);
    children[1]=children[0];
    children[0]=newChild;
    newChild->setParent(this);

  } else {

    assert(!getParent());
    setParent(newChild);
    newChild->children[0]=this;
    newChild->children[1]=0;
  }
}
//______________________________________________________________________________
StiTreeNode *StiTreeNode::getLastNode() const
{
  const StiTreeNode *node = this;
  const StiTreeNode *next = 0;
  while ((next = node->getNextNode())) {
    node=next;
    assert(node!=this);
  }
  return (StiTreeNode*)node;
}
//______________________________________________________________________________
StiTreeNode *StiTreeNode::getFirstNode()  const
{
  const StiTreeNode *node = this;
  const StiTreeNode *back = 0;
  while ((back = node->getPrevNode())) {
    node=back;
    assert(node!=this);
  }
  return (StiTreeNode*)node;
}
//______________________________________________________________________________
void StiTreeNode::cutTail(int direction)  
{
   direction = (direction)? -1:0;
   remove(direction);
}
//______________________________________________________________________________
void  StiTreeNode::remove(StiTreeNode **fstNode,StiTreeNode **lstNode)
{
   assert(!children[1]);
   StiTreeNode **kLeft = (parent     )? &parent->children[0]:fstNode;
   StiTreeNode **kRite = (children[0])? &children[0]->parent:lstNode;
   *kLeft = children[0];
   *kRite = parent;
}





