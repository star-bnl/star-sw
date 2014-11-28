#include <assert.h>
#include "StiTreeNode.h"
#include "StiFactory.h"
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
void StiTreeNode::Remove(Int_t childIndex) {
  StiTreeNode *node =0;
  if (childIndex>=0) 	{ //remove children
    node =children[childIndex];children[childIndex]=0;
    if (!childIndex) 	{children[0]=children[1]; children[1]=0;}
    if (!node) 			return;
    if (node->parent!=this)	return;
    node->Remove(0);
    
  } else 		{//remove parent
    node = parent; parent=0;
    if (!node) 			return;
    node->Remove(0);
    node->Remove(0);
    node->Remove(-1);
  }
  node->Reset();
  BFactory::Free(node);
}
//______________________________________________________________________________
StiTreeNode *StiTreeNode::Disconnect(Int_t all) {
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
/// Returns the number of children of this node.
///
/// @return	an int giving the number of children of this node
//______________________________________________________________________________
Int_t StiTreeNode::ChildCount() const
{
  Int_t n=0; 
  if(children[0]) n++;
  if(children[1]) n++;
  return n;
}
//______________________________________________________________________________
const StiTreeNode& StiTreeNode::operator=(const StiTreeNode& node) {
  Reset();
  parent = node.parent;	
  return *this;
}
//______________________________________________________________________________
void StiTreeNode::Add(StiTreeNode *  newChild,Int_t direction) {
  if (direction==0) {
    assert(!children[1]);
    children[1]=children[0];
    children[0]=newChild;
    newChild->SetParent(this);
  } else {
    assert(!Parent());
    SetParent(newChild);
    newChild->children[0]=this;
    newChild->children[1]=0;
  }
}
//______________________________________________________________________________
StiTreeNode *StiTreeNode::LastNode() const {
  const StiTreeNode *node = this;
  const StiTreeNode *next = 0;
  while ((next = node->NextNode())) {
    node=next;
    assert(node!=this);
  }
  return (StiTreeNode*)node;
}
//______________________________________________________________________________
StiTreeNode *StiTreeNode::FirstNode()  const {
  const StiTreeNode *node = this;
  const StiTreeNode *back = 0;
  while ((back = node->PrevNode())) {
    node=back;
    assert(node!=this);
  }
  return (StiTreeNode*)node;
}
//______________________________________________________________________________
void StiTreeNode::CutTail(Int_t direction) {
  direction = (direction)? -1:0;
  Remove(direction);
}

