#include "StiDefaultMutableTreeNode.h"

ClassImp(StiDefaultMutableTreeNode)
//-----------------------------------------------------------------------------
// A <code>StiDefaultMutableTreeNode</code> is a general-purpose node in a tree data
// structure. A tree node may have at most one parent and 0 or more children.
// <code>StiDefaultMutableTreeNode</code> provides operations for examining and modifying a
// node's parent and children and also operations for examining the tree that
// the node is a part of.  A node's tree is the set of all nodes that can be
// reached by starting at the node and following all the possible links to
// parents and children.  A node with no parent is the root of its tree; a
// node with no children is a leaf.  A tree may consist of many subtrees,
// each node acting as the root for its own subtree.
// <p>
// This class provides enumerations for efficiently traversing a tree or
// subtree in various orders or for following the path between two nodes.
// A <code>StiDefaultMutableTreeNode</code> may also hold a reference to a user object, the
// use of which is left to the user.  Asking a <code>StiDefaultMutableTreeNode</code> for its
// string representation with <code>toString()</code> returns the string
// representation of its user object.
// <p>
// <b>This is not a thread safe class.</b>If you intend to use
// a StiDefaultMutableTreeNode (or a tree of TreeNodes) in more than one thread, you
// need to do your own synchronizing. A good convention to adopt is
// synchronizing on the root node of a tree.
// <p>
// While StiDefaultMutableTreeNode implements the MutableTreeNode interface and
// will allow you to add in any implementation of MutableTreeNode not all
// of the methods in StiDefaultMutableTreeNode will be applicable to all
// StiTreeNodes implementations. Especially with some of the enumerations
// that are provided, using some of these methods assumes the
// StiDefaultMutableTreeNode contains only StiDefaultMutableNode instances. All
// of the TreeNode/MutableTreeNode methods will behave as defined no
// matter what implementations are added.
// <p>
//
// @see StiTreeNode
//
//-----------------------------------------------------------------------------


StiDefaultMutableTreeNode::StiDefaultMutableTreeNode()
//--------------------------------------------------------------
// Creates a tree node that has no parent and no children, but which
// allows children.
//--------------------------------------------------------------
{
  initialize(0,true,1);
}

StiDefaultMutableTreeNode::StiDefaultMutableTreeNode(TObject *  userObj) 
//--------------------------------------------------------------
// Creates a tree node with no parent, no children, but which allows 
// children, and initializes it with the specified user object.
// 
// @param userObject an TObject *  provided by the user that constitutes
//                   the node's data
//--------------------------------------------------------------
{
  initialize(userObj, true,1);
}

StiDefaultMutableTreeNode::StiDefaultMutableTreeNode(TObject *  userObj, bool allowsChild) 
     //--------------------------------------------------------------
     // Creates a tree node with no parent, no children, initialized with
     // the specified user object, and that allows children only if
     // specified.
     // 
     // @param userObject an TObject *  provided by the user that constitutes
     //        the node's data
     // @param allowsChildren if true, the node is allowed to have child
     //        nodes -- otherwise, it is always a leaf node
     //--------------------------------------------------------------
{
  initialize(userObj, allowsChild, 1);
}

StiDefaultMutableTreeNode::StiDefaultMutableTreeNode(TObject *  userObj,
						     int        childArraySize) 
     //--------------------------------------------------------------
     // Creates a tree node with no parent, no children, initialized with
     // the specified user object, and that allows children, with an initial
     // number as specified
     // 
     // @param userObject an TObject *  provided by the user that constitutes
     //        the node's data
     // @param childrenArraySize initial number of children
     //--------------------------------------------------------------
{
  initialize(userObj, true, childArraySize);
}

void StiDefaultMutableTreeNode::initialize(TObject *  userObj, 
					   bool    allowsChild,
					   int        childrenArraySize)
{
  //-------------------------------------------------------------------------------
  //  Initialize data members of the class
  //-------------------------------------------------------------------------------
  
  parent         = 0;
  allowsChildren = allowsChild;
  userObject     = userObj;
  if (allowsChildren)
    {
      int size = childrenArraySize > 0 ? childrenArraySize : 1;
      children = new TObjArray(size);
    }
}


//-------------------------------------------------------------------------------
//  Primitives
//-------------------------------------------------------------------------------

void StiDefaultMutableTreeNode::insert(StiTreeNode * newChild, int childIndex)
  //--------------------------------------------------------------
  // Removes <code>newChild</code> from its present parent (if it has a
  // parent), sets the child's parent to this node, and then adds the child
  // to this node's child array at index <code>childIndex</code>.
  // <code>newChild</code> must not be null and must not be an ancestor of
  // this node.
  //
  // @param	newChild	the StiTreeNode * to insert under this node
  // @param	childIndex	the index in this node's child array
  //				where this node is to be inserted
  // @see	#isNodeDescendant
  //--------------------------------------------------------------
{
  if (!allowsChildren) 
    {
      cout << "StiDefaultMutableTreeNode::insert() - ERROR" << endl
	   << "     Attempting to insert a node into a node " << endl
	   << "     which does not accept children" << endl;
    }
  else if (newChild == 0) 
    {
      cout << "StiDefaultMutableTreeNode::insert() - ERROR" << endl
	   << "     Attempting to insert null object " << endl;
    } 
  else if (isNodeAncestor(newChild)) 
    {
      cout << "StiDefaultMutableTreeNode::insert() - ERROR" << endl
	   << "     Attempting to insert node which is an ancestor" << endl;
    }
  
  StiDefaultMutableTreeNode * oldParent = (StiDefaultMutableTreeNode *) newChild->getParent();
  
  if (oldParent != 0) 
    {
      oldParent->remove(newChild);
    }
  newChild->setParent(this);
  if (children == 0) 
    {
      children = new TObjArray(1);
    }
  children->AddAt(newChild, childIndex);
}

void StiDefaultMutableTreeNode::remove(int childIndex) 
//--------------------------------------------------------------
// Removes the child at the specified index from this node's children
// and sets that node's parent to 0. The child node to remove
// must be a <code>StiTreeNode *</code>.
//
// @param	childIndex	the index in this node's child array
//				of the child to remove
// @exception	ArrayIndexOutOfBoundsException	if
//				<code>childIndex</code> is out of bounds
//--------------------------------------------------------------
{
  StiDefaultMutableTreeNode * child = (StiDefaultMutableTreeNode * )getChildAt(childIndex);
  children->RemoveAt(childIndex);
  child->setParent(0);
}

//--------------------------------------------------------------
// Sets this node's parent to <code>newParent</code> but does not 
// change the parent's child array.  This method is called from
// <code>insert()</code> and <code>remove()</code> to
// reassign a child's parent, it should not be messaged from anywhere
// else.
//
// @param	newParent	this node's new parent
//--------------------------------------------------------------
void StiDefaultMutableTreeNode::setParent(StiTreeNode *  newParent) 
{
  parent = newParent;
}

//--------------------------------------------------------------
// Returns this node's parent or 0 if this node has no parent.
//
// @return	this node's parent TreeNode, or 0 if this node has no parent
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getParent() 
{
  return parent;
}

//--------------------------------------------------------------
// Returns the child at the specified index in this node's child array.
//
// @param	index	an index into this node's child array
//						is out of bounds
// @return	the StiTreeNode in this node's child array at  the specified index
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getChildAt(int index) 
{
  if (children == 0)
    {
      cout << "StiDefaultMutableTreeNode::getChildAt(int index) - ERROR" << endl 
	   <<     "node has no children" << endl;
    }
  return (StiTreeNode * )(*children)[index];
}

//--------------------------------------------------------------
// Returns the number of children of this node.
//
// @return	an int giving the number of children of this node
//--------------------------------------------------------------
int StiDefaultMutableTreeNode::getChildCount() 
{
  if (children == 0) 
    {
      return 0;
    } 
  else 
    {
      return children->GetSize();
    }
}

//--------------------------------------------------------------
// Returns the index of the specified child in this node's child array.
// If the specified node is not a child of this node, returns
// <code>-1</code>.  This method performs a linear search and is O(n)
// where n is the number of children.
//
// @param	aChild	the StiTreeNode to search for among this node's children
// @return	an int giving the index of the node in this node's child 
//          array, or <code>-1</code> if the specified node is a not
//          a child of this node
//--------------------------------------------------------------
int StiDefaultMutableTreeNode::getIndex(StiTreeNode *  aChild) 
{
  if (aChild == 0) 
    {
      cout << "StiDefaultMutableTreeNode::getIndex(StiTreeNode *  aChild) - ERROR" << endl 
	   << "      argument is 0";
    }
  
  if (!isNodeChild(aChild)) 
    {
      return -1;
    }
  return children->IndexOf(aChild);	// linear search
}

//--------------------------------------------------------------
// Determines whether or not this node is allowed to have children. 
// If <code>allows</code> is false, all of this node's children are
// removed.
// <p>
// Note: By default, a node allows children.
//
// @param	allows	true if this node is allowed to have children
//--------------------------------------------------------------
void StiDefaultMutableTreeNode::setAllowsChildren(bool allows)
{
  if (allows != allowsChildren) 
    {
      allowsChildren = allows;
      if (!allowsChildren) 
	{
	  removeAllChildren();
	}
    }
}

//--------------------------------------------------------------
// Returns true if this node is allowed to have children.
//
// @return	true if this node allows children, else false
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::getAllowsChildren() 
{
  return allowsChildren;
}

//--------------------------------------------------------------
// Sets the user object for this node to <code>userObject</code>.
//
// @param	userObject	the TObject *  that constitutes this node's 
//                          user-specified data
// @see	#getUserObject
// @see	#toString
//--------------------------------------------------------------
void StiDefaultMutableTreeNode::setUserObject(TObject *  object) 
{
  userObject = object;
}

//--------------------------------------------------------------
// Returns this node's user object.
//
// @return	the TObject *  stored at this node by the user
// @see	#setUserObject
// @see	#toString
//--------------------------------------------------------------
TObject *  StiDefaultMutableTreeNode::getUserObject() 
{
  return userObject;
}


//--------------------------------------------------------------
//  Derived methods
//--------------------------------------------------------------

//--------------------------------------------------------------
// Removes the subtree rooted at this node from the tree, giving this
// node a 0 parent.  Does nothing if this node is the root of its
// tree.
//--------------------------------------------------------------
void   StiDefaultMutableTreeNode::removeFromParent() 
{
  StiDefaultMutableTreeNode *  parent = (StiDefaultMutableTreeNode * )getParent();
  if (parent != 0) {
    parent->remove(this);
  }
}

//--------------------------------------------------------------
// Removes <code>aChild</code> from this node's child array, giving it a
// 0 parent->
//
// @param	aChild	a child of this node to remove
// @exception	IllegalArgumentException	if <code>aChild</code>
//					is 0 or is not a child of this node
//--------------------------------------------------------------
void StiDefaultMutableTreeNode::remove(StiTreeNode *  aChild) 
{
  if (aChild == 0) 
    {
      cout << "StiDefaultMutableTreeNode::remove(StiTreeNode *  aChild) - ERROR" << endl 
	   << "      argument is 0";
    }
  
  if (!isNodeChild(aChild)) 
    {
      cout << "StiDefaultMutableTreeNode::remove(StiTreeNode *  aChild)  - ERROR" << endl 
	   << "    argument is not a child";
    }
  remove(getIndex(aChild));	// linear search
}

//--------------------------------------------------------------
// Removes all of this node's children, setting their parents to 0.
// If this node has no children, this method does nothing.
//--------------------------------------------------------------
void StiDefaultMutableTreeNode::removeAllChildren() 
{
  for (int i = getChildCount()-1; i >= 0; i--) 
    {
      remove(i);
    }
}

//--------------------------------------------------------------
// Removes all of this node's children, setting their parents to 0.
// If this node has no children, this method does nothing.
//--------------------------------------------------------------
void StiDefaultMutableTreeNode::removeAllChildrenBut(StiTreeNode *  aChild) 
{
  for (int i = getChildCount()-1; i >= 0; i--) 
    {
      if (getChildAt(i)!=aChild)
	{
	  remove(i);
	}
    }
}


//--------------------------------------------------------------
// Removes <code>newChild</code> from its parent and makes it a child of
// this node by adding it to the end of this node's child array.
//
// @see		#insert
// @param	newChild	node to add as a child of this node
// @exception	IllegalArgumentException    if <code>newChild</code>
//						is 0
// @exception	IllegalStateException	if this node does not allow
//						children
//--------------------------------------------------------------
void StiDefaultMutableTreeNode::add(StiTreeNode *  newChild) 
{
  if(newChild != 0 && newChild->getParent() == this)
    insert(newChild, getChildCount() - 1);
  else
    insert(newChild, getChildCount());
}



//--------------------------------------------------------------
//  Tree Queries
//--------------------------------------------------------------

//--------------------------------------------------------------
// Returns true if <code>anotherNode</code> is an ancestor of this node
// -- if it is this node, this node's parent, or an ancestor of this
// node's parent->  (Note that a node is considered an ancestor of itself.)
// If <code>anotherNode</code> is 0, this method returns false.  This
// operation is at worst O(h) where h is the distance from the root to
// this node.
//
// @see		#isNodeDescendant
// @see		#getSharedAncestor
// @param	anotherNode	node to test as an ancestor of this node
// @return	true if this node is a descendant of <code>anotherNode</code>
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::isNodeAncestor(StiTreeNode *  anotherNode) 
{
  if (anotherNode == 0) {
    return false;
  }
  
  StiTreeNode *  ancestor = this;
  
  do {
    if (ancestor == anotherNode) {
      return true;
    }
  } while((ancestor = ancestor->getParent()) != 0);
  
  return false;
}

//--------------------------------------------------------------
// Returns true if <code>anotherNode</code> is a descendant of this node
// -- if it is this node, one of this node's children, or a descendant of
// one of this node's children.  Note that a node is considered a
// descendant of itself.  If <code>anotherNode</code> is 0, returns
// false.  This operation is at worst O(h) where h is the distance from the
// root to <code>anotherNode</code>.
//
// @see	#isNodeAncestor
// @see	#getSharedAncestor
// @param	anotherNode	node to test as descendant of this node
// @return	true if this node is an ancestor of <code>anotherNode</code>
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::isNodeDescendant(StiDefaultMutableTreeNode *  anotherNode) 
{
  if (anotherNode == 0)
    return false;
  
  return anotherNode->isNodeAncestor(this);
}

//
// Returns the nearest common ancestor to this node and <code>aNode</code>.
// Returns 0, if no such ancestor exists -- if this node and
// <code>aNode</code> are in different trees or if <code>aNode</code> is
// 0.  A node is considered an ancestor of itself.
//
// @see	#isNodeAncestor
// @see	#isNodeDescendant
// @param	aNode	node to find common ancestor with
// @return	nearest ancestor common to this node and <code>aNode</code>,
//		or 0 if none
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getSharedAncestor(StiDefaultMutableTreeNode *  aNode)
{
  if (aNode == this) {
    return this;
  } else if (aNode == 0) {
    return 0;
  }
  
  int		level1, level2, diff;
  StiTreeNode * node1;
  StiTreeNode * node2;
  
  level1 = getLevel();
  level2 = aNode->getLevel();
  
  if (level2 > level1) {
    diff = level2 - level1;
    node1 = aNode;
    node2 = this;
  } else {
    diff = level1 - level2;
    node1 = this;
    node2 = aNode;
  }
  
  // Go up the tree until the nodes are at the same level
  while (diff > 0) {
    node1 = node1->getParent();
    diff--;
  }
  
  // Move up the tree until we find a common ancestor.  Since we know
  // that both nodes are at the same level, we won't cross paths
  // unknowingly (if there is a common ancestor, both nodes hit it in
  // the same iteration).
  
  do {
    if (node1 == node2) {
      return node1;
    }
    node1 = node1->getParent();
    node2 = node2->getParent();
  } while (node1 != 0);// only need to check one -- they're at the
  // same level so if one is 0, the other is
  
  if (node1 != 0 || node2 != 0) {
    cout << "StiDefaultMutableTreeNode::getSharedAncestor(StiDefaultMutableTreeNode *) - ERROR" << endl 
	 << "nodes should be 0";
  }
  
  return 0;
}


//--------------------------------------------------------------
// Returns true if and only if <code>aNode</code> is in the same tree
// as this node.  Returns false if <code>aNode</code> is 0.
//
// @see	#getSharedAncestor
// @see	#getRoot
// @return	true if <code>aNode</code> is in the same tree as this node;
//		false if <code>aNode</code> is 0
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::isNodeRelated(StiDefaultMutableTreeNode *  aNode) 
{
  return (aNode != 0) && (getRoot() == aNode->getRoot());
}


//--------------------------------------------------------------
// Returns the depth of the tree rooted at this node -- the longest
// distance from this node to a leaf.  If this node has no children,
// returns 0.  This operation is much more expensive than
// <code>getLevel()</code> because it must effectively traverse the entire
// tree rooted at this node.
//
// @see	#getLevel
// @return	the depth of the tree whose root is this node
//--------------------------------------------------------------
int StiDefaultMutableTreeNode::getDepth() 
{
  cout << "StiDefaultMutableTreeNode::getDepth() - WARNING - not coded" << endl;
  return -1;
}

//--------------------------------------------------------------
// Returns the number of levels above this node -- the distance from
// the root to this node.  If this node is the root, returns 0.
//
// @see	#getDepth
// @return	the number of levels above this node
//--------------------------------------------------------------
int StiDefaultMutableTreeNode::getLevel() 
{
  StiTreeNode *  ancestor;
  int levels = 0;
  
  ancestor = this;
  while((ancestor = ancestor->getParent()) != 0){
    levels++;
  }
  
  return levels;
}

//--------------------------------------------------------------
// Returns the root of the tree that contains this node.  The root is
// the ancestor with a 0 parent->
//
// @see	#isNodeAncestor
// @return	the root of the tree that contains this node
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getRoot() 
{
  StiTreeNode *  ancestor = this;
  StiTreeNode *  previous;
  
  do {
    previous = ancestor;
    ancestor = ancestor->getParent();
  } while (ancestor != 0);
  
  return previous;
}


//--------------------------------------------------------------
// Returns true if this node is the root of the tree.  The root is
// the only node in the tree with a 0 parent; every tree has exactly
// one root.
//
// @return	true if this node is the root of its tree
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::isRoot()
{
  return getParent() == 0;
}


//--------------------------------------------------------------
// Returns the node that follows this node in a preorder traversal of this
// node's tree.  Returns 0 if this node is the last node of the
// traversal.  This is an inefficient way to traverse the entire tree; use
// an enumeration, instead.
//
// @see	#preorderEnumeration
// @return	the node that follows this node in a preorder traversal, or
//		0 if this node is last
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getNextNode() 
{
  if (getChildCount() == 0) {
    // No children, so look for nextSibling
    StiDefaultMutableTreeNode *  nextSibling = getNextSibling();
    
    if (nextSibling == 0) {
      StiDefaultMutableTreeNode *  aNode = (StiDefaultMutableTreeNode * )getParent();
      
      do {
	if (aNode == 0) {
	  return 0;
	}
	
	nextSibling = aNode->getNextSibling();
	if (nextSibling != 0) {
	  return nextSibling;
	}
	
	aNode = (StiDefaultMutableTreeNode * )aNode->getParent();
      } while(true);
    } else {
      return nextSibling;
    }
  } else {
    return (StiDefaultMutableTreeNode * )getChildAt(0);
  }
}


//--------------------------------------------------------------
// Returns the node that precedes this node in a preorder traversal of
// this node's tree.  Returns 0 if this node is the first node of the
// traveral -- the root of the tree.  This is an inefficient way to
// traverse the entire tree; use an enumeration, instead.
//
// @see	#preorderEnumeration
// @return	the node that precedes this node in a preorder traversal, or
//		0 if this node is the first
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getPreviousNode() 
{
  StiDefaultMutableTreeNode *  previousSibling;
  StiDefaultMutableTreeNode *  myParent = (StiDefaultMutableTreeNode * )getParent();
  
  if (myParent == 0) {
    return 0;
  }
  
  previousSibling = getPreviousSibling();
  
  if (previousSibling != 0) {
    if (previousSibling->getChildCount() == 0)
      return previousSibling;
    else
      return previousSibling->getLastLeaf();
  } else {
    return myParent;
  }
}

//--------------------------------------------------------------
//  Child Queries
//--------------------------------------------------------------

//--------------------------------------------------------------
// Returns true if <code>aNode</code> is a child of this node.  If
// <code>aNode</code> is 0, this method returns false.
//
// @return	true if <code>aNode</code> is a child of this node; false if 
//  		<code>aNode</code> is 0
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::isNodeChild(StiTreeNode *  aNode) 
{
  bool retval;
  
  if (aNode == 0) {
    retval = false;
  } else {
    if (getChildCount() == 0) {
      retval = false;
    } else {
      retval = (aNode->getParent() == this);
    }
  }
  
  return retval;
}


//--------------------------------------------------------------
// Returns this node's first child.  If this node has no children,
//
// @return	the first child of this node
// @exception	NoSuchElementException	if this node has no children
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getFirstChild() 
{
  if (getChildCount() == 0) {
    cout << "StiDefaultMutableTreeNode::getFirstChild()  - ERROR" << endl 
	 << "        node has no children" << endl;
  }
  return getChildAt(0);
}


//--------------------------------------------------------------
// Returns this node's last child.  If this node has no children,
//
// @return	the last child of this node
// @exception	NoSuchElementException	if this node has no children
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getLastChild() 
{
  if (getChildCount() == 0) 
    {
      cout << "StiDefaultMutableTreeNode::getLastChild() - ERROR" << endl 
	   << "          node has no children" << endl;
    }
  return getChildAt(getChildCount()-1);
}


//--------------------------------------------------------------
// Returns the child in this node's child array that immediately
// follows <code>aChild</code>, which must be a child of this node.  If
// <code>aChild</code> is the last child, returns 0.  This method
// performs a linear search of this node's children for
// <code>aChild</code> and is O(n) where n is the number of children; to
// traverse the entire array of children, use an enumeration instead.
//
// @see		#children
// @exception	IllegalArgumentException if <code>aChild</code> is
//					0 or is not a child of this node
// @return	the child of this node that immediately follows
//		<code>aChild</code>
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getChildAfter(StiTreeNode *  aChild) 
{
  if (aChild == 0) {
    cout << "StiDefaultMutableTreeNode::getChildAfter(StiTreeNode *) - ERROR" << endl 
	 << "       argument is 0" << endl;
  }
  
  int index = getIndex(aChild);		// linear search
  
  if (index == -1) {
    cout << "StiDefaultMutableTreeNode::getChildAfter(StiTreeNode *) - ERROR" << endl 
	 << "       node is not a child" << endl;
  }
  
  if (index < getChildCount() - 1) {
    return getChildAt(index + 1);
  } else {
    return 0;
  }
}


//--------------------------------------------------------------
// Returns the child in this node's child array that immediately
// precedes <code>aChild</code>, which must be a child of this node.  If
// <code>aChild</code> is the first child, returns 0.  This method
// performs a linear search of this node's children for <code>aChild</code>
// and is O(n) where n is the number of children.
//
// @exception	IllegalArgumentException if <code>aChild</code> is 0
//						or is not a child of this node
// @return	the child of this node that immediately precedes
//		<code>aChild</code>
//--------------------------------------------------------------
StiTreeNode *  StiDefaultMutableTreeNode::getChildBefore(StiTreeNode *  aChild) 
{
  if (aChild == 0) {
    cout << "StiDefaultMutableTreeNode::getChildBefore(StiTreeNode *  aChild) - ERROR" << endl 
	 << "argument is 0" << endl;
}
  
  int index = getIndex(aChild);		// linear search
  
  if (index == -1) {
    cout << "StiDefaultMutableTreeNode:: - ERROR" << endl 
	 << "  --> argument is not a child" << endl;
  }
  
  if (index > 0) {
    return getChildAt(index - 1);
  } else {
    return 0;
  }
}


//--------------------------------------------------------------
//  Sibling Queries
//--------------------------------------------------------------


//--------------------------------------------------------------
// Returns true if <code>anotherNode</code> is a sibling of (has the
// same parent as) this node.  A node is its own sibling.  If
// <code>anotherNode</code> is 0, returns false.
//
// @param	anotherNode	node to test as sibling of this node
// @return	true if <code>anotherNode</code> is a sibling of this node
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::isNodeSibling(StiTreeNode *  anotherNode) 
{
  bool retval;
  
  if (anotherNode == 0) {
    retval = false;
  } else if (anotherNode == this) {
    retval = true;
  } else {
    StiTreeNode *   myParent = getParent();
    retval = (myParent != 0 && myParent == anotherNode->getParent());
    
    if (retval && !((StiDefaultMutableTreeNode * )getParent())
	->isNodeChild(anotherNode)) {
      cout << "StiDefaultMutableTreeNode::isNodeSibling(StiTreeNode *  anotherNode) - ERROR" << endl 
	   << "sibling has different parent" << endl;
    }
  }
  
  return retval;
}


//--------------------------------------------------------------
// Returns the number of siblings of this node.  A node is its own sibling
// (if it has no parent or no siblings, this method returns
// <code>1</code>).
//
// @return	the number of siblings of this node
//--------------------------------------------------------------
int StiDefaultMutableTreeNode::getSiblingCount() 
{
  StiTreeNode *  myParent = getParent();
  
  if (myParent == 0) {
    return 1;
  } else {
    return myParent->getChildCount();
  }
}


//--------------------------------------------------------------
// Returns the next sibling of this node in the parent's children array.
// Returns 0 if this node has no parent or is the parent's last child.
// This method performs a linear search that is O(n) where n is the number
// of children; to traverse the entire array, use the parent's child
// enumeration instead.
//
// @see	#children
// @return	the sibling of this node that immediately follows this node
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getNextSibling() 
{
  StiDefaultMutableTreeNode *  retval;
  
  StiDefaultMutableTreeNode *  myParent = (StiDefaultMutableTreeNode * )getParent();
  
  if (myParent == 0) {
    retval = 0;
  } else {
    retval = (StiDefaultMutableTreeNode * )myParent->getChildAfter(this);	// linear search
  }
  
  if (retval != 0 && !isNodeSibling(retval)) {
    cout << "StiDefaultMutableTreeNode::getNextSibling() - ERROR" << endl 
	 << "child of parent is not a sibling" << endl;
  }
  
  return retval;
}


//--------------------------------------------------------------
// Returns the previous sibling of this node in the parent's children
// array.  Returns 0 if this node has no parent or is the parent's
// first child.  This method performs a linear search that is O(n) where n
// is the number of children.
//
// @return	the sibling of this node that immediately precedes this node
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getPreviousSibling() 
{
  StiDefaultMutableTreeNode *  retval;
  
  StiDefaultMutableTreeNode *  myParent = (StiDefaultMutableTreeNode * )getParent();
  
  if (myParent == 0) {
    retval = 0;
  } else {
    retval = (StiDefaultMutableTreeNode * )myParent->getChildBefore(this);	// linear search
  }
  
  if (retval != 0 && !isNodeSibling(retval))
    {
      cout << "StiDefaultMutableTreeNode::getPreviousSibling() - ERROR" << endl
	   << "       child of parent is not a sibling" << endl;
    }
  return retval;
}



//--------------------------------------------------------------
//  Leaf Queries
//--------------------------------------------------------------

//--------------------------------------------------------------
// Returns true if this node has no children.  To distinguish between
// nodes that have no children and nodes that <i>cannot</i> have
// children (e.g. to distinguish files from empty directories), use this
// method in conjunction with <code>getAllowsChildren</code>
//
// @see	#getAllowsChildren
// @return	true if this node has no children
//--------------------------------------------------------------
bool StiDefaultMutableTreeNode::isLeaf() 
{
  return (getChildCount() == 0);
}


//--------------------------------------------------------------
// Finds and returns the first leaf that is a descendant of this node --
// either this node or its first child's first leaf.
// Returns this node if it is a leaf.
//
// @see	#isLeaf
// @see	#isNodeDescendant
// @return	the first leaf in the subtree rooted at this node
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getFirstLeaf() {
  StiDefaultMutableTreeNode *  node = this;
  
  while (!node->isLeaf()) {
    node = (StiDefaultMutableTreeNode * )node->getFirstChild();
  }
  
  return node;
}


//--------------------------------------------------------------
// Finds and returns the last leaf that is a descendant of this node --
// either this node or its last child's last leaf. 
// Returns this node if it is a leaf.
//
// @see	#isLeaf
// @see	#isNodeDescendant
// @return	the last leaf in the subtree rooted at this node
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getLastLeaf() 
{
  StiDefaultMutableTreeNode *  node = this;
  
  while (!node->isLeaf()) {
    node = (StiDefaultMutableTreeNode * )node->getLastChild();
  }
  
  return node;
}


//--------------------------------------------------------------
// Returns the leaf after this node or 0 if this node is the
// last leaf in the tree.
// <p>
// In this implementation of the <code>MutableNode</code> interface,
// this operation is very inefficient. In order to determine the
// next node, this method first performs a linear search in the 
// parent's child-list in order to find the current node. 
// <p>
// That implementation makes the operation suitable for short
// traversals from a known position. But to traverse all of the 
// leaves in the tree, you should use <code>depthFirstEnumeration</code>
// to enumerate the nodes in the tree and use <code>isLeaf</code>
// on each node to determine which are leaves.
//
// @see	#depthFirstEnumeration
// @see	#isLeaf
// @return	returns the next leaf past this node
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getNextLeaf() {
  StiDefaultMutableTreeNode *  nextSibling;
  StiDefaultMutableTreeNode *  myParent = (StiDefaultMutableTreeNode * )getParent();
  
  if (myParent == 0)
    return 0;
  
  nextSibling = getNextSibling();	// linear search
  
  if (nextSibling != 0)
    return nextSibling->getFirstLeaf();
  
  return myParent->getNextLeaf();	// tail recursion
}


//--------------------------------------------------------------
// Returns the leaf before this node or 0 if this node is the
// first leaf in the tree.
// <p>
// In this implementation of the <code>MutableNode</code> interface,
// this operation is very inefficient. In order to determine the
// previous node, this method first performs a linear search in the 
// parent's child-list in order to find the current node. 
// <p>
// That implementation makes the operation suitable for short
// traversals from a known position. But to traverse all of the 
// leaves in the tree, you should use <code>depthFirstEnumeration</code>
// to enumerate the nodes in the tree and use <code>isLeaf</code>
// on each node to determine which are leaves.
//
// @see		#depthFirstEnumeration
// @see		#isLeaf
// @return	returns the leaf before this node
//--------------------------------------------------------------
StiDefaultMutableTreeNode *  StiDefaultMutableTreeNode::getPreviousLeaf() 
{
  StiDefaultMutableTreeNode *  previousSibling;
  StiDefaultMutableTreeNode *  myParent = (StiDefaultMutableTreeNode * )getParent();
  
  if (myParent == 0)
    return 0;
  
  previousSibling = getPreviousSibling();	// linear search
  
  if (previousSibling != 0)
    return previousSibling->getLastLeaf();
  
  return myParent->getPreviousLeaf();		// tail recursion
}

