//StiCompositeTreeNode                        
//M.L. Miller (Yale Software)
//07/01

/*! \class StiCompositeTreeNode
  StiCompositeTreeNode is a templated class that can be used to represent
  objects
  in a tree structure.  The objects to be organized are stored as T* pointers by
  StiCompositeTreeNode and are accessed via getData() and setData(T*) methods.
  Additionaly, a StiCompositeTreeNode can have 0 or 1 parent and 0-n daughters.
  A
  node with no parent is called a <b>root</b>.  A node with no daughters is
  called a
  <b>leaf</b>.  Internally, the daughters treated as daughters stored in a
  vector.
  As such, StiCompositeTreeNode provides access (via STL iterators) to the
  bounds
  of the vector. This has the consequence that traversal of the tree can
  be performed via recursive calls to the STL algorithms.  For such cases
  one merely needs to define funtors that either perform some action given
  a node (e.g., stream the node to screen), or evaluate whether a given node
  (or a comparison between two nodes) satisfies some logical condition.
  For more, see example in StlUtilities.h.
  <p>
  StiCompositeTreeNode is a special tree-node class in that it was designed
  with the
  ability to store daughters in a sorted order, which is especially useful for
  efficient traversal through the tree.  One could manually sort the tree by
  the data
  stored, or one can use the StiOrderKey typedef.  Currently this typedef is
  set to
  a double, and one can eager cache the sort-key to avoid calls into the data
  structure.  Because StiCompositeTreeNode provides random-access iterators
  into the
  daughters, STL algorithms can easily be used (recursively) to perform tasks
  on an
  entire tree (e.g., sort, find, for_each).  Many of these are already
  implemented
  in StlUtilities.h.
  <p>
  see also: StiCompositeLeafIterator
  
  \author M.L. Miller (Yale Software)
  
  \note A node with no childeren is called a 'leaf', or sometimes
  a 'data node'.  In reality, all StiCompositeTreeNodes <b>can</b>
  hold data.  In practice, however, most uses of StiCompositeTreeNode
  will store valid data only on leaves.
  
  \warning StiCompositeTreeNode stores data by pointer, so only objects created
  by new should be passed as data to the node.
  
  \warning It is assumed that StiCompositeTreeNode owns no objects!
  That means all nodes must be deleted manually deleted by user.
  This is not a thread safe class.
  
*/

#ifndef StiCompositeTreeNode_H
#define StiCompositeTreeNode_H

#include "Stiostream.h"
#include <iterator>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

/*
  using std::find;
  using std::vector;
  using std::string;
  using std::cout;
  using std::ostream;
  using std::endl;
*/

/*! This is used to eager-cache information for sorting and/or traversal of
  the tree.
*/
struct StiOrderKey
{
    StiOrderKey() 
	: key(0.), index(0) {};
    StiOrderKey(double k, unsigned int i) 
	: key(k), index(i) {};
    
    double key;
    unsigned int index;
};

template <class T>
class StiCompositeTreeNode
{
public:

    ///We provide only a default constructor.
    StiCompositeTreeNode();

    ///Default Destructor
    virtual ~StiCompositeTreeNode();
    
    //Sets
    void reset(){;}
    void unset(){;}

    ///Set the name of the node
    void setName(const string&);

    ///Set the order-key for the node
    void setOrderKey(const StiOrderKey&);

    ///Set the data to be hung on the node.
    void setData(T*);
    
    //Gets

    ///Return the name of the node.
    const string& getName() const;

    ///Return the number of children that belong to this node
    unsigned int getChildCount() const;

    ///Return a (non-const!) pointer to the parent of this node.
    StiCompositeTreeNode* getParent() const;

    ///Return a reference to the the orderkey of this node.
    const StiOrderKey& getOrderKey() const;

    ///Return a (non-const!) pointer to the data hung on this node.
    T* getData() const;
    
    //Action

    ///Add a child to this node.
    virtual void add(StiCompositeTreeNode*);
    
public:
    //provide random access iterators to daughters

    ///For internal convenience
    typedef vector<StiCompositeTreeNode *> vec_type;
    
    ///For internal convenience
    typedef vector<StiCompositeTreeNode *> StiCompositeTreeNodeVector;

    ///Provide the iterator into the parent that can be dereferenced to get this node
    typename vec_type::iterator whereInParent() {
	return (mparent) ? (mparent->begin()+mkey.index) : mVec.end();
    }

    ///Provide random access iterator to the beginning of the vector of children
    typename vec_type::iterator begin() {return mVec.begin();}
        
    ///Provide random access iterator to the end of the vector of children.
    typename vec_type::iterator end() {return mVec.end();}

    ///Provide const_iterator to the beginning of the vector of children
    typename vec_type::const_iterator begin() const {return mVec.begin();}
    
    ///Provied const_iterator to the end of the vector of children.
    typename vec_type::const_iterator end() const {return mVec.end();}

    ///Provide reverse iterator to the beginning of the vector of children.
    typename vec_type::reverse_iterator rbegin() {return mVec.rbegin();}
    
    ///Provide reverse iterator to the end of the vector of children.
    typename vec_type::reverse_iterator rend() {return mVec.rend();}

    ///Provide const_reverse_iterator tot he beginning of the vector of children.
    typename vec_type::const_reverse_iterator rbegin() const {return mVec.rbegin();}
    
    ///Provide const_reverse_iterator to the end of the vector of children.
    typename vec_type::const_reverse_iterator rend() const {return mVec.rend();}
    
private:

    ///Set the parent for this node.  It can only be called internally
    /// to maintain consistency in all parent-child relationships.
    void setParent(StiCompositeTreeNode* val);

    ///The vector of children
    vec_type mVec;

    ///A pointer to the parent of this node
    StiCompositeTreeNode* mparent;

    ///A pointer to the data to hung on this node
    T* mdata;

    ///The order key association with this node
    StiOrderKey mkey;

    ///The name of the node.
    string mname;
};

//Implementation

/*! When the node is created all members are initialized to deafult
  values (namely 0 or null) and one must then set all information by hand.
*/
template <class T>
StiCompositeTreeNode<T>::StiCompositeTreeNode()
    : mparent(0), mdata(0)
{
    mkey.key=0.;
    mkey.index=0;
}

template <class T>
StiCompositeTreeNode<T>::~StiCompositeTreeNode()
{
}

template <class T>
inline void StiCompositeTreeNode<T>::setName(const string& val) 
{
    mname = val;
}

template <class T>
inline void StiCompositeTreeNode<T>::setOrderKey(const StiOrderKey& val)
{
    mkey=val;
}

/*! The pointer to data (T* mData) defaults to 'null' on creation.
  If no call to setData() is made, then mData remains set to 'null'.
 */
template <class T>
inline void StiCompositeTreeNode<T>::setData(T* val) 
{
    assert(!mdata);
    mdata=val;
}

template <class T>
inline const string& StiCompositeTreeNode<T>::getName() const 
{
    return mname;
}

template <class T>
inline unsigned int StiCompositeTreeNode<T>::getChildCount() const
{
    return mVec.size();
}

template <class T>
inline StiCompositeTreeNode<T>*
StiCompositeTreeNode<T>::getParent() const 
{
    return mparent;
}

template <class T>
inline const StiOrderKey& StiCompositeTreeNode<T>::getOrderKey() const 
{
    return mkey;
}

template <class T>
inline T* StiCompositeTreeNode<T>::getData() const
{
    return mdata;
}

/*! For the sake of consistency in the parent-child relationship of one
  node to another, you will find no public method setParent().  Instead,
  this task is performed internally by a call to add().  Thus, once a
  node2 is added to node1 via node1->add(node2), node1 is automatically set
  as the parent of node2, and no intervention by the user is necessary. <p>
  Additionally, node2 cannot be added as a daughter to node1 if node2 is
  already a daughter of node1.  In such a case, an internal check in the
  add() method will recognize the situation and return with no action
  taken.
 */
template <class T>
void StiCompositeTreeNode<T>::add(StiCompositeTreeNode* newChild) 
{
    if (!newChild) return;
    typename StiCompositeTreeNodeVector::iterator where = find(mVec.begin(), mVec.end(),
						      newChild);
    //Remove if we see an efficiency penalty
    if (where!=end()) {
	cout <<"StiCompositeTreeNode::add().  ERROR:\t";
	cout <<"Child Already exists.  Abort"<<endl;
	return;
    }
    //else add to list, set parent
    newChild->setParent(this);
    mVec.push_back(newChild);
}

/*! This is an internal method.  For more see documentation for the public
  method add().
 */
template <class T>
inline void StiCompositeTreeNode<T>::setParent(StiCompositeTreeNode* val) 
{
    if (mparent) {
	cout <<"StiCompositeTreeNode::setParent()\tError:\t";
	cout <<"parent already exists"<<endl;
	return;
    }
    mparent=val;
    return;
}

// \warning This iterator makes no sense for nodes with no parent.  In

/* \warning This iterator makes no sense for nodes with no parent.  In 
  that case whereInParent() returns end().  Therefore, a safe call to whereInParent() 
  would be as follows: <code> \n
  StiCompositeTreeNode<T>::vec_type::iterator where = node->whereInParent(); \n
  if (where!=node->end()) {\\you're ok \n
  }
  <\code>
  
  template <class T>
  StiCompositeTreeNode<T>::vec_type::iterator //return type
  StiCompositeTreeNode<T>::whereInParent()
  {
  return (mparent) ? (mparent->begin()+mkey.index) : mVec.end();
  }
  
  template <class T>
  StiCompositeTreeNode<T>::vec_type::iterator //return type
  StiCompositeTreeNode<T>::begin()
  {
  return mVec.begin();
  }

  template <class T>
  StiCompositeTreeNode<T>::vec_type::iterator //return type
  StiCompositeTreeNode<T>::end()
  {
  return mVec.end();
  }
  
  template <class T>
  StiCompositeTreeNode<T>::vec_type::const_iterator //return type
  StiCompositeTreeNode<T>::begin() const
  {
  return mVec.begin();
  }

  template <class T>
  StiCompositeTreeNode<T>::vec_type::const_iterator //return type
  StiCompositeTreeNode<T>::end() const
  {
  return mVec.end();
  }
  
  template <class T>
  StiCompositeTreeNode<T>::vec_type::reverse_iterator //return type
  StiCompositeTreeNode<T>::rbegin()
  {
  return mVec.rbegin();
  }

  template <class T>
  StiCompositeTreeNode<T>::vec_type::reverse_iterator //return type
  StiCompositeTreeNode<T>::rend()
  {
  return mVec.rend();
  }
  
  template <class T>
  StiCompositeTreeNode<T>::vec_type::const_reverse_iterator //return type
  StiCompositeTreeNode<T>::rbegin() const
  {
  return mVec.rbegin();
  }
  
  template <class T>
  StiCompositeTreeNode<T>::vec_type::const_reverse_iterator //return type
  StiCompositeTreeNode<T>::rend() const
  {
  return mVec.rend();
  }
*/

//For now, include some typdefs that will make for easy user includes
#include "StiDetector.h"
typedef StiDetector data_t;

#ifndef __CINT__
typedef StiCompositeTreeNode<StiDetector> StiDetectorNode;
#else
class StiDetectorNode;
#endif
typedef vector<StiDetectorNode*> StiDetectorNodeVector;

//non-members
inline ostream& operator<<(ostream& os, const StiOrderKey& theKey)
{
	return os<<"key: "<<theKey.key<<" index: "<<theKey.index;
}

#endif
