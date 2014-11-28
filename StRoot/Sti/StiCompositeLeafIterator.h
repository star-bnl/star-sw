//StiCompositeLeafIterator.h
//M.L. Miller (Yale Software)
//07/01

/*! \class StiCompositeLeafIterator
  StiCompositeLeafIterator  is a templated iterator class that is
  complimenatary to StiCompositeTreeNode.  Given a pointer to a
  StiCompositeTreeNode in its  constructor, StiCompositeLeafIterator provides
  access to all of the leaves of that node.  Leaves are defined as nodes that
  have no daughters, and are actaully found using the templated helper class
  LeafFinder.
  StiCompositeLeafIterator stores pointers to the leaves in an internal
  container and provides limited access to the leaves.  Ultimately,
  StiCompositeLeafIterator should conform to the requirements of (at least)
  an STL forward iterator, <b>however it does not yet</b>.  As such, it
  will currently work when passed to some, but not all, STL algorithms.
  However, let it be noted that it provides access to
  const_iterators into the vector of leaves.  These are true STL iterators
  and can be passed to any algorithm that takes const_iterators.
  <p>
  Note that StiCompositeLeafIterator is a "meta" iterator.  That is, it
  is a combination of an iterator and a container.  As such, it must
  provide functionality for both.  This is reflected in its providal of
  operators, e.g., operator++(), and memberfunctions that denote the
  container characteristics, specifically begin() and end().  These names
  have been changed to const_begin() and const_end() to reflect
  that they return const_iterators.
  <p>
  \author M.L. Miller (Yale Software)
 */

/** \example StiCompositeLeafIterator_ex.cxx
 */

#ifndef StiCompositeLeafIterator_HH
#define StiCompositeLeafIterator_HH

#include <vector>
using std::vector;

#include "StiCompositeTreeNode.h"
#include "StlUtilities.h"

template <class T>
class StiCompositeLeafIterator
{
public:
    ///For internal convenience.
    typedef StiCompositeTreeNode<T> tnode_t;
    
    ///For internal convenience.
    typedef vector<tnode_t*> tnode_vec;

    ///Only the daughters of <b>node</b> will automatically be found.
    StiCompositeLeafIterator(tnode_t* node);

    ///Defualt destructor.
    virtual ~StiCompositeLeafIterator();
    
    ///Reset iterator to point to first leaf
    void reset();
    void unset(){;}
    
    ///Dereference iterator, just as an STL iterator.
    tnode_t* operator*() const;
    
    ///Define only prefix of ++ (only a forward iterator)
    void operator++();
    
    ///Define !=
    bool operator!=(const typename tnode_vec::const_iterator& rhs) {
	  return (mcurrentleaf != rhs);
    }

    ///Returns the number of leaves found for the tree node passed in
    /// constructor.
    unsigned int getLeafCount() const;
    
public:
    //Safe forward Access to leaves

    ///Return an iterator marking the beginning of the leaf vector
    typename tnode_vec::iterator begin() { return mleaves.begin();}

    
    ///Return an iterator marking the end of the leaf vector
    typename tnode_vec::iterator end() { return mleaves.end(); }

    
    ///Return a const_iterator marking the beginning of the leaf vector.
    typename tnode_vec::const_iterator const_begin() const {return mleaves.begin();}

    ///Return a const_iterator marking the end of the leaf vector.
    typename tnode_vec::const_iterator const_end() const {return mleaves.end();}
    
protected:
    ///This is not implemented.  One must pass a node to the constructor
    /// in order for the leaves to be found.
    StiCompositeLeafIterator();

    ///Internal function used to find leaves.  It is called in the constructor.
    void findLeaves();

    ///We store a pointer to the root of the tree for internal convenience.
    tnode_t* mcurrentnode;

    ///We have to store an interator into the leaf vector for traversal.
    typename tnode_vec::const_iterator mcurrentleaf;

    ///The vector of leaves.
    tnode_vec mleaves;
    
private:
};

/*!
  The created instance of StiCompositeLeafIterator will find only the leaves
  that belong to the argument to the constructor call, <b>node</b>.
  That is, the iterator will treat node as if it is the root of a tree.
  Suppose that node is itself has a parent, and that parent has leaves that
  exist on a branch other than node.  In such a case, these leaves will be
  ignored by StiCompositeLeafIterator.  Therefore, if one wants to find
  all possible leaves of a given tree, one must be sure that <b>node</b>
  corresponds to the true root of the tree.
 */
template <class T>
StiCompositeLeafIterator<T>::StiCompositeLeafIterator(tnode_t* node)
    : mcurrentnode(node) 
{
    findLeaves();
}

template <class T>
StiCompositeLeafIterator<T>::~StiCompositeLeafIterator()
{
}

/*! A call to reset does not invalidate the leaves that this iterator
  corresponds to.  Instead, it simple resets the iterator to point to the
  first leaf in the leaf vector.  Therefore, one may call reset() with
  impuninity.  However, this also means that a StiCompositeLeafIterator
  can never be assigned to point to a different collection of leaves.
  This can only be accomplished by constructing a new instance of
  StiCompositeLeafIterator that points to a different node.
 */
template <class T>
inline void StiCompositeLeafIterator<T>::reset()
{
    mcurrentleaf=mleaves.begin();
}

/*! Suppose that you had declared the following typedefs: <code> \n
  typedef StiCompositeTreeNode<Foo> tnode_t; \n
  typedef StiCompositeLeafIterator<Foo> tleafit_t; \n </code>
  And you added the following code: <code> \n
  tnode_t root; \n
  ... code to hang other nodes on the root ... \n </code>
  Then you dereference the leaf iterator as follows: <code> \n
  for (tleafit_t it(root); it!=it.const_end(); ++it) { \n
     Foo* myFoo = *it; \n
  }\n
  </code>
*/
template <class T>
inline typename StiCompositeLeafIterator<T>::tnode_t*
StiCompositeLeafIterator<T>::operator*() const
{
    return (*mcurrentleaf);
}

/*! This simply increments the iterator to point to the next leaf in the
  leave vector.
 */
template <class T>
inline void StiCompositeLeafIterator<T>::operator ++()
{
    ++mcurrentleaf;
}

/*! We provide the inequality operator that takes as an argument a real
  std::vector::const_iterator.  That is, if one has an iterator into a vector
  of type StiCompositeTreeNode<T>, then one can check if that iterator is
  not equal to the current internal state of this StiCompositeLeafIterator.
 */
/*
  template <class T>
  inline bool StiCompositeLeafIterator<T>::operator != (const tnode_vec::const_iterator& rhs) 
  {
  return (mcurrentleaf != rhs);
  }
*/

/*! This returns the number of leaves that can be found by following
  all possible paths downward from the node passed to the constructor.
*/
template <class T>
inline unsigned int StiCompositeLeafIterator<T>::getLeafCount() const 
{
    return mleaves.size();
}


/*! We provide safe forward access to the vector of leaves.  This is a
  real STL iterator (std::vector::const_iterator) and can be used
  accordingly.  It <b>can</b> be used to modify the container
  of leaves that the iterator points into. <p>
  begin() marks a valid iterator, and it points to the first entry
  in the leaf vector.
 */
/*
  template <class T>
  inline StiCompositeLeafIterator<T>::tnode_vec::iterator //return type
  StiCompositeLeafIterator<T>::begin()
  {
  return mleaves.begin();
  }
*/


/*! We provide safe forward access to the vector of leaves.  This is a
  real STL iterator (std::vector::const_iterator) and can be used
  accordingly.  It <b>can</b> be used to modify the container
  of leaves that the iterator points into. <p>
  const_begin() marks a valid iterator, and it points to the first entry
  in the leaf vector.
 */
/*
  template <class T>
  inline StiCompositeLeafIterator<T>::tnode_vec::iterator //return type
  StiCompositeLeafIterator<T>::end()
  {
  return mleaves.end();
  }
*/

/*! We provide safe forward access to the vector of leaves.  This is a
  real STL iterator (std::vector::const_iterator) and can be used
  accordingly.  Howver, it <b>cannot</b> be used to modify the container
  of leaves that the iterator points into. <p>
  const_begin() marks a valid iterator, and it points to the first entry
  in the leaf vector.
 */
/*
  template <class T>
  inline StiCompositeLeafIterator<T>::tnode_vec::const_iterator //return type
  StiCompositeLeafIterator<T>::const_begin() const
  {
  return mleaves.begin();
  }
*/

/*! We provide safe forward access to the vector of leaves.  This is a
  real STL iterator (std::vector::const_iterator) and can be used
  accordingly.  Howver, it <b>cannot</b> be used to modify the container
  of leaves that the iterator points into. <p>
  const_end() marks an <b>invalid</b> iterator, and it points to one entry
  <b>past</b> the last valid entry in the leaf vector.
 */
/*
  template <class T>
  inline StiCompositeLeafIterator<T>::tnode_vec::const_iterator //return type
  StiCompositeLeafIterator<T>::const_end() const
  {
  return mleaves.end();
  }

*/

template <class T>
void StiCompositeLeafIterator<T>::findLeaves() 
{
    LeafFinder<T> myLeafFinder(mleaves);
    myLeafFinder(mcurrentnode);
    reset();
}

#endif
