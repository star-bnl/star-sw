//StiDetectorNodeLeafIterator.h
//M.L. Miller (Yale Software)
//07/01

/*! \class StiDetectorNodeLeafIterator
  StiDetectorNodeLeafIterator  is a templated iterator class that is
  complimenatary to StiDetectorNode.  Given a pointer to a
  StiDetectorNode in its  constructor, StiDetectorNodeLeafIterator provides
  access to all of the leaves of that node.  Leaves are defined as nodes that
  have no daughters, and are actaully found using the templated helper class
  LeafFinder.
  StiDetectorNodeLeafIterator stores pointers to the leaves in an internal
  container and provides limited access to the leaves.  Ultimately,
  StiDetectorNodeLeafIterator should conform to the requirements of (at least)
  an STL forward iterator, <b>however it does not yet</b>.  As such, it
  will currently work when passed to some, but not all, STL algorithms.
  However, let it be noted that it provides access to
  const_iterators into the vector of leaves.  These are true STL iterators
  and can be passed to any algorithm that takes const_iterators.
  <p>
  Note that StiDetectorNodeLeafIterator is a "meta" iterator.  That is, it
  is a combination of an iterator and a container.  As such, it must
  provide functionality for both.  This is reflected in its providal of
  operators, e.g., operator++(), and memberfunctions that denote the
  container characteristics, specifically begin() and end().  These names
  have been changed to const_begin() and const_end() to reflect
  that they return const_iterators.
  <p>
  \author M.L. Miller (Yale Software)
 */

/** \example StiDetectorNodeLeafIterator_ex.cxx
 */

#ifndef StiDetectorNodeLeafIterator_HH
#define StiDetectorNodeLeafIterator_HH

#include <vector>
using std::vector;

#include "StiDetectorNode.h"
#include "StlUtilities.h"

class StiDetectorNodeLeafIterator {
 public:
  //!For internal convenience.
  typedef StiDetectorNode tnode_t;
  typedef vector<tnode_t*> tnode_vec;
  //!Only the daughters of <b>node</b> will automatically be found.
  StiDetectorNodeLeafIterator(tnode_t* node) : mcurrentnode(node) {FindLeaves();}
  virtual ~StiDetectorNodeLeafIterator() {}
  void Reset() {mcurrentleaf=mleaves.begin();}
  void Unset() {}
  //!Dereference iterator, just as an STL iterator.
  tnode_t* operator*() const { return (*mcurrentleaf);}
  //!Define only prefix of ++ (only a forward iterator)
  void operator++() { ++mcurrentleaf;}
  Bool_t operator!=(const tnode_vec::const_iterator& rhs) { return (mcurrentleaf != rhs);}
  //!Returns the number of leaves found for the tree node passed in  constructor.
  UInt_t  GetLeafCount() const {return mleaves.size();}
public:
  //Safe forward Access to leaves
  //!Return an iterator marking the beginning of the leaf vector
  tnode_vec::iterator begin() { return mleaves.begin();}
  //!Return an iterator marking the end of the leaf vector
  tnode_vec::iterator end() { return mleaves.end(); }
  //!Return a const_iterator marking the beginning of the leaf vector.
  tnode_vec::const_iterator const_begin() const {return mleaves.begin();}
  //!Return a const_iterator marking the end of the leaf vector.
  tnode_vec::const_iterator const_end() const {return mleaves.end();}
protected:
  //!This is not implemented.  One must pass a node to the constructor
  //! in order for the leaves to be found.
  StiDetectorNodeLeafIterator();
  //!Internal function used to find leaves.  It is called in the constructor.
  void FindLeaves() {LeafFinder myLeafFinder(mleaves); myLeafFinder(mcurrentnode); Reset();}
  //!We store a pointer to the Root of the tree for internal convenience.
  tnode_t* mcurrentnode;
  //!We have to store an interator into the leaf vector for traversal.
  tnode_vec::const_iterator mcurrentleaf;
  //!The vector of leaves.
  tnode_vec mleaves;
};
#endif
