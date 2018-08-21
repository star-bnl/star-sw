//StxDetectorNode                        
//M.L. Miller (Yale Software)
//07/01

/*! \class StxDetectorNode
  StxDetectorNode is a templated class that can be used to represent
  objects
  in a tree structure.  The objects to be organized are stored as T* pointers by
  StxDetectorNode and are accessed via Data() and SetData(T*) methods.
  Additionaly, a StxDetectorNode can have 0 or 1 parent and 0-n daughters.
  A
  node with no parent is called a <b>Root</b>.  A node with no daughters is
  called a
  <b>leaf</b>.  Internally, the daughters treated as daughters stored in a
  vector.
  As such, StxDetectorNode provides access (via STL iterators) to the
  bounds
  of the vector. This has the consequence that traversal of the tree can
  be performed via recursive calls to the STL algorithms.  For such cases
  one merely needs to define funtors that either perform some action given
  a node (e.g., stream the node to screen), or evaluate whether a given node
  (or a comparison between two nodes) satisfies some logical condition.
  For more, see example in StlUtilities.h.
  <p>
  StxDetectorNode is a special tree-node class in that it was designed
  with the
  ability to store daughters in a sorted order, which is especially useful for
  efficient traversal through the tree.  One could manually sort the tree by
  the data
  stored, or one can use the StxOrderKey typedef.  Currently this typedef is
  set to
  a double, and one can eager cache the sort-key to avoid calls into the data
  structure.  Because StxDetectorNode provides random-access iterators
  into the
  daughters, STL algorithms can easily be used (recursively) to perform tasks
  on an
  entire tree (e.g., sort, find, for_each).  Many of these are already
  implemented
  in StlUtilities.h.
  <p>
  see also: StxCompositeLeafIterator
  
  \author M.L. Miller (Yale Software)
  
  \note A node with no childeren is called a 'leaf', or sometimes
  a 'data node'.  In reality, all StxDetectorNodes <b>can</b>
  hold data.  In practice, however, most uses of StxDetectorNode
  will store valid data only on leaves.
  
  \warning StxDetectorNode stores data by pointer, so only objects created
  by new should be passed as data to the node.
  
  \warning It is assumed that StxDetectorNode owns no objects!
  That means all nodes must be deleted manually deleted by user.
  This is not a thread safe class.
  
*/

#ifndef StxDetectorNode_H
#define StxDetectorNode_H

#include "Stiostream.h"
#include <iterator>
#include <vector>
#include "TString.h"
#include <algorithm>
#include "StxDetector.h"
#include "TNamed.h"
class StxDetectorNode;
class StxOrderKey;
using namespace std;
ostream& operator<<(ostream& os, const StxDetectorNode& node);
ostream& operator<<(ostream& os, const StxOrderKey& theKey);
/*! This is used to eager-cache information for sorting and/or traversal of
  the tree.
*/
struct StxOrderKey {
  StxOrderKey(Double_t k=0, UInt_t i=0) : key(k), index(i) {};
  Double_t key;
  UInt_t   index;
};

class StxDetectorNode : public TNamed {
 public:
  typedef vector<StxDetectorNode *> vec_type;
  typedef vector<StxDetectorNode *> StxDetectorNodeVector;
  StxDetectorNode() : TNamed(), mparent(0), mdata(0), mkey() {}
    ///Default Destructor
  virtual ~StxDetectorNode() {}
  //Sets
  void Reset(){}
  void Unset(){}
  void SetOrderKey(const StxOrderKey& val) { mkey=val; }
  void SetData(StxDetector* val) {mdata=val;}

  unsigned int ChildCount() const {return mVec.size();}
  const vec_type& getChildren() const {return *&mVec;}
  StxDetectorNode* Parent() const {return mparent;}
  const StxOrderKey& OrderKey() const {return mkey;}
  StxDetector* Data() const {return mdata;}
  void Print(Option_t *option="") const;
  //Action
  virtual void Add(StxDetectorNode*);
  vec_type::iterator whereInParent() {
    return (mparent) ? (mparent->begin()+mkey.index) : mVec.end();
  }
  vec_type::iterator begin() {return mVec.begin();}
  vec_type::iterator end() {return mVec.end();}
  vec_type::const_iterator begin() const {return mVec.begin();}
  vec_type::const_iterator end() const {return mVec.end();}
  vec_type::reverse_iterator rbegin() {return mVec.rbegin();}
  vec_type::reverse_iterator rend() {return mVec.rend();}
  vec_type::const_reverse_iterator rbegin() const {return mVec.rbegin();}
  vec_type::const_reverse_iterator rend() const {return mVec.rend();}
    
private:
  void SetParent(StxDetectorNode* val);
  vec_type mVec;
  StxDetectorNode* mparent;
  StxDetector* mdata;
  StxOrderKey mkey;
};


/*! For the sake of consistency in the parent-child relationship of one
  node to another, you will find no public method SetParent().  Instead,
  this task is performed internally by a call to Add().  Thus, once a
  node2 is added to node1 via node1->Add(node2), node1 is automatically set
  as the parent of node2, and no intervention by the user is necessary. <p>
  Additionally, node2 cannot be added as a daughter to node1 if node2 is
  already a daughter of node1.  In such a case, an internal check in the
  Add() method will recognize the situation and return with no action
  taken.
 */
#endif

