//StiCompositeLeafIterator.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiCompositeLeafIterator_HH
#define StiCompositeLeafIterator_HH

#include "StiCompositeTreeNode.h"
#include "StlUtilities.h"

template <class T>
class StiCompositeLeafIterator
{
public:
    
    typedef StiCompositeTreeNode<T> tnode_t;
    typedef vector<tnode_t*> tnode_vec;

    StiCompositeLeafIterator(tnode_t* node) :mcurrentnode(node) {findLeaves();}
    virtual ~StiCompositeLeafIterator() {};
    
    //Reset iterator to begin
    void reset() {mcurrentleaf=mleaves.begin();}
    
    //Dereference iterator, ala STL
    tnode_t* operator*() const {return (*mcurrentleaf);}
    
    //Define only prefix of ++ (only a forward iterator)
    void operator++() {++mcurrentleaf;}
    
    //Define !=
    bool operator!=(const tnode_vec::const_iterator& rhs) {
	return (mcurrentleaf != rhs);
    }
    
    unsigned int getLeafCount() const {return mleaves.size();}
    
    //Safe forward Access to leaves
    tnode_vec::const_iterator const_begin() const {return mleaves.begin();}
    tnode_vec::const_iterator const_end() const {return mleaves.end();}
    
protected:
    StiCompositeLeafIterator();
    void findLeaves() {
	LeafFinder<T> myLeafFinder(mleaves);
	myLeafFinder(mcurrentnode);
	reset();
    }
    
    tnode_t* mcurrentnode;
    tnode_vec::const_iterator mcurrentleaf;
    tnode_vec mleaves;
    
private:
};

#endif
