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
    
    StiCompositeLeafIterator(tnode_t* node);
    virtual ~StiCompositeLeafIterator();
    
    //Reset iterator to first leaf
    void reset();
    
    //Dereference iterator, ala STL
    tnode_t* operator*() const;
    
    //Define only prefix of ++ (only a forward iterator)
    void operator++();
    
    //Define !=
    bool operator!=(const tnode_vec::const_iterator&);
    
    unsigned int getLeafCount() const;
    
public:
    //Safe forward Access to leaves
    tnode_vec::const_iterator const_begin() const;
    tnode_vec::const_iterator const_end() const;
    
protected:
    StiCompositeLeafIterator(); //not implemented
    void findLeaves();
    
    tnode_t* mcurrentnode;
    tnode_vec::const_iterator mcurrentleaf;
    tnode_vec mleaves;
    
private:
};

template <class T>
StiCompositeLeafIterator<T>::StiCompositeLeafIterator(tnode_t* node) : mcurrentnode(node) 
{
    findLeaves();
}

template <class T>
StiCompositeLeafIterator<T>::~StiCompositeLeafIterator()
{
}

template <class T>
inline void StiCompositeLeafIterator<T>::reset()
{
    mcurrentleaf=mleaves.begin();
}

template <class T>
inline StiCompositeLeafIterator<T>::tnode_t* StiCompositeLeafIterator<T>::operator*() const
{
    return (*mcurrentleaf);
}

template <class T>
inline void StiCompositeLeafIterator<T>::operator ++()
{
    ++mcurrentleaf;
}

template <class T>
inline bool StiCompositeLeafIterator<T>::operator != (const tnode_vec::const_iterator& rhs) 
{
    return (mcurrentleaf != rhs);
}

template <class T>
inline unsigned int StiCompositeLeafIterator<T>::getLeafCount() const 
{
    return mleaves.size();
}

template <class T>
inline StiCompositeLeafIterator<T>::tnode_vec::const_iterator //return type
StiCompositeLeafIterator<T>::const_begin() const
{
    return mleaves.begin();
}

template <class T>
inline StiCompositeLeafIterator<T>::tnode_vec::const_iterator //return type
StiCompositeLeafIterator<T>::const_end() const
{
    return mleaves.end();
}

template <class T>
void StiCompositeLeafIterator<T>::findLeaves() 
{
    LeafFinder<T> myLeafFinder(mleaves);
    myLeafFinder(mcurrentnode);
    reset();
}

#endif
