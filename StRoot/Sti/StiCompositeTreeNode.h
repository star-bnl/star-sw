//StiCompositeTreeNode
//M.L. Miller (Yale Software)
//07/01

#ifndef StiCompositeTreeNode_H
#define StiCompositeTreeNode_H

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using std::find;
using std::vector;
using std::string;
using std::cout;
using std::ostream;
using std::endl;

typedef double StiOrderKey_t;

template <class T>
class StiCompositeTreeNode
{
public:

    StiCompositeTreeNode();
    virtual ~StiCompositeTreeNode();
    
    //Sets
    void setName(const string&);
    
    void setOrderKey(const StiOrderKey_t&);
    
    void setData(T*);
    
    //Gets
    const string& getName() const;
    
    unsigned int getChildCount() const;
    
    StiCompositeTreeNode* getParent() const;
    
    const StiOrderKey_t& getOrderKey() const;
    
    T* getData() const;
    
    //Action
    virtual void add(StiCompositeTreeNode*);
    
public:
    //provide random access iterators to daughters
    typedef vector<StiCompositeTreeNode *> vec_type;
    typedef vector<StiCompositeTreeNode *> StiCompositeTreeNodeVector;
    
    vec_type::iterator begin();
    vec_type::iterator end();
    
    vec_type::const_iterator begin() const;
    vec_type::const_iterator end() const;
    
    vec_type::reverse_iterator rbegin();
    vec_type::reverse_iterator rend();
    
    vec_type::const_reverse_iterator rbegin() const;
    vec_type::const_reverse_iterator rend() const;
    
private:
    
    void setParent(StiCompositeTreeNode* val);
    
    vec_type mVec;
    StiCompositeTreeNode* mparent;
    T* mdata;  
    StiOrderKey_t mkey;
    string mname;
};

//Implementation

template <class T>
StiCompositeTreeNode<T>::StiCompositeTreeNode() : mparent(0), mdata(0), mkey(0) 
{
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
inline void StiCompositeTreeNode<T>::setOrderKey(const StiOrderKey_t& val)
{
    mkey=val;
}

template <class T>
inline void StiCompositeTreeNode<T>::setData(T* val) 
{
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
inline const StiOrderKey_t& StiCompositeTreeNode<T>::getOrderKey() const 
{
    return mkey;
}

template <class T>
inline T* StiCompositeTreeNode<T>::getData() const
{
    return mdata;
}

template <class T>
void StiCompositeTreeNode<T>::add(StiCompositeTreeNode* newChild) 
{
    if (!newChild) return;
    StiCompositeTreeNodeVector::iterator where = find(mVec.begin(), mVec.end(), newChild);
    //Remove if we see an efficiency penalty
    if (where!=end()) {
	cout <<"StiCompositeTreeNode::add()\t Child Already exists.  Abort"<<endl;
	return;
    }
    //else add to list, set parent
    newChild->setParent(this);
    mVec.push_back(newChild);
}

template <class T>
inline void StiCompositeTreeNode<T>::setParent(StiCompositeTreeNode* val) 
{
    if (mparent) {
	cout <<"StiCompositeTreeNode::setParent()\tError:\tparent already exists"<<endl;
	return;
	}
    mparent=val;
    return;
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

//For now, include some typdefs that will make for easy user includes
#include "StiDetector.h"
typedef StiDetector data_t;

#ifndef __CINT__
typedef StiCompositeTreeNode<data_t> data_node;
#else
class data_node;
#endif
  
typedef vector<data_node*> data_node_vec; 

//typedef StiObjectFactory<data_node> data_node_factory;

#endif
