//StlUtilities.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StlUtilities_HH
#define StlUtilities_HH

#include "Stiostream.h"

#include <algorithm>
using std::for_each;
using std::find_if;
using std::sort;

#include <string>
using std::string;

#include "StiCompositeTreeNode.h"

template <class T>
struct StreamNodeName
{
    void operator()(const StiCompositeTreeNode<T>* node) {
	cout <<node->getName()<<endl;
    }
};

template <class T>
class SameNodeName
{
public:
    SameNodeName(const string& name) : mname(name) {};
    bool operator()(const StiCompositeTreeNode<T>* rhs) {
	return mname == rhs->getName();
    }
    
private:
    SameNodeName(); //Not implemented
    string mname;
};

template <class T>
struct StreamNodeData
{
    void operator()(const StiCompositeTreeNode<T>* node) {
	T* data = node->getData();
	//cout <<node->getName()<<endl;
	if (data) {cout <<" "<<(*data)<<endl;}
    }
};

template <class T>
struct NodeLessThan
{
    bool operator() (const StiCompositeTreeNode<T>* lhs, const StiCompositeTreeNode<T>* rhs) {
	return ( lhs->getOrderKey().key < rhs->getOrderKey().key );
    }
    
};

template <class T>
struct SameOrderKey
{
    bool operator() (const StiCompositeTreeNode<T>* rhs) {
	return (morderKey.key == rhs->getOrderKey().key );
    }
    StiOrderKey morderKey;
};

template <class T>
struct SameName
{
    bool operator() (const StiCompositeTreeNode<T>* rhs) {
	return (mname == rhs->getName());
    }
    string mname;
};

template <class T>
class IndexNode
{
public:
    IndexNode(unsigned int i) : counter(i) {};
    
    void operator() (StiCompositeTreeNode<T>* node) {
	//Order key is not meant to be altered by the average user,
	//so we have to copy and re-set.
	
	StiOrderKey newKey = node->getOrderKey();
	newKey.index= counter++;
	node->setOrderKey(newKey);
    }
    
private:
    IndexNode();
    unsigned int counter;
};

template <class T>
struct IndexDaughters
{
    void operator() (StiCompositeTreeNode<T>* node) {
	if (node->getChildCount()>0) {
	    //swap this to a one-line call, once it works:
	    //IndexNode<data_t> indexer(0);
	    for_each(node->begin(), node->end(), IndexNode<T>(0) );
	    for_each(node->begin(), node->end(), IndexDaughters() );
	}
    }
};

template <class T>
struct SortDaughters
{
    void operator() (StiCompositeTreeNode<T>* node) {
	if ( node->getChildCount()>0 )
	    {
		sort(node->begin(), node->end(), NodeLessThan<T>());
		for_each(node->begin(), node->end(), SortDaughters() );
	    }
    }
};

template <class T>
struct RecursiveStreamNode
{
    void operator() (StiCompositeTreeNode<T>* node) {
	//cout <<"Parent: "<<node->getName()<<endl;
	cout <<"Name: "<<node->getName()<<"\tOrderKey: "<<node->getOrderKey()<<endl;
	if (node->getChildCount()>0) {
	    cout <<" Daughters"<<endl;
	    for_each(node->begin(), node->end(), RecursiveStreamNode<T>());
	}
    }
};

template <class T>
struct DataNameLessThan
{
    typedef StiCompositeTreeNode<T> TNode;
    inline bool operator() (const TNode* lhs, TNode* rhs) {
	return lhs->getData()->getName() < rhs->getData()->getName();
    }
};

template <class T>
class LeafFinder
{
public:
    typedef vector<StiCompositeTreeNode<T>*> tvector_t;
    
    LeafFinder(tvector_t& v)  : vec(v) {};
    
    void operator()(StiCompositeTreeNode<T>* node) {
	if (node->getChildCount()>0) {
	    (*this) = for_each(node->begin(), node->end(), *this);
	}
	else {
	    vec.push_back(node );
	}
    }
    
    void operator=(const LeafFinder& rhs) {copyToThis(rhs);}
    
    tvector_t& vec;

private:
    void copyToThis(const LeafFinder& rhs) {vec = rhs.vec;}
    
    LeafFinder(); //Not implemented
};

template <class T>
inline T gFindClosestOrderKey(T begin, T end, const StiOrderKey& findThis)
{
    T where = end;
    double min = 1.e100;
    for (T it=begin; it!=end; ++it) {
	double val = fabs(findThis.key-(*it)->getOrderKey().key);
	if (val<min) {
	    min=val;
	    where = it;
	}
    }
    return where;
}

template <class T>
struct SameData
{
    SameData(T* data) : thedata(data) {};
    const T* thedata;
    bool operator()(const StiCompositeTreeNode<T>* rhs) {
	return (thedata == rhs->getData());
    }
};

//Stream a pointer
template <class T>
struct PtrStreamer
{
    void operator()(const T* val) {
	cout <<*val<<endl;
    }
};

#endif
