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

#include "TString.h"

#include "StxDetectorNode.h"

struct StreamNodeName
{
    void operator()(const StxDetectorNode* node) {
	cout <<node->GetName()<<endl;
    }
};

class SameNodeName
{
public:
    SameNodeName(const Char_t *name) : mname(name) {};
    bool operator()(const StxDetectorNode* rhs) {
	return mname == rhs->GetName();
    }
    
private:
    SameNodeName(); //Not implemented
    TString mname;
};

struct StreamNodeData
{
    void operator()(const StxDetectorNode* node) {
	StxDetector* data = node->Data();
	//cout <<node->GetName()<<endl;
	if (data) {cout <<" "<<(*data)<<endl;}
    }
};

struct NodeLessThan
{
           bool operator() (const StxDetectorNode* lhs, const StxDetectorNode* rhs) {
	return ( lhs->OrderKey().key < rhs->OrderKey().key );
    }
    
};

struct SameOrderKey
{
    bool operator() (const StxDetectorNode* rhs) {
	return (morderKey.key == rhs->OrderKey().key );
    }
    StxOrderKey morderKey;
};

struct SameName
{
    bool operator() (const StxDetectorNode* rhs) {
	return (mname == rhs->GetName());
    }
    TString mname;
};

class IndexNode
{
public:
    IndexNode(unsigned int i) : counter(i) {};
    
    void operator() (StxDetectorNode* node) {
	//Order key is not meant to be altered by the average user,
	//so we have to copy and re-set.
	
	StxOrderKey newKey = node->OrderKey();
	newKey.index= counter++;
	node->SetOrderKey(newKey);
    }
    
private:
    IndexNode();
    unsigned int counter;
};

struct IndexDaughters
{
    void operator() (StxDetectorNode* node) {
	if (node->ChildCount()>0) {
	    //swap this to a one-line call, once it works:
	    //IndexNode<data_t> indexer(0);
	    for_each(node->begin(), node->end(), IndexNode(0) );
	    for_each(node->begin(), node->end(), IndexDaughters() );
	}
    }
};

struct SortDaughters
{
    void operator() (StxDetectorNode* node) {
	if ( node->ChildCount()>0 )
	    {
		sort(node->begin(), node->end(), NodeLessThan());
		for_each(node->begin(), node->end(), SortDaughters() );
	    }
    }
};

struct RecursiveStreamNode
{
    void operator() (StxDetectorNode* node) {
	//cout <<"Parent: "<<node->GetName()<<endl;
	cout <<"Name: "<<node->GetName()<<"\tOrderKey: "<<node->OrderKey()<<endl;
	if (node->ChildCount()>0) {
	    cout <<" Daughters"<<endl;
	    for_each(node->begin(), node->end(), RecursiveStreamNode());
	}
    }
};

class LeafFinder
{
public:
    typedef vector<StxDetectorNode*> tvector_t;
    
    LeafFinder(tvector_t& v)  : vec(v) {};
    
    void operator()(StxDetectorNode* node) {
	if (node->ChildCount()>0) {
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
inline T gFindClosestOrderKey(T begin, T end, const StxOrderKey& findThis)
{
    T where = end;
    double min = 1.e100;
    for (T it=begin; it!=end; ++it) {
	double val = fabs(findThis.key-(*it)->OrderKey().key);
	if (val<min) {
	    min=val;
	    where = it;
	}
    }
    return where;
}

struct SameData
{
    SameData(StxDetector* data) : thedata(data) {};
    const StxDetector* thedata;
    bool operator()(const StxDetectorNode* rhs) {
	return (thedata == rhs->Data());
    }
};

//Stream a pointer
struct PtrStreamer
{
    void operator()(const StxDetector* val) {
	cout <<*val<<endl;
    }
};

#endif
