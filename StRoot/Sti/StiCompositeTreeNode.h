//StiCompositeTreeNode
//M.L. Miller (Yale Software)
//07/01

#ifndef StiCompositeTreeNode_H
#define StiCompositeTreeNode_H

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#include "StiObjectFactory.h"

using std::find;
using std::vector;
using std::string;
using std::cout;
using std::ostream;
using std::endl;

typedef double StiOrderKey_t;

template <class T>
class StiCompositeTreeNode : public vector<StiCompositeTreeNode<T>*>
{
    typedef vector<StiCompositeTreeNode *> StiCompositeTreeNodeVector;	
    
public:
    
    StiCompositeTreeNode() : mparent(0), mdata(0), mkey(0) {};
    virtual ~StiCompositeTreeNode() {};
    
    //Sets -----------
    void setName(const string& name) {mname=name;}
    
    void setOrderKey(const StiOrderKey_t& val) {mkey=val;}
    
    void setData(T* val) {mdata=val;}
    
    //Gets -----------
    const string& getName() const {return mname;}
    
    unsigned int getChildCount() const {return size();}
    
    StiCompositeTreeNode* getParent() const {return mparent;}
    
    const StiOrderKey_t& getOrderKey() const {return mkey;}
    
    T* getData() const {return mdata;}
    
    //Action ---------
    
    virtual void add(StiCompositeTreeNode *  newChild) {
	if (!newChild) return;
	StiCompositeTreeNodeVector::iterator where = find(begin(), end(), newChild);
	//Remove if we see an efficiency penalty
	if (where!=end()) {
	    cout <<"StiCompositeTreeNode::add()\t Child Already exists.  Abort"<<endl;
	    return;
	}
	//else add to list, set parent
	newChild->setParent(this);
	push_back(newChild);
    }
    
protected:
    
    //push_back the vector only via StiCompositeTreeNode::add() method
    virtual void push_back(StiCompositeTreeNode* val) {
	StiCompositeTreeNodeVector::push_back(val);
    }
    
private:
    
    void setParent(StiCompositeTreeNode* val) {
	if (mparent) {
	    cout <<"StiCompositeTreeNode::setParent()\tError:\tparent already exists"<<endl;
	    return;
	}
	mparent=val;
	return;
    }
    
    StiCompositeTreeNode* mparent;
    T* mdata;  
    StiOrderKey_t mkey;
    string mname;
};

//For now, include some typdefs that will make for easy user includes
#include "StiDetector.h"
typedef StiDetector data_t;

#ifndef __CINT__
typedef StiCompositeTreeNode<data_t> data_node;
#else
class data_node;
#endif

typedef vector<data_node*> data_node_vec; 
typedef StiObjectFactory<data_node> data_node_factory;

#endif
