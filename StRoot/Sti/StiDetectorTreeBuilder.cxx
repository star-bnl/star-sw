//StiDetectorTreeBuilder.cxx
//M.L. Miller (Yale Software)
//07/01

//Std
#include <iostream>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>

//Sti
#include "Messenger.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiCompositeTreeNode.h"
#include "StiDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "StiCodedDetectorBuilder.h"
#include "StiDetectorFinder.h" 

ostream& operator<<(ostream&, const StiDetector&);

StiDetectorTreeBuilder::StiDetectorTreeBuilder()
    : mroot(0), mnodefactory(0), mdetfactory(0), mregion(0)
{
  mDetectorBuilder = new StiCodedDetectorBuilder();
}

StiDetectorTreeBuilder::~StiDetectorTreeBuilder()
{
  delete mDetectorBuilder;
}

data_node* StiDetectorTreeBuilder::build(StiObjectFactoryInterface<StiDetectorNode>* nodefactory,
					 StiObjectFactoryInterface<StiDetector>* detfactory)
{
    if (mroot) {
	*(Messenger::instance(MessageType::kDetectorMessage)) << "StiDetectorTreeBuilder::build()\tError!\t"
						 << "root tree already built"<<endl;
	return 0;
    }
    
    mnodefactory = nodefactory;
    mdetfactory = detfactory;
    buildRoot();
    loopOnDetectors();
    SortDaughters<data_t> mysorter;
    mysorter(mregion);
    
    return mroot;
}

void StiDetectorTreeBuilder::buildRoot()
{
    mroot = mnodefactory->getObject();
    mroot->setName("star");

    //make 3 daughters
    data_node* mid = mnodefactory->getObject();
    mid->setName("midrapidity");

    mroot->add(mid);
    mregion = mid;
}


void StiDetectorTreeBuilder::addToTree(StiDetector* layer)
{
    //Where do we hang in radius?
    StiOrderKey_t radius = layer->getPlacement()->getLayerRadius();
    string radstring = "_radius";
    data_node* radialnode = hangWhere(mregion, radius, radstring);

    //Where do we hang in phi?
    StiOrderKey_t refAngle = layer->getPlacement()->getCenterRefAngle();
    string phistring = "_refAngle";
    data_node* phinode = hangWhere(radialnode, refAngle, phistring);
    phinode->setData(layer);
}

// Starting with the given parent, use the ordering key of the given type
// to determine where the new detector should be hung.
data_node* StiDetectorTreeBuilder::hangWhere(
    data_node* parent, StiOrderKey_t order, string& keystring)
{
    SameOrderKey<data_t> mySameOrderKey;
    mySameOrderKey.morderKey = order;

    data_node_vec::iterator where = find_if(parent->begin(), parent->end(), 
                                            mySameOrderKey);

    if (where == parent->end()) {
	data_node* temp = mnodefactory->getObject();
	char* tempname = new char[100];
	sprintf(tempname,"_%f", order);
	keystring.append(tempname);
	string newname = parent->getName();
	newname.append(keystring);
	
	temp->setName(newname);
	temp->setOrderKey(order);
	parent->add(temp);
	delete tempname;
	return temp;
    }
    else {
	return (*where);
    }
}

void StiDetectorTreeBuilder::loopOnDetectors()
{

  while(mDetectorBuilder->hasMore()){
 
    StiDetector* layer = mdetfactory->getObject();
    mDetectorBuilder->fillNext(layer);

    addToTree(layer);

    // add to by-name map
    StiDetectorFinder *pFinder = StiDetectorFinder::instance();
    pFinder->addDetector(layer);

  }

  return;
}
