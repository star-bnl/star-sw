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
    cout <<"StiDetectorTreeBuilder::StiDetectorTreeBuilder()\t";
    mDetectorBuilder = new StiCodedDetectorBuilder();
    cout <<"done"<<endl;
}

StiDetectorTreeBuilder::~StiDetectorTreeBuilder()
{
  delete mDetectorBuilder;
}

StiDetectorNode* StiDetectorTreeBuilder::build(Factory<StiDetectorNode>* nodefactory,
					 Factory<StiDetector>* detfactory)
{
    cout <<"StiDetectorTreeBuilder::build()"<<endl;
    if (mroot) {
	cout << "StiDetectorTreeBuilder::build()\tError!\t"
	     << "root tree already built"<<endl;
	return 0;
    }

    if (!nodefactory && !detfactory) {
	cout << "StiDetectorTreeBuilder::build()\tError!\t"
	     << "null factory pointer.  ABORT"<<endl;
	return 0;
    }
    
    mnodefactory = nodefactory;
    mdetfactory = detfactory;
    cout <<"Build root"<<endl;
    buildRoot();
    cout <<"loopOnDetectors"<<endl;
    loopOnDetectors();
    cout <<"Sort Tree"<<endl;
    //Now sort the tree:
    SortDaughters<StiDetector> mysorter;
    mysorter(mregion);

    //Now index the tree to give efficient sibling traversal
    cout <<"Index Tree"<<endl;
    IndexDaughters<StiDetector> myindexer;
    myindexer(mregion);
    cout <<"Done"<<endl;
    
    return mroot;
}

void StiDetectorTreeBuilder::buildRoot()
{
    mroot = mnodefactory->getInstance();
    mroot->setName("star");

    //make 3 daughters
    StiDetectorNode* mid = mnodefactory->getInstance();
    mid->setName("midrapidity");

    mroot->add(mid);
    mregion = mid;
}


void StiDetectorTreeBuilder::addToTree(StiDetector* layer)
{
    //Where do we hang in radius?
    StiOrderKey radius;
    radius.key = layer->getPlacement()->getLayerRadius();
    string radstring = "_radius";
    StiDetectorNode* radialnode = hangWhere(mregion, radius, radstring);

    //Where do we hang in phi?
    StiOrderKey refAngle;
    refAngle.key = layer->getPlacement()->getCenterRefAngle();
    string phistring = "_refAngle";
    StiDetectorNode* phinode = hangWhere(radialnode, refAngle, phistring);

    //Maintain the relationship between these two.
    //It's not so elegant to have the Detector know about the node that it's stored on, but
    //it's fast way to get from the detector to the node, and it allows the project to be
    //generally independent of the tree-node, ie, the tracker only has to know about
    //StiDetector objects.
    //So, we put the extra layer of coupling in StiDetector, not Tracke, SeedFinder, etc...
    phinode->setData(layer);
    layer->setTreeNode(phinode);
}

// Starting with the given parent, use the ordering key of the given type
// to determine where the new detector should be hung.
StiDetectorNode* StiDetectorTreeBuilder::hangWhere(
    StiDetectorNode* parent, const StiOrderKey& order, string& keystring)
{
    SameOrderKey<StiDetector> mySameOrderKey;
    mySameOrderKey.morderKey = order;

    StiDetectorNodeVector::iterator where = find_if(parent->begin(), parent->end(), 
                                            mySameOrderKey);

    if (where == parent->end()) {
	//cout <<"hangWhere().  Start new node"<<endl;
	StiDetectorNode* temp = mnodefactory->getInstance();
	char* tempname = new char[100];
	sprintf(tempname,"_%f", order.key);
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
 
    StiDetector* layer = mdetfactory->getInstance();
    mDetectorBuilder->fillNext(layer);

    addToTree(layer);

    // add to by-name map
    StiDetectorFinder *pFinder = StiDetectorFinder::instance();
    pFinder->addDetector(layer);

  }

  return;
}
