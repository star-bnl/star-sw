//StiDetectorTreeBuilder.cxx
//M.L. Miller (Yale Software)
//07/01

#include <iostream>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdexcept>
#include "Sti/Base/Messenger.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiCompositeTreeNode.h"
#include "StiDetectorBuilder.h"
#include "StiDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "StiDetectorFinder.h" 
#include "StiToolkit.h"

ostream& operator<<(ostream&, const StiDetector&);

StiDetectorTreeBuilder::StiDetectorTreeBuilder()
  : mroot(0), 
    mnodefactory(StiToolkit::instance()->getDetectorNodeFactory()), 
    mregion(0),
    _detectorFinder(StiDetectorFinder::instance() ),
    _messenger( *Messenger::instance(MessageType::kDetectorMessage) )
{
  _messenger <<"StiDetectorTreeBuilder::StiDetectorTreeBuilder() - INFO - Started/Done"<<endl;
}

StiDetectorTreeBuilder::~StiDetectorTreeBuilder()
{}

StiDetectorNode* StiDetectorTreeBuilder::build(StiDetectorBuilder * builder)
{
  _messenger <<"StiDetectorTreeBuilder::build() - Started"<<endl;
  if (mroot) 
    {
      _messenger << "StiDetectorTreeBuilder::build()\tError!\troot tree already built"<<endl;
      throw logic_error("StiDetectorTreeBuilder::build() - ERROR - Attempting to build on top of an existing detector");
    }
  if (!builder)
    throw logic_error("StiDetectorTreeBuilder::build() - ERROR - no builder provided");
  if (!mnodefactory)
    throw logic_error("StiDetectorTreeBuilder::build() - ERROR - no Factory<StiDetectorNode> provided");
  mDetectorBuilder = builder;
  _messenger <<"StiDetectorTreeBuilder::build() - INFO - Build root"<<endl;
  buildRoot();
  loopOnDetectors();
  _messenger <<"StiDetectorTreeBuilder::build() - INFO - Sort Tree"<<endl;
  //Now sort the tree:
  SortDaughters<StiDetector> mysorter;
  mysorter(mregion);
  
  //Now index the tree to give efficient sibling traversal
  _messenger <<"StiDetectorTreeBuilder::build() - INFO - Index Tree"<<endl;
  IndexDaughters<StiDetector> myindexer;
  myindexer(mregion);
  _messenger <<"StiDetectorTreeBuilder::build() - INFO - Done"<<endl;
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
	//_messenger <<"hangWhere().  Start new node"<<endl;
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
  _messenger << "StiDetectorTreeBuilder::loopOnDetectors() - INFO - Started"<<endl;
  while(mDetectorBuilder->hasMore())
    {
      //StiDetector* layer = mdetfactory->getInstance();
      //mDetectorBuilder->fillNext(layer);
      StiDetector* detector = mDetectorBuilder->next();
      if (!detector)
	throw runtime_error("StiDetectorTreeBuilder::loopOnDetectors() - ERROR - detector==0");
      detector->build();
      addToTree(detector);
      // add to by-name map
      _detectorFinder->addDetector(detector);
    }
  _messenger << "StiDetectorTreeBuilder::loopOnDetectors() - INFO - Done"<<endl;
  return;
}
