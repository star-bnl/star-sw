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
    _messenger <<"StiDetectorTreeBuilder::StiDetectorTreeBuilder() -I- Started/Done"<<endl;
}

StiDetectorTreeBuilder::~StiDetectorTreeBuilder()
{}

StiDetectorNode* StiDetectorTreeBuilder::build(StiDetectorBuilder * builder)
{
    _messenger <<"StiDetectorTreeBuilder::build() -I- Started"<<endl;
    if (mroot) 	{
	_messenger << "StiDetectorTreeBuilder::build()\tError!\troot tree already built"<<endl;
	throw logic_error("StiDetectorTreeBuilder::build() -E- Attempting to build on top of an existing detector");
    }
    if (!builder)
	throw logic_error("StiDetectorTreeBuilder::build() -E- no builder provided");
    if (!mnodefactory)
	throw logic_error("StiDetectorTreeBuilder::build() -E- no Factory<StiDetectorNode> provided");
    mDetectorBuilder = builder;
    _messenger <<"StiDetectorTreeBuilder::build() -I- Build root"<<endl;

    buildRoot();

    loopOnDetectors();

    _messenger <<"StiDetectorTreeBuilder::build() -I- Sort Tree"<<endl;

    //Now sort the tree:
    SortDaughters<StiDetector> mysorter;
    mysorter(mroot); 
    
    //Now index the tree to give efficient sibling traversal
    _messenger <<"StiDetectorTreeBuilder::build() -I- Index Tree"<<endl;
    IndexDaughters<StiDetector> myindexer;
    myindexer(mroot);
    _messenger <<"StiDetectorTreeBuilder::build() -I- Done"<<endl;
    return mroot;
}

void StiDetectorTreeBuilder::buildRoot()
{
    mroot = mnodefactory->getInstance();
    mroot->setName("star");
    //make 3 daughters
    StiDetectorNode* mid = mnodefactory->getInstance();
    StiDetectorNode* fwd = mnodefactory->getInstance();
    StiDetectorNode* bwd = mnodefactory->getInstance();
    
    mid->setName("midrapidity");
    fwd->setName("forwardrapidity");
    bwd->setName("backwardrapidity");
    
    StiOrderKey midKey;
    midKey.key = static_cast<double>(StiPlacement::kMidRapidity);
    StiOrderKey fwdKey;
    fwdKey.key = static_cast<double>(StiPlacement::kForwardRapidity);
    StiOrderKey bwdKey;
    bwdKey.key = static_cast<double>(StiPlacement::kBackwardRapidity);
    
    mid->setOrderKey(midKey);
    fwd->setOrderKey(fwdKey);
    bwd->setOrderKey(bwdKey);
    
    mroot->add(mid);
    mroot->add(fwd);
    mroot->add(bwd);
    
    mregion = mid;
}

///Add the given detector object to the detector tree.
void StiDetectorTreeBuilder::addToTree(StiDetector* layer)
{
    //Which region do we hang it on?

    StiPlacement* placement = layer->getPlacement();
    
    SameOrderKey<StiDetector> mySameOrderKey;
    StiOrderKey tempOrderKey;
    tempOrderKey.key = static_cast<double>( placement->getRegion() );
    mySameOrderKey.morderKey = tempOrderKey; //order is of type const StiOrderKey&
    
    StiDetectorNodeVector::iterator where = find_if(mroot->begin(), mroot->end(), mySameOrderKey); 
    
    if (where==mroot->end())
      throw runtime_error("StiDetectorTreeBuilder::addToTree(StiDetector* layer) -F- mid-rapidity region not found");

    //ok, now we have the region
    mregion = (*where);
    
    //Where do we hang in radius (or z-position for fwd/bwd)?
    StiPlacement::StiRegion theRegion = layer->getPlacement()->getRegion();
    
    StiOrderKey radius;
    string radstring;
    
    if ( theRegion == StiPlacement::kMidRapidity ) {
	radius.key = layer->getPlacement()->getLayerRadius();
	radstring = "_radius";
    }
    else if ( theRegion==StiPlacement::kForwardRapidity) {
	radius.key = layer->getPlacement()->getZcenter();
	radstring = "_zcenter";
    }
    else if ( theRegion==StiPlacement::kBackwardRapidity ) {
	//for backward, we have to sort by -1.*zCenter
	radius.key = -1.* layer->getPlacement()->getZcenter();
    }
    else 
      throw runtime_error("StiDetectorBuiler::addToTree() -F- unkown region requested");
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
StiDetectorNode* StiDetectorTreeBuilder::hangWhere(StiDetectorNode* parent, const StiOrderKey& order,
						   string& keystring)
{
    SameOrderKey<StiDetector> mySameOrderKey;
    mySameOrderKey.morderKey = order; //order is of type const StiOrderKey&
    
    StiDetectorNodeVector::iterator where = find_if(parent->begin(), parent->end(), mySameOrderKey);

    if (where == parent->end()) {
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
  _messenger << "StiDetectorTreeBuilder::loopOnDetectors() -I- Started"<<endl;
  while(mDetectorBuilder->hasMore())
    {
      StiDetector* detector = mDetectorBuilder->next();
      if (!detector)
	throw runtime_error("StiDetectorTreeBuilder::loopOnDetectors() -E- detector==0");
      detector->build();
      addToTree(detector);
      // add to by-name map
      _detectorFinder->addDetector(detector);
    }
  _messenger << "StiDetectorTreeBuilder::loopOnDetectors() -I- Done"<<endl;
  return;
}
