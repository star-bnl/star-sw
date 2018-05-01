//StiDetectorTreeBuilder.cxx
//M.L. Miller (Yale Software)
//07/01

#include <cassert>
#include "Stiostream.h"
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdexcept>

#include "St_base/StMessMgr.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiCompositeTreeNode.h"
#include "StiDetectorBuilder.h"
#include "StiDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "StiToolkit.h"

ostream& operator<<(ostream&, const StiDetector&);

StiDetectorTreeBuilder::StiDetectorTreeBuilder()
    : mroot(0), 
      mnodefactory(StiToolkit::instance()->getDetectorNodeFactory()), 
    mregion(0)
{
    LOG_INFO <<"StiDetectorTreeBuilder::StiDetectorTreeBuilder() : Started/Done"<<endm;
}

StiDetectorTreeBuilder::~StiDetectorTreeBuilder()
{}

StiDetectorNode* StiDetectorTreeBuilder::build(StiDetectorBuilder * builder)
{
    LOG_INFO <<"StiDetectorTreeBuilder::build() : Started"<<endm;
    if (mroot) 	
      {
	LOG_INFO << "StiDetectorTreeBuilder::build()\tError!\troot tree already built"<<endm;
	assert(mroot);
      }
    assert(builder);
    assert(mnodefactory);
    mDetectorBuilder = builder;
    LOG_INFO <<"StiDetectorTreeBuilder::build() : Build root"<<endm;
		
    buildRoot();
    loopOnDetectors();
    LOG_INFO <<"StiDetectorTreeBuilder::build() : Sort Tree"<<endm;
    //Now sort the tree:
    SortDaughters<StiDetector> mysorter;
    //mysorter(mregion); (old)
    mysorter(mroot); //new (MLM)
    //Now index the tree to give efficient sibling traversal
    LOG_INFO <<"StiDetectorTreeBuilder::build() : Index Tree"<<endm;
    IndexDaughters<StiDetector> myindexer;
    myindexer(mroot);
    //myindexer(mregion);
    LOG_INFO <<"StiDetectorTreeBuilder::build() : Done"<<endm;
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


void StiDetectorTreeBuilder::addToTree(StiDetector* layer)
{
  //LOG_INFO << "StiDetectorTreeBuilder::addToTree(StiDetector*) : Started"<<endm;
  //Which region do we hang it on?
  StiPlacement* placement = layer->getPlacement();
  SameOrderKey<StiDetector> mySameOrderKey;
  StiOrderKey tempOrderKey;
  tempOrderKey.key = static_cast<double>( placement->getRegion() );
  mySameOrderKey.morderKey = tempOrderKey; //order is of type const StiOrderKey&
  StiDetectorNodeVector::iterator where = find_if(mroot->begin(), mroot->end(), mySameOrderKey); 
  
  if (where==mroot->end()) {
    //must abort!!! If this happens do not go on!!!
    LOG_ERROR <<"StiDetectorContainer::build() : mid-rapidity region not found - where==0"<<endm;
    abort();
  }

    //ok, now we have the region
    mregion = (*where);
    
    //Where do we hang in radius (or z-position for fwd/bwd)?
    StiPlacement::StiRegion theRegion = layer->getPlacement()->getRegion();
    
    StiOrderKey radius;
    string radstring;
    StiPlacement *place=layer->getPlacement();
    
    if ( theRegion == StiPlacement::kMidRapidity ) {
	radius.key = place->getLayerRadius();
	radstring = "_radius";
    }
    else {
	LOG_ERROR <<"StiDetectorBuiler::addToTree() : unkown region:\t"<<theRegion
	     <<"\tfrom detector:\t"<<layer->getName()<<"\tabort"<<endm;
	abort();
    }

    StiDetectorNode* radialnode = hangWhere(mregion, radius, radstring);
    //Where do we hang in phi?
    StiOrderKey refAngle;
    refAngle.key = layer->getPlacement()->getLayerAngle();
    string phistring = "_refAngle";
    StiDetectorNode* phinode = hangWhere(radialnode, refAngle, phistring);

   assert(phinode);
    //Maintain the relationship between these two.
    //It's not so elegant to have the Detector know about the node that it's stored on, but
    //it's fast way to get from the detector to the node, and it allows the project to be
    //generally independent of the tree-node, ie, the tracker only has to know about
    //StiDetector objects.
    //So, we put the extra layer of coupling in StiDetector, not Tracke, SeedFinder, etc...
    if (phinode->getData()) phinode = hangWhere(radialnode, refAngle, phistring,1);
    assert(!phinode->getData()); 
    phinode->setData(layer);
    layer->setTreeNode(phinode);

}

// Starting with the given parent, use the ordering key of the given type
// to determine where the new detector should be hung.
StiDetectorNode* StiDetectorTreeBuilder::hangWhere(StiDetectorNode* parent, const StiOrderKey& order,
						   string& keystring, int newOne)
{
    SameOrderKey<StiDetector> mySameOrderKey;
    mySameOrderKey.morderKey = order; //order is of type const StiOrderKey&
    
    StiDetectorNodeVector::iterator where = find_if(parent->begin(), parent->end(), mySameOrderKey);

    if (newOne || where == parent->end()) {
	//LOG_INFO <<"hangWhere().  Start new node"<<endm;
	StiDetectorNode* temp = mnodefactory->getInstance();
	char tempname[100];
	sprintf(tempname,"_%f", order.key);
	keystring.append(tempname);
	string newname = parent->getName();
	newname.append(keystring);
	
	temp->setName(newname);
	temp->setOrderKey(order);
	parent->add(temp);
	return temp;
    }
    else {
	return (*where);
    }
}

void StiDetectorTreeBuilder::loopOnDetectors()
{
  //LOG_INFO << "StiDetectorTreeBuilder::loopOnDetectors() : Started"<<endm;
  while(mDetectorBuilder->hasMore())
    {
      //StiDetector* layer = mdetfactory->getInstance();
      //mDetectorBuilder->fillNext(layer);
      StiDetector* detector = mDetectorBuilder->next();
      assert(detector);
      detector->build();
      addToTree(detector);
      // add to by-name map
    }
  // LOG_INFO << "StiDetectorTreeBuilder::loopOnDetectors() : Done"<<endm;
  return;
}
