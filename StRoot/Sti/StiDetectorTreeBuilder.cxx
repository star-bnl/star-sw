//StiDetectorTreeBuilder.cxx
//M.L. Miller (Yale Software)
//07/01

#include <cassert>
#include "Stiostream.h"
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdexcept>
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
    cout <<"StiDetectorTreeBuilder::StiDetectorTreeBuilder() -I- Started/Done"<<endl;
}

StiDetectorTreeBuilder::~StiDetectorTreeBuilder()
{}

StiDetectorNode* StiDetectorTreeBuilder::build(StiDetectorBuilder * builder)
{
    cout <<"StiDetectorTreeBuilder::build() - Started"<<endl;
    if (mroot) 	
      {
	cout << "StiDetectorTreeBuilder::build()\tError!\troot tree already built"<<endl;
	assert(mroot);
      }
    assert(builder);
    assert(mnodefactory);
    mDetectorBuilder = builder;
    cout <<"StiDetectorTreeBuilder::build() -I- Build root"<<endl;
		
    buildRoot();
    loopOnDetectors();
    cout <<"StiDetectorTreeBuilder::build() -I- Sort Tree"<<endl;
    //Now sort the tree:
    SortDaughters<StiDetector> mysorter;
    //mysorter(mregion); (old)
    mysorter(mroot); //new (MLM)
    //Now index the tree to give efficient sibling traversal
    cout <<"StiDetectorTreeBuilder::build() -I- Index Tree"<<endl;
    IndexDaughters<StiDetector> myindexer;
    myindexer(mroot);
    //myindexer(mregion);
    cout <<"StiDetectorTreeBuilder::build() -I- Done"<<endl;
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
  //cout << "StiDetectorTreeBuilder::addToTree(StiDetector*) -I- Started"<<endl;
  //Which region do we hang it on?
  StiPlacement* placement = layer->getPlacement();
  SameOrderKey<StiDetector> mySameOrderKey;
  StiOrderKey tempOrderKey;
  tempOrderKey.key = static_cast<double>( placement->getRegion() );
  mySameOrderKey.morderKey = tempOrderKey; //order is of type const StiOrderKey&
  StiDetectorNodeVector::iterator where = find_if(mroot->begin(), mroot->end(), mySameOrderKey); 
  
  if (where==mroot->end()) {
    //must abort!!! If this happens do not go on!!!
    cout <<"StiDetectorContainer::build() - ERROR - mid-rapidity region not found - where==0"<<endl;
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
	cout <<"StiDetectorBuiler::addToTree().  unkown region:\t"<<theRegion
	     <<"\tfrom detector:\t"<<layer->getName()<<"\tabort"<<endl;
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
	//cout <<"hangWhere().  Start new node"<<endl;
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
  //cout << "StiDetectorTreeBuilder::loopOnDetectors() -I- Started"<<endl;
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
  // cout << "StiDetectorTreeBuilder::loopOnDetectors() -I- Done"<<endl;
  return;
}
