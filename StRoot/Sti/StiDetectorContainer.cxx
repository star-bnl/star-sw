#include <Stiostream.h>
#include <math.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string>
#include <stdexcept>
#include <algorithm>
using std::find_if;
using std::for_each;
using std::binary_search;
#include "StiMapUtilities.h"
#include "StiDetector.h"
#include "StiDetectorBuilder.h"
#include "StiMaterial.h"
#include "StiDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "StiCompositeLeafIterator.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"

ostream& operator<<(ostream&, const NameMapKey&);
ostream& operator<<(ostream*, const StiDetector&);

StiDetectorContainer::StiDetectorContainer(const string & name, const string & description)
    : Named(name),
      Described(description),
      mroot(0), 
      mLeafIt(0)
{
    cout <<"StiDetectorContainer::StiDetectorContainer() -I- Started/Done"<<endl;
}

StiDetectorContainer::~StiDetectorContainer()
{
	cout <<"StiDetectorContainer::~StiDetectorContainer()"<<endl;
	if (mLeafIt) 
		{
			delete mLeafIt;
			mLeafIt=0;
    }
}

/*! This is used, e.g., to set the iterators to a certain point in
  preparation  for propogation of a new track.  If no StiDetector pointer is
  found that is equal to <b>layer</b>, then an error message is streamed to
  the screen and reset() is called.
*/
void StiDetectorContainer::setToDetector(const StiDetector* layer)
{

    if (!layer->getTreeNode()) {
	cout <<"StiDetectorContainer::setToDetector(StiDetector*). ERROR:\t"
		   <<"Detector has null node pointer.  Abort"<<endl;
	return;
    }
    setToLeaf( layer->getTreeNode() );
}

/*! A call to reset simply sets the pointer to the default StiDetector object.
  It does not alter the state of the detector model.
*/
void StiDetectorContainer::reset()
{
    SameOrderKey<StiDetector> mySameOrderKey;
    StiOrderKey tempOrderKey;
    tempOrderKey.key = static_cast<double>( StiPlacement::kMidRapidity );
    mySameOrderKey.morderKey = tempOrderKey; //order is of type const StiOrderKey&    
    StiDetectorNodeVector::iterator where = find_if(mroot->begin(), mroot->end(), mySameOrderKey); 
    
    if (where==mroot->end()) {
	//must abort!!! If this happens do not go on!!!
	cout <<"StiDetectorContainer::build() - ERROR - mid-rapidity region not found - where==0"<<endl;
	abort();
    }
    
    mregion = mregion = where;
    //This will seg fault if (*mregion)->begin()==(*mregion)->end() !!!!!!!
    mradial_it = (*mregion)->begin(); //change (MLM)
    mphi_it = (*mradial_it)->begin();
    
    return;
}

StiDetector* StiDetectorContainer::operator*() const
{
    //cout << "StiDetectorContainer::operator*() const - INFO - Started" <<endl;
//VP    if (!mphi_it)
//VP	throw runtime_error("StiDetectorContainer::operator*() const - FATAL - mphi_it==0");
    if (!(*mphi_it))
	throw runtime_error("StiDetectorContainer::operator*() const - FATAL - *mphi_it==0");
    StiDetector * det = (*mphi_it)->getData();
    if (!det)
	throw runtime_error("StiDetectorContainer::operator*() const - FATAL - *mphi_it==0");
    return det;
}

bool StiDetectorContainer::moveToNextRegion()
{
    cout <<"StiDetectorContainer::moveToNextRegion()"<<endl;
    StiDetectorNodeVector::const_iterator region = mregion;
    if ( ++region == mroot->end() )
      {
	cout <<"StiDetectorContainer::moveToNextRegion() -I- Nowhere to go."<<endl;
	return false;
      }
    //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK"<<endl;
    
    StiDetectorNodeVector::const_iterator radial_it = (*mregion)->begin();
    //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK 2"<<endl;
    if (radial_it==(*mregion)->end())
      return false;
    StiDetectorNodeVector::const_iterator phi_it = (*radial_it)->begin();
    //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK 3"<<endl;
    if (phi_it==(*radial_it)->end())
      return false;
    //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK 4"<<endl;
   
    mregion    = region;
    mradial_it = radial_it;
    mphi_it    = phi_it;
    //cout <<"StiDetectorContainer::moveToNextRegion() -I- moved to new region OK"<<endl;
    return true;
}

bool StiDetectorContainer::moveToPreviousRegion()
{
    if (mregion == mroot->begin() ) {
	cout <<"StiDetectorContainer::moveToPreviousRegion():\t Nowhere to go. return false"<<endl;
	return false;
    }

    --mregion;
    //now reset to beginning of region
    mradial_it = (*mregion)->begin();
    mphi_it = (*mradial_it)->begin();
    return true;
}

/*! A call to moveIn() may not always alter the StiDetector to which the
  container points.  Notably, if there is nowhere else to 'move in to', then
  moveIn() will have no action.  So, to see if the action succeeded, one must
  store a pointer to the StiDetector represented by the current state of the
  container, call moveIn(), and then check that the pointer to the
  StiDetector represented by the new state of the container is different than
  that of the previous state. <p>
  Additionally, when a call to moveIn() is made, the container 'selects' the
  StiDetector object that is closest in phi to the StiDetector object that is
  being 'movedIn' from.  Therefore, a call to moveIn() usually need not be
  followed by a call to movePlusPhi() or moveMinusPhi(), except in cases of
  extreme assymetry, such as navigation through the Silicon Vertex Tracker.
*/
bool StiDetectorContainer::moveIn()
{
    //if (mradial_it == mregion->begin() ) { //change (MLM)
    if (mradial_it == (*mregion)->begin() ) { //change (MLM)
	// cout <<"StiDetectorContainer::moveIn():\t";
	// cout <<"Nowhere to go. return false"<<endl;
	return false;
    }
    
    //remember where we started:
    const StiDetectorNode* oldPhiNode = *mphi_it;
    
    --mradial_it;
    mphi_it = (*mradial_it)->begin();

    if ( (*mradial_it)->getChildCount() == oldPhiNode->getParent()->getChildCount()) {
	// cout <<"Index into array"<<endl;
	mphi_it = (*mradial_it)->begin()+oldPhiNode->getOrderKey().index;
	return true;
    }
    else {
	// cout <<"Do linear search"<<endl;
	return setPhi( oldPhiNode->getOrderKey() );
    }
}

bool StiDetectorContainer::setPhi(const StiOrderKey& oldOrder)
{
    mphi_it = gFindClosestOrderKey((*mradial_it)->begin(),
				   (*mradial_it)->end(), oldOrder);
    if (mphi_it == (*mradial_it)->end()) {
	cout <<"StiDetectorContainer::setPhiIterator()\tError:\t";
	cout <<"Find Phi failed"<<endl;
	reset();
	return false;
    }
    //cout <<"setPhi(): oldOrder: "<<oldOrder<<"\tnewOrder: "<<(*mphi_it)->getOrderKey()<<endl;
    return true;
}

/*! A call to moveOut() may not always alter the StiDetector to which the
  container points.  Notably, if there is nowhere else to 'move out to', then
  moveOut() will have no action.  So, to see if the action succeeded, one must
  store a pointer to the StiDetector represented by the current state of the
  container, call moveOut(), and then check that the pointer to the
  StiDetector represented by the new state of the container is different than
  that of the previous state. <p>
  Additionally, when a call to moveOut() is made, the container 'selects' the
  StiDetector object that is closest in phi to the StiDetector object that is
  being 'movedOut' from.  Therefore, a call to moveIn() usually need not be
  followed by a call to movePlusPhi() or moveMinusPhi(), except in cases of
  extreme assymetry, such as navigation through the Silicon Vertex Tracker.
*/
bool StiDetectorContainer::moveOut()
{
    //remember where we started:
    const StiDetectorNode* oldPhiNode = *mphi_it;
    
    //if there's nowher to go, get out before doing work!
    // cout <<"StiDetectorContainer::moveOut()"<<endl;

    //if ( (++mradial_it<mregion->end())==false) { //change (MLM)
    if ( (++mradial_it<(*mregion)->end())==false) { //change (MLM)

	// cout <<"StiDetectorContainer::moveOut():\t";
	// cout <<"Nowhere to go. return false"<<endl;
	--mradial_it;
	return false;
    }
    
    if ( (*mradial_it)->getChildCount() == oldPhiNode->getParent()->getChildCount()) {
	// cout <<"Index into array"<<endl;
	mphi_it = (*mradial_it)->begin()+oldPhiNode->getOrderKey().index;
	return true;
    }
    else {
	// cout <<"Do linear search"<<endl;
	return setPhi( oldPhiNode->getOrderKey() );
    }
}

/*! Plus phi is defined as clockwise if viewing sectors 1-12 from the membrane,
  increasing phi in STAR TPC global coordinates.  A call to movePlusPhi() will
  always have a valid action.  That is, the call will wrap around past 2pi.
*/
void StiDetectorContainer::movePlusPhi()
{
    ++mphi_it;
    if (mphi_it == (*mradial_it)->end()) { //Wrap around 2pi
	mphi_it = (*mradial_it)->begin();
    }
}

/*! Minus phi is defined as counter-clockwise if viewing sectors 1-12 from
  the membrane,
  decreasing phi in STAR TPC global coordinates.  A call to moveMinusPhi() will
  always have a valid action.  That is, the call will wrap around past 2pi.
*/
void StiDetectorContainer::moveMinusPhi()
{
    if (mphi_it == (*mradial_it)->begin()) { //Wrap around 2pi
	mphi_it = (*mradial_it)->end();
    }
    --mphi_it;
}

/*! Recursively load all detector definition files from the given directory.
  There is internal protection to avoid building the detector representation
  more than once.
*/
void
StiDetectorContainer::build(StiDetectorBuilder * builder)
{
    cout <<"StiDetectorContainer::build() - INFO - Starting"<<endl;
    cout <<"StiDetectorContainer::build() - INFO - Building using builder:"<<builder->getName()<<endl;
    // build the volumes
    //builder->build();
    // pass volumes to TreeBuilder before we make like a tree and leave...
    StiDetectorTreeBuilder treeBuilder;
    mroot = treeBuilder.build(builder);
    if (!mroot)
			throw runtime_error("StiDetectorContainer::build() - ERROR - mroot==0");
    mLeafIt = new StiCompositeLeafIterator<StiDetector>(mroot);
    cout <<"StiDetectorContainer::build() - INFO - reset()"<<endl;
    return;
}
/*
  void StiDetectorContainer::print() const
  {
  RecursiveStreamNode<StiDetector> myStreamer;
  myStreamer( mroot );
  }*/

//We assume that the node is a leaf in phi
void StiDetectorContainer::setToLeaf(StiDetectorNode* leaf)
{
    //Now we try the new index-iterator scheme:
    mphi_it = leaf->whereInParent();
    if (mphi_it == leaf->end()) {
	cout <<"StiDetectorContainer::setToLeaf(StiDetectorNode*). ERROR:\t"
		   <<"Node not found in parent.  Abort"<<endl;
	reset();
	return;
    }

    StiDetectorNode* parentInRadius = (*mphi_it)->getParent();
    mradial_it = parentInRadius->whereInParent();
    if (mradial_it == parentInRadius->end()) {
	cout <<"StiDetectorContainer::setToLeaf(StiDetectorNode*). ERROR:\t"
		   <<"Node not found in parent.  Abort"<<endl;
	reset();
	return;
    }
    
    //cout <<"\nleaf: "<<leaf->getName()<<" "<<leaf->getOrderKey()<<endl;
    //cout <<"*mradial_it: "<<(*mradial_it)->getName()<<" "<<(*mradial_it)->getOrderKey()<<endl;
    //cout <<"*mphi_it: "<<(*mphi_it)->getName()<<" "<<(*mphi_it)->getOrderKey()<<endl;

}

/* remove
   void StiDetectorContainer::setToDetector(double radius)
   {
   StiOrderKey theKey;
   theKey.key = radius;
   //mradial_it = gFindClosestOrderKey(mregion->begin(), mregion->end(), theKey); //change (MLM)
   mradial_it = gFindClosestOrderKey((*mregion)->begin(), (*mregion)->end(), theKey); //change (MLM)
   
   //if (mphi_it == mregion->end()) { //change (MLM)
   if (mphi_it == (*mregion)->end()) { //change (MLM)
   
   cout <<"StiDetectorContainer::setToDetector(double)\tError:\t";
   cout <<"Find radius failed"<<endl;
   
   mradial_it = mregion->begin(); (//change (MLM)
   }
   mphi_it = (*mradial_it)->begin();
   return;
   }
*/

/*!If no detector exists at this position, the iterator is set to the
  innermost layer closest to phi=0, s.t. phi>0.
*/
/*
  void StiDetectorContainer::setToDetector(double radius, double angle)
  {
  //First, set the radius
  setToDetector(radius);
  
  //Now set the phi
  StiOrderKey theKey;
  theKey.key=angle;
  mphi_it = gFindClosestOrderKey((*mradial_it)->begin(),
  (*mradial_it)->end(), theKey);
  if (mphi_it == (*mradial_it)->end()) {
  
  cout <<"StiDetectorContainer::setToDetector(double, double)\tError:\t";
  cout <<"Find Phi failed"<<endl;
  
  mphi_it = (*mradial_it)->begin();
  }
  }
*/
