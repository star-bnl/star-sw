//StiDetectorContainer.cxx
//M.L. Miller (Yale Software)
//02/02/01

//Std
#include <iostream.h>
#include <math.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string>

#include <algorithm>
using std::find_if;
using std::for_each;
using std::binary_search;

//Sti
#include "Messenger.h"
#include "StiMapUtilities.h"
#include "StiDetector.h"
#include "StiMaterial.h"
#include "StiDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "StiCompositeLeafIterator.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"

StiDetectorContainer* StiDetectorContainer::sinstance = 0;

ostream& operator<<(ostream&, const NameMapKey&);
ostream& operator<<(ostream*, const StiDetector&);

StiDetectorContainer* StiDetectorContainer::instance()
{
    return (sinstance) ? sinstance : new StiDetectorContainer();
}

void StiDetectorContainer::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
    return;
} 

StiDetectorContainer::StiDetectorContainer()
    : mroot(0), mregion(0), mLeafIt(0),
      mMessenger( *Messenger::instance(MessageType::kDetectorMessage) )
{
    cout <<"StiDetectorContainer::StiDetectorContainer()"<<endl;
    sinstance = this;
}

StiDetectorContainer::~StiDetectorContainer()
{
    cout <<"StiDetectorContainer::~StiDetectorContainer()"<<endl;
    if (mLeafIt) {
	delete mLeafIt;
	mLeafIt=0;
    }
}

void StiDetectorContainer::setToDetector(double radius)
{
    StiOrderKey theKey;
    theKey.key = radius;
    mradial_it = gFindClosestOrderKey(mregion->begin(), mregion->end(), theKey);
    if (mphi_it == mregion->end()) {
	
	mMessenger <<"StiDetectorContainer::setToDetector(double)\tError:\t";
	mMessenger <<"Find radius failed"<<endl;
	
	mradial_it = mregion->begin();
    }
    mphi_it = (*mradial_it)->begin();
    return;
}

/*!If no detector exists at this position, the iterator is set to the
  innermost layer closest to phi=0, s.t. phi>0.
*/
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

	mMessenger <<"StiDetectorContainer::setToDetector(double, double)\tError:\t";
	mMessenger <<"Find Phi failed"<<endl;
	
	mphi_it = (*mradial_it)->begin();
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
    mradial_it = mregion->begin();
    mphi_it = (*mradial_it)->begin();
    return;
}

StiDetector* StiDetectorContainer::operator*() const
{
    return (*mphi_it)->getData();
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
    if (mradial_it == mregion->begin() ) {
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
    // mMessenger <<"StiDetectorContainer::moveOut()"<<endl;
    if ( (++mradial_it<mregion->end())==false) {
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
StiDetectorContainer::buildDetectors(Factory<StiDetectorNode>* nodefactory,
				     Factory<StiDetector>* detfactory)
{
    cout <<"StiDetectorContainer::buildDetectors"<<endl;
    cout <<"Make builder"<<endl;
    StiDetectorTreeBuilder mybuilder;
    cout <<"Use builder"<<endl;
    mroot = mybuilder.build(nodefactory, detfactory);

    //Set region to midrapidity, hard-coded for now, update later to allow for other regions
    SameName<StiDetector> mySameName;
    mySameName.mname = "midrapidity";
    StiDetectorNodeVector::iterator where = find_if(mroot->begin(), mroot->end(), mySameName);
    if (where==mroot->end()) {
	cout <<"Error:\tmidrapidity region not found"<<endl;
    }
    //Find leaves
    mLeafIt = new StiCompositeLeafIterator<StiDetector>(mroot);
    
    //Sort by name for O(log(n)) calls to setDetector()
    //sort(mLeafIt->begin(), mLeafIt->end(), DataNameLessThan<StiDetector>() );
    
    mregion = (*where);
    reset();

    //print();
    cout <<"Done"<<endl;
    
    return;
}

void StiDetectorContainer::print() const
{
    //ok, let's look at what we have:
    RecursiveStreamNode<StiDetector> myStreamer;
    myStreamer( mroot );

}

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
