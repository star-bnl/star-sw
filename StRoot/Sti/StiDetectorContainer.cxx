//StiDetectorContainer.cxx
//M.L. Miller (Yale Software)
//02/02/01

//Std
#include <iostream.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string>

#include <algorithm>
using std::find_if;

//Sti
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

StiDetectorContainer::StiDetectorContainer() : mroot(0), mregion(0), mLeafIt(0)
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
    mradial_it = gFindClosestOrderKey(mregion->begin(), mregion->end(), radius);
    if (mphi_it == mregion->end()) {
	cout <<"StiDetectorContainer::setToDetector(double)\tError:\t";
	cout <<"Find radius failed"<<endl;
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
    mphi_it = gFindClosestOrderKey((*mradial_it)->begin(),
				   (*mradial_it)->end(), angle);
    if (mphi_it == (*mradial_it)->end()) {
	cout <<"StiDetectorContainer::setToDetector(double, double)\tError:\t";
	cout <<"Find Phi failed"<<endl;
	mphi_it = (*mradial_it)->begin();
    }
}

/*! This is used, e.g., to set the iterators to a certain point in
  preparation  for propogation of a new track.  If no StiDetector pointer is
  found that is equal to <b>layer</b>, then an error message is streamed to
  the screen and reset() is called.
*/
void StiDetectorContainer::setToDetector(StiDetector* layer)
{
    SameData<data_t> mySameData;
    mySameData.thedata = layer;

    data_node_vec::const_iterator where = find_if(mLeafIt->const_begin(),
						  mLeafIt->const_end(),
						  mySameData);
    if (where==mLeafIt->const_end()) {
	cout <<"StiDetectorContainer::setToDetector(StiDetector*)\tError:\t";
	cout <<"layer not found in leaves.  Reset and Abort"<<endl;
	reset();
	return;
    }
    else {
	setToLeaf(*where);
    }
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
void StiDetectorContainer::moveIn()
{
    if (mradial_it == mregion->begin() ) {
	cout <<"StiDetecotrContainer::moveIn()\tNowhere to go"<<endl;
	return;
    }
    else {
	double oldOrder = (*mphi_it)->getOrderKey();
	--mradial_it;
	mphi_it = gFindClosestOrderKey((*mradial_it)->begin(),
				       (*mradial_it)->end(), oldOrder);
	if (mphi_it == (*mradial_it)->end()) {
	    cout <<"StiDetectorContainer::moveIn()\tError:\t";
	    cout <<"Find Phi failed"<<endl;
	    mphi_it = (*mradial_it)->begin();
	}
	return;
    }
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
void StiDetectorContainer::moveOut()
{
    ++mradial_it;
    if (mradial_it == mregion->end()) {
	cout <<"StiDetectorContainer::moveOut(). ERROR:\t";
	cout <<"Nowhere to go"<<endl;
	--mradial_it;
	return;
    }
    else {
	double oldOrder = (*mphi_it)->getOrderKey();
	mphi_it = gFindClosestOrderKey((*mradial_it)->begin(),
				       (*mradial_it)->end(), oldOrder);
	if (mphi_it == (*mradial_it)->end()) {
	    cout <<"StiDetectorContainer::moveOut()\tError:\t";
	    cout <<"Find Phi failed"<<endl;
	    mphi_it = (*mradial_it)->begin();
	}
	return;
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
StiDetectorContainer::buildDetectors(StiObjectFactoryInterface<StiDetectorNode>*
				     nodefactory,
				     StiObjectFactoryInterface<StiDetector>*
				     detfactory)
{
    StiDetectorTreeBuilder mybuilder;
    mroot = mybuilder.build(nodefactory, detfactory);

    //Set region to midrapidity, hard-coded for now, update later to allow for other regions
    SameName<data_t> mySameName;
    mySameName.mname = "midrapidity";
    data_node_vec::iterator where = find_if(mroot->begin(), mroot->end(), mySameName);
    if (where==mroot->end()) {
	cout <<"Error:\tmidrapidity region not found"<<endl;
    }
    //Find leaves
    mLeafIt = new StiCompositeLeafIterator<data_t>(mroot);

    mregion = (*where);
    reset();

    return;
}

void StiDetectorContainer::print() const
{
}

//We assume that the node is a leaf in phi
void StiDetectorContainer::setToLeaf(data_node* node)
{
    //cout <<"StiDetectorContainer::setToLeaf()"<<endl;
    data_node* parent_in_phi = node->getParent();
    mphi_it = find(parent_in_phi->begin(), parent_in_phi->end(), node);
    if (mphi_it == parent_in_phi->end() ) {
	cout <<"StiDetectorContainer::setToLeaf()\tError!\t parent in phi iterator not found"<<endl;
	return;
    }
    //Find where we are in radial ordering
    data_node* parent_in_radius = parent_in_phi->getParent();
    mradial_it = find(parent_in_radius->begin(), parent_in_radius->end(), parent_in_phi);
    if (mradial_it == parent_in_radius->end() ) {
	cout <<"StiDetectorContainer::setToLeaf()\tError!\t parent in radius iterator not found"<<endl;
	return;
    }
}

