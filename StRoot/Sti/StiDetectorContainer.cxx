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
	cout <<"StiDetectorContainer::setToDetector(double)\tError:\tFind radius failed"<<endl;
	mradial_it = mregion->begin();
    }
    mphi_it = (*mradial_it)->begin();
    return;
}

void StiDetectorContainer::setToDetector(double radius, double angle)
{
    //First, set the radius
    setToDetector(radius);
    
    //Now set the phi
    mphi_it = gFindClosestOrderKey((*mradial_it)->begin(), (*mradial_it)->end(), angle);
    if (mphi_it == (*mradial_it)->end()) {
	cout <<"StiDetectorContainer::setToDetector(double, double)\tError:\tFind Phi failed"<<endl;
	mphi_it = (*mradial_it)->begin();
    }
}

void StiDetectorContainer::setToDetector(StiDetector* layer)
{
    //StiCompositeLeafIterator<data_t> leafit(mroot); //Find the leaves
    
    SameData<data_t> mySameData;
    mySameData.thedata = layer;

    data_node_vec::const_iterator where = find_if(mLeafIt->const_begin(), mLeafIt->const_end(), mySameData);
    if (where==mLeafIt->const_end()) {
	cout <<"StiDetectorContainer::setToDetector(StiDetector*)\tError:\tlayer not found in leaves.  Reset and Abort"<<endl;
	reset();
	return;
    }
    else {
	setToLeaf(*where);
    }
}

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

void StiDetectorContainer::moveIn()
{
    if (mradial_it == mregion->begin() ) {
	cout <<"StiDetecotrContainer::moveIn()\tNowhere to go"<<endl;
	return;
    }
    else {
	double oldOrder = (*mphi_it)->getOrderKey();
	--mradial_it;
	mphi_it = gFindClosestOrderKey((*mradial_it)->begin(), (*mradial_it)->end(), oldOrder);
	if (mphi_it == (*mradial_it)->end()) {
	    cout <<"StiDetectorContainer::moveIn()\tError:\tFind Phi failed"<<endl;
	    mphi_it = (*mradial_it)->begin();
	}
	return;
    }
}

void StiDetectorContainer::moveOut()
{
    ++mradial_it;
    if (mradial_it == mregion->end()) {
	cout <<"StiDetectorContainer::moveOut()\tNowhere to go"<<endl;
	--mradial_it;
	return;
    }
    else {
	double oldOrder = (*mphi_it)->getOrderKey();
	mphi_it = gFindClosestOrderKey((*mradial_it)->begin(), (*mradial_it)->end(), oldOrder);
	if (mphi_it == (*mradial_it)->end()) {
	    cout <<"StiDetectorContainer::moveOut()\tError:\tFind Phi failed"<<endl;
	    mphi_it = (*mradial_it)->begin();
	}
	return;
    }
}

void StiDetectorContainer::movePlusPhi()
{
    ++mphi_it;
    if (mphi_it == (*mradial_it)->end()) { //Wrap around 2pi
	mphi_it = (*mradial_it)->begin();
    }
}

void StiDetectorContainer::moveMinusPhi()
{
    if (mphi_it == (*mradial_it)->begin()) { //Wrap around 2pi
	mphi_it = (*mradial_it)->end();
    }
    --mphi_it;
}

// Recursively load all detector definition files from the given directory.
void StiDetectorContainer::buildDetectors(data_node_factory* nodefactory, detector_factory* detfactory)
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

