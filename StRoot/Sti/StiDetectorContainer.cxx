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

//StiGui
//#include "StiGui/StiRootDrawableDetector.h"

//Sti
#include "StiMapUtilities.h"
#include "StiDetector.h"
#include "StiMaterial.h"
#include "StiFactoryTypedefs.h"
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

StiDetectorContainer::StiDetectorContainer() : mroot(0), mregion(0)
{
    cout <<"StiDetectorContainer::StiDetectorContainer()"<<endl;
    sinstance = this;
}

StiDetectorContainer::~StiDetectorContainer()
{
    cout <<"StiDetectorContainer::~StiDetectorContainer()"<<endl;
//    clearAndDestroy();
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
    StiCompositeLeafIterator<data_t> leafit(mroot); //Find the leaves
    
    SameData<data_t> mySameData;
    mySameData.thedata = layer;

    data_node_vec::const_iterator where = find_if(leafit.const_begin(), leafit.const_end(), mySameData);
    if (where==leafit.const_end()) {
	cout <<"StiDetectorContainer::setToDetector(StiDetector*)\tError:\tlayer not found in leaves.  Reset and Abort"<<endl;
	reset();
	return;
    }
    else {
	setToStartPoint(*where);
    }
}

void StiDetectorContainer::reset()
{
    //mradial_it = mregion->begin();
    //mphi_it = (*mradial_it)->begin();
    mstart_it = mstartvec.begin();
    setToStartPoint(*mstart_it);
    
    return;
}

StiDetector* StiDetectorContainer::operator*() const
{
    return (*mphi_it)->getData();
}

void StiDetectorContainer::moveIn()
{
    //cout <<"moveIn()"<<endl;
    if (mradial_it == mregion->begin() ) {
	cout <<"StiDetecotrContainer::moveIn()\tNowhere to go"<<endl;
	//cout <<"leaving moveIn()"<<endl;
	return;
    }
    else {
	double oldOrder = (*mphi_it)->getOrderKey();
	//cout <<"\toldOrder:\t"<<oldOrder<<endl;
	--mradial_it;
	//cout <<"--'ed mradial_it"<<endl;
	//cout <<(*mradial_it)->getName()<<endl;
	mphi_it = gFindClosestOrderKey((*mradial_it)->begin(), (*mradial_it)->end(), oldOrder);
	//cout <<"did gFindClosestOrderKey"<<endl;
	if (mphi_it == (*mradial_it)->end()) {
	    cout <<"StiDetectorContainer::moveIn()\tError:\tFind Phi failed"<<endl;
	    mphi_it = (*mradial_it)->begin();
	}
	//cout <<"leaving moveIn()"<<endl;
	return;
    }
}

void StiDetectorContainer::moveOut()
{
    //cout <<"moveOut()"<<endl;
    ++mradial_it;
    if (mradial_it == mregion->end()) {
	cout <<"StiDetectorContainer::moveOut()\tNowhere to go"<<endl;
	--mradial_it;
	//cout <<"leaving moveOut()"<<endl;
	return;
    }
    else {
	double oldOrder = (*mphi_it)->getOrderKey();
	mphi_it = gFindClosestOrderKey((*mradial_it)->begin(), (*mradial_it)->end(), oldOrder);
	if (mphi_it == (*mradial_it)->end()) {
	    cout <<"StiDetectorContainer::moveOut()\tError:\tFind Phi failed"<<endl;
	    mphi_it = (*mradial_it)->begin();
	}
	//cout <<"leaving moveOut()"<<endl;
	return;
    }
}

void StiDetectorContainer::movePlusPhi()
{
    //cout <<"movePlusPhi()"<<endl;
    ++mphi_it;
    if (mphi_it == (*mradial_it)->end()) { //Wrap around 2pi
	//cout <<"StiDetectorContainer::movePlusPhi()\tWrap around 2pi"<<endl;
	mphi_it = (*mradial_it)->begin();
    }
    //cout <<"leaving movePlusPhi()"<<endl;
}

void StiDetectorContainer::moveMinusPhi()
{
    //cout <<"moveMinusPhi()"<<endl;
    if (mphi_it == (*mradial_it)->begin()) { //Wrap around 2pi
	//cout <<"StiDetectorContainer::moveMinusPhi()\tWrap around 2pi"<<endl;
	mphi_it = (*mradial_it)->end();
    }
    --mphi_it;
    //cout <<"leaving moveMinusPhi()"<<endl;
}

// Recursively load all detector definition files from the given directory.
void StiDetectorContainer::buildDetectors(const char* buildDirectory, data_node_factory* nodefactory,
					 detector_factory* detfactory)
{
    StiDetectorTreeBuilder mybuilder;
    mroot = mybuilder.build(buildDirectory, nodefactory, detfactory);

    //Set region to midrapidity, hard-coded for now, update later to allow for other regions
    SameName<data_t> mySameName;
    mySameName.mname = "midrapidity";
    data_node_vec::iterator where = find_if(mroot->begin(), mroot->end(), mySameName);
    if (where==mroot->end()) {
	cout <<"Error:\tmidrapidity region not found"<<endl;
    }
    mregion = (*where);
    findStartPoints();
    reset();

    return;
}

void StiDetectorContainer::print() const
{
}

bool StiDetectorContainer::hasMoreStartPoints() const
{
    data_node_vec::const_iterator last = mstartvec.end();
    --last;
    bool val = (mstart_it < last);
    //cout <<"hasMoreStartPoints():\t"<<val<<endl;
    return val;
}

void StiDetectorContainer::nextStartPoint()
{
    if (mstart_it != mstartvec.end()) {
	++mstart_it;
	setToStartPoint(*mstart_it);
    }
}

//void StiDetectorContainer::previousStartPoint()
//{
//  if (mstart_it != mstartvec.begin() ) {
//--mstart_it;
//setToStartPoint(*mstart_it);
//  }
//}

//Assume that the node is a leaf in phi
void StiDetectorContainer::setToStartPoint(data_node* node)
{
    //cout <<"StiDetectorContainer::setToStartPoint()"<<endl;
    //Find where we are in phi ordering
    //cout <<"Set To node:\t"<<node->getName()<<endl;
    data_node* parent_in_phi = node->getParent();
    //cout <<"Parent_in_phi:\t"<<parent_in_phi->getName()<<endl;
    mphi_it = find(parent_in_phi->begin(), parent_in_phi->end(), node);
    if (mphi_it == parent_in_phi->end() ) {
	cout <<"StiDetectorContainer::setToStartPoint()\tError!\t parent in phi iterator not found"<<endl;
	return;
    }
    //cout <<"(*phi_it):\t"<<(*mphi_it)->getName()<<endl;
    //Find where we are in radial ordering
    data_node* parent_in_radius = parent_in_phi->getParent();
    //cout <<"Parent_in_radius:\t"<<parent_in_radius->getName()<<endl;
    mradial_it = find(parent_in_radius->begin(), parent_in_radius->end(), parent_in_phi);
    if (mradial_it == parent_in_radius->end() ) {
	cout <<"StiDetectorContainer::setToStartPoint()\tError!\t parent in radius iterator not found"<<endl;
	return;
    }
    //cout <<"(*radial_it):\t"<<(*mradial_it)->getName()<<endl;
}

void StiDetectorContainer::findStartPoints() 
{
    if (!mroot) {
	cout <<"StiDetectorContainer::findStartPoints\tError!\troot undefined"<<endl;
	return;
    }

    //Find last radial point
    else if (mregion->size()<=0) {
	cout <<"StiDetectorContainer::findStartPoints\tError!\tRegion has zero size"<<endl;
	return;
    }
    else {
	data_node_vec::iterator radial_outside = (mregion->end());
	--radial_outside;
	//Get last points 
	for (data_node_vec::iterator it=(*radial_outside)->begin(); it!=(*radial_outside)->end(); ++it) {
	    mstartvec.push_back(*it);
	}
    }
    mstart_it = mstartvec.begin();
}
