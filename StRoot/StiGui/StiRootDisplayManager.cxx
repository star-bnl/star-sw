//StiRootDisplayManager.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

//Root
#include "TCanvas.h"
#include "TShape.h"
#include "TVolume.h"
#include "TBRIK.h"
#include "TPolyMarker3D.h"
#include "StiTPolyMarker3D.h"

//SCL
#include "StThreeVector.hh"

//Sti
#include "Sti/StiDetector.h"

//StiGui
#include "StiDrawable.h"
#include "StiRootDrawable.h"
#include "StiDrawableHits.h"
#include "StiRootDrawableHits.h"
#include "StiRootDisplayManager.h"

StiRootDisplayManager* StiRootDisplayManager::sinstance = 0;

StiRootDisplayManager::StiRootDisplayManager(TCanvas* c)
    : mcanvas(c), mzone(0), mnode(0)
{
    cout <<"StiRootDisplayManager::StiRootDisplayManager(int, int, int)"<<endl;
    if (!mcanvas) {
	cout <<"StiRootDisplayManager::StiRootDisplayManager() ERROR:\t";
	cout <<"Canvas null.  Seg-fault"<<endl;
    }

    else {
	mcanvas->cd();
	
	mnode = new TVolume();
	mnode->SetName("mainnode");
	mnode->SetTitle("mainnode");
	//mnode = new TVolume("mainnode","mainnode", mzone);
	//mnode->SetVisibility(TVolume::kThisUnvisible);
    }
    
    cout <<"Leaving StiRootDisplayManager::StiRootDisplayManager()"<<endl;
    sinstance = this;
}

StiRootDisplayManager::~StiRootDisplayManager()
{
    cout <<"StiRootDisplayManager::~StiRootDisplayManager()"<<endl;
    delete mcanvas;
    mcanvas=0;
    delete mzone;
    mzone = 0;
    delete mnode;
    mnode = 0;
}

StiDisplayManager* StiRootDisplayManager::instance(TCanvas* c)
{
    return (sinstance) ? sinstance : new StiRootDisplayManager(c);
}


void StiRootDisplayManager::cd()
{
    //mnode->cd();
    mcanvas->cd();
    return;
}

void StiRootDisplayManager::draw()
{
    mnode->Draw();

    //cout <<"StiRootDisplayManager::draw()"<<endl;
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	StiDrawableHits* val = dynamic_cast<StiDrawableHits*>((*it).second);
	if (val) {
	    //cout <<"Drawing Hits"<<endl;
	    val->draw();
	}
    }
    //cout <<"StiRootDisplayManger::draw()\tDone drawing"<<endl;
    
    return;
}

void StiRootDisplayManager::reset()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	if ((*it).second->canBeRemoved()) {
	    mmap.erase(it);
	}
    }
    
}

void StiRootDisplayManager::update()
{
    mcanvas->Update();
    return;
}

void StiRootDisplayManager::setVisible()
{
    for (stidrawablemap::const_iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second);
	if (!tempDet) {
	    //skip, there's more than just detectors in here!
	}
	else {
	    if (tempDet->isOn()) {
		(*it).second->setVisibility(true);
	    }
	}
    }
    return;
}

void StiRootDisplayManager::setInvisible()
{
    for (stidrawablemap::const_iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	(*it).second->setVisibility(false);
    }
    return;
}

void StiRootDisplayManager::setVisible(const StiDrawable* val)
{
    string name = val->name();
    stidrawablemap::const_iterator where = mmap.find(name);
    if (where==mmap.end())  {
	cout <<"Sorry, object: "<<name<<" not found"<<endl;
	return;
    }
    else {
	(*where).second->setVisibility(true);
    }
    
    return;
}

void StiRootDisplayManager::setInvisible(const StiDrawable* val)
{
    string name = val->name();
    stidrawablemap::const_iterator where = mmap.find(name);
    if (where==mmap.end())  {
	cout <<"Sorry, object: "<<name<<" not found"<<endl;
	return;
    }
    else {
	(*where).second->setVisibility(false);
    }
    
    return;
}

void StiRootDisplayManager::addDrawable(StiDrawable* tempval)
{
    string name = tempval->name();
    //cout <<"Adding detector: "<<name<<endl;
    mmap.insert( stiDrawableMapValType( name, tempval ) );

    //Objects must be added to the main node
    StiRootDrawable* val = dynamic_cast<StiRootDrawable*>(tempval);
    if (!val) {
	return;
    }

    else {
	//Add to main volume
	const StThreeVector<double>& pos = val->position();
	mnode->Add( val->volume(), pos.x(), pos.y(), pos.z(), val->rotation());
    }
    
    return;
}

void StiRootDisplayManager::setZoomSkeletonView()
{
    setVisible();
    setIfcInvisible();
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Tpc");
	if ( where != (*it).first.npos ) { //it's from tpc
	    //now look for outermost padrow
	    where = (*it).first.find("Padrow_1/");
	    if ( where != (*it).first.npos ) { //it's from padrow 1	    
		StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
		if (!tempDet) {
		    cout <<"StiRootDisplayManager::setZoomSkeletonView(). ERROR:\t"
			 <<" cast to StiDetector failed.  Abort"<<endl;
		    return;
		}
		if (tempDet->isOn()) {
		    (*it).second->setVisibility(true);
		}
	    }
	    else {
		(*it).second->setVisibility(false);
	    }
	}
    }
    return;
}

void StiRootDisplayManager::setSkeletonView()
{
    setVisible();
    setIfcInvisible();
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Tpc");
	if ( where != (*it).first.npos ) { //it's from tpc
	    //now look for outermost padrow
	    where = (*it).first.find("Padrow_45");
	    if ( where != (*it).first.npos ) { //it's from padrow 45	    
		StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
		if (!tempDet) {
		    cout <<"StiRootDisplayManager::setSkeletonView(). ERROR:\t"
			 <<" cast to StiDetector failed.  Abort"<<endl;
		    return;
		}
		if (tempDet->isOn()) {
		    (*it).second->setVisibility(true);
		}
	    }
	    else {
		(*it).second->setVisibility(false);
	    }
	}
    }
    return;
}

void StiRootDisplayManager::setSvtVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Svg");
	if ( where != (*it).first.npos ) {
	    StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
	    if (!tempDet) {
		cout <<"StiRootDisplayManager::setSvtVisible(). ERROR:\t"
		     <<" cast to StiDetector failed.  Abort"<<endl;
		return;
	    }
	    if (tempDet->isOn()) {
		(*it).second->setVisibility(true);
	    }
	}
    }
    return;
}

void StiRootDisplayManager::setSvtInvisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Svg");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(false);
	}
    }
    return;
}

void StiRootDisplayManager::setIfcVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Ifc");
	if ( where != (*it).first.npos ) {
	    StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
	    if (!tempDet) {
		cout <<"StiRootDisplayManager::setIfcVisible(). ERROR:\t"
		     <<" cast to StiDetector failed.  Abort"<<endl;
		return;
	    }
	    if (tempDet->isOn()) {
		(*it).second->setVisibility(true);
	    }
	}
    }
    return;
}

void StiRootDisplayManager::setIfcInvisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Ifc");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(false);
	}
    }
    return;
}

void StiRootDisplayManager::setTpcVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Tpc");
	if ( where != (*it).first.npos ) {
	    StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
	    if (!tempDet) {
		cout <<"StiRootDisplayManager::setTpcVisible(). ERROR:\t"
		     <<" cast to StiDetector failed.  Abort"<<endl;
		return;
	    }
	    if (tempDet->isOn()) {
		(*it).second->setVisibility(true);
	    }
	}
    }
    return;
}

void StiRootDisplayManager::setTpcInvisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Tpc");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(false);
	}
    }
    return;
}

void StiRootDisplayManager::print() const
{
    cout <<"\nStiRootDisplayManager::print()"<<endl;
    for (stidrawablemap::const_iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	cout <<(*it).first<<endl;
    }
    return;
}
