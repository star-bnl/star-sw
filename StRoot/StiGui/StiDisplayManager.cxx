//StiDisplayManager.cxx
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
#include "StiDisplayManager.h"

StiDisplayManager* StiDisplayManager::sinstance = 0;

StiDisplayManager::StiDisplayManager(TCanvas* c)
    : mcanvas(c), mzone(0), mnode(0)
{
    cout <<"StiDisplayManager::StiDisplayManager(int, int, int)"<<endl;
    if (!mcanvas) {
	cout <<"StiDisplayManager::StiDisplayManager() ERROR:\t";
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
    
    cout <<"Leaving StiDisplayManager::StiDisplayManager()"<<endl;
    sinstance = this;
}

StiDisplayManager::~StiDisplayManager()
{
    cout <<"StiDisplayManager::~StiDisplayManager()"<<endl;
    delete mcanvas;
    mcanvas=0;
    delete mzone;
    mzone = 0;
    delete mnode;
    mnode = 0;
}

StiDisplayManager* StiDisplayManager::instance(TCanvas* c)
{
    return (sinstance) ? sinstance : new StiDisplayManager(c);
}

void StiDisplayManager::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
    return;
}

void StiDisplayManager::cd()
{
    //mnode->cd();
    mcanvas->cd();
    return;
}

void StiDisplayManager::draw()
{
    mnode->Draw();

    //cout <<"StiDisplayManager::draw()"<<endl;
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	StiDrawableHits* val = dynamic_cast<StiDrawableHits*>((*it).second);
	if (val) {
	    //cout <<"Drawing Hits"<<endl;
	    val->draw();
	}
    }
    //cout <<"StiDisplayManger::draw()\tDone drawing"<<endl;
    
    return;
}

void StiDisplayManager::reset()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	if ((*it).second->canBeRemoved()) {
	    mmap.erase(it);
	}
    }
    
}

void StiDisplayManager::update()
{
    mcanvas->Update();
    return;
}

void StiDisplayManager::setVisible()
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

void StiDisplayManager::setInvisible()
{
    for (stidrawablemap::const_iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	(*it).second->setVisibility(false);
    }
    return;
}

void StiDisplayManager::setVisible(const StiDrawable* val)
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

void StiDisplayManager::setInvisible(const StiDrawable* val)
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

void StiDisplayManager::addDrawable(StiDrawable* tempval)
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

void StiDisplayManager::setZoomSkeletonView()
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
		    cout <<"StiDisplayManager::setZoomSkeletonView(). ERROR:\t"
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

void StiDisplayManager::setSkeletonView()
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
		    cout <<"StiDisplayManager::setSkeletonView(). ERROR:\t"
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

void StiDisplayManager::setSvtVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Svg");
	if ( where != (*it).first.npos ) {
	    StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
	    if (!tempDet) {
		cout <<"StiDisplayManager::setSvtVisible(). ERROR:\t"
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

void StiDisplayManager::setSvtInvisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Svg");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(false);
	}
    }
    return;
}

void StiDisplayManager::setIfcVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Ifc");
	if ( where != (*it).first.npos ) {
	    StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
	    if (!tempDet) {
		cout <<"StiDisplayManager::setIfcVisible(). ERROR:\t"
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

void StiDisplayManager::setIfcInvisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Ifc");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(false);
	}
    }
    return;
}

void StiDisplayManager::setTpcVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Tpc");
	if ( where != (*it).first.npos ) {
	    StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
	    if (!tempDet) {
		cout <<"StiDisplayManager::setTpcVisible(). ERROR:\t"
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

void StiDisplayManager::setTpcInvisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Tpc");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(false);
	}
    }
    return;
}

void StiDisplayManager::print() const
{
    cout <<"\nStiDisplayManager::print()"<<endl;
    for (stidrawablemap::const_iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	cout <<(*it).first<<endl;
    }
    return;
}
