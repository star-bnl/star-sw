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

//StiGui
#include "StiDrawable.h"
#include "StiRootDrawable.h"
#include "StiDisplayManager.h"

StiDisplayManager* StiDisplayManager::sinstance = 0;

StiDisplayManager::StiDisplayManager()
{
    cout <<"StiDisplayManager::StiDisplayManager(int, int, int)"<<endl;
    mcanvas = new TCanvas("c1","Star Integrated Tracker", kXmin, kYmin, kXmax, kYmax);
    //mcanvas->Draw();
    //mzone = new TBRIK("zone","BRIK","void", kdx, kdy, kdz);
    //mzone->SetLineColor(0);

    mnode = new TVolume();
    mnode->SetName("mainnode");
    mnode->SetTitle("mainnode");
    //mnode = new TVolume("mainnode","mainnode", mzone);
    //mnode->SetVisibility(TVolume::kThisUnvisible);
    cd();

    //Temp test of TPolyMarker3D
    cout <<"Starting Loop To Fill Array"<<endl;
    int n=90;
    double* px = new double[n];
    double val=10.;
    for (int i=0; i<n; ++i) {
	px[i]=val++;
    }
    cout <<"Instantiate Poly"<<endl;
    poly = new StiTPolyMarker3D(n, px, 8);
    cout <<"Reset Bit"<<endl;
    poly->ResetBit(kCanDelete);
    
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

StiDisplayManager* StiDisplayManager::instance()
{
    return (sinstance) ? sinstance : new StiDisplayManager();
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
    return;
}

void StiDisplayManager::draw()
{
    mnode->Draw();
    
    //Temp patch
    cout <<"Drawing Poly"<<endl;
    poly->Draw();
    cout <<"Done Drawing Poly"<<endl;

    return;
}

void StiDisplayManager::update()
{
    mcanvas->Update();
    return;
}

void StiDisplayManager::setVisible()
{
    for (stidrawablemap::const_iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	(*it).second->setVisibility(true);
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
    StiRootDrawable* val = dynamic_cast<StiRootDrawable*>(tempval);
    if (!val) {
	cout <<"StiDisplayManagar::addDrawable().  Error:\tCast Failed"<<endl;
	return;
    }
    
    string name = val->name();
    //cout <<"Adding detector: "<<name<<endl;
    mmap.insert( stiDrawableMapValType( name, val ) );

    //Add to main volume
    const StThreeVector<double>& pos = val->position();
    //mnode->Add( val->volume(), 0., 0., 0., val->rotation());
    mnode->Add( val->volume(), pos.x(), pos.y(), pos.z(), val->rotation());
    return;
}

void StiDisplayManager::setSvtVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Svg");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(true);
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

void StiDisplayManager::setTpcVisible()
{
    for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	string::size_type where = (*it).first.find("Tpc");
	if ( where != (*it).first.npos ) {
	    (*it).second->setVisibility(true);
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
