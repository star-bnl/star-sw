//StiDisplayManager.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

//Root
#include "TCanvas.h"
#include "TShape.h"
#include "TNode.h"
#include "TBRIK.h"

#include "Sti/StiDrawable.h"

#include "StiDisplayManager.h"

StiDisplayManager* StiDisplayManager::sinstance = 0;

StiDisplayManager::StiDisplayManager()
{
    cout <<"StiDisplayManager::StiDisplayManager(int, int, int)"<<endl;
    mcanvas = new TCanvas("c1","Star Integrated Tracker", kXmin, kYmin, kXmax, kYmax);
    //mcanvas->Draw();
    mzone = new TBRIK("zone","BRIK","void", kdx, kdy, kdz);
    mzone->SetLineColor(0);
    mnode = new TNode("mainnode","mainnode", mzone);
    mnode->SetVisibility(0);
    cd();
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
    mnode->cd();
    return;
}

void StiDisplayManager::draw()
{
    mnode->Draw();
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

void StiDisplayManager::addDrawable(StiDrawable* val)
{
    string name = val->name();
    cout <<"Adding detector: "<<name<<endl;
    mmap.insert( stiDrawableMapValType( name, val ) );
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
