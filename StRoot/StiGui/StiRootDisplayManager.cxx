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
 
const int StiRootDisplayManager::skeletonView = 0;
const int StiRootDisplayManager::zoomSkeletonView = 1;

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
  delete mcanvas;  mcanvas=0;
  delete mzone;    mzone = 0;
  delete mnode;    mnode = 0;
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
  StiDrawableHits* val;
  for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) 
    {
      val = dynamic_cast<StiDrawableHits*>((*it).second);
      if (val) val->draw();
    }
  return;
}

void StiRootDisplayManager::reset()
{
  for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) 
    {
      if ((*it).second->canBeRemoved()) 
	mmap.erase(it);
    }
}

void StiRootDisplayManager::update()
{
  mcanvas->Update();
  return;
}

void StiRootDisplayManager::setVisible(bool value)
{
  StiDetector* det;
  for (stidrawablemap::const_iterator it=mmap.begin(); it!=mmap.end(); ++it) 
    {
      det = dynamic_cast<StiDetector*>( (*it).second);
      if (det && det->isOn())
	  (*it).second->setVisibility(value);
    }
  return;
}

void StiRootDisplayManager::setVisible(const StiDrawable* val, bool value)
{
  string name = val->name();
  stidrawablemap::const_iterator where = mmap.find(name);
  if (where==mmap.end())  
    {
      cout <<"StiRootDisplayManager::setVisible(const StiDrawable* val, bool value) - Error - Object: "
	   <<name<<" not found"<<endl;
      return;
    }
  else 
    {
      (*where).second->setVisibility(value);
    }
  return;
}

void StiRootDisplayManager::setVisible(const char * name, bool value)
{
  for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) 
    {
      string::size_type where = (*it).first.find(name);
      if ( where != (*it).first.npos ) 
	{
	  StiDetector* tempDet = dynamic_cast<StiDetector*>( (*it).second );
	  if (tempDet)
	    {
	      if (tempDet->isOn())
		(*it).second->setVisibility(value);
	    }
	  else
	    {
	      cout <<"StiRootDisplayManager::setVisible(const char * name, bool value). ERROR:\t"
		   <<" cast to StiDetector failed.  Abort"<<endl;
	      return;
	    }
	}
    }
  return;
}

void StiRootDisplayManager::addDrawable(StiDrawable* tempval)
{
    string name = tempval->name();
    //cout <<"Adding detector: "<<name<<endl;
    mmap.insert( stiDrawableMapValType( name, tempval ) );
    // Add objects to the main node
    StiRootDrawable* val = dynamic_cast<StiRootDrawable*>(tempval);
    if (val) 
      {
	const StThreeVector<double>& pos = val->position();
	mnode->Add( val->volume(), pos.x(), pos.y(), pos.z(), val->rotation());
      }
    return;
}

void StiRootDisplayManager::setView(int view)
{
  setVisible(true);
  setVisible("Ifc",false);
  StiDetector* tempDet;
  for (stidrawablemap::iterator it=mmap.begin(); it!=mmap.end(); ++it) 
    {
      string::size_type where = (*it).first.find("Tpc");
      if ( where != (*it).first.npos ) 
	{ 
	  //it's from tpc - look for outermost padrow
	  switch (view)
	    {
	    default:
	    case skeletonView : where = (*it).first.find("Padrow_45"); break;
	    case zoomSkeletonView : where = (*it).first.find("Padrow_1/"); break;
	    }
	  if ( where != (*it).first.npos ) 
	    { 
	      tempDet = dynamic_cast<StiDetector*>( (*it).second );
	      if (tempDet)
		{
		  if (tempDet->isOn()) 
		    (*it).second->setVisibility(true);
		}
	      else
		{
		  cout <<"StiRootDisplayManager::setView(). ERROR:\t"
		       <<" cast to StiDetector failed.  Abort"<<endl;
		  return;
		}
	    }
	  else 
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
