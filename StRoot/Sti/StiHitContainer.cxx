//StiHitContainer.cxx
//M.L. Miller (Yale Software)
//03/01

//STD
#include <iostream>
#include <fstream>
#include <math.h>

// STL
#include <algorithm>

//StiGui
#include "StiGui/StiRootDrawableHitContainer.h"

//Sti
#include "StiHit.h"
#include "StiHitContainer.h"

using std::sort;
using std::find;
using std::lower_bound;
using std::upper_bound;

StiHitContainer* StiHitContainer::sinstance = 0;

ostream& operator<<(ostream& os, const StiHit& hit);
//Non member functions
ostream& operator<<(ostream&, const HitMapKey&);

StiHitContainer* StiHitContainer::instance(bool drawable)
{
    if (sinstance==0) {
	//Switch on what type to create based on some variable
	if (drawable==true) {
	    sinstance = new StiRootDrawableHitContainer();
	}
	else {
	    sinstance = new StiHitContainer();
	}
    }
    
    return sinstance;
}

void StiHitContainer::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
}

StiHitContainer::StiHitContainer() 
{
    cout <<"StiHitContainer::StiHitContainer()"<<endl;
    mminpoint = new StiHit();
    mmaxpoint = new StiHit();
    cout <<"\tLeaving StiHitContainer()"<<endl;
}

StiHitContainer::~StiHitContainer()
{
    cout <<"StiHitContainer::~StiHitContainer()"<<endl;
    delete mminpoint;
    mminpoint=0;
    delete mmaxpoint;
    mmaxpoint=0;
}

//Null implementation
void StiHitContainer::update()
{
    cout <<"StiHitContainer::update()"<<endl;
}

void StiHitContainer::push_back(StiHit* hit)
{
    mkey.refangle = hit->refangle();
    mkey.position = hit->position();
    mmap[mkey].push_back(hit);
    return;
}

void StiHitContainer::clear()
{
    hitmap::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	(*it).second.clear();
    }
    mvertexvec.clear();
    return;
}

void StiHitContainer::clearAndDestroy()
{
    //cout <<"StiHitContainer::clearAndDestroy()"<<endl;
    hitmap::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	hitvector& tempvec = (*it).second;
	for (hitvector::iterator vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
	    StiHit* temp = (*vit);
	    delete temp;
	    temp = 0;
	}
	tempvec.clear();
    }
    return;
}

unsigned int StiHitContainer::size() const
{
    unsigned int thesize = 0;
    hitmap::const_iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	thesize+=(*it).second.size();
    }
    return thesize;
}

const hitvector& StiHitContainer::hits(double refangle, double position)
{
    mkey.refangle = refangle;
    mkey.position = position; 
    return mmap[mkey];
}

void StiHitContainer::setRefPoint(double position, double refAngle, double y, double z)
{
    mUtilityHit.reset();
    mUtilityHit.setPosition(position);
    mUtilityHit.setRefangle(refAngle);
    mUtilityHit.setY(y);
    mUtilityHit.setZ(z);
    setRefPoint(&mUtilityHit);
}

void StiHitContainer::setRefPoint(StiHit* ref)
{
    mcandidatevec.clear();
    
    mkey.refangle = ref->refangle();
    mkey.position = ref->position();
    //mminpoint->setY( ref->y() -mdeltad );
    //mmaxpoint->setY( ref->y() +mdeltad );
    mminpoint->setZ( ref->z() -mdeltaz );
    mmaxpoint->setZ( ref->z() +mdeltaz );
    
    hitvector& tempvec = mmap[mkey];
    //Search first by distance along z
    mstart = lower_bound(tempvec.begin(), tempvec.end(), mminpoint, StizHitLessThan());
    
    if (mstart!=tempvec.end()) {
	mstop = upper_bound(tempvec.begin(), tempvec.end(),
			    mmaxpoint, StizHitLessThan());
    }
    
    else {
	//cout <<"mstart==tempvec.end()\tAbort"<<endl;
	mstart = tempvec.end();
	mstop = mstart;
        mcurrent = mcandidatevec.end();
	return;
    }

    if (mstart==mstop) {
	mstart=tempvec.end();
	mstop = mstart;
	mcurrent = mcandidatevec.end();
	return;
    }
    
    //Now search over distance along d
    for (hitvector::iterator cit=mstart; cit!=mstop; cit++) {
	if (fabs( (*cit)->y() - ref->y() ) < mdeltad) 
	    mcandidatevec.push_back((*cit));
    }
    mcurrent = mcandidatevec.begin();
    
    return;
}

void StiHitContainer::sortHits()
{
    hitmap::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	hitvector& tempvec = (*it).second;
	sort(tempvec.begin(), tempvec.end(), StizHitLessThan());
    }
    return;
} 

ostream& operator<<(ostream& os, const hitvector& vec)
{
    for (hitvector::const_iterator vit=vec.begin(); vit!=vec.end(); vit++) {
	os<<*(*vit)<<endl;
    }
    return os;
}

ostream& operator<<(ostream& os, const StiHitContainer& store)
{
    for (hitmap::const_iterator it=store.mmap.begin(); it!=store.mmap.end(); it++) {
	os <<endl;
	os <<(*it).second;
    }
    return os;   
}
