//StiHitContainer.cxx
//M.L. Miller (Yale Software)
//03/01

//STD
#include <iostream>
#include <fstream>
#include <math.h>

// STL
#include <algorithm>

#include "StiHit.h"
#include "StiHitContainer.h"

using std::sort;

StiHitContainer* StiHitContainer::sinstance = 0;

StiHitContainer* StiHitContainer::instance()
{
    return (sinstance) ? sinstance : new StiHitContainer();
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
    mminpoint = new StiHit();
    mmaxpoint = new StiHit();
    sinstance = this;
}

StiHitContainer::~StiHitContainer() {};

void StiHitContainer::push_back(StiHit* hit)
{
    mkey.sector = hit->sector();
    mkey.padrow = hit->padrow();
    mmap[mkey].push_back(hit);
    return;
}

void StiHitContainer::clear()
{
    hitmap::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	(*it).second.clear();
    }
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

const hitvector& StiHitContainer::hits(unsigned int sector, unsigned int padrow)
{
    mkey.sector = sector;
    mkey.padrow = padrow; 
    return mmap[mkey];
}

bool StiHitContainer::hasMore() const
{
    return (mcurrent!=mcandidatevec.end()) ? true : false;
}

StiHit* StiHitContainer::getHit()
{
    return (*(mcurrent++));
}

void StiHitContainer::setRefPoint(StiHit* ref)
{
    mcandidatevec.clear();
    
    mkey.sector = ref->sector();
    mkey.padrow = ref->padrow();
    mminpoint->setY( ref->y() -mdeltad );
    mmaxpoint->setY( ref->y() +mdeltad );
    
    hitvector& tempvec = mmap[mkey];

    //Search first by distance along pad
    mstart = lower_bound(tempvec.begin(), tempvec.end(), mminpoint, StidHitLessThan());
    if (mstart!=tempvec.end()) 
        mstop = upper_bound(tempvec.begin(), tempvec.end(), mmaxpoint, StidHitLessThan());
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

    //Now search over z
    for (hitvector::iterator cit=mstart; cit!=mstop; cit++) {
	if (fabs( (*cit)->z() - ref->z() ) < mdeltaz) 
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
	sort(tempvec.begin(), tempvec.end(), StidHitLessThan());
    }
    return;
} 
	
void StiHitContainer::print(unsigned int sector, unsigned int padrow)
{
    mkey.sector = sector;
    mkey.padrow = padrow;
    const hitvector& tempvec = mmap[mkey];
    for (hitvector::const_iterator it=tempvec.begin(); it!=tempvec.end(); it++)    {
	cout <<*(*it)<<endl;
    }
    return;
}

void StiHitContainer::print(unsigned int sector, unsigned int padrow, ofstream& myout)
{
    mkey.sector = sector;
    mkey.padrow = padrow;
    const hitvector& tempvec = mmap[mkey];
    for (hitvector::const_iterator it=tempvec.begin(); it!=tempvec.end(); it++) {
	myout <<*(*it)<<endl;
    }
    return;
}
 
void StiHitContainer::print() const
{
    hitmap::const_iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	cout <<(*it).first<<endl;
	
	const hitvector& tempvec = (*it).second;
	hitvector::const_iterator vit;
	for (vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
	    cout <<*(*vit)<<endl;
	}
    }
    return;
}
