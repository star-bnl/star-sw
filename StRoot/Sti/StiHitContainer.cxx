//StiHitContainer.cxx
//M.L. Miller (Yale Software)
//03/01

//STD
#include <iostream>
#include <fstream>
// STL
#include <algorithm>

#include "StTpcHit.h"
#include "StiHitContainer.h"

using std::sort;

StiHitContainer::StiHitContainer() 
{ 
    mminpoint = new StHit();
    mmaxpoint = new StHit();
}

StiHitContainer::~StiHitContainer() {};

clock_t StiHitContainer::push_back(StHit* mysthit)
{
    clock_t start = clock();
    StTpcHit* hit = dynamic_cast<StTpcHit*>(mysthit);
    if (!hit) return -999;

    mkey.sector = hit->sector();
    mkey.padrow = hit->padrow();
    mmap[mkey].push_back(hit);
    clock_t stop = clock();
    return (stop-start);
}

clock_t StiHitContainer::clear()
{
    //cout <<"StiHitContainer::clear()"<<endl;
    clock_t start = clock();
    hitmap::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	(*it).second.clear();
    }
    clock_t stop = clock();
    return (stop-start);
}

clock_t StiHitContainer::clearAndDestroy()
{
    //cout <<"StiHitContainer::clearAndDestroy()"<<endl;
    clock_t start = clock();
    hitmap::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	hitvector& tempvec = (*it).second;
	for (hitvector::iterator vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
	    StHit* temp = (*vit);
	    delete temp;
	    temp = 0;
	}
	tempvec.clear();
    }
    clock_t stop = clock();
    return (stop-start);
}

int StiHitContainer::size() const
{
    int thesize = 0;
    hitmap::const_iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	thesize+=(*it).second.size();
    }
    return thesize;
}

void StiHitContainer::print(int sector, int padrow)
{
    mkey.sector = sector;
    mkey.padrow = padrow;
    const hitvector& tempvec = mmap[mkey];
    for (hitvector::const_iterator it=tempvec.begin(); it!=tempvec.end(); it++)    {
	//cout <<*(*it)<<endl;
    }
    return;
}

void StiHitContainer::print(int sector, int padrow, ofstream& myout)
{
    mkey.sector = sector;
    mkey.padrow = padrow;
    const hitvector& tempvec = mmap[mkey];
    for (hitvector::const_iterator it=tempvec.begin(); it!=tempvec.end(); it++) {
	//myout <<*(*it)<<endl;
    }
    return;
}
 
bool StiHitContainer::hasMore() const
{
    return (mcurrent!=mcandidatevec.end()) ? true : false;
}

const StHit* StiHitContainer::getHit()
{
    return (*(mcurrent++));
}

clock_t StiHitContainer::setRefPoint(StHit* mysthit)
{
    clock_t start = clock();

    //To be removed in next release
    StTpcHit* ref = dynamic_cast<StTpcHit*>(mysthit);
    if (!ref) return -999;

    mcandidatevec.clear();

    mkey.sector = ref->sector();
    mkey.padrow = ref->padrow();

    mminpoint->y = ref->y -mdeltad;
    mmaxpoint->y = ref->y +mdeltad;

    findHitsNearRef(ref);
    clock_t stop = clock();
    return (stop-start);
}

//Must only be called from setRefPoint()!!!!!
//Assumes that mkey, mminpoint, and mmaxpoint are set alreay
void StiHitContainer::findHitsNearRef(StHit* ref)
{
    //cout <<"StiHitContainer::findHitNear()"<<endl;
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
	if (abs( (*cit)->z- ref->z ) < mdeltaz) 
	   mcandidatevec.push_back((*cit));
    }
    mcurrent = mcandidatevec.begin();
    
    return;
}
    
clock_t StiHitContainer::sortHits()
{
    //cout <<"StiHitContainer::sortHits()"<<endl;
    clock_t start = clock();
    hitmap::iterator it;
    
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	hitvector& tempvec = (*it).second;
	sort(tempvec.begin(), tempvec.end(), StidHitLessThan());
    }
    clock_t stop = clock();
    return (stop-start);
} 
	
void StiHitContainer::print() const
{
    cout <<"StiHitContainer::print()"<<endl;
    hitmap::const_iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	cout <<(*it).first<<endl;
	
	const hitvector& tempvec = (*it).second;
	hitvector::const_iterator vit;
	for (vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
	    //cout <<*(*vit)<<endl;
	}
    }
    return;
}
