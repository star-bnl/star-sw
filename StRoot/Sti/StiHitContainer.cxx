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
ostream& operator<<(ostream&, const DetectorMapKey&);
ostream& operator<<(ostream&, const NameMapKey&);


StiHitContainer* StiHitContainer::instance()
{
    if (sinstance==0) {
	//Switch on what type to create based on some variable
	//sinstance = new StiHitContainer();
	sinstance = new StiRootDrawableHitContainer();
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
    removeAllVertices();
    resetVertexIterator();
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

bool StiHitContainer::hasMore() const
{
    return (mcurrent!=mcandidatevec.end()) ? true : false;
}

//Return without incrementing
StiHit* StiHitContainer::getCurrentHit()
{
    return (*mcurrent);
}

//Return and increment
StiHit* StiHitContainer::getHit()
{
    return (*(mcurrent++));
}

void StiHitContainer::setRefPoint(StiHit* ref)
{
    mcandidatevec.clear();
    
    mkey.refangle = ref->refangle();
    mkey.position = ref->position();
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
	
void StiHitContainer::print(double refangle, double position)
{
    //cout <<"\nStiHitContainer::print(double, double)"<<endl;
    mkey.refangle = refangle;
    mkey.position = position;
    hitmap::const_iterator where = mmap.find(mkey);
    
    if (where==mmap.end()) {
	//cout <<"StiHitContainer::print(double, double) !! Error. key:\t"<<mkey<<" not found"<<endl;
	cout <<"No Hits For DetectorMapKey:\t"<<mkey<<endl;
	return;
    }
    
    const hitvector& tempvec = (*where).second;
    if (tempvec.size()==0) {
	cout <<"No Hits For DetectorMapKey:\t"<<mkey<<endl;
	return;
    }
    
    for (hitvector::const_iterator it=tempvec.begin(); it!=tempvec.end(); it++)    {
	cout <<*(*it)<<endl;
    }
    return;
}

void StiHitContainer::print(double refangle, double position, ofstream& myout)
{
    mkey.refangle = refangle;
    mkey.position = position;
    const hitvector& tempvec = mmap[mkey];
    for (hitvector::const_iterator it=tempvec.begin(); it!=tempvec.end(); it++) {
	myout <<*(*it)<<endl;
    }
    return;
}
 
void StiHitContainer::print() const
{
    cout <<"\nStiHitContainer::print()"<<endl;
    for (hitmap::const_iterator it=mmap.begin(); it!=mmap.end(); it++) {
	cout <<(*it).first<<endl;
	
	const hitvector& tempvec = (*it).second;
	for (hitvector::const_iterator vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
	    cout <<*(*vit)<<endl;
	}
    }
    return;
}

// ------------------------ vertex implementation

void StiHitContainer::addVertex(StiHit* val)
{
    mvertexvec.push_back(val);
}

void StiHitContainer::removeVertex(StiHit* val)
{
    hitvector::iterator where = find(mvertexvec.begin(), mvertexvec.end(), val);
    if (where!=mvertexvec.end()) {
	mvertexvec.erase(where);
    }
}

void StiHitContainer::removeAllVertices()
{
    mvertexvec.clear();
}

unsigned int StiHitContainer::numberOfVertices() const
{
    return mvertexvec.size();
}

void StiHitContainer::resetVertexIterator()
{
    mvertexiterator = mvertexvec.begin();
}

StiHit* StiHitContainer::vertex(unsigned int i) const
{
    return ( i>=0 && i<mvertexvec.size() ) ? mvertexvec[i] : 0;
}

StiHit* StiHitContainer::firstVertex() const
{
    return (mvertexvec.size()!=0) ? mvertexvec[0] : 0;
}

StiHit* StiHitContainer::lastVertex() const
{
    return (mvertexvec.size()!=0) ? mvertexvec[ mvertexvec.size()-1 ] : 0;
}

StiHit* StiHitContainer::nextVertex()
{
    return ( (++mvertexiterator<mvertexvec.end()) && (mvertexiterator>=mvertexvec.begin() ) )
	? (*mvertexiterator) : 0;
}

StiHit* StiHitContainer::previousVertex()
{
    return (--mvertexiterator>=mvertexvec.begin()) && (mvertexiterator<mvertexvec.end())
	? (*mvertexiterator) : 0;
}

void StiHitContainer::printVertices() const
{
    for (hitvector::const_iterator it=mvertexvec.begin(); it!=mvertexvec.end(); ++it) {
	cout <<(*(*it))<<endl;
    }
    return;
}
