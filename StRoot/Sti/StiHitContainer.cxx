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
#include "Messenger.h"
#include "StiHit.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiHitContainer.h"

using std::sort;
using std::find;
using std::lower_bound;
using std::upper_bound;

StiHitContainer* StiHitContainer::sinstance = 0;

ostream& operator<<(ostream& os, const StiHit& hit);
//Non member functions
ostream& operator<<(ostream&, const HitMapKey&);

/*! StiHitContainer is a singleton that serves as a base class for
  StiRootDrawableHitContainer.  A call to instance will always return a
  valid pointer to a StiHitContainer.  However, if the call to instance()
  happens to be the first call, then we must be sure to create the correct 
  type of object.  We solve this problem in a rather sloppy way, by adding a
  boolean parameter <b>drawable</b> that defaults to false.  This parameter
  is only checked if the singleton instance of StiHitContainer has not yet
  been created.  In that situation, bool==true guaruntees the creation of a
  StiRootDrawableHitContainer object, while bool==false guaruntees the
  creation of a StiHitContainer object.  This implies two things: \n
  1) The user must guaruntee that the first call to instance properly
  specifies the type of object to be created. \n
  2) Any call to instance after the first call can be treated normally.
  That is, one need not specify the boolean argument to the instance() call.
*/
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

/*! Use this wisely, if at all.  See the above warning regarding use of kill().
 */
void StiHitContainer::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
}

StiHitContainer::StiHitContainer()
    : mMessenger(*(Messenger::instance(MessageType::kHitMessage)))
{
    mMessenger <<"StiHitContainer::StiHitContainer()"<<endl;
    mminpoint = new StiHit();
    mmaxpoint = new StiHit();
    mMessenger <<"\tLeaving StiHitContainer()"<<endl;
}

StiHitContainer::~StiHitContainer()
{
    mMessenger <<"StiHitContainer::~StiHitContainer()"<<endl;
    delete mminpoint;
    mminpoint=0;
    delete mmaxpoint;
    mmaxpoint=0;
}

/*! Null implementation.  We provide this virtual function for the situation
  when StiHitContainer::instance() behaves polymorphically, e.g., when it
  actually points to a StiRootDrawableDetector object.  In that situation, a
  call to update() will propogate to the most derived class, allowing that
  class to perform necessary tasks (e.g., append hits to display).
 */
void StiHitContainer::update()
{
}

/*! The time complexity of push_back has two components:\n
  1) The correct hit-vector must be retrieved (or inserted if it doesn't
  exist) from the map.  This portion is O(logN) where N is the number of
  hit-vectors in the map.  See the documentation of the hits() method for an
  estimate of N. \n
  2) The point must be added to the hit-vector.  This is guarunteed to be a
  constant time process, where the constant is determined by the vendor STL
  implementation.\n
  \warning A call to push_back() invalidates the sorted state of the
  container.  Thus, once all hits have been added to the container, then one
  must call sortHits().
 */
void StiHitContainer::push_back(StiHit* hit)
{
    mkey.refangle = hit->refangle();
    mkey.position = hit->position();
    mmap[mkey].push_back(hit);
    return;
}

/*! A call to clear() must call std::vector<StiHit*>::clear() for all vectors
  stored in the map.  Thus a call to clear is of O(N) *M where N is the number
  of keys in the map (see documenation of hits() for an estimate of N) and
  M is the time required to clear each vector.
 */
void StiHitContainer::clear()
{
    hitmap::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	(*it).second.clear();
    }
    mvertexvec.clear();
    return;
}

/*! The time complexity of size is of O(logN) where N is the number of
  keys in the map.  For an estimate of N please see the documnation for
  hits() method.
*/
unsigned int StiHitContainer::size() const
{
    unsigned int thesize = 0;
    hitmap::const_iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	thesize+=(*it).second.size();
    }
    return thesize;
}

/*! The values of refangle and position are used to fill an existing
  HitMapKey object.  This object is used to key the retrieval of a vector
  of hits associated with the specified position.  For more on the
  definition of refangle and position, please see StiHit documentation.\n
  A call to hits(double,double) corresponds to the retrieval of an object
  from an STL map based on a key.  Such a retrieval is guarunteed to be of
  O(logN) time complexity, where N is the number of keys in the map.  For
  our practices, N is roughly equal to (TPC) 12*45 + (SVT layer 1) 2*4 +
  (SVT layer 2) 2*6 + (SVT layer 3) 2*8 + (SSD) 20.
 */
const hitvector& StiHitContainer::hits(double refangle, double position)
{
    mkey.refangle = refangle;
    mkey.position = position; 
    return mmap[mkey];
}

hitvector& StiHitContainer::hits(const StiDetector* layer)
{
    mkey.refangle = layer->getPlacement()->getCenterRefAngle();
    mkey.position = layer->getPlacement()->getCenterRadius();
    return mmap[mkey];
}

/*! This form of setRefPoint packs the information passed as arguments into
  a member StiHit object, and passes this to the method setRefPoint(StiHit*).
 */
void StiHitContainer::setRefPoint(double position, double refAngle,
				  double y, double z)
{
    //mMessenger <<"\nStiHitContainer::setRefPoint(double, double, double, double)"<<endl;
    //mMessenger <<"\tposition: "<<position<<"\trefAngle: "<<refAngle<<"\t";
    //mMessenger <<"y: "<<y<<"\tz: "<<z<<endl;
    
    mUtilityHit.reset();
    mUtilityHit.setPosition(position);
    mUtilityHit.setRefangle(refAngle);
    mUtilityHit.setY(y);
    mUtilityHit.setZ(z);
    setRefPoint(&mUtilityHit);
}

/*! The sub-volume is identified by the following algorithm:\n
  1) Identify the detector plane of intereset via the position and refAngle
  of the StiHit pointer passed. \n
  2) Find those hits that satisfy abs(hit->z-z_i)<deltaZ.  This is
  accomplished via a call to the STL algorithms lower_bound and
  upper_bound. \n
  3) Find thos hits that satisfy abs(hit->y-y_i)<deltaD.  This can only be
  accomplished via a linear search over those hits satisfying condition
  
  Once setRefPoint() has been called, one use the iterator like interfaces
  hasMore(), getHit() and getCurrentHit() to access the hits that satisfied
  the search criterion.\n

  The time complexity of setRefPoint has several components: \n
  1) The correct hit-vector must be retrieved from the map.  This is of
  O(logN) where N is the number of keys in the map (see documentation of
  hits() for an estimate of the size of N).\n
  2) The calls to lower_bound and upper_bound are each of O(logM) where M is
  the number of hits in the sorted vector. \n
  3) The linear search over those hits that satisfy criterion 2.  This is
  of O(P) where P is the number of points that passed criterion 2. \n

  This algorithm is easily modifiable to perform the search in the opposite
  order (search in y, then z instead of z, then y).  See the source code for
  the necessary conversion actions.
 */
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
    mstart = lower_bound(tempvec.begin(), tempvec.end(), mminpoint,
			 StizHitLessThan());
    
    if (mstart!=tempvec.end()) {
	mstop = upper_bound(tempvec.begin(), tempvec.end(),
			    mmaxpoint, StizHitLessThan());
    }
    
    else {
	//mMessenger <<"mstart==tempvec.end()\tAbort"<<endl;
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

/*! This function calls the STL sort algorithm for each hit-vector in the
  map.  The StiHit objects are ordered via the struct StizHitLessThan.
  
  \note A call to push_back(StiHit*) invalidates the sorted state of the
  container.  Thus any number of x calls to push_back(StiHit*) must be
  followed by a call to sortHits().\n
  
  The time complexity of sortHits() has two components:\n
  1) The time to access each vector in the map.  This is of O(N) where N is
  the number of keys in the map (see documentation of hits() for an
  estimate of N). \n
  2) The time to sort an individual vector.  This is of O(M logM) where M
  is the number of hits in stored in the vector.
 */
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
