//StiHitContainer.cxx
//M.L. Miller (Yale Software)
//03/01

#include <iostream>
#include <fstream>
#include <math.h>
#include <algorithm>
#include "Sti/Base/Messenger.h"
#include "StiKalmanTrackNode.h"
#include "StiHit.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiHitContainer.h"
using std::sort;
using std::find;
using std::lower_bound;
using std::upper_bound;
using std::stable_partition;
ostream& operator<<(ostream& os, const StiHit& hit);
ostream& operator<<(ostream&, const HitMapKey&);

StiHitContainer::StiHitContainer(const string & name, const string & description)
  : Named(name),
    Described(description),
    mMessenger(*(Messenger::instance(MessageType::kHitMessage)))
{
  cout <<"StiHitContainer::StiHitContainer() -I- Started with name:"<<name<<endl;
  mminpoint = new StiHit();
  mmaxpoint = new StiHit();
  //mMessenger <<"\tLeaving StiHitContainer() -I- Done"<<endl;
}

StiHitContainer::~StiHitContainer()
{
  cout <<"StiHitContainer::~StiHitContainer()"<<endl;
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
{}

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
  const StiDetector* det = hit->detector();
  if (!det) 
    throw runtime_error("StiHitContainer::push_back() -E- Given hit has no associated detector");
  //This is a coupling that I would like to get rid of, not in the spirit of the hit container!
  mkey.refangle = det->getPlacement()->getCenterRefAngle();
  mkey.position = det->getPlacement()->getCenterRadius();
  //mkey.refangle = hit->refangle();
  //mkey.position = hit->position();
  mmap[mkey].theHitVec.push_back(hit);
  return;
}

void StiHitContainer::reset()
{
   HitMapToVectorAndEndType::iterator it;
   HitVectorType::iterator iter;
   for (it=mmap.begin(); it!=mmap.end(); it++) 
     {
       HitVectorType &hits = (*it).second.theHitVec;
       for (iter=hits.begin();iter!=hits.end();iter++)
	 {
	   (*iter)->setTimesUsed(0);
	 }
     }
}

/*! A call to clear() must call std::vector<StiHit*>::clear() for all vectors
  stored in the map.  Thus a call to clear is of O(N) *M where N is the number
  of keys in the map (see documenation of hits() for an estimate of N) and
  M is the time required to clear each vector.
 */
void StiHitContainer::clear()
{
    HitMapToVectorAndEndType::iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	(*it).second.theHitVec.clear();
	(*it).second.theEffectiveEnd = (*it).second.theHitVec.end();
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
    HitMapToVectorAndEndType::const_iterator it;
    for (it=mmap.begin(); it!=mmap.end(); it++) {
	thesize+=(*it).second.theHitVec.size();
    }
    return thesize;
}

/*! The values of refangle and position are used to fill an existing
  HitMapToVectorAndEndTypeKey object.  This object is used to key the retrieval of a vector
  of hits associated with the specified position.  For more on the
  definition of refangle and position, please see StiHit documentation.\n
  A call to hits(double,double) corresponds to the retrieval of an object
  from an STL map based on a key.  Such a retrieval is guarunteed to be of
  O(logN) time complexity, where N is the number of keys in the map.  For
  our practices, N is roughly equal to (TPC) 12*45 + (SVT layer 1) 2*4 +
  (SVT layer 2) 2*6 + (SVT layer 3) 2*8 + (SSD) 20.
 */
const HitVectorType& StiHitContainer::hits(double refangle, double position)
{
    mkey.refangle = refangle;
    mkey.position = position; 
    return mmap[mkey].theHitVec;
}

HitVectorType& StiHitContainer::hits(const StiDetector* layer)
{
    mkey.refangle = layer->getPlacement()->getCenterRefAngle();
    mkey.position = layer->getPlacement()->getCenterRadius();
    return mmap[mkey].theHitVec;
}

HitVectorType::iterator StiHitContainer::hitsBegin(const StiDetector* layer)
{
    mkey.refangle = layer->getPlacement()->getCenterRefAngle();
    mkey.position = layer->getPlacement()->getCenterRadius();
    return mmap[mkey].theHitVec.begin();
}

HitVectorType::iterator StiHitContainer::hitsEnd(const StiDetector* layer)
{
    mkey.refangle = layer->getPlacement()->getCenterRefAngle();
    mkey.position = layer->getPlacement()->getCenterRadius();
    //if (mmap[mkey].theHitVec.end() != mmap[mkey].theEffectiveEnd) {
    //cout <<"StiHitContainer::hitsEnd(const StiDetector*). ERROR:\t"
    //     <<"theEffectiveEnd != theHitVec.end()"<<endl
    //     <<"mkey:\t"<<mkey.refangle<<" "<<mkey.position<<endl;
    //}
    //return mmap[mkey].theHitVec.end();

    return mmap[mkey].theEffectiveEnd;
}

/*! This form of setRefPoint packs the information passed as arguments into
  a member StiHit object, and passes this to the method setRefPoint(StiHit*).
 */
void StiHitContainer::setRefPoint(double position, double refAngle,
				  double y, double z)
{
  mMessenger <<"\nStiHitContainer::setRefPoint(double, double, double, double)"<<endl
	     <<"\tposition: "<<position<<"\trefAngle: "<<refAngle<<"\t"
	     <<"y: "<<y<<"\tz: "<<z<<endl;
  mUtilityHit.set(position,refAngle,y,z);
  setRefPoint(&mUtilityHit);
}

void StiHitContainer::setRefPoint(StiKalmanTrackNode & node)
{
  mdeltad = node.getWindowY();
  mdeltaz = node.getWindowZ();
  mUtilityHit.set(node.getRefPosition(),
		  node.getRefAngle(),
		  node.getY(),
		  node.getZ());
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
    //cp//mminpoint->setZ( ref->z() -mdeltaz );
    //cp//mmaxpoint->setZ( ref->z() +mdeltaz );
    mminpoint->set(ref->position(),ref->refangle(),ref->y(),ref->z()-mdeltaz );
    mmaxpoint->set(ref->position(),ref->refangle(),ref->y(),ref->z()+mdeltaz );
    HitVectorType& tempvec = mmap[mkey].theHitVec;
    HitVectorType::iterator& tempend = mmap[mkey].theEffectiveEnd;
    //Search first by distance along z
    mstart = lower_bound(tempvec.begin(), tempend, mminpoint, StizHitLessThan());
    
    if (mstart!=tempend) {
	mstop = upper_bound(tempvec.begin(), tempend, mmaxpoint, StizHitLessThan());
    }
    
    else {
	//mMessenger <<"mstart==tempend\tAbort"<<endl;
	mstart = tempend;
	mstop = mstart;
        mcurrent = mcandidatevec.end();
	return;
    }

    if (mstart==mstop) {
	mstart=tempend;
	mstop = mstart;
	mcurrent = mcandidatevec.end();
	return;
    }
    
    //Now search over distance along d
    for (HitVectorType::iterator cit=mstart; cit!=mstop; cit++) 
      {
	if (fabs( (*cit)->y() - ref->y() ) < mdeltad)
	{
	  StiHit * hit = *cit;
	  if (hit->timesUsed()==0 && hit->detector()->isActive())
	    {
	      mcandidatevec.push_back(hit);
	    }
	}
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
  HitMapToVectorAndEndType::iterator it;
  for (it=mmap.begin(); it!=mmap.end(); ++it) 
    {
      HitVectorType& tempvec = (*it).second.theHitVec;
      sort(tempvec.begin(), tempvec.end(), StizHitLessThan());
      (*it).second.theEffectiveEnd =(*it).second.theHitVec.end();
    }
  return;
}

void StiHitContainer::partitionUsedHits()
{
    for (HitMapToVectorAndEndType::iterator it=mmap.begin(); it!=mmap.end(); ++it) {
	HitVectorType& tempvec = (*it).second.theHitVec;
	
	mMessenger <<"-- Hits before partition --"<<endl;
	mMessenger <<(*it).second.theHitVec<<endl;
	
	HitVectorType::iterator where =
	    stable_partition(tempvec.begin(), tempvec.end(), StiHitIsUsed() );
	(*it).second.theEffectiveEnd = where;
	
	mMessenger <<"-- Hits after partition --"<<endl;
	mMessenger <<(*it).second.theHitVec<<endl;

    }
}

ostream& operator<<(ostream& os, const HitVectorType& vec)
{
    for (HitVectorType::const_iterator vit=vec.begin(); vit!=vec.end(); vit++) {
	os<<*(*vit)<<endl;
    }
    return os;
}

ostream& operator<<(ostream& os, const StiHitContainer& store)
{
    for (HitMapToVectorAndEndType::const_iterator it=store.mmap.begin(); it!=store.mmap.end(); it++) {
	os <<endl;
	os <<(*it).second.theHitVec;
    }
    return os;   
}


HitVectorType StiHitContainer::getAllHits()
{
  HitVectorType allHits;
  // HitMapToVectorAndEndType & tempMap= mmap.hits();

  for(HitMapToVectorAndEndType::const_iterator iter= mmap.begin(); iter !=mmap.end(); iter++)
    {
      // cout<<"first loop of get all hits entered"<<endl;
      const HitVectorType & t_hits = (*iter).second.theHitVec;
      for (vector<StiHit*>::const_iterator it=t_hits.begin();
	   it!=t_hits.end();
	   ++it)
	{
	  allHits.push_back(*it);
	  
	}
    }
  return allHits;
}


