//StiHitContainer.cxx
//M.L. Miller (Yale Software)
//03/01

#include "Stiostream.h"
#include <fstream>
#include <math.h>
#include <algorithm>
#include "StiKalmanTrackNode.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiHitContainer.h"
#include <float.h>
#include "TMath.h"
using std::sort;
using std::find;
using std::lower_bound;
using std::upper_bound;
using std::stable_partition;

ClassImp(StiHitContainer);
ostream& operator<<(ostream& os, const StiHit& hit);
ostream& operator<<(ostream&, const HitMapKey&);

Int_t VectorAndEnd::fIdCounter = 0;
//________________________________________________________________________________
VectorAndEnd::VectorAndEnd(): fEffectiveEndValid(false) {
  InvalidateEnd();
  fId=fIdCounter++; 
  theEffectiveEnd=theHitVec.end();
  TestId(568);
}   
//________________________________________________________________________________
/*! The time complexity of push_back has two components:\n
  1) The correct hit-vector must be retrieved (or inserted if it doesn't
  exist) from the map.  This portion is O(logN) where N is the number of
  hit-vectors in the map.  See the documentation of the Hits() method for an
  estimate of N. \n
  2) The point must be added to the hit-vector.  This is guarunteed to be a
  constant time process, where the constant is determined by the vendor STL
  implementation.\n
  \warning A call to push_back() invalidates the sorted state of the
  container.  Thus, once all hits have been added to the container, then one
  must call SortHits().
*/
void StiHitContainer::Add(StiHit* hit) {
  const StiDetector* det = hit->Detector();
  assert (det); 
  _key.refangle = det->Key(2);
  _key.position = det->Key(1);
  _map[_key].push_back(hit);
  return;
}
//________________________________________________________________________________
void StiHitContainer::Reset() {
  HitMapToVectorAndEndType::iterator it;
  vector<StiHit*>::iterator iter;
  for (it=_map.begin(); it!=_map.end(); it++) {
    vector<StiHit*> &hits = (*it).second.hits();
    for (iter=hits.begin();iter!=hits.end();iter++) (*iter)->setTimesUsed(0);
  }
}
//________________________________________________________________________________
/*! A call to Clear() must call std::vector<StiHit*>::Clear() for all vectors
  stored in the map.  Thus a call to Clear is of O(N) * M where N is the number
  of keys in the map (see documenation of Hits() for an estimate of N) and
  M is the time required to Clear each vector.
*/
void StiHitContainer::Clear(const Option_t* opt) {
  cout<<"StiHitContainer::Clear() -I- Started"<<endl;
  HitMapToVectorAndEndType::iterator it;
  for (it=_map.begin(); it!=_map.end(); it++)     {
    (*it).second.Clear();
  }
  cout<<"StiHitContainer::Clear() -I- Done"<<endl;
}
//________________________________________________________________________________
/*! The time complexity of size is of O(logN) where N is the number of
  keys in the map.  For an estimate of N please see the documnation for
  Hits() method.
*/
UInt_t StiHitContainer::size() const {
  UInt_t thesize = 0;
  HitMapToVectorAndEndType::const_iterator it;
  for (it=_map.begin(); it!=_map.end(); it++) {
    thesize+=(*it).second.size();
  }
  return thesize;
}
//________________________________________________________________________________
vector<StiHit*>::iterator StiHitContainer::HitsBegin(const StiDetector* layer) {
  _key.refangle = layer->Key(2);
  _key.position = layer->Key(1);
  assert(_map.find(_key) != _map.end());
  return _map[_key].begin();
}
//________________________________________________________________________________
vector<StiHit*>::iterator StiHitContainer::HitsEnd(const StiDetector* layer) {
  _key.refangle = layer->Key(2);
  _key.position = layer->Key(1);
  assert(_map.find(_key) != _map.end());
  return _map[_key].TheEffectiveEnd();
}
//________________________________________________________________________________
/*! Get hits specified by the filter condition implied by the given position 
  and search radius.
  
  The sub-volume is identified by the following algorithm:\n
  1) Identify the detector plane of inteReset via the position and refAngle
  of the StiHit pointer passed. \n
  2) Find those hits that satisfy abs(hit->z-z_i)<deltaZ.  This is
  accomplished via a call to the STL algorithms lower_bound and
  upper_bound. \n
  3) Find thos hits that satisfy abs(hit->y-y_i)<deltaD.  This can only be
  accomplished via a linear search over those hits satisfying condition
  
  The time complexity of Hits has several components: \n
  1) The correct hit-vector must be retrieved from the map.  This is of
  O(logN) where N is the number of keys in the map (see documentation of
  Hits() for an estimate of the size of N).\n
  2) The calls to lower_bound and upper_bound are each of O(logM) where M is
  the number of hits in the sorted vector. \n
  3) The linear search over those hits that satisfy criterion 2.  This is
  of O(P) where P is the number of points that passed criterion 2. \n
  
  This algorithm is easily modifiable to perform the search in the opposite
  order (search in y, then z instead of z, then y).  See the source code for
  the necessary conversion actions.
*/
vector<StiHit*> & StiHitContainer::Hits(StiHit& ref, Double_t dY, Double_t dZ, bool fetchAll) {
  _selectedHits.clear();
  _minPoint.set(ref.Detector(),ref.y()-dY,ref.z()-dZ );
  _maxPoint.set(ref.Detector(),ref.y()+dY,ref.z()+dZ );
  const StiDetector* layer = ref.Detector();
  _key.refangle = layer->Key(2);
  _key.position = layer->Key(1);
  
  static Int_t id = 0;
  if (_map.find(_key) != _map.end()) {
    vector<StiHit*>& tempvec = _map[_key].hits();
    if (tempvec.size())      {
      id = _map[_key].fId;
      
      vector<StiHit*>::iterator tempend = _map[_key].TheEffectiveEnd();
      
#if 1
      //sanity check block
      vector<StiHit*>::iterator  tmptest = tempvec.begin();
      vector<StiHit*>::iterator  tmpend  = tempvec.end();
      if (!tempvec.size() ) {
	cout  << "-- Doing tmptest for id:" <<  id<< " " << ( tmpend != tmptest )
	      << " cmp " << ( tempend !=  tmptest )
	      << " " << tempvec.size() << " --> " << endl;
	assert(0);
      }
#endif   
      
      //Search first by distance along z
      _start = lower_bound(tempvec.begin(), tempend, &_minPoint, StizHitLessThan());
      if (_start!=tempend) 
	_stop = upper_bound(tempvec.begin(), tempend, &_maxPoint, StizHitLessThan());
      else 	{
	_start = tempend;
	_stop  = _start;
      }
      //Now search over distance along d
      StiHit * hit;
      for (vector<StiHit*>::iterator cit=_start; cit!=_stop; cit++) 	  {
	hit = *cit;
	if (TMath::Abs( hit->y() - ref.y() ) < dY)	  {
	  if (fetchAll || (hit->timesUsed()==0 && hit->Detector()->IsActive()) )
	    _selectedHits.push_back(hit);
	}
      }
    }
  }    //  NO _key was provided
  return _selectedHits;
}
//________________________________________________________________________________
/*! This function calls the STL sort algorithm for each hit-vector in the
  map.  The StiHit objects are ordered via the struct StizHitLessThan.
  
  \note A call to push_back(StiHit*) invalidates the sorted state of the
  container.  Thus any number of x calls to push_back(StiHit*) must be
  followed by a call to SortHits().\n
  
  The time complexity of SortHits() has two components:\n
  1) The time to access each vector in the map.  This is of O(N) where N is
  the number of keys in the map (see documentation of Hits() for an
  estimate of N). \n
  2) The time to sort an individual vector.  This is of O(M logM) where M
  is the number of hits in stored in the vector.
*/
void StiHitContainer::SortHits() {
  HitMapToVectorAndEndType::iterator it;
  for (it=_map.begin(); it!=_map.end(); ++it)     {
    vector<StiHit*>& tempvec = (*it).second.hits();
    sort(tempvec.begin(), tempvec.end(), StizHitLessThan());
    (*it).second.InvalidateEnd();
  }
  return;
}
//________________________________________________________________________________
void StiHitContainer::PartitionUsedHits() {
  for (HitMapToVectorAndEndType::iterator it=_map.begin(); it!=_map.end(); ++it)    {
    vector<StiHit*>& tempvec = (*it).second.hits();
    vector<StiHit*>::iterator where =
      stable_partition(tempvec.begin(), tempvec.end(), StiHitIsUsed() );
    (*it).second.setEnd(where);
  }
}
//________________________________________________________________________________
ostream& operator<<(ostream& os, const vector<StiHit*>& vec) {
  for (vector<StiHit*>::const_iterator vit=vec.begin(); vit!=vec.end(); vit++) {
    os<<*(*vit)<<endl;
  }
  return os;
}
//________________________________________________________________________________
/// Get hits selected by the given filter. If no filter is given (i.e. filter==0)
//  then return all hits.
vector<StiHit*> & StiHitContainer::Hits() {
  _selectedHits.clear();
  for(HitMapToVectorAndEndType::const_iterator iter= _map.begin(); iter !=_map.end(); iter++)    {
    const vector<StiHit*> & t_hits = (*iter).second.hits();
    for (vector<StiHit*>::const_iterator it=t_hits.begin();it!=t_hits.end();++it)
      _selectedHits.push_back(*it);
  }  
  return _selectedHits;
}
//________________________________________________________________________________
StiHit * StiHitContainer::NearestHit(StiHit& ref, Double_t dY, Double_t dZ, bool fetchAll) { 
  StiHit* hit = 0;
  StiHit* closestHit = 0;
  Double_t dMax = DBL_MAX;
  Double_t dy, dz, d;
  vector<StiHit*> & hits = Hits(ref,dY,dZ,fetchAll);
  for (vector<StiHit*>::iterator iter=hits.begin();iter!=hits.end();++iter)    {
    hit = *iter;
    dy  = hit->y() - ref.y();
    dz  = hit->z() - ref.z();
    d   = dy*dy + dz*dz;
    if ( d<dMax)      {
      closestHit = hit;
      dMax       = d;
    }
  }
  return closestHit;
}
//________________________________________________________________________________
void StiHitContainer::Print(Option_t *option) const {
  for(HitMapToVectorAndEndType::const_iterator iter= _map.begin(); iter !=_map.end(); iter++)    {
    HitMapToVectorAndEndType::key_type key = (*iter).first;
    cout << key;
    const vector<StiHit*> & hits = (*iter).second.hits();
    Int_t n = 0;
    for (vector<StiHit*>::const_iterator it=hits.begin();it!=hits.end();++it) {
      if (n == 0) 
	cout << "\tdetector: " << (*it)->Detector()->GetName() << endl;
      n++;
    }
  }
}
//________________________________________________________________________________
ostream& operator<<(ostream& os, const StiHitContainer& store) {
  for (HitMapToVectorAndEndType::const_iterator it=store._map.begin(); it!=store._map.end(); it++) {
    os <<endl << (*it).second.hits();
  }
  return os;   
}
