///\File StiTrackSeedFinder.h
///\author M.L. Miller (Yale Software) 03/01
///\author C. Pruneau (Wayne) Jan 2003
#ifndef StiTrackSeedFinder_H_INCLUDED
#define StiTrackSeedFinder_H_INCLUDED
#include <stdexcept>
#include <string>
#include <math.h>
using std::string;

#include "Sti/Base/EditableParameters.h"
#include "Sti/StiHit.h"
class Messenger;
class StiKalmanTrack;
class StiHitContainer;
class StiDetectorContainer;
class StiDetector;
class EditableParameters;
template<class Factorized>class Factory;
class Sti2HitComboFilter;

/// Abstract base defining a mechanism to find track seeds.
/// <p>
/// The seed finders shall be given a unique name to identify them; i.e. it is a Named object.
/// It is also editable through its inheritance to the EditableParameters class.
/// <p>
/// The seed finders require valid pointers to a track factory, a hit container,
/// and a detector container. Pointers to such three objects must be passed
/// to the class constructor. An exception is thrown if any of the three 
/// pointers are null.
/// <p>
/// \author M.L.Miller, Yale, 03/01
/// \author C. Pruneau, Wayne State U. Jan 03
///
class StiTrackSeedFinder : public EditableParameters
{
public:
  StiTrackSeedFinder(const string& name,
		     Factory<StiKalmanTrack> * trackFactory,
		     StiHitContainer         * hitContainer,
		     StiDetectorContainer    * detectorContainer);
  virtual ~StiTrackSeedFinder();
  virtual bool hasMore() = 0;
  virtual StiKalmanTrack* next() = 0;
  virtual void reset() =0;
  virtual EditableParameters * getParameters();
  void setFactory(Factory<StiKalmanTrack>* val);
  void setHitContainer(StiHitContainer*);
  StiHitContainer* getHitContainer();
  
 protected:
  Factory<StiKalmanTrack>* _trackFactory;
  StiHitContainer* _hitContainer;
  StiDetectorContainer* _detectorContainer;
  Messenger &  _messenger;
  
private:
  StiTrackSeedFinder(); //Not implemented
};

//inlines

inline void StiTrackSeedFinder::setFactory(Factory<StiKalmanTrack>* val)
{
  _trackFactory=val;
}

inline void StiTrackSeedFinder::setHitContainer(StiHitContainer* val)
{
  _hitContainer=val;
}

inline StiHitContainer* StiTrackSeedFinder::getHitContainer()
{
  return _hitContainer;
}


//Helper class to filter combinations of StiHits
struct Sti2HitComboFilter
{
    virtual bool operator()(const StiHit*, const StiHit*) const = 0;
    virtual void build(const string val="empty")=0;
};

//This is a simple test for rectangular distance in 2 dimensions
struct StiRectangular2HitComboFilter : public Sti2HitComboFilter
{
    StiRectangular2HitComboFilter() :  deltaD(-1), deltaZ(-1) {};
    virtual bool operator()(const StiHit*, const StiHit*) const;
    virtual void build(const string);
    double deltaD;
    double deltaZ;
};

struct StiCollinear2HitComboFilter : public Sti2HitComboFilter
{
    StiCollinear2HitComboFilter() : deltaPhi(-1.), deltaTheta(-1.) {};
    virtual bool operator()(const StiHit*, const StiHit*) const;
    virtual void build(const string);
    double deltaPhi;
    double deltaTheta;
};

//inlines

inline bool StiRectangular2HitComboFilter::operator()(const StiHit* lhs, const StiHit* rhs) const
{
    return ( (fabs(lhs->y()-rhs->y())<=deltaD) && (fabs(lhs->z()-rhs->z())<deltaZ));
}


#endif

