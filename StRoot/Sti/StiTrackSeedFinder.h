//StiTrackSeedFinder.h
//M.L. Miller (Yale Software)
//03/01

//Abstract base class for track seed finding for the tracker

#ifndef StiTrackSeedFinder_HH
#define StiTrackSeedFinder_HH

#include "StiSeedFinder.h"

class StiRootDrawableHits;
class StiKalmanTrack;
class Sti2HitComboFilter;
class StiHitContainer;
class StiDetectorContainer;
class Messenger;

class StiTrackSeedFinder : public StiSeedFinder
{
public:

    StiTrackSeedFinder(StiDetectorContainer*, StiHitContainer*, Sti2HitComboFilter*);
    virtual ~StiTrackSeedFinder();
    
    //Set Hit Combination filter type
    void setHitComboFilter(Sti2HitComboFilter* val);
    const Sti2HitComboFilter* getHitComboFilter() const;
    
    //Inherited User interface
    virtual bool hasMore() = 0;
    virtual StiKalmanTrack* next() = 0;
    virtual void build() = 0;
    virtual void reset();

    ///The derived class is responsible for ordering the detectors apporpriately.
    virtual void addLayer(StiDetector*) = 0;
    virtual void print() const = 0;
    
protected:
    
    //Shallow members
    StiDetectorContainer* mDetStore;
    StiHitContainer* mhitstore;
    StiRootDrawableHits* mdrawablehits;
    //Deep members
    Sti2HitComboFilter* mhitcombofilter;

private:
    //The following are not implemented
    StiTrackSeedFinder();
};

// Non members ---

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

inline void StiTrackSeedFinder::setHitComboFilter(Sti2HitComboFilter* val)
{
    mhitcombofilter = val;
}

inline const Sti2HitComboFilter* StiTrackSeedFinder::getHitComboFilter() const
{
    return mhitcombofilter;
}

#endif
