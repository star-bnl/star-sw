//StiCompositeSeedFinder.h
//M.L. Miller (Yale Software)
//08/01

#ifndef StiCompositeSeedFinder_HH
#define StiCompositeSeedFinder_HH

#include <vector>
using std::vector;
#include <string>
using std::string;

#include "StiSeedFinder.h"
#include "StiKalmanTrack.h"

class StiTrackSeedFinder;

class StiCompositeSeedFinder : public StiSeedFinder
{
public:
    StiCompositeSeedFinder();
    virtual ~StiCompositeSeedFinder(); //memory leak on vector!

    //Inherited User interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void build();
    
    void reset();

    void setFactory(StiKalmanTrackFactory*);    
    void setBuildPath(const string&);
    
private:
    
     //Nested helper class to encapsulate seed finders with their starting points (*Union*)
    class StiSeedFinderRep
    {
    public:
	StiSeedFinderRep(const string& buildPath, StiKalmanTrackFactory*);

	//We instantiate objects on the heap, so we implement "deep copy"
	StiSeedFinderRep(const StiSeedFinderRep&);
	StiSeedFinderRep& operator=(const StiSeedFinderRep&);
	
	virtual ~StiSeedFinderRep();
	
	//Reset to beginning for each new event
	void reset();

	//Iterate over start points
	bool hasMoreStartPoints() const;
	
	 //Set seed finder for current start point, then increment start point iterator
	StiSeedFinder* seedFinder();
	
    private:
	//Deep copy
	void copyToThis(const StiSeedFinderRep&);
	void build();

	//Set seed-finder for current start point
	void init(); 
	
	StiSeedFinderRep(); //Not implemented

	friend ostream& operator<<(ostream&, const StiSeedFinderRep&);

    private:
	//Shallow members	
	//For start point stuff 
	typedef vector <StiDetector*> StiDetectorVec_t;
	typedef vector<StiDetectorVec_t>  IntDetectorMap_t;
	typedef IntDetectorMap_t::const_iterator IntDetectorMapIterator_t;
	
	IntDetectorMap_t mIntDetMap;
	IntDetectorMap_t::iterator mCurrentStartPoint;
	string mBuildPath;
	StiKalmanTrackFactory* mTrackFactory;

    private:
	//deep members
	StiTrackSeedFinder* mSeedFinder;
	Sti2HitComboFilter* mHitComboFilter;
    };


private:
    typedef vector<StiSeedFinderRep> RepVec_t;
    typedef RepVec_t::iterator RepVecIterator_t;
    
    bool incrementRep();

    StiSeedFinder* mCurrentSeedFinder;
    StiKalmanTrackFactory* mTrackFactory;
    string mBuildPath;
    
    RepVec_t mRepVec;
    RepVecIterator_t mCurrentRep;
};

inline void StiCompositeSeedFinder::setBuildPath(const string& val)
{
    mBuildPath=val;
}

inline void StiCompositeSeedFinder::setFactory(StiKalmanTrackFactory* val)
{
    mTrackFactory = val;
}

ostream& operator<<(ostream&, const StiCompositeSeedFinder::StiSeedFinderRep&);

#endif
