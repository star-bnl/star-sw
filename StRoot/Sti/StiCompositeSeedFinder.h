//StiCompositeSeedFinder.h
//M.L. Miller (Yale Software)
//08/01

#ifndef StiCompositeSeedFinder_HH
#define StiCompositeSeedFinder_HH

#include <vector>
using std::vector;

#include "StiSeedFinder.h"

class StiTrackSeedFinder;

class StiCompositeSeedFinder : public StiSeedFinder
{
public:

    //Nested helper class to encapsulate seed finders with their starting points (*Union*)
    class StiSeedFinderRep
    {
    public:
	typedef vector <StiDetector*> StiDetectorVec_t;
	typedef vector<StiDetectorVec_t>  IntDetectorMap_t;
	typedef IntDetectorMap_t::const_iterator IntDetectorMapIterator_t;
    
	StiSeedFinderRep(StiTrackSeedFinder*, const char* buildPath);
	virtual ~StiSeedFinderRep() {};

	void reset(); //Reset to beginning for each new event

	//Iterate over start points
	bool hasMoreStartPoints() const; //loop on sectors in tpc
	StiSeedFinder* seedFinder(); //Set seed finder for current start point, then increment start point iterator
	
    private:
	enum Initialized {kNotInitialized=-1,  kInitialized=1};
	
	void build();
	void init(); //Set seed-finder for current start point
	
	StiSeedFinderRep(); //Not implemented
	
	StiTrackSeedFinder* mSeedFinder;
	const char* mBuildPath;

	//For start point stuff (for now, assume TPC only for starting points, change later)
	int mMinSector;
	int mMaxSector;
	int mMinPadrow;
	int mMaxPadrow;
	
	IntDetectorMap_t mIntDetMap;
	IntDetectorMap_t::iterator mCurrentStartPoint;

    };
    
    typedef vector<StiSeedFinderRep> RepVec_t;
    typedef RepVec_t::iterator RepVecIterator_t;
    
    StiCompositeSeedFinder();
    virtual ~StiCompositeSeedFinder();

    //Inherited User interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    
    void reset();
    void buildOuterSeedFinder(StiTrackSeedFinder*);
    void buildInnerSeedFinder(StiTrackSeedFinder*);

private:
    bool incrementRep();

    StiSeedFinder* mCurrentSeedFinder;
    
    RepVec_t mRepVec;
    RepVecIterator_t mCurrentRep;
};

#endif
