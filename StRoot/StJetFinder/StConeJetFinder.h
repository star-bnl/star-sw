//StConeJetFinder.h
//M.L. Miller (Yale Software)
//07/02

//adapted from Akio Ogawa's work

#ifndef StConeJetFinder_HH
#define StConeJetFinder_HH

//std
#include <utility>
#include <map>
#include <vector>
#include <cmath>
using std::map;
using std::vector;
using std::pair;

//StJetFinder
#include "StJetFinder.h"
#include "Functors.h"
#include "StJetEtCell.h"

class StJetSpliterMerger;

class StConeJetFinder : public StJetFinder
{
public:
	
    ///Run time pars
    struct StConeJetFinderPars {
	int mNeta;
	int mNphi;
	double mEtaMin;
	double mEtaMax;
	double mPhiMin;
	double mPhiMax;
    };
	
    //useful typdefs
    typedef map<StEtGridKey, StJetEtCell*, StEtGridKeyLessThan> CellMap;
    typedef CellMap::value_type CellMapValType;
    typedef vector<StJetEtCell*> CellVec;
    typedef StJetEtCell::CellList CellList;
    typedef list<StJetEtCell> ValueCellList;
	
    //cstr-dstr
    StConeJetFinder(StConeJetFinderPars& pars);
    virtual ~StConeJetFinder();
	
    //simple access 
    StConeJetFinderPars pars() const; //return by value to prevent changes to pars
	
    ///minimum et threshold to be considered a seed
    void setSeedEtMin(double);
    double seedEtMin() const;
	
    ///minimum et threshold to be considered for addition to the seed
    void setAssocEtMin(double);
    double assocEtMin() const;
	
    ///split jets if E_shared/E_neighbor>splitFraction
    void setSplitFraction(double v);
    double splitFraction() const;
	
    ///Let jet wander to minimum?
    void setPerformMinimization(bool v) {mDoMinimization=v;}
    bool performMinimization() const {return mDoMinimization;}
	
    ///Add seeds at midpoints?
    void setAddMidpoints(bool v) {mAddMidpoints=v;}
    bool addMidpoints() const {return mAddMidpoints;}
	
    ///Do Split/Merge step?
    void setDoSplitMerge(bool v) {mDoSplitMerge=v;}
    bool doSplitMerge() const {return mDoSplitMerge;}

    ///Use r/::sqrt(2) for iteration search?
    void setDoMidpointFix(bool v) {mDoMidpointFix=v;}
    bool doMidpointFix() const {return mDoMidpointFix;}

    ///Require stable midpoints?
    void setRequireStableMidpoints(bool v) {mRequireStableMidpoints=v;}
    bool requiredStableMidpoints() const {return mRequireStableMidpoints;}

    //inherited interface
    virtual void findJets(JetList& protojets);
    virtual void clear();
    virtual void print();
	
protected:
		
    friend struct PreJetInitializer; //needs access to the grid
	
    StConeJetFinder(); ///Only available for derived classes

    //make a polymorphic cell
    virtual StJetEtCell* makeCell(double etaMin, double etaMax, double phiMin, double phiMax);
    virtual void buildGrid(); ///build the grid at construction time
	
    virtual void fillGrid(JetList& protoJets); ///put 'em in the grid
	
    void clearAndDestroy();

    void initializeWorkCell(const StJetEtCell* other);

    void addToPrejets(StJetEtCell* cell);
	
    StJetEtCell* findCellByKey(const StEtGridKey& key);
	
    enum SearchResult {kTooManyTries=0, kLeftVolume=1, kConverged=2, kContinueSearch=3};	
    SearchResult doSearch();
	
    //void doMinimization(StJetEtCell& workCell);
    void doMinimization();
	
    void addSeedsAtMidpoint();
	
    StJetEtCell* defineMidpoint(const StJetEtCell& pj1, const StJetEtCell& pj2) ;
	
    virtual bool acceptSeed(const StJetEtCell* cell);
    virtual bool acceptPair(const StJetEtCell* center, const StJetEtCell* assoc) const;
	
    const StProtoJet& collectCell(StJetEtCell* seed);
	
    //action
    int findPhiKey(double phi) const;
    int findEtaKey(double eta) const;
	
    //is this point in the detector volume?
    bool inVolume(double eta, double phi);
	
    //find a key.  If out of bounds, it aborts program flow.  otherwise, nasty run-time errors!
    StEtGridKey findKey(double eta, double phi) const;
	
    //find iterators into grid
    CellMap::iterator findIterator(double eta, double phi);
    CellMap::iterator findIterator(const StEtGridKey&);
	
    void setSearchWindow();
	
protected:
	
    StConeJetFinderPars mPars; ///run-time pars
	
    CellMap mMap; ///the map references the objects in the vector
    CellVec mVec; ///the vector holds the actual objects
    CellVec::iterator mTheEnd;
	
    double mAssocEtMin;
    double mSeedEtMin;
	
    double mphiWidth;
    double metaWidth;
    int mdeltaPhi;
    int mdeltaEta;
	
    bool mDoMinimization;
    bool mAddMidpoints;
    bool mDoSplitMerge;
    bool mDoMidpointFix;
    bool mRequireStableMidpoints;
	
    StJetEtCell mWorkCell;
    int mSearchCounter;
	
    StJetSpliterMerger* mMerger;
    ValueCellList mPreJets;
	
    typedef std::pair<ValueCellList::iterator, ValueCellList::iterator> ValueCellListItPair;
    typedef vector<ValueCellListItPair> VCLItPairVec;
    VCLItPairVec mMidpointVec;
	
};

//inlines

inline StConeJetFinder::StConeJetFinderPars StConeJetFinder::pars() const
{
    return mPars;
}

inline int StConeJetFinder::findEtaKey(double eta) const
{
    return int( (eta-mPars.mEtaMin)/(mPars.mEtaMax-mPars.mEtaMin)*mPars.mNeta );
}

inline int StConeJetFinder::findPhiKey(double phi) const
{
    while(phi>M_PI) {phi-=2*M_PI;}
    while(phi<-M_PI) {phi+=2*M_PI;}
    return int( (phi-mPars.mPhiMin)/(mPars.mPhiMax-mPars.mPhiMin)*mPars.mNphi );
}

inline void StConeJetFinder::setSeedEtMin(double v)
{
    mSeedEtMin = v;
}

inline double StConeJetFinder::seedEtMin() const
{
    return mSeedEtMin;
}

inline void StConeJetFinder::setAssocEtMin(double v)
{
    mAssocEtMin = v;
}

inline double StConeJetFinder::assocEtMin() const
{
    return mAssocEtMin;
}

//non-members

struct PreJetUpdater //don't assume proto-jet updated
{
    PreJetUpdater() : sumEt(0.) {};
    double sumEt;
	
    //make sure that cell eT reflects the energy *only* in the physical cell
    void operator()(StJetEtCell* cell);
};

struct PreJetLazyUpdater //assume proto-jet updated
{
    PreJetLazyUpdater() : sumEt(0.) {};
    double sumEt;
	
    void operator()(StJetEtCell& cell);
    void operator()(StJetEtCell* cell);
};

struct PostMergeUpdater
{
    void operator()(StJetEtCell& cell);
};

class StConeJetFinder;

struct PreJetInitializer
{
    PreJetInitializer(StConeJetFinder& j) : mConeFinder(j) {};
    StConeJetFinder& mConeFinder;
	
    void operator()(StJetEtCell& cell);
};

#endif

