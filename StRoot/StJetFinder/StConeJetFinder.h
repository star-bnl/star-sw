//StConeJetFinder.h
//M.L. Miller (Yale Software)
//07/02

//adapted from Akio Ogawa's work

#ifndef StConeJetFinder_HH
#define StConeJetFinder_HH

#include "TObject.h"


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
class StConeJetFinder;

/*!
  \class StConePars
  \author M.L. Miller (MIT Software)
  A simple class to encapsulate the requisite run-time parameters of the cone jet algorithm.
*/

///Run time pars
class StConePars : public StJetPars
{
public:

    ///Set the grid spacing:
    void setGridSpacing(int nEta, double etaMin, double etaMax,
			int nPhi, double phiMin, double phiMax);
    
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

    ///Require stable midpoints?
    void setRequireStableMidpoints(bool v) {mRequireStableMidpoints=v;}
    bool requiredStableMidpoints() const {return mRequireStableMidpoints;}

    ///Set cone radius:
    void setConeRadius(double v) {mR = v;}
    double coneRadius() const {return mR;}

    ///Toggle debug streams on/off
    void setDebug(bool v) {mDebug = v;}
    bool debug() const {return mDebug;}
    
private:
    friend class StConeJetFinder;
    friend class StCdfChargedConeJetFinder;

    int mNeta;
    int mNphi;
    double mEtaMin;
    double mEtaMax;
    double mPhiMin;
    double mPhiMax;

    double mR;
    
    double mAssocEtMin;
    double mSeedEtMin;
    
    double mphiWidth;
    double metaWidth;
    int mdeltaPhi;
    int mdeltaEta;
    
    bool mDoMinimization;
    bool mAddMidpoints;
    bool mDoSplitMerge;
    double mSplitFraction;
    bool mRequireStableMidpoints;
    bool mDebug;
    
    ClassDef(StConePars,1)
	};


/*!
  \class StConeJetFinder
  \author M.L. Miller (Yale Software)
  Implementation of the cone algorithm, circa Tevatron RunII Jet Physics working group specification.
*/
class StConeJetFinder : public StJetFinder
{
public:
	
    ///useful typdefs
    typedef map<StEtGridKey, StJetEtCell*, StEtGridKeyLessThan> CellMap;
    typedef CellMap::value_type CellMapValType;
    typedef vector<StJetEtCell*> CellVec;
    typedef StJetEtCell::CellList CellList;
    typedef list<StJetEtCell> ValueCellList;
	
    ///cstr-dstr
    StConeJetFinder(const StConePars& pars);
    virtual ~StConeJetFinder();
	
    ///simple access to the parameters
    StConePars pars() const; 
	
    ///inherited interface
    virtual void findJets(JetList& protojets);     
    virtual void clear();
    virtual void print();
	
protected:
		
    ///needs access to the grid
    friend struct PreJetInitializer; 
	
    ///Only available for derived classes
    StConeJetFinder();

    ///make a polymorphic cell
    virtual StJetEtCell* makeCell(double etaMin, double etaMax, double phiMin, double phiMax);
    ///build the grid at construction time
    virtual void buildGrid();
	
    ///put 'em in the grid
    virtual void fillGrid(JetList& protoJets); 
	
    void clearAndDestroy();

    void initializeWorkCell(const StJetEtCell* other);

    void addToPrejets(StJetEtCell* cell);
	
    StJetEtCell* findCellByKey(const StEtGridKey& key);
	
    enum SearchResult {kTooManyTries=0, kLeftVolume=1, kConverged=2, kContinueSearch=3};	
    SearchResult doSearch();
	
    void doMinimization();
	
    void addSeedsAtMidpoint();
	
    StJetEtCell* defineMidpoint(const StJetEtCell& pj1, const StJetEtCell& pj2) ;
	
    virtual bool acceptSeed(const StJetEtCell* cell);
    virtual bool acceptPair(const StJetEtCell* center, const StJetEtCell* assoc) const;
	
    const StProtoJet& collectCell(StJetEtCell* seed);
	
    ///action
    int findPhiKey(double phi) const;
    int findEtaKey(double eta) const;
	
    ///is this point in the detector volume?
    bool inVolume(double eta, double phi);
	
    ///find a key.  If out of bounds, it aborts program flow.  otherwise, nasty run-time errors!
    StEtGridKey findKey(double eta, double phi) const;
	
    ///find iterators into grid
    CellMap::iterator findIterator(double eta, double phi);
    ///find iterators into grid
    CellMap::iterator findIterator(const StEtGridKey&);
	
    void setSearchWindow();
	
protected:
	
    StConePars mPars; ///run-time pars
	
    CellMap mMap; ///the map references the objects in the vector
    CellVec mVec; ///the vector holds the actual objects
    CellVec::iterator mTheEnd;
	
    StJetEtCell mWorkCell;
    int mSearchCounter;
	
    StJetSpliterMerger* mMerger;
    ValueCellList mPreJets;
	
    typedef std::pair<ValueCellList::iterator, ValueCellList::iterator> ValueCellListItPair;
    typedef vector<ValueCellListItPair> VCLItPairVec;
    VCLItPairVec mMidpointVec;
	
};

//inlines

inline StConePars StConeJetFinder::pars() const
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

inline void StConePars::setSeedEtMin(double v)
{
    mSeedEtMin = v;
}

inline double StConePars::seedEtMin() const
{
    return mSeedEtMin;
}

inline void StConePars::setAssocEtMin(double v)
{
    mAssocEtMin = v;
}

inline double StConePars::assocEtMin() const
{
    return mAssocEtMin;
}


inline void StConePars::setSplitFraction(double v)
{
   mSplitFraction = v;
}

inline double StConePars::splitFraction() const
{
    return mSplitFraction;
}

inline void StConePars::setGridSpacing(int nEta, double etaMin, double etaMax,
				int nPhi, double phiMin, double phiMax)
{
    mNeta = nEta; mEtaMin = etaMin; mEtaMax = etaMax;
    mNphi = nPhi; mPhiMin = phiMin; mPhiMax = phiMax;
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

