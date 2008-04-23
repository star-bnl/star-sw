// -*- mode: c++;-*-
// $Id: StConeJetFinder.h,v 1.13 2008/04/23 21:40:41 tai Exp $
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

#include "StConePars.h"

/*!
  \class StConeJetFinder
  \author M.L. Miller (Yale Software)
  Implementation of the cone algorithm, circa Tevatron RunII Jet Physics working group specification.
*/
class StConeJetFinder : public StJetFinder {

public:
	
    ///useful typdefs
    typedef map<StEtGridKey, StJetEtCell*, StEtGridKeyLessThan> CellMap;
    typedef CellMap::value_type CellMapValType;
    typedef StJetEtCell::CellList CellList;
    typedef list<StJetEtCell> ValueCellList;
	
    ///cstr-dstr
    StConeJetFinder(const StConePars& pars);
    virtual ~StConeJetFinder();
	
  virtual void Init();

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
	
    CellMap _EtCellMap; ///the map references the objects in the vector
    CellList _EtCellList; ///the vector holds the actual objects
    CellList::iterator mTheEnd;
	
    StJetEtCell mWorkCell;
    int mSearchCounter;
	
    StJetSpliterMerger* mMerger;
    ValueCellList mPreJets;
	
    typedef std::pair<ValueCellList::iterator, ValueCellList::iterator> ValueCellListItPair;
    typedef vector<ValueCellListItPair> VCLItPairVec;
    VCLItPairVec mMidpointVec;
	
private:

  virtual void findJets_sub1();
  virtual void findJets_sub2();

};

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

