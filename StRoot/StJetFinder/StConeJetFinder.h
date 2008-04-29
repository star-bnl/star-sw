// -*- mode: c++;-*-
// $Id: StConeJetFinder.h,v 1.26 2008/04/29 20:36:41 tai Exp $
#ifndef StConeJetFinder_HH
#define StConeJetFinder_HH

#include "StJetEtCellFactory.h"

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

#include "StEtaPhiGrid.h"

/*!
  \class StConeJetFinder
  \author M.L. Miller (Yale Software)
  Implementation of the cone algorithm, circa Tevatron RunII Jet Physics working group specification.
*/
class StConeJetFinder : public StJetFinder {

public:
	
  typedef StJetEtCell::CellList CellList;
  typedef list<StJetEtCell> ValueCellList;
	
    ///cstr-dstr
    StConeJetFinder(const StConePars& pars);
    virtual ~StConeJetFinder();
	
  void Init();

    ///simple access to the parameters
    StConePars pars() const; 
	
    ///inherited interface
    void findJets(JetList& protojets);     
    virtual void print();
	
protected:
		
    ///needs access to the grid
    friend struct PreJetInitializer; 
	
    ///Only available for derived classes
    StConeJetFinder();

  virtual StJetEtCellFactory* makeCellFactory();

	
    void initializeWorkCell(const StJetEtCell* other);

    void addToPrejets(StJetEtCell* cell);
	
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
	
protected:

  StConePars mPars; ///run-time pars
	
  StJetEtCell mWorkCell;
  int mSearchCounter;
	
  StJetSpliterMerger* mMerger;
  ValueCellList mPreJets;
	
  typedef std::pair<ValueCellList::iterator, ValueCellList::iterator> ValueCellListItPair;
  typedef vector<ValueCellListItPair> VCLItPairVec;
  VCLItPairVec mMidpointVec;

  StSpinJet::StEtaPhiGrid _cellGrid;
	
private:

  virtual void findJets_sub1();
  virtual void findJets_sub2();

};

inline StConePars StConeJetFinder::pars() const
{
    return mPars;
}

//non-members

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

#endif

