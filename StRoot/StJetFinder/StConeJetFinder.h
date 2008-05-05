// -*- mode: c++;-*-
// $Id: StConeJetFinder.h,v 1.32 2008/05/05 21:10:32 tai Exp $
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
	
  typedef StEtaPhiCell::CellList CellList;
  typedef list<StEtaPhiCell> ValueCellList;
	
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

	
    void initializeWorkCell(const StEtaPhiCell* other);

    void addToPrejets(StEtaPhiCell& cell);
	
    enum SearchResult {kTooManyTries=0, kLeftVolume=1, kConverged=2, kContinueSearch=3};	
    SearchResult doSearch();
	
    void doMinimization();
	
    void addSeedsAtMidpoint();
	
    StEtaPhiCell* defineMidpoint(const StEtaPhiCell& pj1, const StEtaPhiCell& pj2) ;
	
  virtual bool acceptSeed(const StEtaPhiCell* cell);

	
  const StProtoJet& collectCell(StEtaPhiCell* seed);
	
  StConePars mPars; ///run-time pars
	
  StEtaPhiCell *mWorkCell;
  int mSearchCounter;
	
  StJetSpliterMerger* mMerger;
  CellList _preJets;
	
  typedef vector<std::pair<CellList::iterator, CellList::iterator> > CellPairList;
  CellPairList _cellPairList;

  StSpinJet::StEtaPhiGrid _cellGrid;
	
private:


  virtual void findJets_sub1();
  virtual void findJets_sub2();

  virtual bool shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const;

  bool isInTheVolume(double eta, double phi);
  bool areTheyInTheSameCell(double eta1, double phi1, double eta2, double phi2);

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
	
    void operator()(StEtaPhiCell& cell);
    void operator()(StEtaPhiCell* cell);
};

struct PostMergeUpdater
{
    void operator()(StEtaPhiCell& cell);
};

#endif

