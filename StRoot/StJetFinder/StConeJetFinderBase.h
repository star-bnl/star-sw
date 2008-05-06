// -*- mode: c++;-*-
// $Id: StConeJetFinderBase.h,v 1.3 2008/05/06 22:43:49 tai Exp $
#ifndef STCONEJETFINDERBASE_H
#define STCONEJETFINDERBASE_H

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
class StConeJetFinderBase;

#include "StConePars.h"

#include "StEtaPhiGrid.h"

/*!
  \class StConeJetFinder
  \author M.L. Miller (Yale Software)
  Implementation of the cone algorithm, circa Tevatron RunII Jet Physics working group specification.
*/
class StConeJetFinderBase : public StJetFinder {

public:
	
  typedef StEtaPhiCell::CellList CellList;
  typedef list<StEtaPhiCell> ValueCellList;
	
    ///cstr-dstr
    StConeJetFinderBase(const StConePars& pars);
    virtual ~StConeJetFinderBase();
	
  void Init();

    ///simple access to the parameters
    StConePars pars() const; 
	
  virtual void findJets(JetList& protojets) = 0;
	
protected:
		
    ///needs access to the grid
    friend struct PreJetInitializer; 
	
    ///Only available for derived classes
    StConeJetFinderBase();

  virtual StJetEtCellFactory* makeCellFactory();

	
    void initializeWorkCell(const StEtaPhiCell* other);

    void addToPrejets(StEtaPhiCell& cell);
	
    enum SearchResult {kTooManyTries=0, kLeftVolume=1, kConverged=2, kContinueSearch=3};	
    SearchResult doSearch();
	
  virtual bool shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const;

	
  const StProtoJet& collectCell(StEtaPhiCell* seed);
	
  StConePars mPars; ///run-time pars
	
  StEtaPhiCell *mWorkCell;
  int mSearchCounter;
	
  CellList _preJets;
	
  typedef vector<std::pair<CellList::iterator, CellList::iterator> > CellPairList;
  CellPairList _cellPairList;

  StSpinJet::StEtaPhiGrid _cellGrid;
	

  void clearPreviousResult();

  CellList generateEtOrderedList(JetList& protoJetList);

  CellList generateToSearchListFrom(CellList& orderedList);

  void findProtoJets(CellList& toSearchList);

  void storeTheResultIn(JetList& protoJetList);

private:

  virtual void findJetAroundThis(StEtaPhiCell* cell) = 0;

  virtual bool shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const;

  bool isInTheVolume(double eta, double phi);
  bool areTheyInTheSameCell(double eta1, double phi1, double eta2, double phi2);

};

inline StConePars StConeJetFinderBase::pars() const
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

#endif // STCONEJETFINDERBASE_H


