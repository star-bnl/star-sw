// -*- mode: c++;-*-
// $Id: StConeJetFinderBase.h,v 1.9 2008/05/08 04:40:04 tai Exp $
#ifndef STCONEJETFINDERBASE_H
#define STCONEJETFINDERBASE_H

#include "StJetEtCellFactory.h"

#include "TObject.h"



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
	
  StConeJetFinderBase(const StConePars& pars);
  virtual ~StConeJetFinderBase();
	
  void Init();

  virtual void findJets(JetList& protoJetList, const FourVecList& particleList) = 0;
	
protected:
		
    ///needs access to the grid
    friend struct PreJetInitializer; 
	
    ///Only available for derived classes
    StConeJetFinderBase();

  virtual StJetEtCellFactory* makeCellFactory() = 0;

	
    void initializeWorkCell(const StEtaPhiCell* other);

    void addToPrejets(StEtaPhiCell& cell);
	
    void formCone();
	
  virtual bool shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const;

	
  const StProtoJet& collectCell(StEtaPhiCell* seed);
	
  StConePars mPars; ///run-time pars
	
  StEtaPhiCell *mWorkCell;
	
  CellList _preJets;
	

  StSpinJet::StEtaPhiGrid _cellGrid;
	

  void clearPreviousResult();

  CellList generateToSearchListFrom(CellList& orderedList);

  //  virtual void findProtoJets(CellList& toSearchList);

  void storeTheResultIn(JetList& protoJetList);

private:

  //  virtual void findJetAroundThis(StEtaPhiCell* cell) = 0;

  virtual bool shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const;

};

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


