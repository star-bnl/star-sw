// -*- mode: c++;-*-
// $Id: StConeJetFinder.h,v 1.45 2008/08/23 20:47:41 tai Exp $
#ifndef StConeJetFinder_HH
#define StConeJetFinder_HH

#include "StConeJetFinderBase.h"

#include <vector>

class StConeJetFinder : public StConeJetFinderBase {

public:
	
  StConeJetFinder(const StConePars& pars);
  virtual ~StConeJetFinder();

  StJetEtCellFactory* makeCellFactory();

  void findJets(JetList& protoJetList, const FourVecList& particleList);
	
private:

  CellList generateEtOrderedCellList(const FourVecList& particleList);

  CellList findProtoJetsAroundCells(CellList& toSearchList);

  StEtaPhiCell* findJetAroundThis(StEtaPhiCell* cell);


  bool shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const;

  StEtaPhiCell* createJetCellFor(StEtaPhiCell& cell);
  StEtaPhiCell* findJetWithStableCone();
	
  CellList generateMidpointList(const CellList& protoJetCellList);

  CellList findProtoJetsAroundMidpoints(CellList& midpointList);

  bool shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const;

  bool isInTheVolume(double eta, double phi);

  bool areTheyInTheSameCell(double eta1, double phi1, double eta2, double phi2);

  void storeTheResults(JetList& protoJetList, const CellList& protoJetCellList);

  StJetSpliterMerger* mMerger;

  std::vector<StEtaPhiCell*> _toDelete;
  void deleteToDelete();

};

#endif

