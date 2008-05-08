// -*- mode: c++;-*-
// $Id: StConeJetFinder.h,v 1.44 2008/05/08 05:02:13 tai Exp $
#ifndef StConeJetFinder_HH
#define StConeJetFinder_HH

#include "StConeJetFinderBase.h"

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


};

#endif

