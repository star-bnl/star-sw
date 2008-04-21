// -*- mode: c++;-*-
// $Id: StppJetAnalyzer2.h,v 1.5 2008/04/21 21:37:05 tai Exp $
#ifndef STPPJETANALYZER2_HH
#define STPPJETANALYZER2_HH

#include "StppAnaPars.h"

#include <TObject.h>

#include "StJetFinder/StProtoJet.h"

#include <list>

class StMuTrack;
class AbstractFourVec;
class StJetPars;
class StJetFinder;
class StFourPMaker;
class StMuTrackFourVec;
class StJets;

class StppJetAnalyzer2 : public TObject {

public:

  typedef std::list<StProtoJet> ProtoJetList;

  StppJetAnalyzer2(const StppAnaPars* ap, StJetPars* jp, StFourPMaker* fp, ProtoJetList& protoJets);

  virtual ~StppJetAnalyzer2();

  void findJets();

private:

  void collectFourMomentum();
  void applyCuts();

  bool accept4p(StMuTrackFourVec* p);

  bool isChargedTrack(StMuTrackFourVec* p);

  bool acceptJet(StProtoJet &pj);
    
  StJetFinder* _jetFinder;

  ProtoJetList& _protoJetList;
  StFourPMaker* _fourPMaker;

  StppAnaPars _anaPar;


  ClassDef(StppJetAnalyzer2,1)
};


#endif // STPPJETANALYZER2_HH

