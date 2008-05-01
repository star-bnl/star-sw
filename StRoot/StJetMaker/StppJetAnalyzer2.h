// -*- mode: c++;-*-
// $Id: StppJetAnalyzer2.h,v 1.7 2008/05/01 17:32:29 tai Exp $
#ifndef STPPJETANALYZER2_HH
#define STPPJETANALYZER2_HH

#include "StppAnaPars.h"

#include "StJetFinder/StProtoJet.h"

#include <list>

class StMuTrack;
class AbstractFourVec;
class StJetPars;
class StJetFinder;
class StFourPMaker;
class StMuTrackFourVec;

namespace StSpinJet {

class StppJetAnalyzer2 {

public:

  typedef std::list<StProtoJet> ProtoJetList;

  StppJetAnalyzer2(const StppAnaPars* ap, StJetPars* jp, StFourPMaker* fp, ProtoJetList& protoJets);

  virtual ~StppJetAnalyzer2();

  void Init();

  void findJets();

private:

  void collectFourMomentum();
  void applyCutsOnJets();

  bool shoudNotPassToJetFinder(AbstractFourVec* particle);

  bool isChargedTrack(StMuTrackFourVec* p);

  bool shouldNotKeep(StProtoJet &pj);
    
  StJetFinder* _jetFinder;

  ProtoJetList& _protoJetList;
  StFourPMaker* _fourPMaker;

  StppAnaPars _anaPar;

};

}

#endif // STPPJETANALYZER2_HH

