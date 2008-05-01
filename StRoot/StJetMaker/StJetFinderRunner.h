// -*- mode: c++;-*-
// $Id: StJetFinderRunner.h,v 1.1 2008/05/01 17:44:50 tai Exp $
#ifndef STJETFINDERRUNNER_HH
#define STJETFINDERRUNNER_HH

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

class StJetFinderRunner {

public:

  typedef std::list<StProtoJet> ProtoJetList;

  StJetFinderRunner(const StppAnaPars* ap, StJetPars* jp, StFourPMaker* fp, ProtoJetList& protoJets);

  virtual ~StJetFinderRunner();

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

#endif // STJETFINDERRUNNER_HH

