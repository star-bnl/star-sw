// -*- mode: c++;-*-
// $Id: StParticleCollector.h,v 1.2 2008/05/01 21:28:39 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STPARTICLECOLLECTOR_HH
#define STPARTICLECOLLECTOR_HH

#include "StppAnaPars.h"

#include "StJetFinder/StProtoJet.h"

#include <list>

class StMuTrack;
class AbstractFourVec;
class StFourPMaker;
class StMuTrackFourVec;

namespace StSpinJet {

class StParticleCollector {

public:

  typedef std::list<StProtoJet> ProtoJetList;

  StParticleCollector(const StppAnaPars* ap, StFourPMaker* fp, ProtoJetList& protoJets);

  virtual ~StParticleCollector();

  void Do();

private:

  bool shoudNotPassToJetFinder(AbstractFourVec* particle);

  bool isChargedTrack(StMuTrackFourVec* p);

  ProtoJetList& _protoJetList;
  StFourPMaker* _fourPMaker;

  StppAnaPars _anaPar;

};

}

#endif // STPARTICLECOLLECTOR_HH
