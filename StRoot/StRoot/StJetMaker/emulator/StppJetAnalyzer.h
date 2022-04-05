// -*- mode: c++;-*-
// $Id: StppJetAnalyzer.h,v 1.2 2008/08/02 19:23:08 tai Exp $
#ifndef STPPJETANALYZER_HH
#define STPPJETANALYZER_HH

#include <StJetFinder/StProtoJet.h>

#include <TObject.h>

#include <list>

class StJets;

//////////////////////////////////////////////////////////////
//
// This class exists solely for backword compatibility
//
//////////////////////////////////////////////////////////////
class StppJetAnalyzer : public TObject {

public:

  typedef std::list<StProtoJet> StjJetList;
  StppJetAnalyzer(StjJetList& protoJets) : _protoJetList(protoJets), _stJets(0) {}
  virtual ~StppJetAnalyzer() { }
  StjJetList& getJets(void) { return _protoJetList; }
  StJets* getmuDstJets(void) { return _stJets; };
  void setmuDstJets(StJets* v) { _stJets = v; };

private:

  StjJetList& _protoJetList;
  StJets* _stJets;

  ClassDef(StppJetAnalyzer,1)
};


#endif // STPPJETANALYZER_HH

