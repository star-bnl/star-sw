// -*- mode: c++;-*-
// $Id: StppJetAnalyzer.h,v 1.1 2008/07/21 02:00:26 tai Exp $
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

  typedef std::list<StProtoJet> JetList;
  StppJetAnalyzer(JetList& protoJets) : _protoJetList(protoJets), _stJets(0) {}
  virtual ~StppJetAnalyzer() { }
  JetList& getJets(void) { return _protoJetList; }
  StJets* getmuDstJets(void) { return _stJets; };
  void setmuDstJets(StJets* v) { _stJets = v; };

private:

  JetList& _protoJetList;
  StJets* _stJets;

  ClassDef(StppJetAnalyzer,1)
};


#endif // STPPJETANALYZER_HH

