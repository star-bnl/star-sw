// -*- mode: c++;-*-
// $Id: StppJetAnalyzer2.h,v 1.3 2008/04/21 20:42:26 tai Exp $
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

/*!
  \class StppJetAnalyzer2
  \author M.L. Miller (Yale Software)
  StppJetAnalyzer2 is used to outsource the handling of the protojet list that is passed to
  and from the jet finding algorithms.  In doing so, tracks and jets are also passed
  through several acceptance filters.  StppJetAnalyzer2 is responsible for instantiating the
  proper type of jetfinder, based on the type of StJetPars object passed in the constructor.
  Additionally, StppJetAnalyzer2 also instantiates a new StJets object and hangs it on the
  jet tree as a separate branch.  Thus, if there are 'n' jet algorithms run in an analysis,
  there are 'n' StppJetAnalyzer2s constructed, and there are 'n' StJets objects hung on the
  jet TTree.
 */
class StppJetAnalyzer2 : public TObject {

public:

  typedef std::list<StProtoJet> JetList;

  ///Construct a new Analyzer
  StppJetAnalyzer2(const StppAnaPars* ap, StJetPars* jp, StFourPMaker* fp, JetList& protoJets);
  virtual ~StppJetAnalyzer2();


  void print();

  ///filter the list, call StJetFinder::findJets(), and filter the jets
  void findJets();

  ///Access to the stl container of protojets
  JetList& getJets(void) {return mProtoJets;} //!
    
  ///Access to the FourPMaker associated with this analyzer.  This 4-p may be shared with other analyzers
  StFourPMaker* fourPMaker() {return mFourPMaker;}

  // for backword compatability
  StJets* getmuDstJets(void) { return muDstJets; };
  void setmuDstJets(StJets* v) { muDstJets = v; };

private:

  StppJetAnalyzer2();


  bool accept4p(StMuTrackFourVec* p);

  bool isChargedTrack(StMuTrackFourVec* p);

  void acceptJets();
  bool acceptJet(StProtoJet &pj);
  void fillLists();
    
  StJetFinder* mFinder;

  JetList& mProtoJets;
  StFourPMaker* mFourPMaker;

  StppAnaPars mPars;

  // for backword compatability
  StJets* muDstJets;

  ClassDef(StppJetAnalyzer2,1)
};


#endif // STPPJETANALYZER2_HH

