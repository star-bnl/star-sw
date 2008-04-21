// -*- mode: c++;-*-
// $Id: StppJetAnalyzer2.h,v 1.4 2008/04/21 21:20:26 tai Exp $
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


  ClassDef(StppJetAnalyzer2,1)
};


#endif // STPPJETANALYZER2_HH

