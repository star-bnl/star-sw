// $Id: StppJetAnalyzer.h,v 1.7 2008/03/29 18:45:23 tai Exp $
//
// Author List: M.L. Miller
//              Thomas Henry
//              Tai Sakuma

#ifndef STPPJETANALYZER_HH
#define STPPJETANALYZER_HH

#include <TObject.h>

#include "StJetFinder/StProtoJet.h"

#include <list>
#include <vector>

class StMuTrack;
class AbstractFourVec;
class StJetPars;
class StJetFinder;
class StFourPMaker;
class StMuTrackFourVec;
class StJets;

/*!
  \class StppAnaPars
  \author M.L. Miller
  StppAnaPars is a simple class to encapsulate the run-time track and jet cuts that are necessary
  for a jet analysis.
 */
class StppAnaPars : public TObject {

public:

  void setCutPtMin(double v) { mPtMin = v; }
  double ptMin() const { return mPtMin; }

  void setAbsEtaMax(double v) { mEtaMax = v; }
  double etaMax() const { return mEtaMax; }

  void setJetPtMin(double v) { mJetPtMin = v; }
  double jetPtMin() const { return mJetPtMin; }

  void setJetEtaMax(double v) { mJetEtaMax = v; }
  double jetEtaMax() const { return mJetEtaMax; }

  void setJetEtaMin(double v) { mJetEtaMin =v; }
  double jetEtaMin() const { return mJetEtaMin; }

  void setJetNmin(int v) { mJetNmin = v; }
  int jetNmin() const { return mJetNmin; }

  void setNhits(int v) { mNhits=v; }
  int nHits() const { return mNhits; }

  void setFlagMin(int v) { mFlagMin = v; }
  int flagMin() const { return mFlagMin; }

private:

    //cuts that particles must pass to be used for jet-finding
  double mPtMin;
  double mEtaMax;
  int mNhits;
  int mFlagMin;
    
  //Cut to accept found-jets
  double mJetPtMin;
  double mJetEtaMax;
  double mJetEtaMin;
  int mJetNmin;

  friend class StppJetAnalyzer;

  ClassDef(StppAnaPars,1)
};

/*!
  \class StppJetAnalyzer
  \author M.L. Miller (Yale Software)
  StppJetAnalyzer is used to outsource the handling of the protojet list that is passed to
  and from the jet finding algorithms.  In doing so, tracks and jets are also passed
  through several acceptance filters.  StppJetAnalyzer is responsible for instantiating the
  proper type of jetfinder, based on the type of StJetPars object passed in the constructor.
  Additionally, StppJetAnalyzer also instantiates a new StJets object and hangs it on the
  jet tree as a separate branch.  Thus, if there are 'n' jet algorithms run in an analysis,
  there are 'n' StppJetAnalyzers constructed, and there are 'n' StJets objects hung on the
  jet TTree.
 */
class StppJetAnalyzer : public TObject {

public:

  typedef std::list<StProtoJet> JetList;
  typedef std::vector<AbstractFourVec*> FourList;

  ///Construct a new Analyzer
  StppJetAnalyzer(const StppAnaPars* ap, const StJetPars* jp, StFourPMaker* fp);
  virtual ~StppJetAnalyzer();

  ///simple gets/sets

  ///Set the container of (unfiltered) four vectors
  void setFourVec(FourList &tracks);
  void print();

  ///filter the list, call StJetFinder::findJets(), and filter the jets
  void findJets();

  ///internal clear
  void clear();
    
  ///Access to the stl container of protojets
  JetList& getJets(void) {return mProtoJets;} //!
    
  ///Access to the FourPMaker associated with this analyzer.  This 4-p may be shared with other analyzers
  StFourPMaker* fourPMaker() {return mFourPMaker;}

  // for backword compatability
  StJets* getmuDstJets(void) { return muDstJets; };
  void setmuDstJets(StJets* v) { muDstJets = v; };

private:

  StppJetAnalyzer();

  bool accept(StMuTrack*);
  bool accept(StMuTrackFourVec*);
  bool accept(const StProtoJet& pj);
  void acceptJets(void);
  bool acceptJet(StProtoJet &pj);
  void fillLists();
  void fillLists(FourList &tracks);
    
  StJetFinder* mFinder;

  JetList mProtoJets;
  FourList mFourList;
  StFourPMaker* mFourPMaker;

  StppAnaPars mPars;

  // for backword compatability
  StJets* muDstJets;

  ClassDef(StppJetAnalyzer,1)
};


#endif // STPPJETANALYZER_HH

