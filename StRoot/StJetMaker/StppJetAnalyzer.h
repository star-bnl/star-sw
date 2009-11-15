//StppJetAnalyzer.h
//M.L. Miller (Yale Software)
//07/02
//Modified by Thomas Henry
//8/02 -- Turned StppJetAnalyzer into the interface between
//analyzers and events

#ifndef StppJetAnalyzer_HH
#define StppJetAnalyzer_HH

#include "TObject.h"
#include "StJetFinder/StJetFinder.h"
#include "StJetFinder/FourVec.h"
#include "StJetFinder/StProtoJet.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StJets.h"

class StFourPMaker;


class StMuTrackFourVec;

/*!
  \class StppAnaPars
  \author M.L. Miller (MIT Softwarw)
  StppAnaPars is a simple class to encapsulate the run-time track and jet cuts that are necessary
  for a jet analysis.
 */
class StppAnaPars
{
public:

    //gets/sets for run-time pars
    virtual void setCutPtMin(double v) {mPtMin=v;}
    virtual double ptMin() const {return mPtMin;}

    virtual void setAbsEtaMax(double v) {mEtaMax=v;}
    virtual double etaMax() const {return mEtaMax;}

    virtual void setJetPtMin(double v) {mJetPtMin=v;}
    virtual double jetPtMin() const {return mJetPtMin;}

    virtual void setJetEtaMax(double v) {mJetEtaMax=v;}
    virtual double jetEtaMax() const {return mJetEtaMax;}

    virtual void setJetEtaMin(double v) {mJetEtaMin=v;}
    virtual double jetEtaMin() const {return mJetEtaMin;}

    virtual void setJetNmin(int v) {mJetNmin=v;}
    virtual int jetNmin() const {return mJetNmin;}

    virtual void setNhits(int v) {mNhits=v;}
    virtual int nHits() const {return mNhits;}

    virtual void setFlagMin(int v) {mFlagMin = v;}
    virtual int flagMin() const {return mFlagMin;}

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
class StppJetAnalyzer
{
public:
    typedef StJetFinder::JetList JetList;
    typedef vector<AbstractFourVec*> FourList;

    ///Construct a new Analyzer
    StppJetAnalyzer(const StppAnaPars* ap, const StJetPars* jp, StFourPMaker* fp);
    virtual ~StppJetAnalyzer();

    ///simple gets/sets

    ///Set the container of (unfiltered) four vectors
    virtual void setFourVec(FourList &tracks);
    virtual void print();

    ///filter the list, call StJetFinder::findJets(), and filter the jets
    virtual void findJets();

    ///internal clear
    virtual void clear();
    
    ///Access to the stl container of protojets
    JetList& getJets(void) {return mProtoJets;} //!
    
    ///Access to the StJets object  associated with this analyzer
    StJets* getmuDstJets(void) { return muDstJets; };

    ///Access to the FourPMaker associated with this analyzer.  This 4-p may be shared with other analyzers
    StFourPMaker* fourPMaker() {return mFourPMaker;}

    ///Hang the owned StJets object on the TTree
    void addBranch(const char *name, void *stppudst); 

protected:
    bool accept(StMuTrack*);
    bool accept(StMuTrackFourVec*);
    bool accept(const StProtoJet& pj);
    void acceptJets(void);
    bool acceptJet(StProtoJet &pj);
    void fillLists();
    void fillLists(FourList &tracks);
    
protected:
    StJetFinder* mFinder; //!

    JetList mProtoJets; //!
    FourList mFourList; //!
    StJets *muDstJets; //!
    StFourPMaker* mFourPMaker; //!

private:
    StppJetAnalyzer(); //Not implemented
    StppAnaPars mPars;
    ClassDef(StppJetAnalyzer,1)
};

//inlines ---


#endif




/* old
   virtual void setEmcAccepted(bool v) {mEmcAccepted=v;}
   virtual bool emcAccepted() const {return mEmcAccepted;}
   
   virtual void setTpcAccepted(bool v) {mTpcAccepted=v;}
   virtual bool tpcAccepted() const {return mTpcAccepted;}
   
   virtual void setFpdAccepted(bool v) {mFpdAccepted=v;}
   virtual bool fpdAccpted() const {return mFpdAccepted;}

   virtual void setSeedEtMin(double v);
   virtual void setAssocEtMin(double v);

   virtual void setNBinEta(Int_t nBins) { mFinder->setNBinEta(nBins); };
   virtual void setEtaMin(Double_t etaMin) { mFinder->setEtaMin(etaMin); };
   virtual void setEtaMax(Double_t etaMax) { mFinder->setEtaMax(etaMax); };
   virtual void setNBinPhi(Int_t nBins) { mFinder->setNBinPhi(nBins); };
   virtual void setPhiMin(Double_t phiMin) { mFinder->setPhiMin(phiMin); };
   virtual void setPhiMax(Double_t phiMax) { mFinder->setPhiMax(phiMax); };
   virtual void setPtMax(Double_t ptMax) { mFinder->setPtMax(ptMax); };
   virtual Int_t getNBinEta(void) { return mFinder->getNBinEta(); };
   virtual Double_t getEtaMin(void) { return mFinder->getEtaMin(); };
   virtual Double_t getEtaMax(void) { return mFinder->getEtaMax(); };
   virtual Int_t getNBinPhi(void) { return mFinder->getNBinPhi(); };
   virtual Double_t getPhiMin(void) { return mFinder->getPhiMin(); };
   virtual Double_t getPhiMax(void) { return mFinder->getPhiMax(); };
   virtual Double_t getPtMax(void) { return mFinder->getPtMax(); }; 


   protected:
   
   bool mEmcAccepted;
   bool mTpcAccepted;
   bool mFpdAccepted;
*/
