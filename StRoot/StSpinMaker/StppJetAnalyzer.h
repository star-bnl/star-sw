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

class StKtCluJetFinder;
class StConeJetFinder;
//class AbstractFourVec;
//class StProtoJet;
class StMuTrackFourVec;
class StppEvent;
class TH1;
class TFile;
class TNtuple;

class StppJetAnalyzer
{
public:
    typedef StJetFinder::JetList JetList;
    typedef list<AbstractFourVec*> FourList;
    
    StppJetAnalyzer();
    virtual ~StppJetAnalyzer();

    ///simple gets/sets

    virtual void setDebug(bool v);
    virtual bool debug() const {return mDebug;}
    
    virtual void setR(double v);
    
    virtual void setSeedEtMin(double v);
    virtual void setAssocEtMin(double v);

    //gets/sets for run-time pars
    virtual void setEmcAccepted(bool v) {mEmcAccepted=v;}
    virtual bool emcAccepted() const {return mEmcAccepted;}

    virtual void setTpcAccepted(bool v) {mTpcAccepted=v;}
    virtual bool tpcAccepted() const {return mTpcAccepted;}

    virtual void setFpdAccepted(bool v) {mFpdAccepted=v;}
    virtual bool fpdAccpted() const {return mFpdAccepted;}

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

    //action
    virtual void setEvent(StppEvent* e);
    virtual void setFourVec(StMuTrackFourVec* tracks, int numTracks);
    virtual void print();
    virtual void findJets();
    virtual void fillHists();
    virtual void clear();
    virtual void openFile(const char* name);
    
    virtual void fillNtuple(StppEvent* e);


    //results from JetFinder
    JetList& getJets(void) {return mProtoJets;} //!
    StJets* getmuDstJets(void) { return muDstJets; }; 
    void addBranch(const char *name, void *stppudst); 

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
    bool accept(StMuTrack*);
    bool accept(StMuTrackFourVec*);
    bool accept(const StProtoJet& pj);
    void acceptJets(void);
    bool acceptJet(StProtoJet &pj);
    void fillLists();
    void fillLists(StMuTrackFourVec* tracks, int numTracks);
    void bookHists();
    
    //new
    void bookNtuple();
    
protected:
    TFile* mFile; //!
    StppEvent* mEvent; //!
    TNtuple* mNtuple; //!
    StJetFinder* mFinder; //!

    JetList mProtoJets; //!
    FourList mFourList; //!
    StJets *muDstJets; //!

    bool mDebug;
    
    //cuts that particles must pass to be used for jet-finding
    bool mEmcAccepted;
    bool mTpcAccepted;
    bool mFpdAccepted;
    double mPtMin;
    double mEtaMax;
    int mNhits;
    
    //Cut to accept found-jets
    double mJetPtMin;
    double mJetEtaMax;
    double mJetEtaMin;
    int mJetNmin;

private:
    //TH1* mPythiaEt;
    TH1* mEt;
    //TH1* mClusterEt;
    
    ClassDef(StppJetAnalyzer,1)
};

//inlines ---


#endif
