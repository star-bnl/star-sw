//StKonstJetFinder.h
//adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

//adapted from Akio Ogawa's work

#ifndef StKonstJetFinder_HH
#define StKonstJetFinder_HH

//std
#include <map>
#include <vector>
#include <cmath>
using std::map;
using std::vector;

//StJetFinder
#include "StJetFinderKonst.h"
#include "StJetFinder.h"
#include "Functors.h"
#include "StJetEtCell.h"

class StKonstJetFinder : public StJetFinder
{
public:
    StJetFinderKonst jetFinder;
    
    ///Run time pars
    struct StKonstJetFinderPars {
	Double_t coneRadius;
	Double_t etSeed;
	Double_t minJetEt;
	Double_t minCellEt;
        Int_t modeInfoCluster;
    };

    //cstr-dstr
    StKonstJetFinder(StKonstJetFinderPars& pars);
    virtual ~StKonstJetFinder();
    void clearAndDestroy(void);

    //simple access 
    StKonstJetFinderPars pars() const; //return by value to prevent changes to pars
    void setNBinEta(Int_t nBins) { jetFinder.setNBinEta(nBins); };
    void setEtaMin(Double_t etaMin) { jetFinder.setEtaMin(etaMin); };
    void setEtaMax(Double_t etaMax) { jetFinder.setEtaMax(etaMax); };
    void setNBinPhi(Int_t nBins) { jetFinder.setNBinPhi(nBins); };
    void setPhiMin(Double_t phiMin) { jetFinder.setPhiMin(phiMin); };
    void setPhiMax(Double_t phiMax) { jetFinder.setPhiMax(phiMax); };
    void setPtMax(Double_t ptMax) { jetFinder.setPtMax(ptMax); };
    Int_t getNBinEta(void) { return jetFinder.getNBinEta(); };
    Double_t getEtaMin(void) { return jetFinder.getEtaMin(); };
    Double_t getEtaMax(void) { return jetFinder.getEtaMax(); };
    Int_t getNBinPhi(void) { return jetFinder.getNBinPhi(); };
    Double_t getPhiMin(void) { return jetFinder.getPhiMin(); };
    Double_t getPhiMax(void) { return jetFinder.getPhiMax(); };
    Double_t getPtMax(void) { return jetFinder.getPtMax(); };

    void setMinJetEt(Double_t v) { mPars.minJetEt = v; };
    void setMinCellEt(Double_t v) { mPars.minCellEt = v; };

    //inherited interface
    virtual void findJets(JetList& protojets);
    virtual void clear();
    virtual void print();
    virtual Int_t kfindJets(Int_t numTracks, Float_t *pt, Float_t *eta,
      Float_t *phi, Float_t *mass,
      StProtoJet* jets, StProtoJet** tracks) = 0;
    virtual bool acceptSeed(const StJetEtCell *cell);
    
protected:
    Float_t *pt;
    Float_t *eta;
    Float_t *phi;
    Float_t *mass;
    StProtoJet **tracks;
    StProtoJet *jets;

    StKonstJetFinder(); ///Not implemented, must pass pars at construction time!

    StKonstJetFinderPars mPars; ///run-time pars

    double mAssocEtMin;
    double mSeedEtMin;

  ClassDef(StKonstJetFinder,1)
};

//inlines

inline StKonstJetFinder::StKonstJetFinderPars 
    StKonstJetFinder::pars() const
{
    return mPars;
}

#endif

