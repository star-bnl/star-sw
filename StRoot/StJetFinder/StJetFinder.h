//StJetFinder.h
//M.L. Miller (Yale Software)
//4/02

#ifndef StJetFinder_HH
#define StJetFinder_HH

#include <iostream>

#include <list>
using std::list;

#include "TFile.h"
#include "TNtuple.h"

#include "StProtoJet.h"

class StJetFinder
{
public:
    typedef list<StProtoJet> JetList;
    typedef StProtoJet::FourVecList FourVecList;
    StJetFinder();
    virtual ~StJetFinder();

    //access

    void setDebug(bool v);
    bool debug() const;

    void setR(double r) {mR=r;}
    double r() const {return mR;}
    virtual void setNBinEta(Int_t nBins) {};
    virtual void setEtaMin(Double_t etaMin) {};
    virtual void setEtaMax(Double_t etaMax) {};
    virtual void setNBinPhi(Int_t nBins) {};
    virtual void setPhiMin(Double_t phiMin) {};
    virtual void setPhiMax(Double_t phiMax) {};
    virtual void setPtMax(Double_t ptMax) {};
    virtual Int_t getNBinEta(void) { return 0; };
    virtual Double_t getEtaMin(void) { return 0.0; };
    virtual Double_t getEtaMax(void) { return 0.0; };
    virtual Int_t getNBinPhi(void) { return 0; };
    virtual Double_t getPhiMin(void) { return 0.0; };
    virtual Double_t getPhiMax(void) { return 0.0; };
    virtual Double_t getPtMax(void) { return 0.0; };

    //action

    /*! Pass a list of protojets.  This list will be packed with jets+beam jets after.
      The user is responsible for filtering the jets.
    */
    virtual void findJets(JetList& protojets) = 0;
    virtual void clear() = 0;
    virtual void print() = 0;

    virtual void setSeedEtMin(double v) {};
    virtual void setAssocEtMin(double v) {};

protected:
    JetList mJets;
    bool mDebug;
    double mR;
};

inline void StJetFinder::setDebug(bool v)
{
    mDebug = v;
}

inline bool StJetFinder::debug() const
{
    return mDebug;
}

#endif

