//StppKonstKtJetAnalyzer.h
//Adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

#ifndef StppKonstKtJetAnalyzer_H
#define StppKonstKtJetAnalyzer_H

#include "TObject.h"
#include "StJetFinder/StJetFinder.h"
#include "StJetFinder/StKonstKtJetFinder.h"

class StKonstKtJetFinder;
class StKonstConeJetFinder;
class AbstractFourVec;
class StProtoJet;
class StppEvent;
class TH1;
class TFile;
class TNtuple;

class StppKonstKtJetAnalyzer : public StppJetAnalyzer
{
public:
    typedef StJetFinder::JetList JetList;
    typedef list<AbstractFourVec*> FourList;
    
    StppKonstKtJetAnalyzer();
    virtual ~StppKonstKtJetAnalyzer();

    void setRadius(Double_t v) { ((StKonstKtJetFinder*) mFinder)
        ->setRadius(v); };
    Double_t getRadius(void) { return ((StKonstKtJetFinder*) mFinder)
        ->getRadius(); };
    void setMinJetEt(Double_t v) { ((StKonstKtJetFinder*) mFinder)
        ->setMinJetEt(v); };
    void setMinCellEt(Double_t v) { ((StKonstKtJetFinder*) mFinder)
        ->setMinCellEt(v); };

    ClassDef(StppKonstKtJetAnalyzer,1)
};

//inlines ---


#endif
