//StppKonstConeJetAnalyzer.h
//Adapted from M.L. Miller (Yale Software)
//07/02
// Thomas Henry
//08/02

#ifndef StppKonstConeJetAnalyzer_HH
#define StppKonstConeJetAnalyzer_HH

#include "TObject.h"
#include "StJetFinder/StJetFinder.h"
#include "StJetFinder/StKonstConeJetFinder.h"

class StKonstKtJetFinder;
class StKonstConeJetFinder;
class AbstractFourVec;
class StProtoJet;
class StppEvent;
class TH1;
class TFile;
class TNtuple;

class StppKonstConeJetAnalyzer : public StppJetAnalyzer
{
public:
    typedef StJetFinder::JetList JetList;
    typedef list<AbstractFourVec*> FourList;
    
    StppKonstConeJetAnalyzer();
    virtual ~StppKonstConeJetAnalyzer();

    void setConeRadius(Double_t v) { ((StKonstConeJetFinder*) mFinder)
        ->setConeRadius(v); };
    void setEtSeed(Double_t v) { ((StKonstConeJetFinder*) mFinder)
        ->setEtSeed(v); };
    void setMinJetEt(Double_t v) { ((StKonstConeJetFinder*) mFinder)
        ->setMinJetEt(v); };
    void setMinCellEt(Double_t v) { ((StKonstConeJetFinder*) mFinder)
        ->setMinCellEt(v); };

    ClassDef(StppKonstConeJetAnalyzer,1)
};

//inlines ---


#endif
