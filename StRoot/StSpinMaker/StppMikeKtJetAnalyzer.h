//StppMikeKtJetAnalyzer.h
//Adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

#ifndef StppMikeKtJetAnalyzer_HH
#define StppMikeKtJetAnalyzer_HH

#include "TObject.h"
#include "StJetFinder/StJetFinder.h"

class StKtCluJetFinder;
class StConeJetFinder;
class AbstractFourVec;
class StProtoJet;
class StppEvent;
class TH1;
class TFile;
class TNtuple;

class StppMikeKtJetAnalyzer : public StppJetAnalyzer
{
public:
    typedef StJetFinder::JetList JetList;
    typedef list<AbstractFourVec*> FourList;
    
    StppMikeKtJetAnalyzer();
    virtual ~StppMikeKtJetAnalyzer();

    void setClusterR(double v) { mFinder->setR(v); };

    ClassDef(StppMikeKtJetAnalyzer,1)
};

//inlines ---


#endif
