//StppMikeConeJetAnalyzer.h
//Adapted from M.L. Miller (Yale Software)
//07/02
// Thomas Henry
//08/02

#ifndef StppMikeConeJetAnalyzer_HH
#define StppMikeConeJetAnalyzer_HH

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

class StppMikeConeJetAnalyzer : public StppJetAnalyzer
{
public:
    typedef StJetFinder::JetList JetList;
    typedef list<AbstractFourVec*> FourList;
    
    StppMikeConeJetAnalyzer(int, double, double, int, double, double);
    virtual ~StppMikeConeJetAnalyzer();

    void setConeR(double v) { mFinder->setR(v); };
    void setConeSeedEtMin(double v) { mFinder->setSeedEtMin(v); };
    void setConeAssocEtMin(double v) { mFinder->setAssocEtMin(v); };

    ///Let jet wander to minimum?
    void setPerformMinimization(bool v);
    bool performMinimization();
        
    ///Add seeds at midpoints?
    void setAddMidpoints(bool v);
    bool addMidpoints();
        
    ///Do Split/Merge step?
    void setDoSplitMerge(bool v);
    bool doSplitMerge();

    void setSplitFraction(double v);
    double splitFraction(void);

    ClassDef(StppMikeConeJetAnalyzer,1)
};

//inlines ---


#endif
