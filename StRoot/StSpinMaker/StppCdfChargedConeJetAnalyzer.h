//StppCdfChargedConeJetAnalyzer.h
//M.L. Miller (Yale Software)
//12/02

#ifndef StppCdfChargedConeJetAnalyzer_HH
#define StppCdfChargedConeJetAnalyzer_HH

#include "TObject.h"
#include "StJetFinder/StJetFinder.h"
#include "StSpinMaker/StppJetAnalyzer.h"

class StKtCluJetFinder;
class StConeJetFinder;
class AbstractFourVec;
class StProtoJet;
class StppEvent;
class TH1;
class TFile;
class TNtuple;

class StppCdfChargedConeJetAnalyzer : public StppJetAnalyzer
{
public:
    typedef StJetFinder::JetList JetList;
    typedef list<AbstractFourVec*> FourList;
    
    StppCdfChargedConeJetAnalyzer(int, double, double, int, double, double);
    virtual ~StppCdfChargedConeJetAnalyzer();

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

    ///Use r/::sqrt(2) for iteration search?
    void setDoMidpointFix(bool v);
    bool doMidpointFix() const;
    
    ///Require stable midpoints?
    void setRequireStableMidpoints(bool v);
    bool requiredStableMidpoints() const;

    ClassDef(StppCdfChargedConeJetAnalyzer,1)
};

#endif
