/***************************************************************************
 *
 * $Id: StJetMaker.h,v 1.5 2003/06/25 23:04:35 thenry Exp $
 * $Log: StJetMaker.h,v $
 * Revision 1.5  2003/06/25 23:04:35  thenry
 * Added proxy make option
 *
 * Revision 1.4  2003/05/09 20:48:19  thenry
 * removed "../" from #include lines
 *
 * Revision 1.3  2003/04/24 14:15:16  thenry
 * These changes are really the first working version of the StFourPMakers
 * and teh StJetMakers.  This is all c++ stl implementation, and by virtue of
 * that fact, StEmcTpcFourPMaker bady needs to be speed optimized.
 *
 * Revision 1.2  2003/04/04 21:26:42  thenry
 * Repeated jet bug fix + debugging output not removed
 *
 * Revision 1.1  2003/02/27 21:38:10  thenry
 * Created by Thomas Henry
 *
 * Revision 1.0  2003/02/20 thenry
 * StJetMaker was created to allow multiple jet analysis modules to be
 * run simultaneosly with various parameters while the Maker loads the events
 * and analyses them.  Four different jet analyzers exist:
 *
 * Konstanin's Analyzers:
 *     Kt type: StppKonstKtJetAnalyzer
 *     Cone type: StppKonstConeJetAnalyzer
 *
 * Mike's Analyzers:
 *     Kt type: StppMikeKtJetAnalyzer
 *     Cone type: StppMikeConeJetAnalyzer
 *
 * These modules all require the StJetFinder modules.
 *
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  TTree Jet nano-Dst
 *
 ***************************************************************************/
#ifndef StJetMaker_h
#define StJetMaker_h
#include "StMaker.h"
#include "StSpinMaker/StppJetAnalyzer.h"
#include <string>
#include <iostream>
#include <map>
#include <algorithm>
using namespace std;


//#define _GEANT_
#define _BBC_data_
#define _FPD_data_
#define MAXANALYZERS 12
#define DMAXEVENTSPERFILE 1000

class TFile;
class TTree;
class StppEvent;
class StppGeant;
class StBbcTriggerDetector;
class StFpdCollection;
class StMuDst;
class StMuEmcCollection;
class StMuDstMaker;
class StFourPMaker;

class StJetMaker : public StMaker {
public:
    typedef map<string, StppJetAnalyzer*, less<string> > jetBranchesMap;

    StJetMaker(const Char_t *name, StFourPMaker* fPMaker, 
        StMuDstMaker* uDstMaker, const char *outputName);
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    void setMuDst(StMuDst* p) {mudst=p;};
    StppEvent* event() {return jetEvent;} //added by MLM
    TTree* tree() {return jetTree;}  //added by MLM
    void SetMaxEventsPerFile(int newMax) { maxEventsPerFile = newMax; };
    void SetStoreEMC(bool doStore) { saveEMC = doStore; };     

    void SetSaveEventWithNoJets(bool saveIt);
    void addAnalyzer(StppJetAnalyzer* a, const char *);
    jetBranchesMap &getJets(void) { return jetBranches; };
    StppJetAnalyzer* firstanalyzer(void) { 
      return (*(jetBranches.begin())).second;
    };
    int firstNumJets(void) 
      { return firstanalyzer()->getmuDstJets()->nJets(); };
    void SetMaker(StMaker *_maker) { maker = _maker; };

protected:
    void InitFile(void);
    void FinishFile(void);

    StMuEmcCollection* muEmcCol; //!
    jetBranchesMap jetBranches;  
    bool saveEventWithNoJets;
    int maxEventsPerFile;	   //!      
    int eventFileCounter;	   //!
    int fileCounter;		   //!
    bool saveEMC;		   //!
    bool neverSave;                //!

protected:
    StFourPMaker*   fourPMaker;   //!
    StMuDstMaker*   muDstMaker;   //!
    StMaker*        maker;        //!

private:
    const char*     outName;      //!
    StMuDst*        mudst;        //!
    size_t          mGoodCounter; //!
    size_t          mBadCounter;  //!
    TFile           *m_outfile;   //!
    TTree           *jetTree;      //!
    StppEvent       *jetEvent;     //!
#ifdef _GEANT_
    StppGeant       *ppGeant;     //!
#endif
#ifdef _BBC_data_
    StBbcTriggerDetector *bbc;    //!
#endif
#ifdef _FPD_data_
    StFpdCollection *fpd;         //!
#endif
    Int_t            infoLevel;
  
    ClassDef(StJetMaker,1)
	};
#endif















