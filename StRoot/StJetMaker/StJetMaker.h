/***************************************************************************
 *
 * $Id: StJetMaker.h,v 1.9 2008/03/25 00:32:41 tai Exp $
 * $Log: StJetMaker.h,v $
 * Revision 1.9  2008/03/25 00:32:41  tai
 * deleted InitFile() for not being used.
 *
 * Revision 1.8  2008/03/25 00:00:28  tai
 * moved "using namespace std;" from the .h file to the .cxx file
 *
 * Revision 1.7  2008/03/24 23:54:51  tai
 * deleted some #include lines for they were not used.
 *
 * Revision 1.6  2008/03/24 23:01:11  tai
 * deleted mOfstream for it was not used.
 *
 * Revision 1.5  2006/12/15 23:25:32  mmiller
 * First release of StJetSkimEvent and StJetSkimEventMaker to allow for
 * a MuDst'less analysis once the jet finding pass is completed.
 * See updated version of StJetMaker/macros/RunJetFinder2.C and
 * RunFastJetReader.C for the creation and use of StJetSkimEvent.
 *
 * Revision 1.4  2004/10/12 18:17:37  mmiller
 * Organized into subdirectories, add StPythia, StJetSimuUtil, and StEmcHitMakers
 *
 * Revision 1.3  2004/09/22 15:46:20  mmiller
 * Added a double check to verify that jet 4p is equal to the vector sum of
 * the particles 4-p.  Removed troublesome access methods to StJets.  See
 * StJetReader::exampleEventAna() for access to jet-particles.
 *
 * Revision 1.2  2004/09/14 17:27:15  mmiller
 * Fixed bug (lack of StFourPMaker::Clear()).
 *
 * Revision 1.1  2004/07/08 15:41:03  mmiller
 * First release of StJetMaker.  Mike's commit of full clean of StJetFinder, StJetMaker, and StSpinMaker.  builds and runs in dev.
 *
 * Revision 1.9  2003/09/24 20:54:07  thenry
 * Fixed ANSI compatibility problems.
 *
 * Revision 1.8  2003/09/10 19:47:20  perev
 * ansi corrs
 *
 * Revision 1.7  2003/09/07 03:49:03  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.6  2003/09/02 17:58:39  perev
 * gcc 3.2 updates + WarnOff
 *
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
#include "StppJetAnalyzer.h"

#include <string>
#include <map>

class TFile;
class TTree;
class StMuDst;
class StMuDstMaker;
class StFourPMaker;
class StJetPars;
class StppAnaPars;

/*!
  \class StJetMaker
  \author T.Henry (Texas A&M)
  StJetMaker is used to (i) run the jet finding algorithms and (ii) store the results in a TTree.
  Multiple algorithms can be run simultaneously, via the use of the addAnalyzer() method.
  The TTree is a collection of branches, each branch containing an instance of the StJets class.
  The number of branches is dynamic and equal to the number of calls to addAnalyzer().  That is,
  one branch per jet analysis.  The TTree can be analyzed later using the StJetReader class.
 */

class StJetMaker : public StMaker {
public:

    ///A useful typedef for the map of StJets objects.
    typedef std::map<std::string, StppJetAnalyzer*, std::less<std::string> > jetBranchesMap;
    
    ///The constructor requires a valid instance of both a StFourPMaker and a StMuDstMaker
    //!StJetMaker(const Char_t *name, StFourPMaker* fPMaker, StMuDstMaker* uDstMaker, const char *outputFile);
    StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    
    ///Access to the Tree of StJets branches
    TTree* tree();
    
    ///Construct a new jet analysis.
    void addAnalyzer(const StppAnaPars*, const StJetPars*, StFourPMaker*, const char* anaName);
    
    ///Access to StJets objects, stored in a std::map keyed by the StJets name
    jetBranchesMap& getJets();
    
protected:
  //    void InitFile(void);
    void FinishFile(void);

    jetBranchesMap jetBranches;  

protected:
    //!StFourPMaker*   fourPMaker;   //!
    StMuDstMaker*   muDstMaker;   //!

private:
    const char*     outName;      //!
    StMuDst*        mudst;        //!
    size_t          mGoodCounter; //!
    size_t          mBadCounter;  //!
    TFile           *m_outfile;   //!
    TTree           *jetTree;      //!
    Int_t            infoLevel;
    int mEventCounter;

    ClassDef(StJetMaker,0)
	};

//inlines

inline TTree* StJetMaker::tree()
{
    return jetTree;
}


inline StJetMaker::jetBranchesMap& StJetMaker::getJets()
{
    return jetBranches;
}

#endif
