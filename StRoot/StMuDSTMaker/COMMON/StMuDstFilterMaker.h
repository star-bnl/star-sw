/***************************************************************************
 *
 * $Id: StMuDstFilterMaker.h,v 1.11 2014/08/06 11:43:31 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StMuDstFilterMaker_hh
#define StMuDstFilterMaker_hh

#include "StMaker.h"
#include "StChain.h"

#include "StMuDstMaker.h"
#include "StMuDst.h"
#include "StMuEvent.h"
#include "StMuTrack.h"
#ifndef __NO_STRANGE_MUDST__
class StStrangeEvMuDst;
class StV0MuDst;
class StXiMuDst;
#endif
#include <string>

class StMuEmcCollection;

class TTree;
class TFile;

/**
   @class StMuDstFilterMaker
   A small utility maker to create filtered muDst data files
*/
class StMuDstFilterMaker : public StMaker {
 public:
    /// Default constructor; get pointer to StMuDstMaker
    StMuDstFilterMaker(const char* name="muDstFilter");
    ~StMuDstFilterMaker();
    
    int Make();   ///< Filters the muDst and writes the filtered version
    int Finish(); ///< Writes and closes the output file
    virtual const char *GetCVS() const {
	static const char cvs[]="Tag $Name:  $ $Id: StMuDstFilterMaker.h,v 1.11 2014/08/06 11:43:31 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
	return cvs;
    }
  
    void setOutputDirName(const char* name) { mOutDirName = string(name); }
    void setOutputFileName(const char* name) { mOutFileName = string(name); }
    void setMuDstMaker( StMuDstMaker* maker) { mMuDstMaker = maker; }
    void setFilterGlobals(int filterGlobals = 1) { mFilterGlobals = filterGlobals; }
    void setDoBemc(int doBemc=1)                { mDoBemc = doBemc;}
    void setDoEemc(int doEemc=1)                { mDoEemc = doEemc;}

 protected:
    /// specialize this function to apply filters to the individual branches
    template<class T> bool filter(T* t) { return false;}
    /// If this function returns false, the whole event is discarded
    bool filter(StMuDst* mu) {return mu->event() && fabs(mu->event()->primaryVertexPosition().z())<100 ;}
    /// Now I specialize the filter function to select individual object
    bool filter(StMuEvent* ev) {return true; }             // keep all event-wise information
    bool filter(StMuTrack* track);
    bool filter(StMuEmcCollection* emc) { return true; }  // and I want to keep the emc collection   


    StMuDstMaker* mMuDstMaker; // pointer to the StMuDstMaker that is providing the input

    // output file
    TFile* mFile;
    string mOutDirName;
    string mOutFileName;
    string mCurFileName;
    TChain* mChain;
    TTree* mTTree;
    Int_t mFilterGlobals;  ///< If set, keep also globals that fulfill cuts, while primary does not
    Int_t mDoBemc;        ///< Copy barrel data (if it passes cuts)
    Int_t mDoEemc;        ///< Copy endcap data (if it passes cuts)
    void createArrays();
    void clearArrays();
    void clear();
    void close();
    void open(const Char_t *);
    template <class T>
    int addType(TClonesArray* tcaTo , T t);
    template <class T>
    int addType(TClonesArray *tca, TClonesArray* tcaTo , T *t);
    
    /// the list of TClonesArrays to copy
    TClonesArray* mArrays[__NARRAYS__];//->
#ifndef __NO_STRANGE_MUDST__
    TClonesArray* mStrangeArrays[__NSTRANGEARRAYS__];//->
#endif
    TClonesArray* mEmcArrays[__NEMCARRAYS__];//->
    
    ClassDef(StMuDstFilterMaker, 1)
}; 


#endif

/***************************************************************************
 *
 * $Log: StMuDstFilterMaker.h,v $
 * Revision 1.11  2014/08/06 11:43:31  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.10  2011/04/08 01:25:50  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.9  2005/05/18 22:47:29  mvl
 * Fixed StMuDstFilterMaker to work again with changes in MuDstMaker
 * (the change in v1.6 was faulty. Thanks Alex for finding this)
 * Added some new features suggested by Alex Suiade:
 * - Emc data now supported (for SL04k and later MuDst).
 *   Flags added to switch Eemc and Bemc copying seperately (setDoBemc and setDoEemc)
 * - Global tracks are checked seperately. They were only copied
 *   if the corresponding primary fullfills the filter() criteria.
 *   Now they are also copied if only the global track fullfills the criteria
 *   Can be switched with setFilterGlobals()
 *
 * Revision 1.8  2004/07/27 04:31:37  mvl
 * Changed includes to class StV0MuDst etc to reduce interdependence of makers
 *
 * Revision 1.7  2004/07/27 02:37:40  mvl
 * Added includes for some Strange Mudst classes to rpovide base class for Strange Mudst filter
 *
 * Revision 1.6  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.5  2004/04/29 03:35:57  perev
 * protected ==> protected
 *
 * Revision 1.4  2003/11/24 23:36:36  laue
 * commented the StMuEmcCollection out
 *
 * Revision 1.3  2003/09/07 03:49:03  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.2  2003/08/04 14:38:10  laue
 * Alex Suaide's updated for the EMC. Now EEMC is included.
 *
 * Revision 1.1  2003/04/15 18:48:36  laue
 * Minor changes to be able to filter MuDst.root files and an example
 * how to do this. The StMuDstFilterMaker is just an example, it has to be
 * customized (spoilers, chrome weels, etc.) by the user.
 *
 *
 **************************************************************************/
