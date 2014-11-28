/*!
 * \class  StMuAnalysisMaker
 * \brief  A typical Analysis Class for MuDst 
 * \author Wei-Ming Zhang, KSU, Mar 2004
 *
 * This is an example of a maker to perform analysis using MuDst.
 *
 * $Id: StMuAnalysisMaker.h,v 1.2 2014/08/06 11:43:31 jeromel Exp $
 *
 * -------------------------------------------------------------------------
 * $Log: StMuAnalysisMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:31  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2004/08/10 16:09:11  perev
 * new GridCollector stuff
 *
 * -------------------------------------------------------------------------
 */
#ifndef StMuAnalysisMaker_hh     
#define StMuAnalysisMaker_hh
//
//  Include files
#include "StMaker.h"
#include <string>
//
//  Forward declarations
class StMuTrack;
class TFile;
class TH1D;

#ifndef ST_NO_NAMESPACES
using std::string;
#endif
//
//  The class declaration. It innherits from StMaker.
class StMuAnalysisMaker : public StMaker {
public:

    StMuAnalysisMaker(const Char_t *name="muAnalysis");   // constructor
    ~StMuAnalysisMaker();                                 // destructor
    
    void Clear(Option_t *option=""); // called after every event to cleanup 
    Int_t  Init();                   // called once at the beginning of your job
    Int_t  Make();                   // invoked for every event
    Int_t  Finish();                 // called once at the end

    virtual const char *GetCVS() const {
      static const char cvs[]="Tag $Name:  $ $Id: StMuAnalysisMaker.h,v 1.2 2014/08/06 11:43:31 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
      return cvs;
    }

private:

// data member  
    int        mEventCounter;  //!
    string     mFileName;      //!
    TFile      *mFile;         //!

    TH1D *mGlobalPt;           //!
    TH1D *mPrimaryPt;          //!
    TH1D *mL3Pt;               //!
    TH1D *mRefMult;            //!

// method (a simple track filter)
    bool accept(StMuTrack*);            // and this is used to select tracks

    ClassDef(StMuAnalysisMaker,0)
};
#endif
