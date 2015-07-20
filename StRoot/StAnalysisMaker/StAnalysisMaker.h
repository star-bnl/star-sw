//
//  This is a STAR typical comment header. You should modify
//  it to reflect your changes.
//  As a minimum it should contain the name of the author, the
//  date it was written/modified, and a short description of what
//  the class is meant to do. The cvs strings $X$ (where X=Id, Log)
//  are not needed when you do not intend to put the file under
//  cvs control. Remove them.
//  
/*!
 * \class  StAnalysisMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 *
 * This is an example of a maker to perform analysis using StEvent.
 * Use this as a template and customize it for your studies.
 *
 * $Id: StAnalysisMaker.h,v 2.15 2015/07/19 23:02:44 fisyak Exp $
 *
 */
/* -------------------------------------------------------------------------
 * $Log: StAnalysisMaker.h,v $
 * Revision 2.15  2015/07/19 23:02:44  fisyak
 * Add print out for Sst, Gmt, pp2pp
 *
 * Revision 2.14  2014/08/06 11:42:52  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 2.13  2012/12/18 17:16:26  fisyak
 * Add PrintVertex
 *
 * Revision 2.12  2012/10/23 19:44:18  fisyak
 * Add print out for ToF and Emc hits
 *
 * Revision 2.11  2012/09/16 21:59:14  fisyak
 * Compress print out, add PrintEmcHits
 *
 * Revision 2.10  2012/05/07 13:59:44  fisyak
 * enhance print out for primary vertixes
 *
 * Revision 2.9  2012/03/22 23:45:16  fisyak
 * Compress output for Event summary
 *
 * Revision 2.8  2010/09/01 14:33:57  fisyak
 * Clean ups
 *
 * Revision 2.7  2009/11/10 20:17:59  fisyak
 * Add print out for StEvent track and hits
 *
 * Revision 2.6  2009/11/03 15:13:22  fisyak
 * Comment print out, wait till StEvent will be mofidied
 *
 * Revision 2.5  2009/11/03 15:03:56  fisyak
 * Add static method to print StEvent
 *
 * Revision 2.4  2003/09/10 19:47:02  perev
 * ansi corrs
 *
 * Revision 2.3  2002/04/28 00:10:28  jeromel
 * doxygen basic dox added. GetCVS() had wrong signature : corrected to avoid
 * propagation of this typo in new makers.
 *
 * Revision 2.2  2000/07/12 05:23:28  ullrich
 * Updated for better use as template for actual analysis.
 *
 * Revision 2.1  1999/12/30 01:54:57  ogilvie
 * added countPrimaryPions as example how to use PID
 *
 * Revision 2.0  1999/11/04 16:10:03  ullrich
 * Revision for new StEvent
 *
 * -------------------------------------------------------------------------
 */

//
// Every header file should have these macros to protect
// from being included multiple times in the same scope.
// If you change the name of the class change the name
// of the macro.
//
#ifndef StAnalysisMaker_hh     
#define StAnalysisMaker_hh

//
//  Include files. StMaker.h is needed since your maker
//  inherits from StMaker. 
#include "StMaker.h"
#include "TString.h"

//
//  Forward declarations.
//  It is always a good idea to reduce the dependencies
//  to other header files. This can be achieved by
//  forward declaring classes which are only referenced
//  but not contained (by value) in the class decleration.
//  In the implementation then one onviously has to include
//  the referring header. Another advantage of this
//  technique is that the these classes do not get passed
//  through rootcint.
//
class StEvent;
class StTrack;

//
//  The class declaration. Every maker has to
//  inherit from StMaker.
//
class StAnalysisMaker : public StMaker {
public:
  
  StAnalysisMaker(const Char_t *name="analysis");     // constructor
  ~StAnalysisMaker() {}                               // destructor
  
  Int_t  Make();                      // invoked for every event
  Int_t  Finish();                    // called once at the end
  static void summarizeEvent(StEvent *event=0, Int_t mEventCounter=0);
  static void PrintStEvent(TString opt="vpgl3");
  static void PrintVertex(Int_t ivx = -1);
  static void PrintGlobalTrack(Int_t itk = 0);
  static void PrintTpcHits(Int_t sector = 0, Int_t row = 0, Int_t plot = 0, Int_t IdTruth=-1);
  static void PrintToFHits();
  static void PrintSvtHits();
  static void PrintSsdHits();
  static void PrintSstHits();
  static void PrintRnDHits();
  static void PrintEmcHits(Int_t det=-1, Int_t mod=-1, const Option_t *opt = "AdcClustersPoints");
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StAnalysisMaker.h,v 2.15 2015/07/19 23:02:44 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
 private:
  //
  //  Add your data member and new methods here.
  //  The "//!" means that rootcint is not adding
  //  the data member to the streamer. Don't worry
  //  if you don't know what this means.
  //  In general it is a good idea in analysis makers
  //  to always add the //! after a member.
  //
  
  //
  //  Methods (== member functions)
  //  Remember: these are just examples!
  //
  Bool_t accept(StEvent*);            // this method serves as an event filter
  Bool_t accept(StTrack*);            // and this is used to select tracks
  
  //
  //  Data members
  //  Note, that it is recommended to start all member names with
  //  an 'm'. This makes it easier to read the code later.
  //
  Int_t        mEventCounter;  //!
  
  //
  //  This is needed to make your maker known to root4star.
  //  It must be always the last statement in the class.
  //  Note that this is a macro, that's why the ';' is missing.
  //
  ClassDef(StAnalysisMaker,0)
};
#endif
