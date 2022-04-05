/***************************************************************************
 *
 * $Id: StTrgMaker.h,v 1.5 2003/09/10 19:47:41 perev Exp $
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description:  See the comment in the associated .cxx file.
 *
 ***************************************************************************
 *
 * $Log: StTrgMaker.h,v $
 * Revision 1.5  2003/09/10 19:47:41  perev
 * ansi corrs
 *
 * Revision 1.4  2001/12/25 20:01:29  ward
 * Outputs error (closeness to edge) of track extension subsector selection.
 *
 * Revision 1.3  2001/12/22 20:10:04  ward
 * New code for MWC.
 *
 * Revision 1.2  2001/07/27 17:40:18  ward
 * Handles reversed B field, also has code for chking triggerWord.
 *
 * Revision 1.1  2001/04/23 20:00:27  ward
 * Outputs info for CTB calib: slat ADCs and TPC track extensions.
 *
 *
 **************************************************************************/

//
// Every header file should have these macros to protect
// from being included multiple times in the same scope.
// If you change the name of the class change the name
// of the macro.
//
#ifndef StTrgMaker_hh     
#define StTrgMaker_hh

//
//  Include files. StMaker.h is needed since your maker
//  inherits from StMaker. 
//  <string> contains the STL string class. It's a system
//  header therefore it is enclosed in <> and not in double
//  quotes.
//
#include "StMaker.h"
#include <string>

//
//  Forward declerations.
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
class TFile;
class TNtuple;

//
//  On some systems (e.g. Sun) the STL is contained in
//  namespace 'std'. We have to tell the compiler where
//  to look for string. Since not all compilers
//  use namespaces we have to protects is by using the
//  ST_NO_NAMESPACES macro which is automatically set
//  (or unset) when compiling the code with 'cons'.
//
#ifndef ST_NO_NAMESPACES
using std::string;
#endif

//
//  The class declaration. Every maker has to
//  inherit from StMaker.
//
class StTrgMaker : public StMaker {
public:

    StTrgMaker(const Char_t *name="analysis");     // constructor
    ~StTrgMaker();                                 // destructor
    
    void Clear(Option_t *option="");    // called after every event to cleanup 
    Int_t  Init();                      // called once at the beginning of your job
    Int_t  Make();                      // invoked for every event
    Int_t  Finish();                    // called once at the end

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
    double mMagneticField;
    bool accept(StEvent*);            // this method serves as an event filter
    bool accept(StTrack*);            // and this is used to select tracks
    void DoOneTrackCtb(FILE *oo,long q,double curvature,double phi0,
         double psi,double r0,double tanl,double z0);
    void DoOneTrackMwc(FILE *oo,long q,double curvature,double phi0,
         double psi,double r0,double tanl,double z0);
    void CalcCenterOfCircleDefinedByTrack(int q,double radius,double psi,double r0,
      double phi0,double *xcenter,double *ycenter);
    void Location2Sector(double tanl,double xAtMwc,double yAtMwc,
      int *sector,int *subsector,double *errDistPhi,double *errDistRad);
    void FindIntersectionOfTwoCircles(
      double center1x,double center1y,double radius1,  /* input (circle 1) */
      double center2x,double center2y,double radius2,  /* input (circle 2) */
      int *numIntersectionPoints,                      /* output */
      double *intersection1x,double *intersection1y,   /* output */
      double *intersection2x,double *intersection2y    /* output */);
    int TrayNumber(double x,double y,double z);
    void FakeInfo(FILE*, int);

    //
    //  Data members
    //  Note, that it is recommended to start all member names with
    //  an 'm'. This makes it easier to read the code later.
    //
    int        mEventCounter;  //!
    string     mFileName;      //!
    TFile      *mFile;         //!
    TNtuple    *mTuple;        //!

    //
    //  This is needed to make your maker known to root4star.
    //  It must be always the last statement in the class.
    //  Note that this is a macro, that's why the ';' is missing.
    //
    ClassDef(StTrgMaker,0)
};
#endif
