/*!
 * \class StGenericVertexFinder
 *
 * \author Lee Barnby, April 2003
 *
 * (pseudo) Base class for vertex finders
 *
 *
 * We have the option of requiring that at least one track matches
 * the CTB during the first two scans for the vertex (these are coarse scans
 * to locate the probable z vertex
 *
 * myvertex.CTBforSeed();
 * myvertex.NoCTBforSeed();
 *
 * During the final fit (once the z position of the vertex has been constrained)
 * there is no CTB requirement.  To get the number of tracks with CTB match:
 *
 *  myvertex.NCtbMatches()
 *
 * Because NCtbMatches() may be handled multiple ways, the implementation
 * is enforced.
 *
 *
 * $Id: StGenericVertexFinder.h,v 1.8 2004/09/03 00:09:08 jeromel Exp $
 */

#ifndef STAR_StGenericVertexFinder
#define STAR_StGenericVertexFinder

#include "StEventTypes.h"


class StEvent;
class StMaker;

class StGenericVertexFinder {
 public:
  // virtual and '=0' ; those MUST be implemented
  virtual ~StGenericVertexFinder(){};                         // virtual destructor
  virtual bool           fit(StEvent*)=0;                     // fit the vertex
  virtual int            NCtbMatches()=0;                     // returns the number of tracks matched to CTB 
                                                              // and used by the vertex finder
  virtual int            NCtbSlats()=0;                       // returns the number of CTB slats above threshold
  virtual void           UseVertexConstraint(double, double, double, double, double)=0;
  virtual void           printInfo(ostream& = cout) const=0;


  // General (default)
  virtual void           SetMode(Int_t mode=0 ) {mMode = 0;}
  virtual void           Init(){ /* noop */;}

  virtual StThreeVectorD result() const {return mFitResult;}  // result of fit
  virtual StThreeVectorD error()  const {return  mFitError;}  // error on fit result
  virtual int            status() const {return mStatus;}     // status flag

  void                   FillStEvent(StEvent*) const;
  void                   CTBforSeed(){   mRequireCTB = true;}
  void                   NoCTBforSeed(){ mRequireCTB = false;}

  void                   DoUseITTF(){    mUseITTF=kTRUE; }
  void                   DoNotUseITTF(){ mUseITTF=kFALSE;}
  void                   setFlagBase();


  void                   setExternalSeed(const StThreeVectorD&);
  void                   NoVertexConstraint();
  void                   SetFitPointsCut(int fitpoints) {mMinNumberOfFitPointsOnTrack = fitpoints;}

  
 protected: //................................

  StGenericVertexFinder();
  StMaker *mDumMaker;

  bool                   mUseITTF;          // Use only tracks with ITTF encoded method
  UInt_t                 mFlagBase;         // ITTF track flag

  StPhysicalHelixD*      mBeamHelix;        // Beam Line helix
  bool                   mVertexConstrain;  // Use vertex constraint from db

  double                 mWeight ;          // Weight in fit for vertex contraint
  bool                   mRequireCTB;       // Set maker to use CTB

  StThreeVectorD         mExternalSeed;
  bool                   mExternalSeedPresent;


  StThreeVectorD         mFitResult;        // fit result
  StThreeVectorD         mFitError;         // fit errors
  int                    mStatus;           // status flag 
  int                    mMode;             // as a maker mode, used for any Finder behavior change

  unsigned int           mMinNumberOfFitPointsOnTrack;


};



// $Log: StGenericVertexFinder.h,v $
// Revision 1.8  2004/09/03 00:09:08  jeromel
// Modified code to Implement Init() and SetMode() and allow passing a switch
// to chose the vertex finder from within the same code implementation. Was
// needed for ppLMV (one implementation, two algorithm)
//
// Revision 1.7  2004/08/04 21:57:56  balewski
// toward smarter ppLMV5
//
// Revision 1.6  2004/07/24 19:40:38  balewski
// fix swap of vert & errVert
//
// Revision 1.5  2004/07/24 02:57:40  balewski
// clean up of ppLMV, CTB-util separated
//
// Revision 1.4  2004/07/23 02:24:38  jeromel
// Oops ... Worng swithc (had twice Minuit). Now corrected.
//
// Revision 1.3  2004/07/23 00:58:19  jeromel
// Base class method+data member (was duplicated in implementation)
//
// Revision 1.2  2004/04/06 02:43:43  lbarnby
// Fixed identification of bad seeds (no z~0 problem now). Better flagging. Message manager used.
//
// Revision 1.1  2003/05/09 22:22:36  lbarnby
// Initial revision: a base class for STAR (StEvent-based) vertex finders
//
#endif
