/***************************************************************************
 *
 * $Id: StEstMaker.h,v 1.6 2002/04/30 22:49:19 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstMaker
 *
 ***************************************************************************
 *
 * $Log: StEstMaker.h,v $
 * Revision 1.6  2002/04/30 22:49:19  caines
 * Make est work with shifted SVT geom, change search radii to 1cm
 *
 * Revision 1.5  2001/04/25 17:29:57  perev
 * HPcorrs
 *
 * Revision 1.4  2001/03/02 16:00:46  lmartin
 * Data members added to store the cumulated numbers of ideal,good and bad tracks.
 *
 * Revision 1.3  2001/01/25 17:54:17  lmartin
 * Divorced from the real tracking code.
 * Initialize the StEstParams and StEstSegments objects controling the tracking.
 * Instantiate a StEstTracker object and calls the initializing, running and saving
 * methods of this object
 *
 * Revision 1.2  2000/12/07 16:49:23  lmartin
 * Remove unused methods to compile under Sun
 *
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef STAR_StEstMaker
#define STAR_StEstMaker
#include "StMaker.h"

#include <stdlib.h>
class StEstParams;
class StEstSegments;
class St_egr_egrpar;
#include "table_header.h"

class StEstMaker : public StMaker {

 protected:
  
  int      mNPass;          //! number of passes
  int      mNSuperPass;     //! number of superpasses
  int      mIdealTracking;  //! perfect tracking and evaluation performed as well
  int      mDebugLevel;     //! Debugging Level controling the output messages
  int     mCumulNIdealPrim; //! Cumulated number of ideal primary tracks
  int     mCumulNIdealSeco; //! Cumulated number of ideal secondary tracks
  int     mCumulNGoodPrim; //! Cumulated number of good primary tracks
  int     mCumulNGoodSeco; //! Cumulated number of good secondary tracks
  int     mCumulNBadPrim; //! Cumulated number of bad primary tracks
  int     mCumulNBadSeco; //! Cumulated number of bad secondary tracks
  int     mCumulNEvents;  //! Cumulated number of events
  StEstParams**    mParams;//!
  StEstSegments**  mSegments;//!
  St_egr_egrpar*   m_egr_egrpar; //!
  table_head_st*   m_egrpar_h;//!

 public:
  StEstMaker(const char* name = "est");
  ~StEstMaker();
  void PrintSettings();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StEstMaker.h,v 1.6 2002/04/30 22:49:19 caines Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  Int_t Finish();

  ClassDef(StEstMaker, 1)
};


#endif




