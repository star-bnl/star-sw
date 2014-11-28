/***************************************************************************
 *
 * $Id: NonId3DCorrFctn.h,v 1.2 2002/12/12 17:02:49 kisiel Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function           
 *
 ***************************************************************************
 *
 * $Log: NonId3DCorrFctn.h,v $
 * Revision 1.2  2002/12/12 17:02:49  kisiel
 * Use KStar instead of 2*KStar for non-identical particles
 *
 * Revision 1.1  2001/04/03 21:02:51  kisiel
 *
 *
 *   The correlation function for non-identical particle
 *   correlations. Uses selection on pair kinematics
 *   to perform a "3D-like" analysis.
 *
 * Revision 1.2  2000/01/25 17:34:45  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.1  1999/09/23 23:28:02  lisa
 * add helensV0Cut  AND  rename mikes and franks ParticleCuts to TrackCuts  AND  update documentation
 *
 * Revision 1.2  1999/07/06 22:33:20  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef NonId3DCorrFctn_hh
#define NonId3DCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class NonId3DCorrFctn : public StHbtCorrFctn {
public:
  NonId3DCorrFctn(char* title, const int& nbins, const float& QinvLo, 
		     const float& QinvHi);
  NonId3DCorrFctn(char* title, const int& nbins, const float& QinvLo, 
		     const float& QinvHi, const int mqSideSel);
  NonId3DCorrFctn(char* title, const int& nbins, const float& QinvLo, 
		     const float& QinvHi, const float KCompCut);
  virtual ~NonId3DCorrFctn();

  virtual void makeHistos(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();
  virtual void Write();


private:
  StHbt1DHisto* mNumOutP;
  StHbt1DHisto* mDenOutP;  
  StHbt1DHisto* mRatOutP;  
  StHbt1DHisto* mNumOutN;
  StHbt1DHisto* mDenOutN;  
  StHbt1DHisto* mRatOutN;
  StHbt1DHisto* mRatOut; 
  StHbt1DHisto* mRatOutNOverP;

  StHbt1DHisto* mNumSideP;
  StHbt1DHisto* mDenSideP;  
  StHbt1DHisto* mRatSideP;  
  StHbt1DHisto* mNumSideN;
  StHbt1DHisto* mDenSideN;  
  StHbt1DHisto* mRatSideN;
  StHbt1DHisto* mRatSide; 
  StHbt1DHisto* mRatSideNOverP;

  StHbt1DHisto* mNumLongP;
  StHbt1DHisto* mDenLongP;  
  StHbt1DHisto* mRatLongP;  
  StHbt1DHisto* mNumLongN;
  StHbt1DHisto* mDenLongN;  
  StHbt1DHisto* mRatLongN;
  StHbt1DHisto* mRatLong; 
  StHbt1DHisto* mRatLongNOverP;

  StHbt2DHisto* mHOutKSame;
  StHbt2DHisto* mHOutKDiff;
  StHbt2DHisto* mHSideKSame;
  StHbt2DHisto* mHSideKDiff;
  StHbt2DHisto* mHLongKSame;
  StHbt2DHisto* mHLongKDiff;

/*   StHbt2DHisto* mHQSideExitNum; */
/*   StHbt2DHisto* mHKStarExitNumSideP; */
/*   StHbt2DHisto* mHKStarExitNumSideN; */

/*   StHbt2DHisto* mHQSideExitDen; */
/*   StHbt2DHisto* mHKStarExitDenSideP; */
/*   StHbt2DHisto* mHKStarExitDenSideN; */

/*   StHbt2DHisto* mHQOutExitNum; */
/*   StHbt2DHisto* mHKStarExitNumOutP; */
/*   StHbt2DHisto* mHKStarExitNumOutN; */

/*   StHbt2DHisto* mHQOutExitDen; */
/*   StHbt2DHisto* mHKStarExitDenOutP; */
/*   StHbt2DHisto* mHKStarExitDenOutN; */

//  StHbt2DHisto* mHPt1KStarOutNum;
//  StHbt2DHisto* mHPt1KStarOutDen;
//  StHbt2DHisto* mHPt2KStarOutNum;
//  StHbt2DHisto* mHPt2KStarOutDen;

  int mqSideSel;
  float mKCompCut;
  

#ifdef __ROOT__ 
  ClassDef(NonId3DCorrFctn, 1)
#endif
};


#endif

