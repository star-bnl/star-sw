/***************************************************************************
 *
 * $Id: NonIdPurityCorrFctn.h,v 1.1 2002/12/12 17:02:49 kisiel Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function           
 *
 ***************************************************************************
 *
 * $Log: NonIdPurityCorrFctn.h,v $
 * Revision 1.1  2002/12/12 17:02:49  kisiel
 * Use KStar instead of 2*KStar for non-identical particles
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

#ifndef NonIdPurityCorrFctn_hh
#define NonIdPurityCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include <TProfile.h>

class NonIdPurityCorrFctn : public StHbtCorrFctn {
public:
  NonIdPurityCorrFctn(char* title, const int& nbins, const float& QinvLo, 
		      const float& QinvHi, int p1Type, int p2Type);
  virtual ~NonIdPurityCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();
  virtual void Write();

  StHbt1DHisto* NumP();
  StHbt1DHisto* DenP();
  StHbt1DHisto* RatP();
  StHbt1DHisto* NumN();
  StHbt1DHisto* DenN();
  StHbt1DHisto* RatN();
  StHbt1DHisto* Rat();
  TProfile* PairPurityOut();
  TProfile* PairPuritySide();
  TProfile* PairPurityLong();

private:
  StHbt1DHisto* mNumP;
  StHbt1DHisto* mDenP;  
  StHbt1DHisto* mRatP;  
  StHbt1DHisto* mNumN;
  StHbt1DHisto* mDenN;  
  StHbt1DHisto* mRatN;
  StHbt1DHisto* mRat; 
  StHbt1DHisto* mRatNOverP;

/*   StHbt1DHisto* mNumPinvP; */
/*   StHbt1DHisto* mDenPinvP;   */
/*   StHbt1DHisto* mNumPinvN; */
/*   StHbt1DHisto* mDenPinvN;   */
/*   StHbt1DHisto* mRatPinv;  */

/*   StHbt1DHisto* mRatPinvNormP; */
/*   StHbt1DHisto* mRatPinvNormN; */
/*   StHbt1DHisto* mRatPinvNorm; */
/*   StHbt1DHisto* mRatPinvNormNOverP; */

/*   StHbt2DHisto* mQinvPt1; */
/*   StHbt2DHisto* mQinvY1; */

/*   StHbt2DHisto* mHKCVKSame; */
/*   StHbt2DHisto* mHKCVKDiff; */

  int mp1Type, mp2Type;
  TProfile* mPairPurityOut;
  TProfile* mPairPuritySide;
  TProfile* mPairPurityLong;
  
#ifdef __ROOT__ 
  ClassDef(NonIdPurityCorrFctn, 1)
#endif
};

inline StHbt1DHisto* NonIdPurityCorrFctn::NumP()
{return  mNumP;}
inline StHbt1DHisto* NonIdPurityCorrFctn::DenP()
{return  mDenP;}
inline StHbt1DHisto* NonIdPurityCorrFctn::RatP()
{return  mRatP;}
inline StHbt1DHisto* NonIdPurityCorrFctn::NumN()
{return  mNumN;}
inline StHbt1DHisto* NonIdPurityCorrFctn::DenN()
{return  mDenN;}
inline StHbt1DHisto* NonIdPurityCorrFctn::RatN()
{return  mRatN;}
inline StHbt1DHisto* NonIdPurityCorrFctn::Rat()
{return  mRat;}
inline TProfile* NonIdPurityCorrFctn::PairPurityOut()
{return  mPairPurityOut;}
inline TProfile* NonIdPurityCorrFctn::PairPuritySide()
{return  mPairPuritySide;}
inline TProfile* NonIdPurityCorrFctn::PairPurityLong()
{return  mPairPurityLong;}

#endif

