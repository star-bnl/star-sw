/***************************************************************************
 *
 * $Id: EntSep_pTCorrFctn.h,v 1.1 2000/09/14 18:36:54 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple entrance-seperation correlation function
 *   for studying 2-track cuts...
 *
 ***************************************************************************
 *
 * $Log: EntSep_pTCorrFctn.h,v $
 * Revision 1.1  2000/09/14 18:36:54  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 *
 **************************************************************************/

#ifndef EntSep_pTCorrFctn_hh
#define EntSep_pTCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class EntSep_pTCorrFctn : public StHbtCorrFctn {
public:
  EntSep_pTCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
		       const int& nbinExSep, const float& ExSepLo, const float& ExSepHi);
  virtual ~EntSep_pTCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt2DHisto* Numerator2D();
  StHbt2DHisto* Denominator2D();
  StHbt2DHisto* Ratio2D();

private:

  StHbt2DHisto* mNumerator2D;
  StHbt2DHisto* mDenominator2D;
  StHbt2DHisto* mRatio2D;

#ifdef __ROOT__
  ClassDef(EntSep_pTCorrFctn, 1)
#endif

};

inline  StHbt2DHisto* EntSep_pTCorrFctn::Numerator2D(){return mNumerator2D;}
inline  StHbt2DHisto* EntSep_pTCorrFctn::Denominator2D(){return mDenominator2D;}
inline  StHbt2DHisto* EntSep_pTCorrFctn::Ratio2D(){return mRatio2D;}


#endif

