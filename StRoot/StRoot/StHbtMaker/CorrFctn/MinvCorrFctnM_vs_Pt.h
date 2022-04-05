/***************************************************************************
 *
 * $Id: MinvCorrFctnM_vs_Pt.h,v 1.1 2000/03/16 21:17:09 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctnM_vs_Pt.h,v $
 * Revision 1.1  2000/03/16 21:17:09  laue
 * Correlation function added
 *
 * Revision 1.2  1999/07/06 22:33:20  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef MinvCorrFctnM_vs_Pt_hh
#define MinvCorrFctnM_vs_Pt_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class MinvCorrFctnM_vs_Pt : public StHbtCorrFctn {
public:
  MinvCorrFctnM_vs_Pt(char* title, 
		     const int& nbins1, const float& MinvLo1, const float& MinvHi1,
		     const int& nbins2, const float& MinvLo2, const float& MinvHi2);
  virtual ~MinvCorrFctnM_vs_Pt(); 
  
  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  
  virtual void Finish();
  
  StHbt2DHisto* Numerator();
  StHbt2DHisto* Denominator();
  StHbt2DHisto* Difference();
  
 private:
  StHbt2DHisto* mNumerator;
  StHbt2DHisto* mDenominator;
  StHbt2DHisto* mDifference;
  
#ifdef __ROOT__
  ClassDef(MinvCorrFctnM_vs_Pt, 1)
#endif    
};

inline  StHbt2DHisto* MinvCorrFctnM_vs_Pt::Numerator(){return mNumerator;}
inline  StHbt2DHisto* MinvCorrFctnM_vs_Pt::Denominator(){return mDenominator;}
inline  StHbt2DHisto* MinvCorrFctnM_vs_Pt::Difference(){return mDifference;}

#endif

