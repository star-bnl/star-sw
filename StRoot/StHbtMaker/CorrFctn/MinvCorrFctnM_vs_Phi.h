/***************************************************************************
 *
 * $Id: MinvCorrFctnM_vs_Phi.h,v 1.1 2000/05/25 22:09:15 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass vs azimuth angle correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctnM_vs_Phi.h,v $
 * Revision 1.1  2000/05/25 22:09:15  laue
 * New correlation function. Minv vs azimuth angle
 *
 *
 **************************************************************************/

#ifndef MinvCorrFctnM_vs_Phi_hh
#define MinvCorrFctnM_vs_Phi_hh

#define __PI__  3.1415927

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class MinvCorrFctnM_vs_Phi : public StHbtCorrFctn {
public:
  MinvCorrFctnM_vs_Phi(char* title, 
		     const int& nbins1, const float& MinvLo1, const float& MinvHi1,
		     const int& nbins2=36, const float& MinvLo2=-__PI__, const float& MinvHi2=__PI__);
  virtual ~MinvCorrFctnM_vs_Phi(); 
  
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
  ClassDef(MinvCorrFctnM_vs_Phi, 1)
#endif    
};

inline  StHbt2DHisto* MinvCorrFctnM_vs_Phi::Numerator(){return mNumerator;}
inline  StHbt2DHisto* MinvCorrFctnM_vs_Phi::Denominator(){return mDenominator;}
inline  StHbt2DHisto* MinvCorrFctnM_vs_Phi::Difference(){return mDifference;}

#endif

