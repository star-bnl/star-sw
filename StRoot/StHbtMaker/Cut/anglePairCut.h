/***************************************************************************
 *
 * $Id: anglePairCut.h,v 1.1 2001/11/05 16:14:16 rcwells Exp $
 *
 * Author: Randall Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cuts on the angle of the pair relative to the reaction plane           
 *
 ***************************************************************************
 *
 * $Log: anglePairCut.h,v $
 * Revision 1.1  2001/11/05 16:14:16  rcwells
 * Adding anglePairCut class to cut on pair emission angle
 *
 *
 **************************************************************************/


#ifndef anglePairCut_hh
#define anglePairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtPairCut.h"
#include "PhysicalConstants.h"

class anglePairCut : public StHbtPairCut{
public:
  anglePairCut();
  anglePairCut(char* title);
  anglePairCut(const anglePairCut&);
  //~anglePairCut();

  void SetAngleCut(const double& angleLo, const double& angleHi);
  void SetAngleCut(const double& angleLo1, const double& angleHi1,
		   const double& angleLo2, const double& angleHi2);
  StHbt1DHisto* betaT();
  StHbt1DHisto* betaL();
  StHbt1DHisto* betaT2();
  StHbt1DHisto* betaL2();
  StHbt1DHisto* betaTL();

  double EmissionAngle(const StHbtPair*);

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  anglePairCut* Clone();

private:
  double mAngle1[2];
  double mAngle2[2];
  long mNPairsPassed;
  long mNPairsFailed;
  StHbt1DHisto* mBetaT;  //!
  StHbt1DHisto* mBetaL;  //!
  StHbt1DHisto* mBetaT2; //!
  StHbt1DHisto* mBetaL2; //!
  StHbt1DHisto* mBetaTL; //!

#ifdef __ROOT__
  ClassDef(anglePairCut, 1)
#endif
};

inline anglePairCut::anglePairCut(const anglePairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline anglePairCut* anglePairCut::Clone() { anglePairCut* c = new anglePairCut(*this); return c;}
inline void anglePairCut::SetAngleCut(const double& angleLo, const double& angleHi){
  mAngle1[0] = angleLo;
  mAngle1[1] = angleHi;
  mAngle2[0] = 999.0;
  mAngle2[1] = 999.0;
}
inline void anglePairCut::SetAngleCut(const double& angleLo1, const double& angleHi1,
				      const double& angleLo2, const double& angleHi2){
  mAngle1[0] = angleLo1;
  mAngle1[1] = angleHi1;
  mAngle2[0] = angleLo2;
  mAngle2[1] = angleHi2;
}
inline  StHbt1DHisto* anglePairCut::betaT(){return mBetaT;}
inline  StHbt1DHisto* anglePairCut::betaL(){return mBetaL;}
inline  StHbt1DHisto* anglePairCut::betaT2(){return mBetaT2;}
inline  StHbt1DHisto* anglePairCut::betaL2(){return mBetaL2;}
inline  StHbt1DHisto* anglePairCut::betaTL(){return mBetaTL;}

#endif





