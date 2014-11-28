/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : This class is used to calculate the Correlation Function
 * with several hypothesis on the size of the source. 
 * It inherit from StHbtCorrFctn, so it must be plugged in an analysis.
 * Several hypothesis on the size can be made with AddSize(x,y,z,t).
 *    for each size, is add a ThCFGaussSize in it SizeCollection.
 * Several ThCorrFctn can be plugged with AddCorrFctn(ThCorrFctn).
 *    for each ThCFGaussSize the ThCorrFctn is copied and the copy is pluged in 
 * The Weight Calculator should be set by SetWeight(weight)
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ST_HBT_THCF_GAUSSFIT_H
#define ST_HBT_THCF_GAUSSFIT_H


#include "StHbtMaker/ThCorrFctn/StHbtThPairGaussFit.h"
#include "StHbtMaker/Base/StHbtThCorrFctn.hh"
#include "StHbtMaker/ThCorrFctn/StHbtThCFGaussSizeCollection.hh"



class StHbtPair;
class TRandom;
class StHbtFsiWeight;


class StHbtThCFGaussFit : public StHbtCorrFctn {
 public:
  StHbtThCFGaussFit();
  ~StHbtThCFGaussFit();

  void AddCorrFctn(const StHbtThCorrFctn *);
  
  void AddRealPair( const StHbtPair*);
  void AddMixedPair( const StHbtPair*);

  void Finish();
  StHbtString Report();

  void AddSize(const char* aName, double aXYZ, double aT);
  void AddSize(const char* aName, double aX, double aY, double aZ, double aT);

  void SetWeight(StHbtFsiWeight*);

  void UseHiddenMomentum();
  void UseParticleMomentum();

  void UseHiddenPid();
  void UseFixedPid( int const tPid1, double const tMass1);  
  void UseFixedPid( int const tPid1,double const tMass1, int const tPid2,double const tMass2 ); 

  void SetBoostRCMS(double aPlab,double aMBeam, double aMTarget);
  
  void SetRCMS();
  void SetLCMS();
  void SetPRF();

  StHbtThCFGaussSizeCollection *getCollection();

 private:

  double mMaxX,mMaxY,mMaxZ,mMaxT;
  StHbtThPairGaussFit mPair;//!
  StHbtThCFGaussSizeCollection mSizeColl;//!
  
  TRandom mRand;

#ifdef __ROOT__
  ClassDef(StHbtThCFGaussFit,1)
#endif
};

#endif
