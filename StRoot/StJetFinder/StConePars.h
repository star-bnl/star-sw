// -*- mode: c++;-*-
// $Id: StConePars.h,v 1.1 2008/04/21 16:35:11 tai Exp $
#ifndef STCONEPARS_H
#define STCONEPARS_H

#include "StJetPars.h"

class StConePars : public StJetPars {

public:

  ///Set the grid spacing:
  void setGridSpacing(int nEta, double etaMin, double etaMax,
		      int nPhi, double phiMin, double phiMax);
    
  ///minimum et threshold to be considered a seed
  void setSeedEtMin(double);
  double seedEtMin() const;
    
  ///minimum et threshold to be considered for addition to the seed
  void setAssocEtMin(double);
  double assocEtMin() const;
    
  ///split jets if E_shared/E_neighbor>splitFraction
  void setSplitFraction(double v);
  double splitFraction() const;
	
  ///Let jet wander to minimum?
  void setPerformMinimization(bool v) {mDoMinimization=v;}
  bool performMinimization() const {return mDoMinimization;}
	
  ///Add seeds at midpoints?
  void setAddMidpoints(bool v) {mAddMidpoints=v;}
  bool addMidpoints() const {return mAddMidpoints;}
	
  ///Do Split/Merge step?
  void setDoSplitMerge(bool v) {mDoSplitMerge=v;}
  bool doSplitMerge() const {return mDoSplitMerge;}

  ///Require stable midpoints?
  void setRequireStableMidpoints(bool v) {mRequireStableMidpoints=v;}
  bool requiredStableMidpoints() const {return mRequireStableMidpoints;}

  ///Set cone radius:
  void setConeRadius(double v) {mR = v;}
  double coneRadius() const {return mR;}

  ///Toggle debug streams on/off
  void setDebug(bool v) {mDebug = v;}
  bool debug() const {return mDebug;}
    
protected:
  friend class StConeJetFinder;
  friend class StCdfChargedConeJetFinder;

  int mNeta;
  int mNphi;
  double mEtaMin;
  double mEtaMax;
  double mPhiMin;
  double mPhiMax;

  double mR;
    
  double mAssocEtMin;
  double mSeedEtMin;
    
  double mphiWidth;
  double metaWidth;
  int mdeltaPhi;
  int mdeltaEta;
    
  bool mDoMinimization;
  bool mAddMidpoints;
  bool mDoSplitMerge;
  double mSplitFraction;
  bool mRequireStableMidpoints;
  bool mDebug;
    
  ClassDef(StConePars,1)
};

#endif // STCONEPARS_H
