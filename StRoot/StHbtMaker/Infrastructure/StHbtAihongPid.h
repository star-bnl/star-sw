/***************************************************************************
 *
 * $Id: StHbtAihongPid.h,v 1.1 2001/09/05 20:41:41 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *              Class to get Aihongs propability id
 *
 ***************************************************************************
 *
 **************************************************************************/

#ifndef StHbtAihongPid_h
#define StHbtAihongPid_h

#ifdef __ROOT__
#include "StChain.h"
#endif

class StuProbabilityPidAlgorithm;


class StHbtAihongPid  {
 public:
  static StHbtAihongPid* Instance();
  void setPidTable(const char* fileName="PIDTable.root");
  void updateEvent(int refMult);
  void updateTrack(int charge, double p, double eta, double nHitsDedx, double dedx);
  StuProbabilityPidAlgorithm* aihongPid(int refMult, int charge, double p, double eta, double nHitsDedx, double dedx);
  StuProbabilityPidAlgorithm* aihongPid(int charge, double p, double eta, double nHitsDedx, double dedx);
  StuProbabilityPidAlgorithm* aihongPid();
  
  friend class nobody;
 protected: 
  StHbtAihongPid();
  virtual ~StHbtAihongPid() { /* no-op */ }
 private:
  static StHbtAihongPid* _instance;
  StuProbabilityPidAlgorithm* mAihongPid;
  double mAihongCentrality;

#ifdef __ROOT__
  ClassDef(StHbtAihongPid,0)
#endif
};

#endif
 
