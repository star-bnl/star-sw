/***************************************************************************
 *
 * $Id: StHbtAihongPid.h,v 1.3 2003/01/31 19:44:00 magestro Exp $
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
  static StHbtAihongPid* Instance(const char* fileName="PIDTable.root");
  void setPidTable(const char* fileName);
  void updateEvent(int refMult);
  void updateTrack(int charge, double p, double eta, int nHitsDedx, double dedx);
  StuProbabilityPidAlgorithm* aihongPid(int refMult, int charge, double p, double eta, int nHitsDedx, double dedx);
  StuProbabilityPidAlgorithm* aihongPid(int charge, double p, double eta, int nHitsDedx, double dedx);
  StuProbabilityPidAlgorithm* aihongPid();
  
  friend class nobody;
 protected: 
  StHbtAihongPid(const char* fileName);
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
 
