/***************************************************************************
 *
 * $Id: StHbtAihongPid.cxx,v 1.4 2003/09/02 17:58:32 perev Exp $
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: StHbtAihongPid.cxx,v $
 * Revision 1.4  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/01/31 19:44:00  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.2  2002/01/14 16:10:58  laue
 * Location of PID table not hardwired anymore
 *
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtAihongPid.h"
#include <Stiostream.h>
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"

#ifdef __ROOT__
ClassImp(StHbtAihongPid)
#endif


StHbtAihongPid* StHbtAihongPid::_instance=0;

StHbtAihongPid* StHbtAihongPid::Instance(const char* fileName) {
  if (_instance == 0 ) _instance = new StHbtAihongPid(fileName);
  return _instance;
}

StHbtAihongPid::StHbtAihongPid(const char* fileName) { 
  cout << " StHbtAihongPid::StHbtAihongPid() " << endl; 
  mAihongPid = new StuProbabilityPidAlgorithm() ;
  setPidTable(fileName);
}

void StHbtAihongPid::setPidTable(const char* fileName) { 
  StuProbabilityPidAlgorithm::readParametersFromFile(fileName);

}

void StHbtAihongPid::updateEvent(int refMult) {
  if (refMult > 225 ) mAihongCentrality = 0.03;
  else if (refMult > 215 ) mAihongCentrality = 0.05;
  else if (refMult > 200 ) mAihongCentrality = 0.07;
  else if (refMult > 180 ) mAihongCentrality = 0.10;
  else if (refMult > 140 ) mAihongCentrality = 0.18;
  else if (refMult > 130 ) mAihongCentrality = 0.20; 
  else if (refMult > 120 ) mAihongCentrality = 0.23;
  else if (refMult > 115 ) mAihongCentrality = 0.24;
  else if (refMult > 100 ) mAihongCentrality = 0.28;
  else mAihongCentrality = 0.99;
}

void StHbtAihongPid::updateTrack(int charge, double p, double eta, int nHitsDedx, double dedx) {
  mAihongPid->processPIDAsFunction(mAihongCentrality, 0., charge, p, eta, nHitsDedx, dedx);
}
    
StuProbabilityPidAlgorithm* StHbtAihongPid::aihongPid() {
  return mAihongPid;
}

StuProbabilityPidAlgorithm* StHbtAihongPid::aihongPid(int refMult, int charge, double p, double eta, int nHitsDedx, double dedx){
  updateEvent(refMult);
  updateTrack(charge, p, eta, nHitsDedx, dedx);
  return mAihongPid;
}

StuProbabilityPidAlgorithm* StHbtAihongPid::aihongPid(int charge, double p, double eta, int nHitsDedx, double dedx) {
  updateTrack(charge, p, eta, nHitsDedx, dedx);
  return mAihongPid;
}




