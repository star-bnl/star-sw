/***************************************************************************
 *
 * $Id: StHbtAihongPid.cxx,v 1.1 2001/09/05 20:41:41 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: StHbtAihongPid.cxx,v $
 * Revision 1.1  2001/09/05 20:41:41  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtAihongPid.h"
#include <iostream.h>
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"

#ifdef __ROOT__
ClassImp(StHbtAihongPid)
#endif


StHbtAihongPid* StHbtAihongPid::_instance=0;

StHbtAihongPid* StHbtAihongPid::Instance() {
  if (_instance == 0 ) _instance = new StHbtAihongPid();
  return _instance;
}

StHbtAihongPid::StHbtAihongPid() { 
  cout << " StHbtAihongPid::StHbtAihongPid() " << endl; 
  mAihongPid = new StuProbabilityPidAlgorithm() ;
  setPidTable();
}

void StHbtAihongPid::setPidTable(const char* fileName) { 
  StuProbabilityPidAlgorithm::readParametersFromFile("/afs/rhic/star/users/laue/work/PIDTable.root");

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

void StHbtAihongPid::updateTrack(int charge, double p, double eta, double nHitsDedx, double dedx) {
  mAihongPid->processPIDAsFunction(mAihongCentrality, 0., charge, p, eta, nHitsDedx, dedx);
}
    
StuProbabilityPidAlgorithm* StHbtAihongPid::aihongPid() {
  return mAihongPid;
}

StuProbabilityPidAlgorithm* StHbtAihongPid::aihongPid(int refMult, int charge, double p, double eta, double nHitsDedx, double dedx){
  updateEvent(refMult);
  updateTrack(charge, p, eta, nHitsDedx, dedx);
  return mAihongPid;
}

StuProbabilityPidAlgorithm* StHbtAihongPid::aihongPid(int charge, double p, double eta, double nHitsDedx, double dedx) {
  updateTrack(charge, p, eta, nHitsDedx, dedx);
  return mAihongPid;
}




