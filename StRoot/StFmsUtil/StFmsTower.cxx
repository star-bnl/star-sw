// $Id: StFmsTower.cxx,v 1.1 2015/03/10 14:38:54 jeromel Exp $
//
// $Log: StFmsTower.cxx,v $
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsTower.cxx
 \brief     Implementation of StFmsTower, a simple FMS tower wrapper
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#include "StFmsUtil/StFmsTower.h"

#include "StEvent/StFmsHit.h"
#include "StFmsDbMaker/StFmsDbMaker.h"

namespace FMSCluster {
StFmsTower::StFmsTower()
    : mHit(nullptr), mColumn(-1), mRow(-1), mCluster(-1) { }

StFmsTower::StFmsTower(StFmsHit* fmsHit)
    : mHit(fmsHit), mColumn(-1), mRow(-1), mCluster(-1) { }

StFmsTower::~StFmsTower() { }

Bool_t StFmsTower::initialize(StFmsDbMaker* database) {
  if (!mHit || !database) {  // Check for invalid input
    return false;
  }  // if
  // Get row and column from the database
  mRow = database->getRowNumber(mHit->detectorId(), mHit->channel());
  mColumn = database->getColumnNumber(mHit->detectorId(), mHit->channel());
  return mRow > -1 && mColumn > -1;
}

Bool_t StFmsTower::isNeighbor(const StFmsTower& other) const {
  return abs(mColumn - other.column()) + abs(mRow - other.row()) == 1;
}
}  // namespace FMSCluster
