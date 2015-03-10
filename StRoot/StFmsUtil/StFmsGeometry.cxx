// $Id: StFmsGeometry.cxx,v 1.1 2015/03/10 14:38:54 jeromel Exp $
//
// $Log: StFmsGeometry.cxx,v $
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsGeometry.cxx
 \brief     Implementation of StFmsGeometry, an FMS database geometry interface
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 */
#include "StFmsUtil/StFmsGeometry.h"
#include "St_base/StMessMgr.h"
#include "StChain/StMaker.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "tables/St_fmsDetectorPosition_Table.h"

namespace FMSCluster {
StFmsGeometry::StFmsGeometry() { }

StFmsGeometry::~StFmsGeometry() { }

Bool_t StFmsGeometry::initialize(StFmsDbMaker* fmsDbMaker) {
  // If no FMS database was provided, attempt to locate on in the current chain,
  // if one exists
  if (!fmsDbMaker) {
    StMaker* chain = StMaker::GetChain();
    if (chain) {
      fmsDbMaker = static_cast<StFmsDbMaker*>(chain->GetMaker("fmsDb"));
    }  // if
  }  // if
  // Bail out if no FMS database can be located
  if (!fmsDbMaker) {
    LOG_ERROR << "StFmsGeometry unable to locate an StFmsDbMaker - "
      << "geometry will not be initialised!" << endm;
    return false;
  }  // if
  const fmsDetectorPosition_st* dbgeom = fmsDbMaker->DetectorPosition();
  if (dbgeom) {
    // Detector IDs count [0, N), so number of detectors is one greater than max
    const int nDetectors = fmsDbMaker->maxDetectorId() + 1;
    for (int i(0); i < nDetectors; ++i) {
      // The first detector has ID = 0. Subsequent detectors with no data will
      // also have detector ID = 0. This won't overwrite the first entry, as
      // map::insert won't insert if there is already an entry with that key.
      mPositions.insert(std::make_pair(dbgeom[i].detectorId, &dbgeom[i]));
    }  // for
    return true;
  }  // if
  return false;
}

Double_t StFmsGeometry::xOffset(Int_t detectorId) const {
  const fmsDetectorPosition_st* geometry = find(detectorId);
  if (geometry) {
    return geometry->xoffset;
  }  // if
  return 0.;
}

Double_t StFmsGeometry::yOffset(Int_t detectorId) const {
  const fmsDetectorPosition_st* geometry = find(detectorId);
  if (geometry) {
    return geometry->yoffset;
  }  // if
  return 0.;
}

Double_t StFmsGeometry::z(Int_t detectorId) const {
  const fmsDetectorPosition_st* geometry = find(detectorId);
  if (geometry) {
    return geometry->zoffset;
  }  // if
  return 0.;
}

std::vector<Double_t> StFmsGeometry::towerWidths(Int_t detectorId) const {
  const fmsDetectorPosition_st* geometry = find(detectorId);
  std::vector<Double_t> widths(2, 0.);
  if (geometry) {
    widths.at(0) = geometry->xwidth;
    widths.at(1) = geometry->ywidth;
  }  // if
  return widths;
}

const fmsDetectorPosition_st* StFmsGeometry::find(Int_t detectorId) const {
  const fmsDetectorPosition_st* positions = nullptr;
  Table::const_iterator entry = mPositions.find(detectorId);
  if (entry != mPositions.end()) {
    positions = entry->second;
  }  // if
  return positions;
}

StThreeVectorF StFmsGeometry::localToGlobalCoordinates(Double_t x, Double_t y,
                                                 Int_t detectorId) const {
  StThreeVectorF global(0., 0., 0.);
  const fmsDetectorPosition_st* detector = find(detectorId);
  if (!detector) {
    return global;  // Uninitialized StFmsGeometry object or invalid detector ID
  }  // if
  if (isNorth(detectorId)) {
    // Local coordinates are always positive numbers, but north
    // detectors have negative global STAR coordinates hence "minus column".
    // The offset is already stored in the database as a negative value.
    global.setX(detector->xoffset - x);
  } else {
    global.setX(detector->xoffset + x);
  }  // if
  // y offset gives the *top* of the detector in the global system
  // y offset is half the vertical height, as the detectors are centered about
  // y = 0 i.e. y offset = nrows / 2 * row height
  // Note the y coordinate has 0 at the *bottom*, which corresponds to
  // maximally negative in the global system.
  global.setY(y - detector->yoffset);
  global.setZ(detector->zoffset);  // Detector face
  return global;
}

StThreeVectorF StFmsGeometry::columnRowToGlobalCoordinates(Double_t column,
                                                     Double_t row,
                                                     Int_t detectorId) const {
  const fmsDetectorPosition_st* detector = find(detectorId);
  if (detector) {
    // Multiply column and row by tower widths to convert to cm then just call
    // the normal local-to-global conversion
    return localToGlobalCoordinates(column * detector->xwidth,
                                    row * detector->ywidth, detectorId);
  }  // if
  return StThreeVectorF(0., 0., 0.);  // In case of no detector information
}

Bool_t StFmsGeometry::isNorth(Int_t detectorId) {
  switch (detectorId) {
    case kFpdNorth:  // Deliberate fall-through
    case kFpdNorthPreshower:  // Deliberate fall-through
    case kFpdNorthShowerMaxVertical:  // Deliberate fall-through
    case kFpdNorthShowerMaxHorizontal:  // Deliberate fall-through
    case kFmsNorthLarge:  // Deliberate fall-through
    case kFmsNorthSmall:  // Deliberate fall-through
    case kFhcNorth:
      return true;
    default:
      return false;
  }  // switch
}
}  // namespace FMSCluster
