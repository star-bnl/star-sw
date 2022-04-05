//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// Jan 26, 2008
//
#include <assert.h>
// C++ STL
#include "Stiostream.h"
#include <cassert>

// STAR
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

// Local
#include "StEEmcShowerShape.h"

ostream& operator<<(ostream& out, StMuEmcHit& hit)
{
  static StMuEmcUtil util;
  int module, eta, sub;
  assert(util.getEndcapBin(7, hit.getId(), module, eta, sub) == 0);
  out << "sector = " << module-1 << ", strip = " << eta-1
      << ", adc = " << hit.getAdc() << ", E = " << hit.getEnergy();
  return out;
}

ostream& operator<<(ostream& out, const StEEmcShowerShape& shape)
{
  out << "run = " << shape.runNumber()
      << ", event = " << shape.eventNumber()
      << ", energy = " << shape.energy()
      << ", sector = " << shape.sector()
      << '\n';
  out << "U-plane: nStrips = " << shape.numberOfUstrips() << '\n'
      << "high strip id: " << shape.highUstripId() << '\n';
  for (int i = 0; i < shape.numberOfUstrips(); ++i) {
    StMuEmcHit* hit = shape.uStrip(i);
    out << *hit << '\n';
  }
  out << "V-plane: nStrips = " << shape.numberOfVstrips() << '\n'
      << "high strip id: " << shape.highVstripId() << '\n';
  for (int i = 0; i < shape.numberOfVstrips(); ++i) {
    StMuEmcHit* hit = shape.vStrip(i);
    out << *hit << '\n';
  }
  return out;
}

ClassImp(StEEmcShowerShape);

StEEmcShowerShape::StEEmcShowerShape()
{
  mUstrips = new TClonesArray("StMuEmcHit");
  mVstrips = new TClonesArray("StMuEmcHit");
}

StEEmcShowerShape::StEEmcShowerShape(const StEEmcShowerShape& shape)
{
  if (this == &shape) return;

  setRunNumber(shape.runNumber());
  setEventNumber(shape.eventNumber());
  setEnergy(shape.energy());
  setSector(shape.sector());
  setHighUstripId(shape.highUstripId());
  setHighVstripId(shape.highVstripId());
  setPosition(shape.position());

  mUstrips = new TClonesArray("StMuEmcHit");
  mVstrips = new TClonesArray("StMuEmcHit");

  for (int i = 0; i < shape.numberOfUstrips(); ++i) addUstrip(shape.uStrip(i));
  for (int i = 0; i < shape.numberOfVstrips(); ++i) addVstrip(shape.vStrip(i));
}

StEEmcShowerShape::~StEEmcShowerShape()
{
  delete mUstrips;
  delete mVstrips;
}
