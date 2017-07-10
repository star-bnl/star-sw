#ifndef StPicoEpdTile_h
#define StPicoEpdTile_h

#include <cstdlib>
#include "TObject.h"

#include "StPicoEvent/StPicoCommon.h"

using namespace StarPicoDst;


/**
 * EPD tile class for STAR picoDst
 * Total size of this object is 6 bytes
 *
 * If all EPD tiles are saved, this makes 2*12*31*6 = 4.4 kB
 *
 * 1/8 install in 2017:  if all EPD tiles are saved, this makes 1*3*31*6 = 558 bytes
 * in 2017, we will save all EPD tiles.  After 2017, we will not.
 *
 * - Mike Lisa 20 May 2017
 */
class StPicoEpdTile : public TObject
{
public:

  StPicoEpdTile();
  StPicoEpdTile(int PP, int TT, DetectorSide EW, int ADC, int TAC, int TDC, bool hasTAC);

  virtual void Print(const Char_t *option = "") const;

  bool hasTAC() const;
  int  adc() const;
  int  tac() const;
  int  tdc() const;
  DetectorSide side() const;

  int id() const;
  int PP() const;         // 1...12
  int TT() const;         // 1...31

protected:

  /// Packed channel Id: 100*PP + TT
  /// sign: +/- = West/East
  /// PP and TT are phototube indices start at 1 [1, 12] and [1, 31] respectively
  Short_t mId;

  /// Packed channel data: bits  0-11 are ADC; bits 12-23 are TAC;
  ///                      bits 24-28 are TDC; bit 29 is noTAC flag
  ULong_t mQTdata;

  ClassDef(StPicoEpdTile, 1)
};

inline DetectorSide StPicoEpdTile::side() const { return mId < 0 ? DetectorSide::East : DetectorSide::West;}

inline int  StPicoEpdTile::id() const { return mId; }
inline int  StPicoEpdTile::PP() const { return std::abs(mId / 100); }
inline int  StPicoEpdTile::TT() const { return std::abs(mId % 100); }

inline int  StPicoEpdTile::adc() const { return mQTdata & 0x0FFF; }
inline int  StPicoEpdTile::tac() const { return (mQTdata >> 12) & 0x0FFF; }
inline int  StPicoEpdTile::tdc() const { return (mQTdata >> 24) & 0x001F; }
inline bool StPicoEpdTile::hasTAC() const { return (mQTdata >> 29) & 0x1; }

#endif
