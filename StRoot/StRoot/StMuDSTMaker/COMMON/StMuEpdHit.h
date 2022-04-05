#ifndef StMuEpdHit_h
#define StMuEpdHit_h

#include <cstdlib>
#include "StObject.h"

//
// \class StMuEpdHit
// \author Mike Lisa
// \date 14 Jan 2018
// \brief Holds signals for tiles in STAR Event Plane Detector

/*************************************************
 * Hit Class for the STAR Event Plane Detector.
 * There is one "hit" for each (good) tile in the detector
 * 
 * Total size of this object is 10 bytes
 * This object contains only
 *  1) the id of the tile, compacted into a Short_t
 *  2) the QT information (ADC, TDC, TAC, hasTac flag, status flag)
 *     compacted into 32 bits (Int_t
 *  3) the gain-corrected energy loss, in units of the
 *     most probable value (MPV) of a single MIP in the tile,
 *     according to a Landau distribution.  Stored as Float_t
 *
 * The StMuEpdHit in the StMuDST is basically the same as
 *  the StEpdHit object in StEvent
 *
 * - Mike Lisa Jan 2018
 ************************************************/
class StEpdHit;

class StMuEpdHit : public StObject
{
public:

  /// default constructor.  sets all values empty
  StMuEpdHit();
  /// constructor setting all values
  /// \param position     Supersector position on the wheel [1,12]
  /// \param tile         Tile number on the Supersector [1,31]
  /// \param EW           Which side is the wheel on?  -1 for East; +1 for West
  /// \param ADC          ADC reported by QT board  [0,4095]
  /// \param TAC          TAC reported by QT board (if there is one) [0,4095]
  /// \param TDC          TDC reported by QT board [0,32]
  /// \param hasTAC       true/fals if this channel has a TAC
  /// \param nMIP         gain-calibrated signal; energy loss in terms of MPV of Landau for a MIP
  /// \param statusIsGood good status, according to database
  /// \param truthId      id of particle most responsible for energy loss (simulation)
  StMuEpdHit(Int_t position, Int_t tile, Short_t EW, Int_t ADC, Int_t TAC, Int_t TDC, bool hasTAC, Float_t nMIP, bool statusIsGood, Int_t truthId);

  /// constructor based on StEpdHit
  /// this is what will be used in constructing the StMuDst from the StEvent
  /// \param epdHit :  the StEpdHit object stored in the StEvent
  StMuEpdHit(StEpdHit* epdHit);

  /// true if this channel has a valid TAC value
  bool hasTac() const;
  /// ADC value [0,4095]
  Int_t  adc() const;
  /// TAC value [0,4095]
  Int_t  tac() const;
  /// TDC value [0,31]
  Int_t  tdc() const;
  /// +1 if tile is on West side; -1 if on East side
  Short_t side() const;

  /// unique tile identifier
  ///   absolulte value is 100*position + tile
  ///   sign is +1/-1 for West/East
  Short_t id() const;
  /// position of supersector on a wheel [1,12]
  Int_t position() const;     // 1...12
  /// tile on the supersector [1,31]
  Int_t tile() const;         // 1...31
  /// the packed data from the QT board: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  Int_t qtData() {return mQTdata;}
  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP
  Float_t nMIP() {return mnMIP;}
  /// false if tile is bad or missing, according to (time-dependent) database
  bool isGood() const;

  /// It is expected that this will not be invoked, but if you want
  /// to use it, you have to pack the data yourself.  I do not want
  /// ability to individually set ADC,TDC,TAC.  Better to use the constructor
  /// \param packedData: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  void setQTdata(Int_t packedData){mQTdata=packedData;}
  /// It is expected that this will not be invoked, but rather the constructor used
  /// \param id = sign*(100*position+tile) where sign=+/- for West/East wheel
  void setId(Short_t id){mId = id;}
  /// \param gain calibrated energy loss in tile, in units of Landau MPV for one MIP
  void SetnMIP(Float_t nMIP){mnMIP = nMIP;}

  /// set the id of particle most responsible for energy loss in tile (monte carlo)
  void setIdTruth(Int_t id){mTruthId = id;}

  /// returns the particle number for the particle most responsible for energy loss (monte carlo)
  Int_t idTruth(){return mTruthId;}

protected:

  /// Packed channel Id:
  ///  abs(mID)  = 100*positionId + tileId
  ///  sign(mID) = +/- = West/East
  Short_t mId;

  /// Packed channel data: bits  0-11 are ADC; bits 12-23 are TAC;
  ///                      bits 24-28 are TDC; bit 29 is noTAC flag
  ///                      bit 30 is the good/bad (1/0) status flag
  Int_t mQTdata;

  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP
  Float_t mnMIP;

  /// greatest contributer to energy deposition (MC)
  Int_t mTruthId;

  ClassDef(StMuEpdHit, 1)
};

inline Short_t StMuEpdHit::side() const { return mId < 0 ? -1 : +1;}

inline Short_t StMuEpdHit::id() const { return mId; }
inline Int_t   StMuEpdHit::position() const { return std::abs(mId / 100); }
inline Int_t   StMuEpdHit::tile() const { return std::abs(mId % 100); }

inline Int_t   StMuEpdHit::adc() const { return mQTdata & 0x0FFF; }
inline Int_t   StMuEpdHit::tac() const { return (mQTdata >> 12) & 0x0FFF; }
inline Int_t   StMuEpdHit::tdc() const { return (mQTdata >> 24) & 0x001F; }
inline bool    StMuEpdHit::hasTac() const { return (mQTdata >> 29) & 0x1; }
inline bool    StMuEpdHit::isGood() const { return (mQTdata >> 30) & 0x1; }
#endif
