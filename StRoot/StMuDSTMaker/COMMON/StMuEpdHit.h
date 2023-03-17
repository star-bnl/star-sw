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
 *
 *
 * Update March 2023 (More than 5 years later.  Not bad.)  Mike Lisa
 * - We now have the DAQ upgrade and DEP readout on the West side.
 *   DEP information will *always* be there.  QT only at low rate.
 *   So, now have to store DEP information.
 *
 * It means several new methods, and two new data members:
 * 1) unsigned short DEPdata
 * 2) float nMIP_DEP
 * So, now the size of one StMuEpdHit is 16 bytes
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
  /// \param nMIP_QT      gain-calibrated signal; energy loss in terms of MPV of Landau for a MIP - based on QT data
  /// \param statusIsGood good status, according to database
  /// \param truthId      id of particle most responsible for energy loss (simulation)
  /// \param DEPdata      raw data from DEP - March 2023
  /// \param nMIP_DEP     gain-calibrated signal; energy loss in terms of MPV of Landau for a MIP - based on DEP data - March 2023
  StMuEpdHit(Int_t position, Int_t tile, Short_t EW, Int_t ADC, Int_t TAC, Int_t TDC, bool hasTAC, Float_t nMIP_QT, bool statusIsGood, Int_t truthId, UShort_t DEPdata, Float_t nMIP_DEP);

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
  /// \param gain calibrated energy loss in tile, in units of Landau MPV for one MIP - based on QT data - March 2023
  void setnMIP_QT(Float_t nMIP_QT){mnMIP_QT = nMIP_QT;}
  /// set the id of particle most responsible for energy loss in tile (monte carlo)
  void setIdTruth(Int_t id){mTruthId = id;}
  /// returns the particle number for the particle most responsible for energy loss (monte carlo)
  Int_t idTruth(){return mTruthId;}

  // Now a bunch of methods for DEP - March 2023
  /// store the raw DEP data
  void setDEPdata(unsigned short DEPdata){mDEPdata=DEPdata;}
  /// store the gain-calibrated energy loss based on DEP data
  void setnMIP_DEP(float nMipDep){mnMIP_DEP=nMipDep;}
  /// get the raw DEP data
  UShort_t depData() const {return mDEPdata;}
  /// get the gain-calibrated energy loss based on DEP data
  Float_t nMIP_DEP() const {return mnMIP_DEP;}
  /// true if there is QT information available for this (will be false for sure at high rates on west side)
  bool qtDataAvailable() const;
  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP - based on QT data
  Float_t nMIP_QT() const {return mnMIP_QT;}
  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP - based on QT data if there is QT data.  Otherwise from DEP
  Float_t nMIP() const {return (this->qtDataAvailable())?mnMIP_QT:mnMIP_DEP;}


protected:

  /// Packed channel Id:
  ///  abs(mID)  = 100*positionId + tileId
  ///  sign(mID) = +/- = West/East
  Short_t mId;

  /// Packed channel data: bits  0-11 are ADC; bits 12-23 are TAC;
  ///                      bits 24-28 are TDC; bit 29 is noTAC flag
  ///                      bit 30 is the good/bad (1/0) status flag
  Int_t mQTdata;

  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP - based on QT
  Float_t mnMIP_QT;

  /// raw DEP data - March 2023
  UShort_t mDEPdata;

  /// gain-calibrated energy loss in tile, in units of Landau MPV for one MIP - based on DEP - March 2023
  Float_t mnMIP_DEP;

  /// greatest contributer to energy deposition (MC)
  Int_t mTruthId;

  ClassDef(StMuEpdHit, 2)
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
inline bool    StMuEpdHit::qtDataAvailable() const {return (this->adc()!=0);}

#endif
