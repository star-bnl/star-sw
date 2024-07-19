#ifndef StPicoEpdHit_h
#define StPicoEpdHit_h

// C++ headers
#include <cstdlib>

// ROOT headers
#include "TObject.h"

//
// \class StPicoEpdHit
// \author Mike Lisa
// \date 6 March 2018
// \brief Holds signals for tiles in STAR Event Plane Detector

/*************************************************
 * Hit Class for the STAR Event Plane Detector.
 * There is one "hit" for each (good) tile in the detector
 *   that has a nonzero ADC value
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
 * The StPicoEpdHit in the StMuDST is basically the same as
 *  the StMuEpdHit object in the muDst and
 *  the StEpdHit object in StEvent
 * Except 
 *   1) it does not inherit from StObject, so can be used in "vanilla root"
 *   2) there is no "Truth ID" which is used for simulations.
 * 
 *
 * To access GEOMETRICAL information (e.g. location of a tile, etc)
 *  use the StEpdGeom class.  All you have to do is send StEpdGeom
 *  the StPicoEpdHit::id(), and you'll get all you want.
 *  (Also works for StEpdHit::id() and StMuEpdHit::id())
 *
 * - Mike Lisa March 2018
 *
 * ================================================
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

class StPicoEpdHit : public TObject {

 public:

  /// default constructor.  sets all values empty
  StPicoEpdHit();
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
  /// \param DEPdata      raw data from DEP - March 2023
  /// \param nMIP_DEP     gain-calibrated signal; energy loss in terms of MPV of Landau for a MIP - based on DEP data - March 2023
  StPicoEpdHit(Int_t position, Int_t tile, Int_t EW, Int_t ADC, Int_t TAC, Int_t TDC,
	       Bool_t hasTAC, Float_t nMIP_QT, Bool_t statusIsGood, UShort_t DEPdata = 0, Float_t nMIP_DEP = 0);

  /// constructor just taking id, QT, nMIP
  /// \param id           tile id, encoding size, position and tile
  /// \param QTdata       bit-compressed QT data
  /// \param nMIP_QT      calibrated ADC from QT
  /// \param DEPdata      raw DEP data - March 2023
  /// \param nMIP_DEP     calibrated DEP data - March 2023
  StPicoEpdHit(Short_t id, Int_t QTdata, Float_t nMIP_QT, UShort_t DEPdata = 0, Float_t nMIP_DEP = 0);

  /// Copy constructor
  StPicoEpdHit(const StPicoEpdHit &hit);
  /// Destructor
  virtual ~StPicoEpdHit();
  /// Print EPD hit information
  virtual void Print(const Char_t *option = "") const;

  /// true if this channel has a valid TAC value
  Bool_t hasTac() const             { return (mQTdata >> 29) & 0x1; }
  /// ADC value [0,4095]
  Int_t  adc() const                { return mQTdata & 0x0FFF; }
  /// TAC value [0,4095]
  Int_t  tac() const                { return (mQTdata >> 12) & 0x0FFF; }
  /// TDC value [0,31]
  Int_t  tdc() const                { return (mQTdata >> 24) & 0x001F; }
  /// +1 if tile is on West side; -1 if on East side
  Short_t side() const              { return mId < 0 ? -1 : +1;}

  /// unique tile identifier
  ///   absolulte value is 100*position + tile
  ///   sign is +1/-1 for West/East
  Short_t id() const                { return mId; }
  /// position of supersector on a wheel [1,12]
  Int_t position() const            { return std::abs(mId / 100); }
  /// tile on the supersector [1,31]
  Int_t tile() const                { return std::abs(mId % 100); }
  /// row number [1,16]
  Int_t row() const                 { return (std::abs(mId % 100))/2 + 1; }
  /// the packed data from the QT board: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  Int_t qtData() const              { return mQTdata; }
  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP
  /// if the tile is identified as bad in the database, returns zero.  Note you
  /// can always access the raw ADC value, regardless of good/bad
  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP - based on QT data if there is QT data.  Otherwise from DEP
  Float_t nMIP() const { if (!isGood()) return 0; return qtDataAvailable() ? mnMIP : mnMIP_DEP; }
  /// false if tile is bad or missing, according to (time-dependent) database
  Bool_t isGood() const             { return (mQTdata >> 30) & 0x1; }
  /// truncated nMIP.  This is usually the most useful thing to the physics analyzer.
  ///    if nMIP < thresh, returns zero
  ///    if nMIP > MAX,    returns MAX
  ///    otherwise         returns nMIP
  Float_t TnMIP(float MAX=2.0, float thresh=0.3) const   {float nm = this->nMIP(); if (nm<thresh) return 0.0; else return nm<MAX ? nm : MAX;}

  /// If you use the setQTdata method, you need to pack the data yourself.
  /// It is useful if you are getting the QT data from, e.g. StMuEpdHit
  /// \param packedData: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  void setQTdata(Int_t packedData)  { mQTdata=packedData; }
  /// It is expected that this will not be invoked, but rather the constructor used
  /// \param id = sign*(100*position+tile) where sign=+/- for West/East wheel
  void setId(Short_t id)            { mId = id; }
  /// \param gain calibrated energy loss in tile, in units of Landau MPV for one MIP - based on QT
  //@{
  void setnMIP(Float_t nMIP)        { mnMIP = nMIP; }
  void setnMIP_QT(Float_t nMIP_QT)  { mnMIP = nMIP_QT; }
  //@}

  // Now a bunch of methods for DEP - March 2023
  /// store the raw DEP data
  void setDEPdata(unsigned short DEPdata) { mDEPdata = DEPdata; }
  /// store the gain-calibrated energy loss based on DEP data
  void setnMIP_DEP(float nMipDep) { mnMIP_DEP = nMipDep; }
  /// get the raw DEP data
  UShort_t depData() const { return mDEPdata; }
  /// get the gain-calibrated energy loss based on DEP data
  Float_t nMIP_DEP() const { return isGood() ? mnMIP_DEP : 0; }
  /// true if there is QT information available for this (will be false for sure at high rates on west side)
  bool qtDataAvailable() const { return adc() > 0; }
  /// gain calibrated energy loss in tile based on QT, in units of Landau MPV for one MIP
  /// if the tile is identified as bad in the database, returns zero.  Note you
  /// can always access the raw ADC value, regardless of good/bad
  Float_t nMIP_QT() const { return isGood() ? mnMIP : 0; }

 protected:

  /// Packed channel Id:
  ///  abs(mID)  = 100*positionId + tileId
  ///  sign(mID) = +/- = West/East
  Short_t mId;

  /// Packed channel data: bits  0-11 are ADC; bits 12-23 are TAC;
  ///                      bits 24-28 are TDC; bit 29 is noTAC flag
  ///                      bit 30 is the good/bad (1/0) status flag
  Int_t mQTdata;

  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP - based on QT data
  Float_t mnMIP;

  /// raw DEP data - March 2023
  UShort_t mDEPdata;

  /// gain-calibrated energy loss in tile, in units of Landau MPV for one MIP - based on DEP - March 2023
  Float_t mnMIP_DEP;

  ClassDef(StPicoEpdHit, 2)
};

#endif
