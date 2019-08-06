#ifndef StPicoBbcHit_h
#define StPicoBbcHit_h

// ROOT headers
#include "TObject.h"

//
// \class StPicoBbcHit
// \author Mike Lisa
// \date 6 March 2018
// \brief Holds signals for ***phototubes*** in INNER STAR
//   Beam-beam counter (BBC)

/*************************************************
 * Hit Class for the STAR Beam-Beam Counter (BBC)
 * There is one "hit" for each (good) *phototube* in the detector
 *   that has a nonzero ADC value
 * 
 * Total size of this object is 6 bytes
 * This object contains only
 *  1) the id of the PMT, compacted into a Short_t
 *  2) the QT information (ADC, TDC, TAC, hasTac flag, status flag)
 *     compacted into 32 bits (Int_t)
 * 
 *
 * The BBC is a painful detector, because it has 16 phototubes
 *  distributed to 18 tiles, per side.  This class stores the
 *  data by phototube (as it must).
 *
 * To access GEOMETRICAL information (e.g. location of a tile, etc)
 *  use the StBbcGeom class.  All you have to do is send StBbcGeom
 *  the StPicoBbcHit::id(), and you'll get all you want.
 *
 * - Mike Lisa March 2018
 ************************************************/

//_________________
class StPicoBbcHit : public TObject {

 public:

  /// Default constructor.  sets all values empty
  StPicoBbcHit();
  /// constructor setting all values
  /// \param PMTnumber    phototube number. [1,16]
  /// \param EW           Which side is the wheel on?  -1 for East; +1 for West
  /// \param ADC          ADC reported by QT board  [0,4095]
  /// \param TAC          TAC reported by QT board (if there is one) [0,4095]
  /// \param TDC          TDC reported by QT board [0,32]
  /// \param hasTAC       true/fals if this channel has a TAC
  /// \param statusIsGood good status, according to database
  StPicoBbcHit(Int_t PMTnumber, Int_t EW, Int_t ADC, Int_t TAC,
	       Int_t TDC, Bool_t hasTAC, Bool_t statusIsGood);
  /// Copy constructor
  StPicoBbcHit(const StPicoBbcHit &hit);
  /// Destructor
  virtual ~StPicoBbcHit();
  // Print BBC hit information
  virtual void Print(const Char_t *option = "") const;

  /// True if this channel has a valid TAC value
  Bool_t hasTac() const              { return (mQTdata >> 29) & 0x1; }
  /// ADC value [0,4095]
  Int_t  adc() const                 { return mQTdata & 0x0FFF; }
  /// TAC value [0,4095]
  Int_t  tac() const                 { return (mQTdata >> 12) & 0x0FFF; }
  /// TDC value [0,31]
  Int_t  tdc() const                 { return (mQTdata >> 24) & 0x001F; }
  /// +1 if tile is on West side; -1 if on East side
  Short_t side() const               { return mId < 0 ? -1 : +1;}

  /// unique PMT identifier
  ///   absolute value is phototube number, between 1 and 16, inclusive
  ///   sign is +1/-1 for West/East
  Short_t id() const                 { return mId; }
  /// The packed data from the QT board: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  Int_t qtData() const               { return mQTdata; }
  /// false if tile is bad or missing, according to (time-dependent) database
  Bool_t isGood() const              { return (mQTdata >> 30) & 0x1; }


  /// If you use the setQTdata method, you need to pack the data yourself.
  /// It is useful if you are getting the QT data from, e.g. StMuBbcHit
  /// \param packedData: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  void setQTdata(Int_t packedData)   { mQTdata=packedData; }
  /// It is expected that this will not be invoked, but rather the constructor used
  /// \param id = sign*(100*position+tile) where sign=+/- for West/East wheel
  void setId(Short_t id)             { mId = id; }

 protected:

  /// Phototube Id:
  ///  abs(mID)  = phototube number, between 1 and 16
  ///  sign(mID) = +/- = West/East
  Short_t mId;

  /// Packed channel data: bits  0-11 are ADC; bits 12-23 are TAC;
  ///                      bits 24-28 are TDC; bit 29 is noTAC flag
  ///                      bit 30 is the good/bad (1/0) status flag
  Int_t mQTdata;

  ClassDef(StPicoBbcHit, 1)
};

#endif
