#ifndef StPicoEpdHit_h
#define StPicoEpdHit_h

#include "TObject.h"
//#include <cstdlib>

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
  /// \param nMIP         gain-calibrated signal; energy loss in terms of MPV of Landau for a MIP
  /// \param statusIsGood good status, according to database
  StPicoEpdHit(int position, int tile, int EW, int ADC, int TAC, int TDC, bool hasTAC, float nMIP, bool statusIsGood);
  /// constructor just taking id, QT, nMIP
  /// \param id           tile id, encoding size, position and tile
  /// \param QTdata       bit-compressed QT data
  /// \nMIP               calibrated ADC
  StPicoEpdHit(short id, int QTdata, float nMIP);
  /// Copy constructor
  StPicoEpdHit(const StPicoEpdHit &hit);
  /// Destructor
  virtual ~StPicoEpdHit();

  /// true if this channel has a valid TAC value
  bool hasTac() const;
  /// ADC value [0,4095]
  int  adc() const;
  /// TAC value [0,4095]
  int  tac() const;
  /// TDC value [0,31]
  int  tdc() const;
  /// +1 if tile is on West side; -1 if on East side
  short side() const;

  /// unique tile identifier
  ///   absolulte value is 100*position + tile
  ///   sign is +1/-1 for West/East
  short id() const;
  /// position of supersector on a wheel [1,12]
  int position() const;     // 1...12
  /// tile on the supersector [1,31]
  int tile() const;         // 1...31
  /// row number [1,16]
  int row() const;
  /// the packed data from the QT board: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  int qtData() const;
  /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP
  float nMIP() const;
  /// false if tile is bad or missing, according to (time-dependent) database
  bool isGood() const;


  /// If you use the setQTdata method, you need to pack the data yourself.
  /// It is useful if you are getting the QT data from, e.g. StMuEpdHit
  /// \param packedData: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  void setQTdata(Int_t packedData);
  /// It is expected that this will not be invoked, but rather the constructor used
  /// \param id = sign*(100*position+tile) where sign=+/- for West/East wheel
  void setId(Short_t id);
  /// \param gain calibrated energy loss in tile, in units of Landau MPV for one MIP
  void setnMIP(float nMIP);

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

  ClassDef(StPicoEpdHit, 1)
};

inline void    StPicoEpdHit::setnMIP(float nMIP){mnMIP = nMIP;}
inline void    StPicoEpdHit::setId(short id){mId = id;}
inline void    StPicoEpdHit::setQTdata(int packedData){mQTdata=packedData;}


inline float   StPicoEpdHit::nMIP()     const {return mnMIP;}
inline int     StPicoEpdHit::qtData()   const {return mQTdata;}


inline short   StPicoEpdHit::side()     const { return mId < 0 ? -1 : +1;}
inline short   StPicoEpdHit::id()       const { return mId; }
inline int     StPicoEpdHit::position() const { return std::abs(mId / 100); }
inline int     StPicoEpdHit::tile()     const { return std::abs(mId % 100); }
inline int     StPicoEpdHit::row()      const { return (std::abs(mId % 100))/2 + 1; }

inline int     StPicoEpdHit::adc() const { return mQTdata & 0x0FFF; }
inline int     StPicoEpdHit::tac() const { return (mQTdata >> 12) & 0x0FFF; }
inline int     StPicoEpdHit::tdc() const { return (mQTdata >> 24) & 0x001F; }
inline bool    StPicoEpdHit::hasTac() const { return (mQTdata >> 29) & 0x1; }
inline bool    StPicoEpdHit::isGood() const { return (mQTdata >> 30) & 0x1; }
#endif
