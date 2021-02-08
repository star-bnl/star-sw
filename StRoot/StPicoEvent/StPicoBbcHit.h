#ifndef StPicoBbcHit_h
#define StPicoBbcHit_h

#include "TObject.h"
//#include <cstdlib>

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

  /// default constructor.  sets all values empty
  StPicoBbcHit();
  /// constructor setting all values
  /// \param PMTnumber    phototube number. [1,16]
  /// \param EW           Which side is the wheel on?  -1 for East; +1 for West
  /// \param ADC          ADC reported by QT board  [0,4095]
  /// \param TAC          TAC reported by QT board (if there is one) [0,4095]
  /// \param TDC          TDC reported by QT board [0,32]
  /// \param hasTAC       true/fals if this channel has a TAC
  /// \param statusIsGood good status, according to database
  StPicoBbcHit(int PMTnumber, int EW, int ADC, int TAC, int TDC, bool hasTAC, bool statusIsGood);
  /// copy constructor
  StPicoBbcHit(const StPicoBbcHit &hit);
  /// destructor
  virtual ~StPicoBbcHit();

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

  /// unique PMT identifier
  ///   absolute value is phototube number, between 1 and 16, inclusive
  ///   sign is +1/-1 for West/East
  short id() const;
  /// the packed data from the QT board: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  int qtData() const;
  /// false if tile is bad or missing, according to (time-dependent) database
  bool isGood() const;


  /// If you use the setQTdata method, you need to pack the data yourself.
  /// It is useful if you are getting the QT data from, e.g. StMuBbcHit
  /// \param packedData: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
  ///                    bit 29=0/1 for has/does not have TAC;
  ///                    bit 30=0/1 if tile is marked bad/good in database
  void setQTdata(Int_t packedData);
  /// It is expected that this will not be invoked, but rather the constructor used
  /// \param id = sign*(100*position+tile) where sign=+/- for West/East wheel
  void setId(Short_t id);

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

inline void    StPicoBbcHit::setId(short id){mId = id;}
inline void    StPicoBbcHit::setQTdata(int packedData){mQTdata=packedData;}


inline int     StPicoBbcHit::qtData()   const {return mQTdata;}


inline short   StPicoBbcHit::side()     const { return mId < 0 ? -1 : +1;}
inline short   StPicoBbcHit::id()       const { return mId; }

inline int     StPicoBbcHit::adc() const { return mQTdata & 0x0FFF; }
inline int     StPicoBbcHit::tac() const { return (mQTdata >> 12) & 0x0FFF; }
inline int     StPicoBbcHit::tdc() const { return (mQTdata >> 24) & 0x001F; }
inline bool    StPicoBbcHit::hasTac() const { return (mQTdata >> 29) & 0x1; }
inline bool    StPicoBbcHit::isGood() const { return (mQTdata >> 30) & 0x1; }
#endif
