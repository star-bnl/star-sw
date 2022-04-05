/*!
 * \class StEpdHit
 * \author Mike Lisa
 * \date 29 Dec 2017
 * \brief Stores information for tiles in STAR Event Plane Detector
 */
/***************************************************************************
 *
 * $Id: StEpdHit.h,v 2.1 2018/02/08 17:35:02 ullrich Exp $
 *
 * Author: Mike Lisa, Jan 2018
 ***************************************************************************
 *
 * Description:
 *
 * Hit Class for the STAR Event Plane Detector.
 * There is one "hit" for each (good) tile in the detector
 *
 * Total size of this object is 10 bytes
 * This object contains only
 *  1) the id of the tile, compacted into a Short_t
 *  2) the QT information (ADC, TDC, TAC, hasTac flag, status flag)
 *     compacted into 32 bits (int
 *  3) the gain-corrected energy loss, in units of the
 *     most probable value (MPV) of a single MIP in the tile,
 *     according to a Landau distribution.  Stored as Float_t
 *
 * - Mike Lisa Jan 2018
 *
 ***************************************************************************
 *
 * $Log: StEpdHit.h,v $
 * Revision 2.1  2018/02/08 17:35:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StEpdHit_h
#define StEpdHit_h

#include <cstdlib>
#include "StObject.h"

class StEpdHit : public StObject
{
public:
    
    /// default constructor.  sets all values empty
    StEpdHit();
    
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
    /// \param truthId      particle id of particle most responsible for energy loss (simulation)
    StEpdHit(int position, int tile, short EW, int ADC, int TAC, int TDC, bool hasTAC, float nMIP, bool statusIsGood, int truthId);
    
    //  virtual void Print(const char *option = "") const;
    
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
    
    /// the packed data from the QT board: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
    ///                    bit 29=0/1 for has/does not have TAC;
    ///                    bit 30=0/1 if tile is marked bad/good in database
    int qtData() const;
    
    /// gain calibrated energy loss in tile, in units of Landau MPV for one MIP
    float nMIP() const;
    
    /// false if tile is bad or missing, according to (time-dependent) database
    bool isGood() const;
    
    /// This method unlikely to be used frequently.  Sets the QT data.
    ///   You have to bit-pack the data yourself
    /// \param packedData: ADC=bits 0-11; TAC=bits 12-23; TDC=bits 24-28;
    ///                    bit 29=0/1 for has/does not have TAC;
    ///                    bit 30=0/1 if tile is marked bad/good in database
    void setQTdata(int packedData);
    
    /// \param gain calibrated energy loss in tile, in units of Landau MPV for one MIP
    void setnMIP(float nMIP);
    
    /// set identifier of particle most responsible for energy loss (simulation)
    void setIdTruth(int id);
    
    /// identifier of particle most responsible for energy loss (simulation)
    int idTruth() const;
    
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
    
    /// identifier of particle most responsible for energy loss (simulation)
    Int_t mTruthId;
    
    ClassDef(StEpdHit, 1)
};

inline int   StEpdHit::qtData() const {return mQTdata;}
inline float StEpdHit::nMIP() const {return mnMIP;}
inline void  StEpdHit::setQTdata(int packedData) {mQTdata=packedData;}
inline void  StEpdHit::setnMIP(float nMIP) {mnMIP = nMIP;}
inline void  StEpdHit::setIdTruth(int id) {mTruthId = id;}
inline int   StEpdHit::idTruth() const {return mTruthId;}
inline short StEpdHit::side() const { return mId < 0 ? -1 : +1;}
inline short StEpdHit::id() const { return mId; }
inline int   StEpdHit::position() const { return std::abs(mId / 100); }
inline int   StEpdHit::tile() const { return std::abs(mId % 100); }
inline int   StEpdHit::adc() const { return mQTdata & 0x0FFF; }
inline int   StEpdHit::tac() const { return (mQTdata >> 12) & 0x0FFF; }
inline int   StEpdHit::tdc() const { return (mQTdata >> 24) & 0x001F; }
inline bool  StEpdHit::hasTac() const { return (mQTdata >> 29) & 0x1; }
inline bool  StEpdHit::isGood() const { return (mQTdata >> 30) & 0x1; }
#endif
