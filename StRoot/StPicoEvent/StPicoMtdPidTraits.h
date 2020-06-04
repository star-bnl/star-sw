/*!
 *  \class StPicoMtdPidTraits
 *
 *  \brief Class storing the matching information
 *  between TPC tracks and MTD hits for muon PID.
 * 
 */

#ifndef StPicoMtdPidTraits_h
#define StPicoMtdPidTraits_h

#include "TObject.h"
class StMuMtdPidTraits;
class StMuMtdHit;

class StPicoMtdPidTraits : public TObject
{
public:
  StPicoMtdPidTraits();

  /*!
   *
   *  \brief Default constructor
   *  \param hit Pointer to the MTD hit in MuDst
   *  \param trait Pointer to the MtdPidTrait in MuDst
   *  \param index Index to the associated TPC track
   */
  StPicoMtdPidTraits(const StMuMtdHit* hit, const StMuMtdPidTraits* trait, const Int_t index);


  /*!
   *
   *  \brief Default destructor
   */
  virtual ~StPicoMtdPidTraits();


  virtual void Print(const Char_t* option = "") const;

  // Matching information
  Int_t    trackIndex()        const;
  Int_t    mtdHitIndex()       const;
  Int_t    gChannel()          const;
  Int_t    backleg()           const;
  Int_t    module()            const;
  Int_t    cell()              const;
  Int_t    matchFlag()         const;
  Float_t  deltaY()            const;
  Float_t  deltaZ()            const;
  Float_t  deltaTimeOfFlight() const;
  Float_t  beta()              const;

  // Setting functions
  void    setTrackIndex(Int_t index);
  void    setMtdHitIndex(Int_t index);
  void    setMatchFlag(Char_t flag);
  void    setDeltaY(Float_t dy);
  void    setDeltaZ(Float_t dz);
  void    setDeltaTimeOfFlight(Float_t t);
  void    setBeta(Float_t beta);

private:
  Short_t   mTrackIndex;            ///< Index to the associated track in the event
  Short_t   mMtdHitIndex;           ///< Index to the associated MTD hit in the event
  Char_t    mMatchFlag;             ///< Matching flag indicating multiple matches
  Float_t   mDeltaY;                ///< DeltaY between matched track-hit pair
  Float_t   mDeltaZ;                ///< DeltaZ between matched track-hit pair
  Float_t   mDeltaTimeOfFlight;     ///< Difference between measured and expected time-of-flight
  Float_t   mBeta;                  ///< Beta of matched tracks
  Short_t   mMtdHitChan;            ///< (backleg-1) * 60 + (module-1) * 12 + cell

  ClassDef(StPicoMtdPidTraits, 1)
};
inline Int_t    StPicoMtdPidTraits::trackIndex()        const { return mTrackIndex; }
inline Int_t    StPicoMtdPidTraits::mtdHitIndex()       const { return mMtdHitIndex; }
inline Int_t    StPicoMtdPidTraits::gChannel()          const { return mMtdHitChan; }
inline Int_t    StPicoMtdPidTraits::backleg()           const { return mMtdHitChan / 60 + 1; }
inline Int_t    StPicoMtdPidTraits::module()            const { return (mMtdHitChan % 60) / 12 + 1; }
inline Int_t    StPicoMtdPidTraits::cell()              const { return mMtdHitChan % 12; }
inline Int_t    StPicoMtdPidTraits::matchFlag()         const { return mMatchFlag; }
inline Float_t  StPicoMtdPidTraits::deltaY()            const { return mDeltaY; }
inline Float_t  StPicoMtdPidTraits::deltaZ()            const { return mDeltaZ; }
inline Float_t  StPicoMtdPidTraits::deltaTimeOfFlight() const { return mDeltaTimeOfFlight; }
inline Float_t  StPicoMtdPidTraits::beta()              const { return mBeta; }
inline void    StPicoMtdPidTraits::setTrackIndex(Int_t index) { mTrackIndex = (Short_t) index; }
inline void    StPicoMtdPidTraits::setMtdHitIndex(Int_t index) { mMtdHitIndex = (Short_t) index; }
inline void    StPicoMtdPidTraits::setMatchFlag(Char_t flag) { mMatchFlag = flag; }
inline void    StPicoMtdPidTraits::setDeltaY(Float_t dy) { mDeltaY = dy; }
inline void    StPicoMtdPidTraits::setDeltaZ(Float_t dz) { mDeltaZ = dz; }
inline void    StPicoMtdPidTraits::setDeltaTimeOfFlight(Float_t t) { mDeltaTimeOfFlight = t; }
inline void    StPicoMtdPidTraits::setBeta(Float_t beta) { mBeta = beta; }
#endif
