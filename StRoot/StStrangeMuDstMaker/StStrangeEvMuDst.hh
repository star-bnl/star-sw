/*!
 * \class StStrangeEvMuDst
 * \author Gene Van Buren, UCLA, 24-Mar-2000
 * \author Peter G. Jones, University of Birmingham, 19-Aug-1999
 *
 *               Strangeness event micro dst class
 *
 */

#ifndef StStrangeEvMuDst_hh
#define StStrangeEvMuDst_hh
#include "StStrangeMuDst.hh"
#include "TMath.h"
#include "stdlib.h"


class StEvent;
class StMcEvent;

class StStrangeEvMuDst : public StStrangeMuDst {
  friend class StMuMomentumShiftMaker;

public:
  StStrangeEvMuDst();
  ~StStrangeEvMuDst();
  StStrangeEvMuDst(StEvent&);
  StStrangeEvMuDst(StMcEvent&);
  void Clear();
  void Clear(Option_t* option) {TObject::Clear(option);}

  static void SetCorrectionFile(char*); /// Set files to use
  static void SetFractionFile(char*);

  /// @name Accessor functions
  //@{
  /// Run number
  Int_t   run()                    const;
  /// Event number
  Int_t   event()                  const;
  /// Trigger bias
  Bool_t  unbiasedTrigger()        const;
  /// L0 trigger word
  UInt_t  l0TriggerWord()          const;
  /// Return whether FTPC information is good
  Bool_t  ftpcGoodRun();
  Float_t primaryVertexX()         const;
  Float_t primaryVertexY()         const; /// Primary vtx position coordinates
  Float_t primaryVertexZ()         const;
  /// Magnetic field
  Float_t magneticField()          const;
  //@}

  /// @name Multiplicities/Centralities
  //@{
  Int_t   globalTracks()           const;
  Int_t   tpcTracks()              const;
  Int_t   ftpcEastTracks()         const;
  Int_t   ftpcWestTracks()         const;
  Int_t   primaryTracks()          const;
  Int_t   tpcPrimTracks()          const;
  Int_t   ftpcEastPrimTracks()     const;
  Int_t   ftpcWestPrimTracks()     const;
  Int_t   primaryNegTracks()       const;
  Float_t primaryCorrectedTracks() const;
  /// ADC value given by ZDC west (for pAu event tagging)
  UInt_t  zdcWestADC()             const;
  /// Cross section fraction
  Float_t fractionSigma()          const;
  //@}

  /// @name Set functions
  //@{
  void Fill(StEvent&);
  void Fill(StMcEvent&);
  void setMagneticField(Float_t);
  void setL0TriggerWord(UInt_t);
  void setGlobalTracks(Int_t);
  void setPrimaryTracks(Int_t);
  /// Set a neg value of the run number if FTPC information is bad
  void tagRunNumber(Int_t);
  //@}


protected:
                                  // These are written out
  /// Negative mRun value for bad FTPC run (set via tagRunNumber())
  Int_t   mRun;
  /// Negative mEvent value for L3Bias (automatic during Fill())
  Int_t   mEvent;
  Float_t mPrimaryVertexX;
  Float_t mPrimaryVertexY;
  Float_t mPrimaryVertexZ;
  Int_t   mGlobalTracks;
  Int_t   mPrimaryTracks;
  Int_t   mPrimaryNegTracks;
  Float_t mMagneticField;
  UInt_t  mL0TriggerWord;

private:

  Int_t FTPC(const Int_t) const;

  ClassDef(StStrangeEvMuDst,8)
};

inline         StStrangeEvMuDst::StStrangeEvMuDst(StEvent& event)
               { Fill(event); }
inline         StStrangeEvMuDst::StStrangeEvMuDst(StMcEvent& event)
               { Fill(event); }

inline void    StStrangeEvMuDst::tagRunNumber(Int_t r)
               { mRun = - TMath::Abs(r); }
inline Bool_t  StStrangeEvMuDst::ftpcGoodRun()
               { return (mRun > 0); }

inline Int_t   StStrangeEvMuDst::run() const
               { return TMath::Abs(mRun); }
inline Int_t   StStrangeEvMuDst::event() const
               { return TMath::Abs(mEvent); }
inline Bool_t  StStrangeEvMuDst::unbiasedTrigger() const
               { return (mEvent > 0); }
inline UInt_t  StStrangeEvMuDst::l0TriggerWord() const
               { return ((run() < 3999999) ? mL0TriggerWord :
                                             (UInt_t) mL0TriggerWord%100000); }
inline UInt_t  StStrangeEvMuDst::zdcWestADC() const
               { return ((run() < 3999999) ? 0U :
                                             (UInt_t) mL0TriggerWord/100000); }
inline Float_t StStrangeEvMuDst::magneticField() const 
               { return mMagneticField; }

inline Float_t StStrangeEvMuDst::primaryVertexX() const 
               { return mPrimaryVertexX; }
inline Float_t StStrangeEvMuDst::primaryVertexY() const 
               { return mPrimaryVertexY; }
inline Float_t StStrangeEvMuDst::primaryVertexZ() const 
               { return mPrimaryVertexZ; }

// Store FTPCwest*724 + FTPCeast in most significant 19 bits (max = 723 each)
// Store main TPC in least significant 13 bits (max = 8191)
inline Int_t   StStrangeEvMuDst::FTPC(const Int_t x) const
               { return ((Int_t) (((UInt_t) x)>>13)); }

inline Int_t   StStrangeEvMuDst::globalTracks() const
               { div_t ftpc = div(FTPC(mGlobalTracks),724);
                 return (tpcTracks() + ftpc.quot + ftpc.rem); }
inline Int_t   StStrangeEvMuDst::tpcTracks() const
               { return mGlobalTracks & 0x1fff; }
inline Int_t   StStrangeEvMuDst::ftpcEastTracks() const
               { return FTPC(mGlobalTracks)%724; }
inline Int_t   StStrangeEvMuDst::ftpcWestTracks() const
               { return FTPC(mGlobalTracks)/724; }

inline Int_t   StStrangeEvMuDst::primaryTracks() const
               { div_t ftpc = div(FTPC(mPrimaryTracks),724);
                 return (tpcPrimTracks() + ftpc.quot + ftpc.rem); }
inline Int_t   StStrangeEvMuDst::tpcPrimTracks() const
               { return mPrimaryTracks & 0x1fff; }
inline Int_t   StStrangeEvMuDst::ftpcEastPrimTracks() const
               { return FTPC(mPrimaryTracks)%724; }
inline Int_t   StStrangeEvMuDst::ftpcWestPrimTracks() const
               { return FTPC(mPrimaryTracks)/724; }
inline Int_t   StStrangeEvMuDst::primaryNegTracks() const
               { return mPrimaryNegTracks; }

inline void    StStrangeEvMuDst::setMagneticField(Float_t b)
               { mMagneticField = b; }
inline void    StStrangeEvMuDst::setL0TriggerWord(UInt_t trgWrd)
               { mL0TriggerWord = trgWrd; }
inline void    StStrangeEvMuDst::setGlobalTracks(Int_t newGlob)
               { mGlobalTracks = newGlob;}
inline void    StStrangeEvMuDst::setPrimaryTracks(Int_t newPrim)
               { mPrimaryTracks = newPrim;}

#endif


/***********************************************************************
 * $Id: StStrangeEvMuDst.hh,v 3.9 2005/03/17 05:06:51 genevb Exp $
 * $Log: StStrangeEvMuDst.hh,v $
 * Revision 3.9  2005/03/17 05:06:51  genevb
 * Add StMuMomentumShiftMaker friend
 *
 * Revision 3.8  2003/05/30 21:20:19  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.7  2003/02/10 16:00:29  genevb
 * Implement cleared events
 *
 * Revision 3.6  2002/05/17 14:05:28  genevb
 * Added L3 unbiased trigger info
 *
 * Revision 3.5  2002/04/30 16:02:48  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.4  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.3  2001/09/14 21:22:26  genevb
 * Avoid hiding TObject::Clear()
 *
 * Revision 3.2  2001/08/23 13:20:54  genevb
 * Many bug workarounds...
 *
 * Revision 3.1  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.0  2000/07/14 12:56:49  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.1  2000/06/09 22:17:10  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.3  2000/03/31 03:20:24  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
 * Revision 1.2  2000/03/29 20:52:13  genevb
 * Added StKinkMuDst, replaced arrays
 *
 * Revision 1.1  2000/03/29 03:10:07  genevb
 * Int_troduction of Strangeness Micro DST package
 *
 ***********************************************************************/
