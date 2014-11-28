// $Id: StFtpcPoint.hh,v 1.15 2004/05/07 14:19:25 oldi Exp $
// $Log: StFtpcPoint.hh,v $
// Revision 1.15  2004/05/07 14:19:25  oldi
// const added to GedtDetectorId() and GetHardwarePosition().
// Creation of StEvent/StFtpcHit removed. This is done in a new constructor of StFtpcit itself, now.
//
// Revision 1.14  2004/04/06 18:36:13  oldi
// New data mebers for pad and time position and pad and time sigma added.
// Reference to StFtpcHit added.
// Possibility to update StFtpcHit coordinates directly included.
//
// Revision 1.13  2004/02/12 19:37:10  oldi
// *** empty log message ***
//
// Revision 1.12  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.11  2003/01/16 18:04:33  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.10  2002/11/06 13:45:42  oldi
// Flag for clusters not to be used for tracking introduced.
// Code clean ups.
//
// Revision 1.9  2002/06/04 13:34:59  oldi
// Transformation of local FTPC coordinates in global coordinates introduced.
// An additional flag keeps track in which coordinate system the point position
// is measured.
// 'Tracked' flag is set correctly, now.
//
// Revision 1.8  2002/04/05 16:50:40  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.7  2002/01/29 11:08:03  oldi
// Write() renamed to WriteCluster() resp. WriteTrack() to avoid compiler warnings.
// As a result the functions TObject::Write() are available again (directly).
//
// Revision 1.6  2001/01/25 15:21:51  oldi
// Review of the complete code.
// Fix of several bugs which caused memory leaks:
//  - Tracks were not allocated properly.
//  - Tracks (especially split tracks) were not deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way). I changed all occurences to TObjArray which makes the
//    program slightly slower but much more save (in terms of memory usage).
// Speed up of HandleSplitTracks() which is now 12.5 times faster than before.
// Cleanup.
//
// Revision 1.5  2000/11/10 18:37:46  oldi
// New constructor added.
// StThreeVector replaced by TVector3 to be able to use ROOT output (e.g. Write()).
// Cleanup.
//
// Revision 1.4  2000/08/01 12:24:04  hummler
// add writing to table functionality (ToTable() function)
//
// Revision 1.3  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.2  2000/06/13 14:49:25  oldi
// Added SetTrackedFlag(Bool_t tracked) and GetTrackedFlag() to take care of the
// bit 5 of mFlags.
// Changed SetUsage(Bool_t f) to change the bit 5 of mFlags, too.
//
// Revision 1.1  2000/05/11 15:14:49  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//

///////////////////////////////////////////////////////////////////////////////////
//                                                                               //
// StFtpcPoint class - representation of one FTPC cluster for the FTPC trackers. //
//                                                                               //
///////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcPoint
#define STAR_StFtpcPoint

#include "TObject.h"
#include "TObjArray.h"
#include "TVector3.h"
#include "TMath.h"

#include "StDetectorId.h"
#include "StFtpcTrack.hh"

class StFtpcHitCollection;
class StFtpcHit;
class StFtpcTrack;

class StFtpcPoint : public TObject {
  
private:

  StFtpcHit *mStEventHit;    // pointer to the corresponding hit in StEvent
  
  TVector3   mCoord;         // vector of cluster coordinates
  TVector3   mError;         // vector of errors on cluster coordinates
  
  Bool_t     mGlobalCoord;   // flag that indicates if the hit is stored in global or local (FTPC) coordinates
  Bool_t     mUsed;          // flag that indicates if the hit is assigned to a track 
  Int_t      mHitNumber;     // number of this cluster in this event
  Int_t      mNextHitNumber; // number of next hit on same track
  Int_t      mTrackNumber;   // track number to which this cluster belongs to

  // additional input variables from cluster finder
  
  Float_t    mPadPos;        // pad position of hit
  Float_t    mTimePos;       // time position of hit
  Float_t    mPadPosSigma;   // sigma pad position of hit
  Float_t    mTimePosSigma;  // sigma time position of hit
  Long_t     mPadRow;        // FTPC row number
  Long_t     mSector;        // FTPC readout sector number
  Long_t     mNumberPads;    // number of pads in cluster
  Long_t     mNumberBins;    // number of consecutive timebins in cluster
  Long_t     mMaxADC;        // cluster peak height (adc channels)
  Long_t     mCharge;        // cluster charge (adc channels)
  Long_t     mFlags;         // bit0:unfolded, bit1:unfold failed,
                             // bit2:saturated, bit3:bad shape, 
                             // bit4:cut off, bit5:tracked, 
                             // bit6:global coords, bit7:don't use for tracking
  Double_t   mSigmaPhi;      // cluster sigma in pad direction
  Double_t   mSigmaR;        // cluster sigma in drift direction


  // Residuals
  Double_t   mXPrimResidual;     // x distance of measured point to primary momentum fit
  Double_t   mYPrimResidual;     // y distance of measured point to primary momentum fit
  Double_t   mRPrimResidual;     // r of measured point to r of primary momentum fit
  Double_t   mPhiPrimResidual;   // angle of measured point to angle of primary momentum fit
  Double_t   mXGlobResidual;     // x distance of measured point to global momentum fit
  Double_t   mYGlobResidual;     // y distance of measured point to global momentum fit
  Double_t   mRGlobResidual;     // r of measured point to r of global momentum fit
  Double_t   mPhiGlobResidual;   // angle of measured point to angle of global momentum fit

public:
  
                 StFtpcPoint();                                 // default constructor
                 StFtpcPoint(Long_t   row, 
			     Long_t   sector, 
			     Long_t   n_pads, 
			     Long_t   n_bins, 
			     Long_t   max_adc, 
			     Long_t   charge, 
			     Float_t  padpos,
			     Float_t  timepos,
			     Float_t  padpossigma,
			     Float_t  timepossigma,
			     Double_t x, 
			     Double_t y, 
			     Double_t z, 
			     Double_t x_err, 
			     Double_t y_err, 
			     Double_t z_err, 
			     Double_t s_phi, 
			     Double_t s_r, 
			     Long_t   flags);                   // constructor to be filled directly from the cluster finder
                 StFtpcPoint(const StFtpcPoint &point);         // copy constructor
                 StFtpcPoint(Double_t *x, Int_t row);           // constructor which take an arbitrary point as input
  virtual       ~StFtpcPoint();                                 // destructor

  // coordinate transformations
  void TransformFtpc2Global();
  void TransformGlobal2Ftpc();
  void SetStFtpcHitCoord();

  virtual Int_t  ToStEvent(StFtpcHitCollection *ftpcHitColl);   // writes cluster to StFtpcHit class within StEvent
  
  // getter
  StFtpcHit* GetStFtpcHit() { return mStEventHit; }

  TVector3 GetCoord()  { return mCoord;    }
  TVector3 GetError()  { return mError;    }
  
  Double_t GetX()          const { return mCoord.X();   }
  Double_t GetY()          const { return mCoord.Y();   }
  Double_t GetZ()          const { return mCoord.Z();   }
  Double_t GetRadius()     const { return TMath::Sqrt(mCoord.X()*mCoord.X() + mCoord.Y()*mCoord.Y()); }
  Double_t GetXerr()       const { return mError.X();   }
  Double_t GetYerr()       const { return mError.Y();   }
  Double_t GetZerr()       const { return mError.Z();   }
  Double_t GetSigmaPhi()   const { return mSigmaPhi;    }
  Double_t GetSigmaR()     const { return mSigmaR;      }
  
  StFtpcTrack *GetTrack(TObjArray *tracks) const;
         void  SetTrackedFlag(Bool_t tracked);
       Bool_t  GetTrackedFlag();
         void  SetGlobalFlag(Bool_t global);
       Bool_t  GetGlobalFlag();
         void  SetUnusableForTrackingFlag(Bool_t global);
       Bool_t  GetUnusableForTrackingFlag();

  Bool_t   IsUsable();
  Bool_t   IsUnusable();
  Bool_t   IsInGlobalCoord()  { return GetGlobalFlag();  }
  Bool_t   GetUsage()         { return GetTrackedFlag(); }
  Int_t    GetHitNumber()     const { return mHitNumber;       }
  Int_t    GetNextHitNumber() const { return mNextHitNumber;   }
  Int_t    GetTrackNumber()   const { return mTrackNumber;     }
  Float_t  GetPadPos()        const { return mPadPos;          }
  Float_t  GetTimePos()       const { return mTimePos;         }
  Float_t  GetPadPosSigma()   const { return mPadPosSigma;     }
  Float_t  GetTimePosSigma()  const { return mTimePosSigma;    }
  Long_t   GetPadRow()        const { return mPadRow;          }
  Long_t   GetSector()        const { return mSector;          }
  Long_t   GetNumberPads()    const { return mNumberPads;      }
  Long_t   GetNumberBins()    const { return mNumberBins;      }
  Long_t   GetMaxADC()        const { return mMaxADC;          }
  Long_t   GetCharge()        const { return mCharge;          }
  Long_t   GetFlags()         const { return mFlags;           }
   Int_t   GetDetectorId() const;
  Long_t   GetHardwarePosition() const;
  
  Double_t GetXPrimResidual()     const { return mXPrimResidual;       }
  Double_t GetYPrimResidual()     const { return mYPrimResidual;       }
  Double_t GetRPrimResidual()     const { return mRPrimResidual;       }
  Double_t GetPhiPrimResidual()   const { return mPhiPrimResidual;     }
  Double_t GetXGlobResidual()     const { return mXGlobResidual;       }
  Double_t GetYGlobResidual()     const { return mYGlobResidual;       }
  Double_t GetRGlobResidual()     const { return mRGlobResidual;       }
  Double_t GetPhiGlobResidual()   const { return mPhiGlobResidual;     }

  // setter
  void    SetStFtpcHit(StFtpcHit* f) { mStEventHit = f; }

  void    SetX(Double_t f)        {     mCoord.SetX(f); }
  void    SetY(Double_t f)        {     mCoord.SetY(f); } 
  void    SetZ(Double_t f)        {     mCoord.SetZ(f); }
  void    SetXerr(Double_t f)     {     mError.SetX(f); }
  void    SetYerr(Double_t f)     {     mError.SetY(f); }
  void    SetZerr(Double_t f)     {     mError.SetZ(f); }
  
  void    SetGlobalCoord(Bool_t f)  {    SetGlobalFlag(f);  }
  void    SetUsage(Bool_t f)        {   SetTrackedFlag(f);  }
  void    SetHitNumber(Int_t f)     {     mHitNumber =  f;  }
  void    SetNextHitNumber(Int_t f) { mNextHitNumber =  f;  }
  void    SetTrackNumber(Int_t f)   {   mTrackNumber =  f;  }
  void    SetPadPos(Float_t f)      {        mPadPos =  f;  }
  void    SetTimePos(Float_t f)     {       mTimePos =  f;  }
  void    SetPadPosSigma(Float_t f) {   mPadPosSigma =  f;  }
  void    SetTimePosSigma(Float_t f){  mTimePosSigma =  f;  }
  void    SetPadRow(Long_t f)       {        mPadRow =  f;  }
  void    SetSector(Long_t f)       {        mSector =  f;  }
  void    SetNumberPads(Long_t f)   {    mNumberPads =  f;  }
  void    SetNumberBins(Long_t f)   {    mNumberBins =  f;  }
  void    SetMaxADC(Long_t f)       {        mMaxADC =  f;  }
  void    SetCharge(Long_t f)       {        mCharge =  f;  }
  void    SetFlags(Long_t f)        {         mFlags =  f;  }
  void    SetSigmaPhi(Double_t f)   {      mSigmaPhi =  f;  }
  void    SetSigmaR(Double_t f)     {        mSigmaR =  f;  }
  
  void    SetResidualsToZero();
  void    SetXPrimResidual(Double_t f)  {      mXPrimResidual = f;  }
  void    SetYPrimResidual(Double_t f)  {      mYPrimResidual = f;  }
  void    SetRPrimResidual(Double_t f)  {      mRPrimResidual = f;  }
  void    SetPhiPrimResidual(Double_t f){    mPhiPrimResidual = f;  }
  void    SetXGlobResidual(Double_t f)  {      mXGlobResidual = f;  }
  void    SetYGlobResidual(Double_t f)  {      mYGlobResidual = f;  }
  void    SetRGlobResidual(Double_t f)  {      mRGlobResidual = f;  }
  void    SetPhiGlobResidual(Double_t f){    mPhiGlobResidual = f;  }
  
  ClassDef(StFtpcPoint, 1)   //Ftpc point class
};



#endif
