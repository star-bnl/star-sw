// $Id: StFtpcPoint.hh,v 1.1 2000/05/11 15:14:49 oldi Exp $
// $Log: StFtpcPoint.hh,v $
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
#include "TClonesArray.h"

#include "StThreeVector.hh"

#include "tables/St_fcl_fppoint_Table.h"
#include "tables/St_ffs_gepoint_Table.h"

#include "StFtpcTrack.hh"

class StFtpcPoint : public TObject {
  
private:
  
  StThreeVector<double> mCoord;  // vector of cluster coordinates
  StThreeVector<double> mError;  // vector of errors on cluster coordinates
  
  Bool_t    mUsed;          // flag that indicates if the hit is assigned to a track 
  Int_t     mHitNumber;     // number of this cluster in this event
  Int_t     mNextHitNumber; // number of next hit on same track
  Int_t     mTrackNumber;   // track number to which this cluster belongs to
  
  // additional input variables from cluster finder
  
  Long_t    mPadRow;      // FTPC row number
  Long_t    mSector;      // FTPC readout sector number
  Long_t    mNumberPads;  // number of pads in cluster
  Long_t    mNumberBins;  // number of consecutive timebins in cluster
  Long_t    mMaxADC;      // cluster peak height (adc channels)
  Long_t    mCharge;      // cluster charge (adc channels)
  Long_t    mFlags;       // bit4: cut off, bit5: tracked
  Double_t  mSigmaPhi;    // cluster sigma in pad direction
  Double_t  mSigmaR;      // cluster sigma in drift direction
  
public:
  
                 StFtpcPoint();                                 // default constructor
                 StFtpcPoint(fcl_fppoint_st *point_st);         // constructor for data after cluster finding
  virtual       ~StFtpcPoint();                                 // destructor
  virtual Int_t  Write();                                       // writes cluster to disc
  
  // getter
  StThreeVector<double> GetCoord()  { return mCoord;    }
  StThreeVector<double> GetError()  { return mError;    }
  
  Double_t GetX()          const { return mCoord.x();   }
  Double_t GetY()          const { return mCoord.y();   } 
  Double_t GetZ()          const { return mCoord.z();   }
  Double_t GetXerr()       const { return mError.x();   }
  Double_t GetYerr()       const { return mError.y();   }
  Double_t GetZerr()       const { return mError.z();   }
  Double_t GetSigmaPhi()   const { return mSigmaPhi;    }
  Double_t GetSigmaR()     const { return mSigmaR;      }
  
  StFtpcTrack *GetTrack(TClonesArray *tracks) const;
  
  Bool_t   GetUsage()         const { return mUsed;          }
  Int_t    GetHitNumber()     const { return mHitNumber;     }
  Int_t    GetNextHitNumber() const { return mNextHitNumber; }
  Int_t    GetTrackNumber()   const { return mTrackNumber;   }
  Long_t   GetPadRow()        const { return mPadRow;        }
  Long_t   GetSector()        const { return mSector;        }
  Long_t   GetNumberPads()    const { return mNumberPads;    }
  Long_t   GetNumberBins()    const { return mNumberBins;    }
  Long_t   GetMaxADC()        const { return mMaxADC;        }
  Long_t   GetCharge()        const { return mCharge;        }
  Long_t   GetFlags()         const { return mFlags;         }
  
  // setter  
  void    SetX(Double_t f)        {     mCoord.setX(f); }
  void    SetY(Double_t f)        {     mCoord.setY(f); } 
  void    SetZ(Double_t f)        {     mCoord.setZ(f); }
  void    SetXerr(Double_t f)     {     mError.setX(f); }
  void    SetYerr(Double_t f)     {     mError.setY(f); }
  void    SetZerr(Double_t f)     {     mError.setZ(f); }
  
  void    SetUsage(Bool_t f)        {          mUsed =  f;  }
  void    SetHitNumber(Int_t f)     {     mHitNumber =  f;  }
  void    SetNextHitNumber(Int_t f) { mNextHitNumber =  f;  }
  void    SetTrackNumber(Int_t f)   {   mTrackNumber =  f;  }
  void    SetPadRow(Long_t f)       {        mPadRow =  f;  }
  void    SetSector(Long_t f)       {        mSector =  f;  }
  void    SetNumberPads(Long_t f)   {    mNumberPads =  f;  }
  void    SetNumberBins(Long_t f)   {    mNumberBins =  f;  }
  void    SetMaxADC(Long_t f)       {        mMaxADC =  f;  }
  void    SetCharge(Long_t f)       {        mCharge =  f;  }
  void    SetFlags(Long_t f)        {         mFlags =  f;  }
  void    SetSigmaPhi(Double_t f)   {      mSigmaPhi =  f;  }
  void    SetSigmaR(Double_t f)     {        mSigmaR =  f;  }
  
  
  ClassDef(StFtpcPoint, 1)   //Ftpc point class
};

#endif
