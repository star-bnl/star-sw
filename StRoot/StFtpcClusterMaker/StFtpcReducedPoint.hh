// $Id: StFtpcReducedPoint.hh,v 1.1 2000/11/24 15:02:34 hummler Exp $
// $Log: StFtpcReducedPoint.hh,v $
// Revision 1.1  2000/11/24 15:02:34  hummler
// commit changes omitted in last commit
//

///////////////////////////////////////////////////////////////////////////////////
//                                                                               //
// StFtpcPoint class - representation of one FTPC cluster for the FTPC trackers. //
//                                                                               //
///////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcReducedPoint
#define STAR_StFtpcReducedPoint

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"

#include "tables/St_fcl_fppoint_Table.h"

class StFtpcTrack;

class StFtpcReducedPoint : public TObject {
  
private:
  
  TVector3  mCoord;         // vector of cluster coordinates
  TVector3  mError;         // vector of errors on cluster coordinates
    
  // additional input variables from cluster finder
  
  Long_t    mPadRow;        // FTPC row number
  Long_t    mSector;        // FTPC readout sector number
  Long_t    mNumberPads;    // number of pads in cluster
  Long_t    mNumberBins;    // number of consecutive timebins in cluster
  Long_t    mMaxADC;        // cluster peak height (adc channels)
  Long_t    mCharge;        // cluster charge (adc channels)
  Long_t    mFlags;         // bit4: cut off, bit5: tracked
  Double_t  mSigmaPhi;      // cluster sigma in pad direction
  Double_t  mSigmaR;        // cluster sigma in drift direction

public:
  
                 StFtpcReducedPoint();                                 // default constructor
  virtual       ~StFtpcReducedPoint();                                 // destructor
  virtual Int_t  ToTable(fcl_fppoint_st *point_st);                                       // writes cluster to STAF table
  
  
  Double_t GetX()          const { return mCoord.X();   }
  Double_t GetY()          const { return mCoord.Y();   }
  Double_t GetZ()          const { return mCoord.Z();   }
  Double_t GetXerr()       const { return mError.X();   }
  Double_t GetYerr()       const { return mError.Y();   }
  Double_t GetZerr()       const { return mError.Z();   }
  Double_t GetSigmaPhi()   const { return mSigmaPhi;    }
  Double_t GetSigmaR()     const { return mSigmaR;      }
  
  Long_t   GetPadRow()        const { return mPadRow;        }
  Long_t   GetSector()        const { return mSector;        }
  Long_t   GetNumberPads()    const { return mNumberPads;    }
  Long_t   GetNumberBins()    const { return mNumberBins;    }
  Long_t   GetMaxADC()        const { return mMaxADC;        }
  Long_t   GetCharge()        const { return mCharge;        }
  Long_t   GetFlags()         const { return mFlags;         }
  
  // setter  
  void    SetX(Double_t f)        {     mCoord.SetX(f); }
  void    SetY(Double_t f)        {     mCoord.SetY(f); } 
  void    SetZ(Double_t f)        {     mCoord.SetZ(f); }
  void    SetXerr(Double_t f)     {     mError.SetX(f); }
  void    SetYerr(Double_t f)     {     mError.SetY(f); }
  void    SetZerr(Double_t f)     {     mError.SetZ(f); }
  
  void    SetPadRow(Long_t f)       {        mPadRow =  f;  }
  void    SetSector(Long_t f)       {        mSector =  f;  }
  void    SetNumberPads(Long_t f)   {    mNumberPads =  f;  }
  void    SetNumberBins(Long_t f)   {    mNumberBins =  f;  }
  void    SetMaxADC(Long_t f)       {        mMaxADC =  f;  }
  void    SetCharge(Long_t f)       {        mCharge =  f;  }
  void    SetFlags(Long_t f)        {         mFlags =  f;  }
  void    SetSigmaPhi(Double_t f)   {      mSigmaPhi =  f;  }
  void    SetSigmaR(Double_t f)     {        mSigmaR =  f;  }
  
  
  ClassDef(StFtpcReducedPoint, 1)   //Ftpc point class
};




#endif
