#ifndef STAR_StDstPointChair
#define STAR_StDstPointChair

#include "StHitChair.h"
#include "StDetectorDefinitions.h"


///////////////////////////////////////////////////////////////////////////////////
// This chair privides the custom methods to the packed
// dst_point columns:
// and provide extra row check:
///////////////////////////////////////////////////////////////////////////////////

#include "tables/St_dst_point_Table.h"

class StDstPointChair : public StHitChair {
  private:
    Int_t   m_LastIndx;        // the index of the last unpacked coordinate
    Float_t m_MaxFactor;       // 2380*tpc 23800*svt 2380*ftpc
    Float_t m_MaxErrFactor;    // 2**(-17)*tpc 2**(-26)*svt 2**(-26)ssd ?*ftpc
    Float_t m_MaxRange;        // 220+tpc    22+svt  270+ftpc
    Int_t   m_DetectorId;      // Id of the current detector (tpc,svt,ftpc)
    dst_point_st *m_LastRow;   // the last row accessed

  protected:
    StDstPointChair() {;}
    void SetDetectorId(Int_t detId=kTpcIdentifier);

  public:
    StDstPointChair(St_dst_point *hits): StHitChair(hits), m_LastIndx(-1){;}
   ~StDstPointChair(){;}
// User's method
    virtual Int_t   DetectorId (Int_t i)        const;
    virtual Int_t   Sector     (Int_t i)	const;
    virtual Int_t   PadRow     (Int_t i)        const;
    virtual Int_t   TrackId    (Int_t i)        const {return ((dst_point_st*)fTab)[i].id_track;}
//  virtual Int_t   HitPosition(Int_t i)     	const;//NotImp
    virtual Float_t GetX       (Int_t i)        const;
    virtual Float_t GetY       (Int_t i)        const;
    virtual Float_t GetZ       (Int_t i)        const;
    virtual Float_t GetXError  (Int_t i)       	const;
    virtual Float_t GetYError  (Int_t i)       	const;
    virtual Float_t GetZError  (Int_t i)       	const;
    virtual Float_t GetCharge  (Int_t i)        const;
    virtual ULong_t GetFlag    (Int_t i)        const;
    virtual ULong_t GetFitFlag (Int_t i)        const;


    dst_point_st   &operator[](Int_t i);

    UShort_t        SectorRow(Int_t i,UShort_t sector,UShort_t row){/* to be implemented */return 0;}
    Long_t          IdPosition(Int_t i, Int_t track_Id, Int_t position){/* to be implemented */return 0;}

//  Utilities
    static Int_t    DetectId(const Char_t *name="tpc");
    static Float_t  Factor(Float_t &range,Float_t &errF,Int_t detId);
    static Float_t  Factor(Float_t &range,Float_t &errF,const Char_t *name="tpc");
    static UShort_t UpckDetectorId(Int_t hwPosition);

    static Long_t   GetDetecorIdMask();
    static Long_t   GetRowMask();
    static Long_t   GetRowShift();
    static Long_t   GetSectorMask();
    static Long_t   GetSectorShift();

    static UShort_t UpckSec(Int_t hwPosition);
    static UShort_t UpckRow(Int_t hwPosition);
    static ULong_t  UpckX(const Int_t *position);
    static ULong_t  UpckY(const Int_t *position);
    static ULong_t  UpckZ(const Int_t *position);

    void            SetMaxFactor(Float_t maxFactor);
    void            SetMaxRange(Float_t maxRange);
    void            SetDetectorRange(Int_t hwPosition);

    dst_point_st   *GetTable(Int_t i=0) const;

    Float_t         GetRealPoint(Int_t unPackgedPoint) const;
    Float_t         GetRealError(Int_t unPackgedPoint) const;
    Int_t           GetLastDetectorId() const ;

    ClassDef(StDstPointChair,0)
};

//______________________________________________________________
inline dst_point_st &StDstPointChair::operator[](Int_t i){return *GetTable(i);}

//______________________________________________________________
inline Float_t  StDstPointChair::Factor(Float_t &range,Float_t &errF,const Char_t *name) 
                {  return Factor(range,errF,DetectId(name));        }

//______________________________________________________________
inline Long_t   StDstPointChair::GetDetecorIdMask(){return 0xF;       }

//______________________________________________________________
inline Int_t    StDstPointChair::GetLastDetectorId() const { return m_DetectorId; }

//______________________________________________________________
inline Float_t  StDstPointChair::GetRealError(Int_t unPackgedPoint) const
                { return Float_t(unPackgedPoint)*m_MaxErrFactor; }

//______________________________________________________________
inline Float_t  StDstPointChair::GetRealPoint(Int_t unPackgedPoint) const
                { return Float_t(unPackgedPoint)/m_MaxFactor - m_MaxRange; }

//______________________________________________________________
inline Long_t   StDstPointChair::GetRowMask()     { return 0x00007E00;}

//______________________________________________________________
inline Long_t   StDstPointChair::GetRowShift()    { return 9;         }
//______________________________________________________________
inline Long_t   StDstPointChair::GetSectorMask()  { return 0x000001F0;}
//______________________________________________________________
inline Long_t   StDstPointChair::GetSectorShift() { return 4;         }
//______________________________________________________________
inline UShort_t StDstPointChair::UpckDetectorId(Int_t hwPosition)
                {return UShort_t(hwPosition & GetDetecorIdMask());    }
//______________________________________________________________
inline UShort_t StDstPointChair::UpckRow(Int_t hwPosition)    
                { assert(UpckDetectorId(hwPosition) == kTpcIdentifier);
                  // bits 9-14  pad row number (1-45)
                  return UShort_t((hwPosition & GetRowMask()) >> GetRowShift()) ;
                }
//______________________________________________________________
inline UShort_t StDstPointChair::UpckSec(Int_t hwPosition)    
                {  assert(UpckDetectorId(hwPosition) == kTpcIdentifier);
                   // bits 4-8   sector number (1-24)
                   return UShort_t((hwPosition & GetSectorMask()) >> GetSectorShift()) ;
                }
//______________________________________________________________
inline void     StDstPointChair::SetMaxFactor(Float_t maxFactor)
                {m_MaxFactor  = maxFactor;} 
//______________________________________________________________
inline void     StDstPointChair::SetMaxRange(Float_t maxRange)  
                {m_MaxRange   = maxRange;} 
//______________________________________________________________
inline void     StDstPointChair::SetDetectorId(Int_t detId)     
                {m_DetectorId = detId;}
//______________________________________________________________
inline dst_point_st *StDstPointChair::GetTable(Int_t i) const 
                    {
                      if (m_LastIndx != i) {
                        ((StDstPointChair *)this)->m_LastIndx = i;
                        ((StDstPointChair *)this)->m_LastRow = 
                             ((St_dst_point *)GetThisTable())->GetTable(i);
                      }
                      return m_LastRow; 
                    }
//______________________________________________________________
inline Int_t StDstPointChair::DetectorId(Int_t i) const 
                {return UpckDetectorId(fTab_dst[i].hw_position);}
//______________________________________________________________
inline Int_t StDstPointChair::Sector(Int_t i)   const 
                { return UpckSec(fTab_dst[i].hw_position);}
//______________________________________________________________
inline Int_t StDstPointChair::PadRow(Int_t i)   const 
                { return UpckRow(fTab_dst[i].hw_position);}
//______________________________________________________________
inline void     StDstPointChair::SetDetectorRange(Int_t hwPosition) {
                  Int_t detId = UpckDetectorId(hwPosition);
                  if (GetLastDetectorId() != detId ) {
                     SetDetectorId(detId);
                     SetMaxFactor(Factor(m_MaxRange,m_MaxErrFactor,detId));  
                  } 
                }
//______________________________________________________________
inline ULong_t  StDstPointChair::UpckX(const Int_t *position)
                { return   (*position) & 0x000FFFFF;}
//______________________________________________________________
inline ULong_t  StDstPointChair::UpckY(const Int_t *position)
                { return   ((position[1] & 0x3FF) << 10)  | (position[0]  >> 20); }
//______________________________________________________________
inline ULong_t  StDstPointChair::UpckZ(const Int_t *position)
                {  return   position[1] >> 10; }
//______________________________________________________________
inline Float_t  StDstPointChair::GetX(Int_t i)           const
{
  dst_point_st *row = GetTable(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealPoint(UpckX(row->position));
}
//______________________________________________________________
inline Float_t  StDstPointChair::GetY(Int_t i)           const
{
  dst_point_st *row = GetTable(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealPoint(UpckY(row->position));
}
//______________________________________________________________
inline Float_t  StDstPointChair::GetZ(Int_t i)           const
{
  dst_point_st *row = GetTable(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealPoint(UpckZ(row->position));
}

//______________________________________________________________
inline Float_t  StDstPointChair::GetXError(Int_t i)      const
{
  dst_point_st *row = GetTable(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealError(UpckX(row->position));
}
//______________________________________________________________
inline Float_t  StDstPointChair::GetYError(Int_t i)      const
{
  dst_point_st *row = GetTable(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealError(UpckY(row->position));
}
//______________________________________________________________
inline Float_t  StDstPointChair::GetZError(Int_t i)       const
{
  dst_point_st *row = GetTable(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealError(UpckZ(row->position));
}
//______________________________________________________________
inline Float_t StDstPointChair::GetCharge  (Int_t i)          const
{
  dst_point_st *row = GetTable(i);
  const ULong_t iflag = row->charge/(1L<<16);
  const ULong_t tpcq  = row->charge - iflag*(1L<<16);
  return Float_t(tpcq)/(1<<25); // mCharge
}
//______________________________________________________________
inline ULong_t  StDstPointChair::GetFlag    (Int_t i)         const
{
  dst_point_st *row = GetTable(i);
  const ULong_t iflag = row->charge/(1L<<16);
  return (iflag&255);  // First 8 bits mFlag
}
//______________________________________________________________
inline ULong_t StDstPointChair::GetFitFlag (Int_t i)          const
{
  dst_point_st *row = GetTable(i);
  const ULong_t iflag = row->charge/(1L<<16);
  return (iflag>>8);// Last  8 bits mFitFlag
}

#endif

