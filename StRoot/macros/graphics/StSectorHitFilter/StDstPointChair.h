#ifndef STAR_StDstPointChair
#define STAR_StDstPointChair

#include "StDetectorDefinitions.h"

#ifdef __CC5__
#  include <TChair.h>
#else
#  include "TChair.h"
#endif

///////////////////////////////////////////////////////////////////////////////////
// This chair privides the custom methods to the packed
// dst_point columns:
// and provide extra row check:
///////////////////////////////////////////////////////////////////////////////////

#include "tables/St_dst_point_Table.h"

class StDstPointChair : public TChair {
  private:
    Int_t   m_LastIndx;        // the index of the last unpacked coordinate
    Float_t m_MaxFactor;       // 2380*tpc 23800*svt 2380*ftpc
    Float_t m_MaxRange;        // 220+tpc    22+svt  270+ftpc
    Int_t   m_DetectorId;      // Id of the current detector (tpc,svt,ftpc)
    dst_point_st *m_LastRow;   // the last row accessed

  protected:
    StDstPointChair() {;}
    void SetDetectorId(Int_t detId=kTpcIdentifier);

  public:
    StDstPointChair(St_dst_point *hits): TChair(hits), m_LastIndx(-1){;}
   ~StDstPointChair(){;}
// User's method
    UShort_t        DetectorId(Int_t i)      const;
    UShort_t        Sector(Int_t i)          const;
    UShort_t        PadRow(Int_t i)          const;
    Float_t         GetX(Int_t i)            const;
    Float_t         GetY(Int_t i)            const;
    Float_t         GetZ(Int_t i)            const;
    dst_point_st   &operator[](Int_t i);

    UShort_t        SectorRow(Int_t i,UShort_t sector,UShort_t row){/* to be implemented */return 0;}
    Long_t          IdPosition(Int_t i, Long_t track_Id, Long_t position){/* to be implemented */return 0;}

//  Utilities
    static Int_t    DetectId(const Char_t *name="tpc");
    static Float_t  Factor(Float_t &range,Int_t detId);
    static Float_t  Factor(Float_t &range,const Char_t *name="tpc");
    static UShort_t UpckDetectorId(Long_t hwPosition);

    static Long_t   GetDetecorIdMask();
    static Long_t   GetRowMask();
    static Long_t   GetRowShift();
    static Long_t   GetSectorMask();
    static Long_t   GetSectorShift();

    static Long_t   GetTrackFactor(); 
    static UShort_t GetSectorFactor(); 
    static UShort_t UpckSec(Long_t hwPosition);
    static UShort_t UpckRow(Long_t hwPosition);
    static ULong_t  UpckX(const Long_t *position);
    static ULong_t  UpckY(const Long_t *position);
    static ULong_t  UpckZ(const Long_t *position);

    void            SetMaxFactor(Float_t maxFactor);
    void            SetMaxRange(Float_t maxRange);
    void            SetDetectorRange(Long_t hwPosition);

    dst_point_st   *Table(Int_t i=0) const;

    Float_t         GetRealPoint(Long_t unPackgedPoint) const;
    Int_t           GetLastDetectorId() const ;

    ClassDef(StDstPointChair,0)
};

//______________________________________________________________
inline dst_point_st &StDstPointChair::operator[](Int_t i){return *Table(i);}

//______________________________________________________________
inline Float_t  StDstPointChair::Factor(Float_t &range,const Char_t *name) 
                {  return Factor(range,DetectId(name));               }

//______________________________________________________________
inline Long_t   StDstPointChair::GetDetecorIdMask(){return 0xF;       }

//______________________________________________________________
inline Int_t    StDstPointChair::GetLastDetectorId() const { return m_DetectorId; }

//______________________________________________________________
inline Float_t  StDstPointChair::GetRealPoint(Long_t unPackgedPoint) const
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
inline UShort_t StDstPointChair::UpckDetectorId(Long_t hwPosition)
                {return UShort_t(hwPosition & GetDetecorIdMask());    }
//______________________________________________________________
inline UShort_t StDstPointChair::UpckRow(Long_t hwPosition)    
                { assert(UpckDetectorId(hwPosition) == kTpcIdentifier);
                  // bits 9-14  pad row number (1-45)
                  return UShort_t((hwPosition & GetRowMask()) >> GetRowShift()) ;
                }
//______________________________________________________________
inline UShort_t StDstPointChair::UpckSec(Long_t hwPosition)    
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
inline dst_point_st *StDstPointChair::Table(Int_t i) const 
                    {
                      if (m_LastIndx != i) {
                        ((StDstPointChair *)this)->m_LastIndx = i;
                        ((StDstPointChair *)this)->m_LastRow = 
                             ((St_dst_point *)GetTable())->GetTable(i);
                      }
                      return m_LastRow; 
                    }
//______________________________________________________________
inline UShort_t StDstPointChair::DetectorId(Int_t i) const 
                {return UpckDetectorId(Table(i)->hw_position);}
//______________________________________________________________
inline UShort_t StDstPointChair::Sector(Int_t i)   const 
                { return UpckSec(Table(i)->hw_position);}
//______________________________________________________________
inline UShort_t StDstPointChair::PadRow(Int_t i)   const 
                { return UpckRow(Table(i)->hw_position);}
//______________________________________________________________
inline void     StDstPointChair::SetDetectorRange(Long_t hwPosition) {
                  Int_t detId = UpckDetectorId(hwPosition);
                  if (GetLastDetectorId() != detId ) {
                     SetDetectorId(detId);
                     SetMaxFactor(Factor(m_MaxRange,detId));  
                  } 
                }
//______________________________________________________________
inline ULong_t  StDstPointChair::UpckX(const Long_t *position)
                { return   (*position) & 0x000FFFFF;}
//______________________________________________________________
inline ULong_t  StDstPointChair::UpckY(const Long_t *position)
                { return   ((position[1] & 0x3FF) << 10)  | (position[0]  >> 20); }
//______________________________________________________________
inline ULong_t  StDstPointChair::UpckZ(const Long_t *position)
                {  return   position[1] >> 10; }
//______________________________________________________________
inline Float_t  StDstPointChair::GetX(Int_t i)            const
{
  dst_point_st *row = Table(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealPoint(UpckX(row->position));
}
//______________________________________________________________
inline Float_t  StDstPointChair::GetY(Int_t i)            const
{
  dst_point_st *row = Table(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealPoint(UpckY(row->position));
}
//______________________________________________________________
inline Float_t  StDstPointChair::GetZ(Int_t i)            const
{
  dst_point_st *row = Table(i);
  ((StDstPointChair *)this)->SetDetectorRange(row->hw_position);
  return GetRealPoint(UpckZ(row->position));
}

#endif

