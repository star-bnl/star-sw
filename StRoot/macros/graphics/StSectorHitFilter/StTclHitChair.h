#ifndef STAR_StTclHitChair
#define STAR_StTclHitChair
#ifdef __CC5__
#include <TChair.h>
#else
#include "TChair.h"
#endif
///////////////////////////////////////////////////////////////////////////////////
// This chair privides the custom methods to the packed
// tcl_hit columns:
// 	long track; /* track_id*1000+position on on a track (starts from 1) */
//	short row;  /* 100*isector TPC row number */
// and provide extra row check:
///////////////////////////////////////////////////////////////////////////////////

#include "tables/St_tcl_tphit_Table.h"

class StTclHitChair : public TChair {
  protected:
    StTclHitChair() {;}
  public:
    StTclHitChair(St_tcl_tphit *hits): TChair(hits){;}
   ~StTclHitChair(){;}

    static Long_t   GetTrackFactor(); 
    static UShort_t GetSectorFactor(); 
    static UShort_t UpckSec(UShort_t row);
    static UShort_t UpckRow(UShort_t row);
    static Long_t   UpckTrackId (Long_t track);
    static Long_t   UpckPosition(Long_t track);
    static Long_t   PckTrack(Long_t trackId, Long_t position);
    static UShort_t PckRow(UShort_t sector,UShort_t row);

    UShort_t Sector(Int_t i)          const;
    UShort_t PadRow(Int_t i)          const;
    Long_t   TrackId(Int_t i)         const;
    Long_t   Position(Int_t i)        const;
    Bool_t   IsValid(Int_t i)         const;
    tcl_tphit_st *GetTable(Int_t i=0) const;
    tcl_tphit_st &operator[](Int_t i){return *GetTable(i);}

    UShort_t SectorRow(Int_t i,UShort_t sector,UShort_t row);
    Long_t IdPosition(Int_t i, Long_t track_Id, Long_t position);

    ClassDef(StTclHitChair,0)
};

inline Long_t   StTclHitChair::GetTrackFactor()         
                { return 1000;}

inline UShort_t StTclHitChair::GetSectorFactor()        
                { return  100;} 

inline UShort_t StTclHitChair::UpckSec(UShort_t row)    
                { return row / GetSectorFactor() ;}

inline UShort_t StTclHitChair::UpckRow(UShort_t row)    
                { return row  - UpckSec(row)*GetSectorFactor() ;}

inline Long_t   StTclHitChair::UpckTrackId(Long_t track)
                { return track/GetTrackFactor();}

inline Long_t   StTclHitChair::UpckPosition(Long_t track)
                { return track - UpckTrackId(track)*GetTrackFactor();}

inline Long_t   StTclHitChair::PckTrack(Long_t trackId, Long_t position)
                { return trackId*GetTrackFactor() + position; }

inline UShort_t StTclHitChair::PckRow(UShort_t sector,UShort_t row)
                { return sector*GetSectorFactor() + row;}

inline tcl_tphit_st *StTclHitChair::GetTable(Int_t i) const { return ((St_tcl_tphit *)GetThisTable())->GetTable(i);}

inline UShort_t StTclHitChair::Sector(Int_t i)   const { return UpckSec(GetTable(i)->row);}
inline UShort_t StTclHitChair::PadRow(Int_t i)   const { return UpckRow(GetTable(i)->row);}
inline Long_t   StTclHitChair::TrackId(Int_t i)  const { return UpckTrackId(GetTable(i)->track);}
inline Long_t   StTclHitChair::Position(Int_t i) const { return UpckPosition(GetTable(i)->track);}


inline Bool_t StTclHitChair::IsValid(Int_t i)  const {return (GetTable(i)->z)*(Sector(i)-12.5) <= 0 ? kTRUE : kFALSE; }

inline UShort_t StTclHitChair::SectorRow(Int_t i, UShort_t sector,UShort_t row)
{ return GetTable(i)->row = PckRow(sector,row); }

inline Long_t StTclHitChair::IdPosition(Int_t i, Long_t trackId, Long_t position)
{return GetTable(i)->track = PckTrack(trackId,position);}

#endif

