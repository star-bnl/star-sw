//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_Table3PackedPoints.h,v 1.2 1999/12/19 00:12:45 fine Exp $
// $Log: St_Table3PackedPoints.h,v $
// Revision 1.2  1999/12/19 00:12:45  fine
// some corrections for the packed tables
//
// Revision 1.1  1999/12/17 23:28:41  fine
// clean up for the sake of docs + new class St_Table3DPackedPoints introduced
//
// Revision 1.2  1999/11/04 18:03:04  fine
// new ctor for tablepoints introduced to make EventDiplay happy
//
// Revision 1.1  1999/05/18 20:21:24  fine
// New class to 3D table viewer
//  

#ifndef STAR_St_Table3PackedPoints
#define STAR_St_Table3PackedPoints


#include "St_Table3Points.h"

class St_TableElementDescriptor;

class St_Table3PackedPoints :  public St_Table3Points
{
  private:

       Float_t m_XYZUnpacked[3];  // the last unpacked coordinate
       Int_t   m_LastIndx;        // the index of the last unpacked coordinate
       Float_t m_MaxFactor;       // 2380*tpc 23800*svt 2380*ftpc
       Float_t m_MaxRange;        //  220+tpc    22+svt  270+ftpc
       Int_t   m_DetectorId;      // Id of the current detector (tpc,svt,ftpc)

  protected:
       void SetDetectorId(Int_t detId=1);

  public:
        St_Table3PackedPoints();

        St_Table3PackedPoints(St_TableSorter *sorter,const void *key, const Char_t *xyzName,
                              const Char_t *detector="tpc",Option_t *opt="");

        St_Table3PackedPoints(St_TableSorter *sorter,const void *key, const Char_t *xyzName, 
                              Float_t maxFactor, Float_t maxRange, Option_t *opt="");

        St_Table3PackedPoints(St_TableSorter *sorter,Int_t keyIndex, const Char_t *xyzName, 
                              const Char_t *detector="tpc",Option_t *opt="");

        St_Table3PackedPoints(St_TableSorter *sorter,Int_t keyIndex, const Char_t *xyzName, 
                              Float_t maxFactor, Float_t maxRange, Option_t *opt="");

        ~St_Table3PackedPoints();       
        virtual void    SetAnyColumn(const Char_t *anyName, EPointDirection indx);
        virtual Float_t   GetAnyPoint(Int_t idx, EPointDirection xAxis) const;
                Float_t   GetRealPoint(Long_t unPackgedPoint) const;
                Int_t     GetLastDetectorId() const ;
 //-- abstract methods
         virtual void PaintPoints(int, float*, const char*) {}
         virtual const Float_t *GetXYZ(Int_t) {return 0;}
         virtual Float_t *GetXYZ(Float_t *xyz,Int_t idx, Int_t num=1) const;
         void SetMaxFactor(Float_t maxFactor = 23800);
         void SetMaxRange(Float_t maxRange = 220);

 //
        ClassDef(St_Table3PackedPoints,0)  //A 3-D Points
};

inline Float_t St_Table3PackedPoints::GetRealPoint(Long_t unPackgedPoint) const
{ return Float_t(unPackgedPoint)/m_MaxFactor - m_MaxRange; }

inline Int_t St_Table3PackedPoints::GetLastDetectorId() const { return m_DetectorId; }

inline void St_Table3PackedPoints::SetMaxFactor(Float_t maxFactor){m_MaxFactor = maxFactor;} 

inline void St_Table3PackedPoints::SetMaxRange(Float_t maxRange){m_MaxRange = maxRange;} 
inline void St_Table3PackedPoints::SetDetectorId(Int_t detId){ m_DetectorId = detId;}

 
#endif

