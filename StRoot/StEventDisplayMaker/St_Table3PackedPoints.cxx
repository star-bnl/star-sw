//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_Table3PackedPoints.cxx,v 1.2 2000/01/24 22:56:46 fine Exp $

#include <assert.h>

#include  "St_Table3PackedPoints.h"
#include  "St_TableElementDescriptor.h"

///////////////////////////////////////////////////////////////////////////////////
//
//  St_Table3PackedPoints class is to create 3D view of column with packed 3D coordinates
//  of the St_Table objects  with one the same "key column value".
//
///////////////////////////////////////////////////////////////////////////////////

ClassImp(St_Table3PackedPoints)


//________________________________________________________________________________
static Int_t detectId(const Char_t *name="tpc") 
{
  //
  // return > 0 detector index by its name
  //        = 0 wrong detector name found
  //
  const Char_t *detectors[] = {"tpc", "svt" ,"ftpc","ssd"};
  Int_t l = sizeof(detectors)/sizeof(Char_t *);
  for (Int_t i=0; i < l ; i++) 
     if ( strcmp(name,detectors[i]) ) return i+1;
  return 0;
}
//________________________________________________________________________________
static Float_t factor(Float_t &range,Int_t detId) 
{

//     see: CC: FILE:       dst_point_filler.F (looked up on 24.01.2000 )
//      parameter(detid_tpc  = 1)
//      parameter(detid_svt  = 2)
//      parameter(detid_ftpc = 3)
//      parameter(detid_ssd  = 8) // == 0

  const Float_t factors[]   = {2380 , 23800 , 2380, 0, 0, 0, 0, 16000 };
  const Float_t ranges[]    = { 220 ,  22   ,  270, 0, 0, 0, 0,  40   };
  assert(detId <= sizeof(factors)/4);
  Float_t ret = factors[1];
  range =  ranges[1];
  if (detId) {
    range = ranges[detId-1];
    ret = factors[detId-1]; 
  }
  return ret; 
}
//________________________________________________________________________________
static inline Float_t factor(Float_t &range,const Char_t *name="tpc") 
{
  return factor(range,detectId(name));
}

//________________________________________________________________________________
St_Table3PackedPoints::St_Table3PackedPoints(): m_LastIndx(-1){}

//________________________________________________________________________________
St_Table3PackedPoints::St_Table3PackedPoints(St_TableSorter *sorter,const void *key,
                       const Char_t *xyzName,const Char_t *detector,Option_t *opt)
                : St_Table3Points(sorter,key,xyzName,"","",opt),m_LastIndx(-1)

{ 
  SetDetectorId(detectId(detector));
  m_MaxFactor = factor(m_MaxRange,GetLastDetectorId()); 
}
//________________________________________________________________________________
St_Table3PackedPoints::St_Table3PackedPoints(St_TableSorter *sorter,const void *key,
                       const Char_t *xyzName,Float_t maxFactor,Float_t maxRange,Option_t *opt)
                : St_Table3Points(sorter,key,xyzName,"","",opt),m_LastIndx(-1),
                  m_MaxFactor(maxFactor), m_MaxRange(maxRange)
{ }

//________________________________________________________________________________
St_Table3PackedPoints::St_Table3PackedPoints(St_TableSorter *sorter,Int_t keyIndex,
                       const Char_t *xyzName,const Char_t *detector,Option_t *opt)
                : St_Table3Points(sorter,keyIndex,xyzName,"","",opt),m_LastIndx(-1)
{
  SetDetectorId(detectId(detector));
  m_MaxFactor = factor(m_MaxRange,GetLastDetectorId()); 
}
//________________________________________________________________________________
St_Table3PackedPoints::St_Table3PackedPoints(St_TableSorter *sorter,Int_t keyIndex,
                       const Char_t *xyzName,Float_t maxFactor,Float_t maxRange,Option_t *opt)
                : St_Table3Points(sorter,keyIndex,xyzName,"","",opt),m_LastIndx(-1),
                  m_MaxFactor(maxFactor), m_MaxRange(maxRange)
{ SetMaxFactor(); SetMaxRange();}

//________________________________________________________________________________
St_Table3PackedPoints::~St_Table3PackedPoints() {}

//________________________________________________________________________________
Float_t St_Table3PackedPoints::GetAnyPoint(Int_t idx, EPointDirection xAxis) const 
{
  if ( m_LastIndx != idx) GetXYZ(0,idx);
  return m_XYZUnpacked[xAxis];
}
//____________________________________________________________________________
void St_Table3PackedPoints::SetAnyColumn(const Char_t *anyName, EPointDirection indx)
{
   assert(0 && anyName && indx);
}

//____________________________________________________________________________
Float_t *St_Table3PackedPoints::GetXYZ(Float_t *xyz,Int_t idx, Int_t num) const
{
 St_Table  *table = 0;
 if (m_TableSorter) table = m_TableSorter->GetTable();
 if (table) {
    Int_t size = TMath::Min(idx+num,Size());
    const Char_t *tablePtr = (Char_t *)m_Rows;
    Int_t rowSize = table->GetRowSize();
    St_TableElementDescriptor *tableDsc = GetDescriptor(kXPoints);
    Long_t offSet = tableDsc->GetOffset();

    for (Int_t i=idx;i<size;i++) {
      tablePtr += Indx(i)*rowSize + offSet;     
      ULong_t *pos = (ULong_t *)tablePtr;  
      ULong_t *pos1 = pos+1;

      Int_t detId = (*(pos-1)) & 0xF;
      if (GetLastDetectorId() != detId ) {
        ((St_Table3PackedPoints *)this)->SetDetectorId(detId);
        ((St_Table3PackedPoints *)this)->m_MaxFactor = factor(((St_Table3PackedPoints *)this)->m_MaxRange,detId);        
      }

      ULong_t x   =                      (*pos) & 0x000FFFFF;
      ULong_t y   = (((*pos1) & 0x3FF) << 10)  | (*pos  >> 20);
      ULong_t z   =  (*pos1) >> 10;

      ((St_Table3PackedPoints *)this)->m_XYZUnpacked[0] = GetRealPoint(x);

      ((St_Table3PackedPoints *)this)->m_XYZUnpacked[1] = GetRealPoint(y);

      ((St_Table3PackedPoints *)this)->m_XYZUnpacked[2] = GetRealPoint(z);
      ((St_Table3PackedPoints *)this)->m_LastIndx = i;

       if (xyz) 
         memcpy(xyz,m_XYZUnpacked,sizeof(m_XYZUnpacked));
    }
  }
  return xyz;
}
//____________________________________________________________________________
// $Log: St_Table3PackedPoints.cxx,v $
// Revision 1.2  2000/01/24 22:56:46  fine
// new packing schema for ssd introduced
//
// Revision 1.1  1999/12/20 17:32:29  fine
// St_Table3Point has been removed from St_base
//
// Revision 1.3  1999/12/20 01:40:54  fine
// correction for detid_ssd
//
// Revision 1.2  1999/12/19 00:12:45  fine
// some corrections for the packed tables
//
// Revision 1.1  1999/12/17 23:28:41  fine
// clean up for the sake of docs + new class St_Table3DPackedPoints introduced
//
//____________________________________________________________________________
//  bfc parameters to test this class:
// x bfc.C(20,"off tdaq tpc global dst xout","/afs/rhic/star/data4/samples/st_physics_0349044_raw_0001.daq")
