//*-- Author :    Valery Fine   14/05/99  (E-mail: fine@bnl.gov)
// $Id: St_TablePoints.cxx,v 1.3 1999/11/16 16:29:57 fine Exp $
// $Log: St_TablePoints.cxx,v $
// Revision 1.3  1999/11/16 16:29:57  fine
// TObject::GetObjectInfo() implemented
//
// Revision 1.2  1999/11/04 18:03:10  fine
// new ctor for tablepoints introduced to make EventDiplay happy
//
// Revision 1.1  1999/05/18 20:21:25  fine
// New class to 3D table viewer
//  

#include "St_TablePoints.h"

///////////////////////////////////////////////////////////////////////////////////////
//                                                                                   //
//                                                                                   //
//  #include "St_TablePoints.h"                                                      //
//  #include "St_<your_table_name_here>.h"                                           //
//                                                                                   //
//  class St_<your_table_name_here>_Points : public St_TablePoints                   //
//  {                                                                                //
//    protected:                                                                     //
//       <your_table_name_here>_st   *m_Rows; // pointer to the STAF table           //
//    public:                                                                        //
//       St_<your_table_name_here>_Points(St_TableSorter *sorter,const void *key,Option_t *opt):
//                St_TablePoints(sorter,key,opt){}                                   //
//       virtual  ~St_<your_table_name_here>_Points(){} // default destructor        //
//       virtual Float_t GetX(Int_t indx) { return ((<your_table_name_here>_st *)m_Rows)[Indx(idx)]-> <x>;}               //
//       virtual Float_t GetY(Int_t indx) { return ((<your_table_name_here>_st *)m_Rows)[Indx(idx)]-> <y>;}               //
//       virtual Float_t GetZ(Int_t indx) { return ((<your_table_name_here>_st *)m_Rows)[Indx(idx)]-> <z>;}               //
//  };                                                                               //
//                                                                                   //
///////////////////////////////////////////////////////////////////////////////////////

ClassImp(St_TablePoints)

//____________________________________________________________________________
St_TablePoints::St_TablePoints()
{
  m_TableSorter =  0;
  m_Key         =  0;
  m_FirstRow    = -1;
  m_Size        =  0;
}
//____________________________________________________________________________
St_TablePoints::St_TablePoints(St_TableSorter *sorter,const void *key,Option_t *opt)
{
  m_TableSorter =  0;
  m_Key         =  0;
  m_FirstRow    = -1;
  m_Size        =  0;
  if (sorter) {
     m_TableSorter = sorter;
     m_Key         = key;
     m_Size        = sorter->CountKey(m_Key,0,kTRUE,&m_FirstRow);
     SetTablePointer(GetTable());
  }
  SetOption(opt);
}

//____________________________________________________________________________
St_TablePoints::St_TablePoints(St_TableSorter *sorter, Int_t keyIndex,Option_t *opt)
{
  m_TableSorter =  0;
  m_Key         =  0;
  m_FirstRow    = -1;
  m_Size        =  0;
  if (sorter) {
     m_TableSorter = sorter;
     m_Key         = sorter->GetKeyAddress(keyIndex);
     m_Size        = sorter->CountKey(m_Key,keyIndex,kFALSE,&m_FirstRow);
     SetTablePointer(GetTable());
  }
  SetOption(opt); 
}

//______________________________________________________________________________
Int_t St_TablePoints::DistancetoPrimitive(Int_t px, Int_t py)
{
//*-*-*-*-*-*-*Compute distance from point px,py to a 3-D points *-*-*-*-*-*-*
//*-*          =====================================================
//*-*
//*-*  Compute the closest distance of approach from point px,py to each segment
//*-*  of the polyline.
//*-*  Returns when the distance found is below DistanceMaximum.
//*-*  The distance is computed in pixels units.
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
   return -1;
}

