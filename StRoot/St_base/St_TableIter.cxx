//*-- Author :    Valery Fine(fine@bnl.gov)   03/12/99  
// Copyright(c) 1997~1999  [BNL] Brookhaven National Laboratory, STAR, All rights reserved
// Author                  Valerie Fine  (fine@bnl.gov)
// Copyright(c) 1997~1999  Valerie Fine  (fine@bnl.gov)
// $Id: St_TableIter.cxx,v 1.5 2000/02/29 01:54:49 fine Exp $
///////////////////////////////////////////////////////////////////////////
//                                                                       //
// St_TableIter - class iterator to loop over sorted St_Table's          //
//                                                                       //
// I allows to loop over the sub-set of the St_Table rows defined by     //
// the foreing key                                                       //
//                                                                       //
// St_TableIter nextVertex(sortedVertex,track[indxRow].stop_vertex_p);   //
//                                                                       //
// if (nextVertex.GetNRows() > 0) {                                      // 
//   St_g2t_vertex &vertex = *((St_g2t_vertex*)sortedVertex->GetTable());//
//   while ( (indxRow = nextVertex() ) >=0 ) {                           //
//     Float_t *x   = &(vertex[indxRow].ge_x[0]);                        //
//     Float_t dist = x[0]*x[0]+x[1]*x[1];                               //
//     Bool_t condition = dist > 133*133 && dist < 179*179;              //
//     if ((vertex[indxRow].ge_proc == 5) && condition )                 //
//                      return color%8+1;                                //
//   }                                                                   //
// }                                                                     //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include "St_TableIter.h"
#include "St_TableSorter.h"

ClassImp(St_TableIter)
//_____________________________________________________________________
St_TableIter::St_TableIter(const St_TableSorter *table, Float_t &keyvalue) 
: m_TableSorter(table), m_Indx(0), m_FirstIndx(0) 
{
  CountKey(keyvalue);
}
//_____________________________________________________________________
St_TableIter::St_TableIter(const St_TableSorter *table, Long_t &keyvalue) 
: m_TableSorter(table), m_Indx(0), m_FirstIndx(0) 
{
  CountKey(keyvalue);
}

//_____________________________________________________________________
St_TableIter::St_TableIter(const St_TableSorter *table, Int_t &keyvalue) 
: m_TableSorter(table), m_Indx(0), m_FirstIndx(0) 
{
  CountKey(keyvalue);
}

//_____________________________________________________________________
St_TableIter::St_TableIter(const St_TableSorter *table, Short_t &keyvalue) 
: m_TableSorter(table), m_Indx(0), m_FirstIndx(0) 
{
  CountKey(keyvalue);
}

//_____________________________________________________________________
St_TableIter::St_TableIter(const St_TableSorter *table, Double_t &keyvalue) 
: m_TableSorter(table), m_Indx(0), m_FirstIndx(0) 
{
  CountKey(keyvalue);
}

//_____________________________________________________________________
Int_t St_TableIter::CountKey(Float_t &keyvalue) 
{
   m_TotalKeys = m_TableSorter->CountKey(&keyvalue,0,kTRUE,&m_FirstIndx);
   return GetNRows();
}
//_____________________________________________________________________
Int_t St_TableIter::CountKey(Long_t &keyvalue) 
{
   m_TotalKeys = m_TableSorter->CountKey(&keyvalue,0,kTRUE,&m_FirstIndx);
   return GetNRows();
}

//_____________________________________________________________________
Int_t St_TableIter::CountKey(Int_t &keyvalue) 
{
   m_TotalKeys = m_TableSorter->CountKey(&keyvalue,0,kTRUE,&m_FirstIndx);
   return GetNRows();
}

//_____________________________________________________________________
Int_t St_TableIter::CountKey(Short_t &keyvalue) 
{
   m_TotalKeys = m_TableSorter->CountKey(&keyvalue,0,kTRUE,&m_FirstIndx);
   return GetNRows();
}

//_____________________________________________________________________
Int_t St_TableIter::CountKey(Double_t &keyvalue) 
{
   m_TotalKeys = m_TableSorter->CountKey(&keyvalue,0,kTRUE,&m_FirstIndx);
   return GetNRows();
}

//_____________________________________________________________________
Int_t St_TableIter::Next(){
   Int_t rowIndx = -1;
   if (m_Indx < m_TotalKeys) {
     rowIndx = m_TableSorter->GetIndex(UInt_t(m_FirstIndx+m_Indx));
     m_Indx++;
   }
   return rowIndx;
}

//_____________________________________________________________________
Int_t St_TableIter::Next(Int_t idx){
   Int_t rowIndx = -1;   
   if (idx < m_TotalKeys) 
     rowIndx = m_TableSorter->GetIndex(UInt_t(m_FirstIndx+idx));
   return rowIndx;
}

//_____________________________________________________________________
Int_t St_TableIter::Reset(Int_t indx){
   Int_t oldIdx = m_Indx;
   m_Indx = TMath::Min(indx,m_TotalKeys);
   return oldIdx;
}
