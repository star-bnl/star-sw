#ifndef ROOT_St_TableIter
#define ROOT_St_TableIter
//*-- Author :    Valery Fine(fine@bnl.gov)   03/12/99  
// $Id: St_TableIter.h,v 1.3 1999/12/05 06:34:16 fine Exp $
// Copyright(c) 1997~1999  [BNL] Brookhaven National Laboratory, STAR, All rights reserved
// Author                  Valerie Fine  (fine@bnl.gov)
// Copyright(c) 1997~1999  Valerie Fine  (fine@bnl.gov)
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TableIter - class iterator to loop over sorted St_Table's         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"

class St_TableSorter;

class St_TableIter : public TObject {
 private: 
       const St_TableSorter *m_TableSorter;
       Int_t  m_Indx;
       Int_t  m_TotalKeys;
       Int_t  m_FirstIndx;
 protected:
   St_TableIter(){;}
   St_TableIter(const St_TableIter &){;}
   
 public:
   St_TableIter(const St_TableSorter *table, Float_t  &keyvalue);
   St_TableIter(const St_TableSorter *table, Double_t &keyvalue);
   St_TableIter(const St_TableSorter *table, Int_t    &keyvalue);
   St_TableIter(const St_TableSorter *table, Long_t   &keyvalue);
   St_TableIter(const St_TableSorter *table, Short_t  &keyvalue);

   virtual ~St_TableIter(){;}

   Int_t CountKey(Float_t &keyvalue);
   Int_t CountKey(Long_t &keyvalue);
   Int_t CountKey(Int_t &keyvalue);
   Int_t CountKey(Short_t &keyvalue);
   Int_t CountKey(Double_t &keyvalue);

   Int_t GetNRows() const;
   Int_t Next();
   Int_t Next(Int_t idx);
   Int_t Reset(Int_t indx=0);
   Int_t operator()();
   Int_t operator[](Int_t idx);
   ClassDef(St_TableIter,0) // Iterator over "sorted" St_Table objects
};

//_____________________________________________________________________
inline Int_t St_TableIter::GetNRows() const { return m_TotalKeys;}
//_____________________________________________________________________
inline Int_t St_TableIter::operator()() { return Next();}

//_____________________________________________________________________
inline Int_t St_TableIter::operator[](Int_t idx) { return Next(idx);}

// $Log: St_TableIter.h,v $
// Revision 1.3  1999/12/05 06:34:16  fine
// Several const methods for St_TableSorter introduced
//
// Revision 1.2  1999/12/04 22:41:37  fine
// clean up sole const methods
//
// Revision 1.1  1999/12/04 06:34:31  fine
// St_TableIter - table iterator to loop over sorted tables. First implementation
//
#endif
