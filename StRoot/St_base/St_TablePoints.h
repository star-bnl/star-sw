//*-- Author :    Valery Fine   14/05/99  (E-mail: fine@bnl.gov)
// $Id: St_TablePoints.h,v 1.4 1999/12/17 23:28:41 fine Exp $

#ifndef STAR_St_TablePoints
#define STAR_St_TablePoints
// ***********************************************************************
// * Observer to draw use ant St_Table object as an element of "event" geometry
// * Copyright(c) 1997~1999  [BNL] Brookhaven National Laboratory, STAR, All rights reserved
// * Author                  Valerie Fine  (fine@bnl.gov)
// * Copyright(c) 1997~1999  Valerie Fine  (fine@bnl.gov)
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// *
// * Permission to use, copy, modify and distribute this software and its
// * documentation for any purpose is hereby granted without fee,
// * provided that the above copyright notice appear in all copies and
// * that both that copyright notice and this permission notice appear
// * in supporting documentation.  Brookhaven National Laboratory makes no
// * representations about the suitability of this software for any
// * purpose.  It is provided "as is" without express or implied warranty.
// ************************************************************************
#include <TPoints3DABC.h>
#include "St_TableSorter.h"
#include "St_Table.h"

class St_TablePoints : public TPoints3DABC
{
  protected:
    St_TableSorter *m_TableSorter;  
    const void     *m_Key;            // pointer to key value to select rows
    Int_t           m_FirstRow;       // The first row to take in account 
    Int_t           m_Size;
    void           *m_Rows;           // Pointer the first row of the STAF table

    virtual void SetTablePointer(void *table);
    St_TablePoints();
  public:
    St_TablePoints(St_TableSorter *sorter,const void *key,Option_t *opt="");
    St_TablePoints(St_TableSorter *sorter, Int_t keyIndex,Option_t *opt="");
   ~St_TablePoints(){}
    virtual Int_t     DistancetoPrimitive(Int_t px, Int_t py);
    virtual Int_t     GetLastPosition()const;
    virtual Float_t   GetX(Int_t idx)  const = 0;
    virtual Float_t   GetY(Int_t idx)  const = 0;
    virtual Float_t   GetZ(Int_t idx)  const = 0;  
    virtual void     *GetTable();
    virtual Option_t *GetOption()      const { return 0;}
    virtual Int_t     Indx(Int_t sortedIndx) const;
    virtual Int_t     SetLastPosition(Int_t idx);
    virtual void      SetOption(Option_t *){;}
    virtual Int_t     SetPoint(Int_t, Float_t, Float_t, Float_t ){return -1;}
    virtual Int_t     SetPoints(Int_t , Float_t *, Option_t *){return -1;}
    virtual Int_t     Size() const;
    ClassDef(St_TablePoints,0)  // Defines the St_Table as an element of "event" geometry
};

//____________________________________________________________________________
inline void St_TablePoints::SetTablePointer(void *table){ m_Rows = table;}

//____________________________________________________________________________
// return the index of the origial row by its index from the sorted table
   inline Int_t St_TablePoints::Indx(Int_t sortedIndx) const
     {return m_TableSorter?m_TableSorter->GetIndex(m_FirstRow+sortedIndx):-1;}
//____________________________________________________________________________
// return the pointer to the original table object
  inline void *St_TablePoints::GetTable(){
    if (m_TableSorter) {
      St_Table *table = m_TableSorter->GetTable();
      if (table) return table->GetArray(); 
    }
    return 0;
  }
//____________________________________________________________________________
  inline Int_t St_TablePoints::Size() const { return m_Size;}
//____________________________________________________________________________
  inline Int_t St_TablePoints::GetLastPosition() const {return Size()-1;}

//____________________________________________________________________________
  inline Int_t St_TablePoints::SetLastPosition(Int_t idx)
  { 
     Int_t pos = GetLastPosition(); 
     m_Size = TMath::Min(pos,idx)+1;
     return pos; 
  }

//____________________________________________________________________________
// $Log: St_TablePoints.h,v $
// Revision 1.4  1999/12/17 23:28:41  fine
// clean up for the sake of docs + new class St_Table3DPackedPoints introduced
//
// Revision 1.3  1999/11/16 16:29:57  fine
// TObject::GetObjectInfo() implemented
//
// Revision 1.2  1999/11/04 18:03:10  fine
// new ctor for tablepoints introduced to make EventDiplay happy
//
// Revision 1.1  1999/05/18 20:21:25  fine
// New class to 3D table viewer
//____________________________________________________________________________

#endif

