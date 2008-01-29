#ifndef STAR_StRtsTable
#define STAR_StRtsTable

#include "TGenericTable.h"

class StRtsTable : public TGenericTable {
   private:
     Int_t  fSector ;
     Int_t  fRow ;
     Int_t  fRdo ;
     Int_t  fPad ;

   public:
      StRtsTable(const char* structName, Int_t n) : TGenericTable(structName,n){}
      void  SetAll(Int_t sec,Int_t pad,Int_t rdo,Int_t row);
      void  SetSector (Int_t s);
      void  SetPad (Int_t p);
      void  SetRdo (Int_t r);
      void  SetRow (Int_t r);

      Int_t Sector () const;
      Int_t Pad () const;
      Int_t Rdo () const;
      Int_t Row () const;

      ClassDef(StRtsTable,1);
};

inline  Int_t StRtsTable::Sector() const {return fSector;}
inline  Int_t StRtsTable::Pad() const    {return fPad;}
inline  Int_t StRtsTable::Rdo() const    {return fRdo;}
inline  Int_t StRtsTable::Row() const    {return fRow;}

inline void  StRtsTable::SetAll(Int_t sec,Int_t pad,Int_t rdo,Int_t row)
{
   SetSector(sec);
   SetPad(pad);
   SetRdo(rdo);
   SetRow(row);
}

inline void  StRtsTable::SetSector (Int_t s) {fSector= s;}
inline void  StRtsTable::SetPad    (Int_t p) {fPad   = p;}
inline void  StRtsTable::SetRdo    (Int_t r) {fRdo   = r;}
inline void  StRtsTable::SetRow    (Int_t r) {fRow   = r;}

#endif
