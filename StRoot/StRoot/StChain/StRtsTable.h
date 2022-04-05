#ifndef STAR_StRtsTable
#define STAR_StRtsTable

#include "TGenericTable.h"

class StRtsTable : public TGenericTable {
   private:
     Int_t  fSector ;
     Int_t  fRow ;
     Int_t  fRdo ;
     Int_t  fPad ;
     void*  fMeta;  //meta data is not copied, and just getting pointer. 
                    //No gurantee to be valid after another reqquest is made to daqReader.

     //  the global daqReader paramenters (see daqReader.h for details)
     static UInt_t fToken ;       // current token
     static UInt_t fTrgcmd ;      // current trigger command
     static UInt_t fDaqcmd ;      // current DAQ command
     static UInt_t fTrgword ;     // the Trigger Word
     static UInt_t fPhyword ;     // the Physics Word
     static UInt_t fDaqbits ;     // "offline" bits aka L3 summary...
     static UInt_t fDaqbits_l1;   // triggers satisfying l1 
     static UInt_t fDaqbits_l2;   // triggers satisfying l2
     static UInt_t fEvpgroups ;   // evp groups aka L3 summary[2]     

     static UInt_t fDetectors ;	// detectors present bit mask according to DAQ!

     static UInt_t fDetsinrun ;         // I nave no idea what it is about (vf)
     static UInt_t fEvpgroupsinrun;     // I nave no idea what it is about (vf)

   public:
      StRtsTable(const char* structName, Int_t n) : TGenericTable(structName,n){}
      StRtsTable(size_t structLength, Int_t n);
      void  SetAll(Int_t sec,Int_t pad,Int_t rdo,Int_t row);
      void  SetSector (Int_t s);
      void  SetPad (Int_t p);
      void  SetRdo (Int_t r);
      void  SetRow (Int_t r);
      void  SetMeta (void* v);

      Int_t Sector () const;
      Int_t Pad () const;
      Int_t Rdo () const;
      Int_t Row () const; 
      const void* Meta() const;

      static UInt_t Token();               // current token
      static void   SetToken(UInt_t token);
      static UInt_t Trgcmd();              // current trigger command
      static void   SetTrgcmd(UInt_t trgcmd);
      static UInt_t Daqcmd();              // current DAQ command
      static void   SetDaqcmd(UInt_t daqcmd);
      static UInt_t Trgword();             // the Trigger Word
      static void   SetTrgword(UInt_t trgword);
      static UInt_t Phyword();             // the Physics Word
      static void   SetPhyword(UInt_t phyword);
      static UInt_t Daqbits();             // "offline" bits aka L3 summary...
      static void   SetDaqbits(UInt_t daqbits);
      static UInt_t Daqbits_l1();          // triggers satisfying l1 
      static void   SetDaqbits_l1(UInt_t daqbits_l1);
      static UInt_t Daqbits_l2();          // triggers satisfying l2
      static void   SetDaqbits_l2(UInt_t daqbits_l2);
      static UInt_t Evpgroups();          // evp groups aka L3 summary[2]     
      static void   SetEvpgroups(UInt_t evpgroups);
  
      static UInt_t Detectors();          // detectors present bit mask according to DAQ!
      static void   SetDetectors(UInt_t detectors);

     ClassDef(StRtsTable,1);
};

inline  Int_t StRtsTable::Sector() const {return fSector;}
inline  Int_t StRtsTable::Pad() const    {return fPad;}
inline  Int_t StRtsTable::Rdo() const    {return fRdo;}
inline  Int_t StRtsTable::Row() const    {return fRow;}
inline  const void*  StRtsTable::Meta() const {return fMeta;}

inline  UInt_t StRtsTable::Token()        {return fToken;   }    // current token
inline  UInt_t StRtsTable::Trgcmd()       {return fTrgcmd;  }    // current trigger command
inline  UInt_t StRtsTable::Daqcmd()       {return fDaqcmd;  }    // current DAQ command
inline  UInt_t StRtsTable::Trgword()      {return fTrgword; }    // the Trigger Word
inline  UInt_t StRtsTable::Phyword()      {return fPhyword; }    // the Physics Word
inline  UInt_t StRtsTable::Daqbits()      {return fDaqbits; }    // "offline" bits aka L3 summary...
inline  UInt_t StRtsTable::Daqbits_l1()   {return fDaqbits_l1;}  // triggers satisfying l1 
inline  UInt_t StRtsTable::Daqbits_l2()   {return fDaqbits_l2;}  // triggers satisfying l2
inline  UInt_t StRtsTable::Evpgroups()    {return fEvpgroups;}   // evp groups aka L3 summary[2]     
  
inline  UInt_t StRtsTable::Detectors()   {return fDetectors;}   // detectors present bit mask according to DAQ!

      
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
inline void  StRtsTable::SetMeta   (void* v) {fMeta  = v;}

inline void   StRtsTable::SetToken(UInt_t token)          { fToken      = token;      }
inline void   StRtsTable::SetTrgcmd(UInt_t trgcmd)        { fTrgcmd     = trgcmd;     }
inline void   StRtsTable::SetDaqcmd(UInt_t daqcmd)        { fDaqcmd     = daqcmd;     }
inline void   StRtsTable::SetTrgword(UInt_t trgword)      { fTrgword    = trgword;    }
inline void   StRtsTable::SetPhyword(UInt_t phyword)      { fPhyword    = phyword;    }
inline void   StRtsTable::SetDaqbits(UInt_t daqbits)      { fDaqbits    = daqbits;    }
inline void   StRtsTable::SetDaqbits_l1(UInt_t daqbits_l1){ fDaqbits_l1 = daqbits_l1; }
inline void   StRtsTable::SetDaqbits_l2(UInt_t daqbits_l2){ fDaqbits_l2 = daqbits_l2; }
inline void   StRtsTable::SetEvpgroups(UInt_t evpgroups)  { fEvpgroups  = evpgroups;  }
inline void   StRtsTable::SetDetectors(UInt_t detectors)  { fDetectors  = detectors;  }

#endif
