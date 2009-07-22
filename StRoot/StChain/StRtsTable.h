#ifndef STAR_StRtsTable
#define STAR_StRtsTable

#include "TGenericTable.h"

class StRtsTable : public TGenericTable {
   private:
     Int_t  fSector ;
     Int_t  fRow ;
     Int_t  fRdo ;
     Int_t  fPad ;

     //  the global daqReader paramenters (see daqReader.h for details)
     UInt_t fToken ;       // current token
     UInt_t fTrgcmd ;      // current trigger command
     UInt_t fDaqcmd ;      // current DAQ command
     UInt_t fTrgword ;     // the Trigger Word
     UInt_t fPhyword ;     // the Physics Word
     UInt_t fDaqbits ;     // "offline" bits aka L3 summary...
     UInt_t fDaqbits_l1;   // triggers satisfying l1 
     UInt_t fDaqbits_l2;   // triggers satisfying l2
     UInt_t fEvpgroups ;   // evp groups aka L3 summary[2]     
  
     UInt_t fDetectors ;	// detectors present bit mask according to DAQ!

     UInt_t fDetsinrun ;         // I nave no idea what it is about (vf)
     UInt_t fEvpgroupsinrun;     // I nave no idea what it is about (vf)

   public:
      StRtsTable(const char* structName, Int_t n) : TGenericTable(structName,n){}
      StRtsTable(size_t structLength, Int_t n);
      void  SetAll(Int_t sec,Int_t pad,Int_t rdo,Int_t row);
      void  SetSector (Int_t s);
      void  SetPad (Int_t p);
      void  SetRdo (Int_t r);
      void  SetRow (Int_t r);

      Int_t Sector () const;
      Int_t Pad () const;
      Int_t Rdo () const;
      Int_t Row () const;

      UInt_t Token()      const;    // current token
      void   SetToken(UInt_t token);
      UInt_t Trgcmd()     const;    // current trigger command
      void   SetTrgcmd(UInt_t trgcmd);
      UInt_t Daqcmd()     const;    // current DAQ command
      void   SetDaqcmd(UInt_t daqcmd);
      UInt_t Trgword()    const;    // the Trigger Word
      void   SetTrgword(UInt_t trgword);
      UInt_t Phyword()    const;    // the Physics Word
      void   SetPhyword(UInt_t phyword);
      UInt_t Daqbits()    const;    // "offline" bits aka L3 summary...
      void   SetDaqbits(UInt_t daqbits);
      UInt_t Daqbits_l1() const;    // triggers satisfying l1 
      void   SetDaqbits_l1(UInt_t daqbits_l1);
      UInt_t Daqbits_l2() const;    // triggers satisfying l2
      void   SetDaqbits_l2(UInt_t daqbits_l2);
      UInt_t Evpgroups()  const;    // evp groups aka L3 summary[2]     
      void   SetEvpgroups(UInt_t evpgroups);
  
      UInt_t Detectors()  const;	  // detectors present bit mask according to DAQ!
      void   SetDetectors(UInt_t detectors);

     ClassDef(StRtsTable,1);
};

inline  Int_t StRtsTable::Sector() const {return fSector;}
inline  Int_t StRtsTable::Pad() const    {return fPad;}
inline  Int_t StRtsTable::Rdo() const    {return fRdo;}
inline  Int_t StRtsTable::Row() const    {return fRow;}

inline  UInt_t StRtsTable::Token()      const {return fToken;   }    // current token
inline  UInt_t StRtsTable::Trgcmd()     const {return fTrgcmd;  }    // current trigger command
inline  UInt_t StRtsTable::Daqcmd()     const {return fDaqcmd;  }    // current DAQ command
inline  UInt_t StRtsTable::Trgword()    const {return fTrgword; }    // the Trigger Word
inline  UInt_t StRtsTable::Phyword()    const {return fPhyword; }    // the Physics Word
inline  UInt_t StRtsTable::Daqbits()    const {return fDaqbits; }    // "offline" bits aka L3 summary...
inline  UInt_t StRtsTable::Daqbits_l1() const {return fDaqbits_l1;}  // triggers satisfying l1 
inline  UInt_t StRtsTable::Daqbits_l2() const {return fDaqbits_l2;}  // triggers satisfying l2
inline  UInt_t StRtsTable::Evpgroups()  const {return fEvpgroups;}   // evp groups aka L3 summary[2]     
  
inline  UInt_t StRtsTable::Detectors()  const {return fDetectors;}   // detectors present bit mask according to DAQ!

      
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
