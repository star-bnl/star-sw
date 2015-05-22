#include "StRtsTable.h"

UInt_t StRtsTable::fToken     =0; //< current token
UInt_t StRtsTable::fTrgcmd    =0; //< current trigger command
UInt_t StRtsTable::fDaqcmd    =0; //< current DAQ command
UInt_t StRtsTable::fTrgword   =0; //< the Trigger Word
UInt_t StRtsTable::fPhyword   =0; //< the Physics Word
UInt_t StRtsTable::fDaqbits   =0; //< "offline" bits aka L3 summary...
UInt_t StRtsTable::fDaqbits_l1=0; //< triggers satisfying l1 
UInt_t StRtsTable::fDaqbits_l2=0; //< triggers satisfying l2
UInt_t StRtsTable::fEvpgroups =0; //< evp groups aka L3 summary[2]     

UInt_t StRtsTable::fDetectors=0;  //< detectors present bit mask according to DAQ!

UInt_t StRtsTable::fDetsinrun=0;     //< I nave no idea what it is about (vf)
UInt_t StRtsTable::fEvpgroupsinrun=0;//< I nave no idea what it is about (vf)

class StRtsTableDescriptor : public TTableDescriptor {
public: 
     StRtsTableDescriptor(size_t structLength) : 
     TTableDescriptor(1)
     {
         tableDescriptor_st &row = *GetTable();
         row.fSize = structLength; 
         row.fTypeSize= sizeof(unsigned char);
         row.fType=kUChar;
     }
}; 
//________________________________________________________
StRtsTable::StRtsTable(size_t structLength, Int_t n) : 
 TGenericTable(StRtsTableDescriptor(structLength),1)
 {
   TTableDescriptor *dsc = GetDescriptorPointer();
   if (dsc)  fSize = dsc->Sizeof();
   if ( !dsc || ((size_t)fSize != structLength) )  {
      assert(0);
      Warning("TGenericTable","Wrong table format");
   }
   if (n > 0) Set(n);
   SetGenericType();
 }
