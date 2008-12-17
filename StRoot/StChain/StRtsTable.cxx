#include "StRtsTable.h"
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
 TGenericTable(*(TTableDescriptor *)&StRtsTableDescriptor(structLength),1)
 {
   TTableDescriptor *dsc = GetDescriptorPointer();
   if (dsc)  fSize = dsc->Sizeof();
   if ( !dsc || (fSize != structLength) )  {
      assert(0);
      Warning("TGenericTable","Wrong table format");
   }
   if (n > 0) Set(n);
   SetGenericType();
 }
