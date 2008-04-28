#include "StRtsTable.h"

//________________________________________________________
StRtsTable::StRtsTable(size_t structLength, Int_t n) : 
 TGenericTable(TTableDescriptor(1),n)
 {
   TTableDescriptor *dsc = GetDescriptorPointer();
   if (dsc) {
      tableDescriptor_st &row = (*dsc)[0];
//         row.fColumnName;
         row.fSize = structLength; 
         row.fTypeSize= sizeof(unsigned char);
         row.fType=kUChar;
      fSize = dsc->Sizeof();
   }
   if ( !dsc || !fSize ) Warning("TGenericTable","Wrong table format");
   if (n > 0) Set(n);
   SetGenericType();
 }
