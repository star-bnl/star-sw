//*-- Author :    Valery Fine   09/08/99  (E-mail: fine@bnl.gov)
// $Id: St_tableDescriptor.cxx,v 1.15 2000/03/07 23:52:21 fine Exp $
#include <stdlib.h> 
#include "St_tableDescriptor.h"
#include "St_Table.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "Stypes.h"                       

St_tableDescriptor *St_tableDescriptor::fgColDescriptors = 0; 
TableImp(tableDescriptor)                     

//______________________________________________________________________________
void St_tableDescriptor::Streamer(TBuffer &R__b) 
{
  // This class needs a special version of Streamer because of pointers
  if (!R__b.IsReading()) R__b.WriteVersion(St_tableDescriptor::IsA());
  St_Table::Streamer(R__b);
}                                             
//______________________________________________________________________________
St_tableDescriptor::St_tableDescriptor(const St_Table *parentTable)
 : St_Table("tableDescriptor",sizeof(tableDescriptor_st))
{ 
  SetType("tableDescriptor");
  if (parentTable) {
      SetName(parentTable->GetName());
      TClass *classPtr = parentTable->GetRowClass();
      if (classPtr) LearnTable(classPtr); 
      else MakeZombie();
  }  
}
//______________________________________________________________________________
St_tableDescriptor::St_tableDescriptor(TClass *classPtr)
 : St_Table("tableDescriptor",sizeof(tableDescriptor_st))
{ 
  // Create a descriptor of the C-structure defined by TClass
  // TClass *classPtr must be a valid pointer to TClass object for
  // "plain" C_struture only !!! 
  SetType("tableDescriptor");
  if (classPtr) {
      SetName(classPtr->GetName());
      LearnTable(classPtr); 
  }  
}

//______________________________________________________________________________
St_tableDescriptor::~St_tableDescriptor()
{
#ifdef NORESTRICTIONS
  if (!IsZombie()) {
    for (Int_t i=0;i<GetNRows();i++) {
      Char_t *name = (Char_t *)ColumnName(i);
      if (name) delete [] name; 
      UInt_t  *indxArray = (UInt_t *)IndexArray(i);
      if (indxArray) delete [] indxArray; 
    }
  }
#endif
}
//____________________________________________________________________________
TString St_tableDescriptor::CreateLeafList() const 
{
  // Create a list of leaf to be useful for TBranch::TBranch ctor
  const Char_t TypeMapTBranch[]="\0FIISDiisbBC";
  Int_t maxRows = NumberOfColumns();
  TString string;
  for (Int_t i=0;i<maxRows;i++){
    if (i) string += ":";
    Int_t nDim = Dimensions(i);
    if (nDim) {
       UInt_t *indx = IndexArray(i);
       if (indx) {
         const Char_t *colName = ColumnName(i);
         Char_t digBuffer[100];       
         for (Int_t j=0;j<nDim;j++) {
	   for (UInt_t l=0;l<indx[j];l++){
	     if (l) string += ":";
	     sprintf(digBuffer,"%s_%d",colName, l);
	     string += digBuffer;
	     if (l==0) { string += "/"; string += TypeMapTBranch[ColumnType(i)];}
	   }
         }
       }
       else {
         string = ""; 
         Error("CreateLeafList()","Can not create leaflist for arrays");
         break;
      }
    } else {
      string += ColumnName(i);
      string += "/";
      string += TypeMapTBranch[ColumnType(i)];
    }
  }
  return string;
}

//____________________________________________________________________________
void St_tableDescriptor::LearnTable(const St_Table *parentTable)
{
  if (!parentTable) {
    MakeZombie();
    return;
  }
  LearnTable(parentTable->GetRowClass());
}

//____________________________________________________________________________
void St_tableDescriptor::LearnTable(TClass *classPtr)
{
//
//  LearnTable() creates an array of the descriptors for elements of the row
//
// It creates a descriptor of the C-structure defined by TClass
// TClass *classPtr must be a valid pointer to TClass object for
// "plain" C_struture only !!! 
//
//  This is to introduce an artificial restriction demanded by STAR database group
//
//    1. the name may be 31 symbols at most
//    2. the number the dimension is 3 at most
//
//  To lift this restriction one has to provide -DNORESTRICTIONS CPP symbol and
//  recompile code (and debug code NOW!)
//

  if (!classPtr) return;

  if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
  if (!(classPtr->GetNdata())) return;

  const Char_t *types;
  Char_t *varname;

  tableDescriptor_st elementDescriptor;

  ReAllocate(classPtr->GetListOfDataMembers()->GetSize());
  Int_t columnIndex = 0;
  TIter next(classPtr->GetListOfDataMembers());
  TDataMember *member = 0;
  while ( (member = (TDataMember *) next()) ) {
    memset(&elementDescriptor,0,sizeof(tableDescriptor_st));
    varname = (Char_t *) member->GetName();
#ifdef NORESTRICTIONS
//  This is remove to introduce an artificial restriction demanded by STAR infrastructure group
                                             elementDescriptor.m_ColumnName = StrDup(varname);
#else
                                             elementDescriptor.m_ColumnName[0] = '\0';
         strncat(elementDescriptor.m_ColumnName,varname,sizeof(elementDescriptor.m_ColumnName));
#endif
    // define index
    TDataType *memberType = member->GetDataType();
                                              elementDescriptor.m_TypeSize = memberType->Size(); 
    types = memberType->GetTypeName();
    elementDescriptor.m_Type = kNAN;
    if (!strcmp("float", types))              elementDescriptor.m_Type = kFloat;  
    else if (!strcmp("int", types))           elementDescriptor.m_Type = kInt;  
    else if (!strcmp("long", types))          elementDescriptor.m_Type = kLong;  
    else if (!strcmp("short", types))         elementDescriptor.m_Type = kShort;  
    else if (!strcmp("double", types))        elementDescriptor.m_Type = kDouble;  
    else if (!strcmp("unsigned int", types))  elementDescriptor.m_Type = kUInt;  
    else if (!strcmp("unsigned long", types)) elementDescriptor.m_Type = kULong ; 
    else if (!strcmp("unsigned short", types))elementDescriptor.m_Type = kUShort; 
    else if (!strcmp("unsigned char", types)) elementDescriptor.m_Type = kUChar;
    else if (!strcmp("char", types))          elementDescriptor.m_Type = kChar;

    Int_t globalIndex = 1;
    if (elementDescriptor.m_Type != kNAN) {
      Int_t dim = 0;
      if ( (dim = member->GetArrayDim()) ) {
                                              elementDescriptor.m_Dimensions = dim;
#ifdef NORESTRICTIONS
                                              elementDescriptor.m_IndexArray = new UInt_t(dim);
#else
       UInt_t maxDim = sizeof(elementDescriptor.m_IndexArray)/sizeof(UInt_t *);
       if (UInt_t(dim) > maxDim) {
                Error("LearnTable","Too many dimenstions - %d", dim);
                dim =  maxDim;
       }
#endif
        for( Int_t indx=0; indx < dim; indx++ ){
                                              elementDescriptor.m_IndexArray[indx] = member->GetMaxIndex(indx);
          globalIndex *= elementDescriptor.m_IndexArray[indx];
        }
      } 
    }
    else Error("LearnTable","Wrong data type for <%s> structure",classPtr->GetName());
    elementDescriptor.m_Size   =  globalIndex * (elementDescriptor.m_TypeSize);
    elementDescriptor.m_Offset = member->GetOffset();
    AddAt(&elementDescriptor,columnIndex); columnIndex++;
  }
}
//______________________________________________________________________________
Int_t St_tableDescriptor::UpdateOffsets(const St_tableDescriptor *newDescriptor)
{
  //                  "Schema evolution"
  // Method updates the offsets with a new ones from another descritor
  //
  Int_t maxColumns = NumberOfColumns();
  Int_t mismathes = 0;

  if (   (UInt_t(maxColumns) == newDescriptor->NumberOfColumns()) 
      && (memcmp(GetArray(),newDescriptor->GetArray(),sizeof(tableDescriptor_st)*GetNRows()) == 0)
     ) return mismathes; // everything fine for sure !

  // Something wrong here, we have to check things piece by piece
  for (Int_t colCounter=0; colCounter < maxColumns; colCounter++) 
  {
    Int_t colNewIndx = newDescriptor->ColumnByName(ColumnName(colCounter));
    // look for analog
    if (    colNewIndx >=0
         && Dimensions(colCounter) == newDescriptor->Dimensions(colNewIndx) 
         && ColumnType(colCounter) == newDescriptor->ColumnType(colNewIndx)
       )  {
      SetOffset(newDescriptor->Offset(colNewIndx),colCounter); 
      if (colNewIndx != colCounter) {
        printf("Schema evolution: \t%d column of the \"%s\" table has been moved to %d-th column\n",
        colCounter,ColumnName(colCounter),colNewIndx);  
        mismathes++;
      }
    }
    else {
      printf("Schema evolution: \t%d column of the \"%s\" table has been lost\n",
        colCounter,ColumnName(colCounter));  
      printf(" Indx = %d, name = %s \n", colNewIndx, ColumnName(colCounter));
      SetOffset(-1,colCounter);
      mismathes++;
    }
  } 
  return mismathes;
}
//____________________________________________________________________________
const Int_t St_tableDescriptor::ColumnByName(const Char_t *columnName) const 
{ 
 // Find the column index but the column name
 const tableDescriptor_st *elementDescriptor = ((St_tableDescriptor *)this)->GetTable();
 Int_t i = -1;
 if (!elementDescriptor) return i;
 Int_t nRows = GetNRows();
 if (nRows) {
   for (i=0; i < nRows; i++,elementDescriptor++) 
     if (strcmp(columnName,elementDescriptor->m_ColumnName) == 0) break;   
 }
 if (i==nRows) i = -1;
 return i;
}
//____________________________________________________________________________
Int_t St_tableDescriptor::Offset(const Char_t *columnName) const 
{  
  Int_t indx = ColumnByName(columnName);
  if (indx >= 0 ) indx = Offset(indx);
  return indx;
}
//____________________________________________________________________________
Int_t St_tableDescriptor::ColumnSize(const Char_t *columnName) const 
{ 
  Int_t indx = ColumnByName(columnName);
  if (indx >= 0 ) indx = ColumnSize(indx);
  return indx;
}
//____________________________________________________________________________
Int_t St_tableDescriptor::TypeSize(const Char_t *columnName) const 
{
  Int_t indx = ColumnByName(columnName);
  if (indx >= 0 ) indx = TypeSize(indx);
  return indx;
}
//____________________________________________________________________________
Int_t St_tableDescriptor::Dimensions(const Char_t *columnName) const 
{
  Int_t indx = ColumnByName(columnName);
  if (indx >= 0 ) indx = Dimensions(indx);
  return indx;
}
//____________________________________________________________________________
St_Table::EColumnType St_tableDescriptor::ColumnType(const Char_t *columnName) const 
{
  Int_t indx = ColumnByName(columnName);
  if (indx >= 0 ) indx = ColumnType(indx);
  return EColumnType(indx);
}

//____________________________________________________________________________
// $Log: St_tableDescriptor.cxx,v $
// Revision 1.15  2000/03/07 23:52:21  fine
// some comments improved
//
// Revision 1.14  2000/03/05 04:11:48  fine
// Automatic schema evolution for St_Table has been activated
//
// Revision 1.13  2000/02/29 22:13:56  fine
// Compare method fixed
//
// Revision 1.12  2000/02/29 01:54:49  fine
// St_Table -> turn automatic schema evolution for table version 2 and above
//
// Revision 1.11  2000/01/25 22:36:40  fine
// Linux warning has been removed
//
// Revision 1.10  2000/01/25 22:25:46  fine
// the number of dimensions for table arrays became 3 instead of 2
//
// Revision 1.9  2000/01/25 02:17:04  fine
// CreateLeafList and dtor have been fixed
//
// Revision 1.8  2000/01/24 04:02:53  fine
// CreateLeafList(): some extra protection
//
// Revision 1.7  2000/01/24 03:55:47  fine
// new nethod CreateLeafList() to create text descriptor compatible with TBranch ctor
//
// Revision 1.6  1999/12/03 20:51:27  fine
// change the data-member name size from 20 to 32 to be in line with dstype.h
//
// Revision 1.5  1999/09/13 13:28:27  fine
// One cast int -> uint introduced to avoid compiler warnings
//
// Revision 1.4  1999/08/13 16:35:53  fine
// The artificial restrictions for tableDescriptor have been introduced since database want that
//
// Revision 1.3  1999/08/12 18:53:49  fine
// clash between St_tableDescriptor::GetSize and St_Table::GetSize resolved
//
// Revision 1.2  1999/08/11 14:44:39  fine
// name clash with ROOT over enum resolved
//
// Revision 1.1  1999/08/11 00:40:11  fine
// new class St_tableDescriptor
//____________________________________________________________________________

