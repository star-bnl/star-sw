//*-- Author :    Valery Fine   24/03/98  (E-mail: fine@bnl.gov)
// $Id: St_Table.cxx,v 1.33 1998/12/21 19:45:47 fisyak Exp $ 
// $Log: St_Table.cxx,v $
// Revision 1.33  1998/12/21 19:45:47  fisyak
// Move ROOT includes to non system
//
// Revision 1.32  1998/12/17 16:57:56  fine
// St_Table: some extra protections have been established (safe "strncat" have replaced the  unsafe  "strncpy")
//
// Revision 1.31  1998/12/17 14:36:51  fisyak
// fix Print for empty tablea, fix copy ctor
//
// Revision 1.30  1998/12/09 20:55:28  fine
// St_Table::Print() method bug has been fixed, some cosmetic improvements
//
// Revision 1.29  1998/12/07 20:23:11  fine
// St_Table:: working versions of the Print() and SavePrimitive methods
//
// Revision 1.28  1998/12/07 03:57:42  fine
// St_Table::SavePrimitive () method has been introduced
//
// Revision 1.27  1998/12/06 00:38:16  fisyak
// Add SavePrimitive
//
// Revision 1.25  1998/12/04 01:54:44  fine
// St_Table::Print(...) - The first version of the table browser has been introduced
//
// Revision 1.24  1998/10/31 00:20:14  fisyak
// Add ds2ReallocTable
//
// Revision 1.23  1998/10/14 22:40:48  fine
// St_Table::ReAllocate method and "plain" C interface to that has been introduced
//
// Revision 1.20  1998/10/04 02:20:12  fine
// St_Table.h Some clashes with TNamed and TObject have been fixed (affected Delete() method)
//
// Revision 1.19  1998/09/28 01:47:48  fisyak
// Use system includes for ROOT
//
// Revision 1.18  1998/09/23 20:22:24  fisyak
// Set kIsNotOwn=23
//
// Revision 1.16  1998/09/21 15:43:02  fine
// St_Table::Update bug has been fixed
//
// Revision 1.15  1998/09/16 22:08:52  fine
// St_DataSetIter - big in dtor has been fixed
// St_Table, St_DataSet - ls method has been improved
//
// Revision 1.14  1998/09/15 20:55:33  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.11  1998/09/07 19:23:39  fine
// St_Table::Print() - malloc/free have been replaced with new [] / delete []  due a problem under Linux
// St_DataSet::~St_DataSet has been changed to take in account the "structural" links. Some opt have been done too
//
// Revision 1.10  1998/08/18 14:05:07  fisyak
// Add to bfc dst
//
// Revision 1.9  1998/07/23 22:12:00  fisyak
// Recover after Correction for root 2.09
//
// Revision 1.8  1998/07/23 21:09:14  fisyak
// Adjust for ROOT 2.09
// 
//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98
// Copyright (C) Valery Fine (Valeri Faine) 1998. All right reserved
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_Table                                                             //
//                                                                      //
// Wraps the array of the STAF C-structures (one STAF Table per element)//
//                                                                      //
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/st2tab.gif"> </P> End_Html //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include <iostream.h>
#include <iomanip.h>

#include "TROOT.h"
#include "TBuffer.h"
#include "TMath.h"
#include "TClass.h"
#include "TString.h"
#include "Api.h"
#include "TRealData.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "St_Table.h"

//______________________________________________________________________________
void *ReAllocate(table_head_st *header, Int_t newsize) 
{
  //
  // header - "plain" C interface to re-allocate the STAF table
  //          "this"  pointer is  supplied indirectly via 
  //          header->dsl_pointer member
  //
  // newsize - is a new size of the STAF table. 
  //           If it is smaller is the old one then nothing happens
  //
 if (header && newsize) 
   return ((St_Table *)header->dsl_pointer)->ReAllocate(newsize);
 else 
   return 0;
}


//______________________________________________________________________________
static void ArrayIndex(Int_t *index,Int_t *size, Int_t dim)
{
  if (dim && index && size) {
    if (++index[dim-1] >= size[dim-1]) {
        index[dim-1] = 0;
        dim--;
        ArrayIndex(index,size, dim);
    }
  }
}

//______________________________________________________________________________
static void AsString(void *buf, const char *name, Int_t width=0)
{
   if (!strcmp("unsigned int", name))
      cout << setw(width) << *(unsigned int *)buf;
   else if (!strcmp("int", name))
      cout <<  setw(width) <<  *(int *)buf;
   else if (!strcmp("unsigned long", name))
      cout <<  setw(width) <<  *(unsigned long *)buf;
   else if (!strcmp("long", name))
      cout <<  setw(width) <<  *(long *)buf;
   else if (!strcmp("unsigned short", name))
      cout <<  setw(width) <<   *(unsigned short *)buf;
   else if (!strcmp("short", name))
      cout <<  setw(width) <<  *(short *)buf;
   else if (!strcmp("unsigned char", name))
      cout <<  setw(width) <<  *(unsigned char *)buf;
   else if (!strcmp("char", name))
      cout <<   setw(width) << *(char *)buf;
   else if (!strcmp("float", name))
      cout <<   setw(width) << setprecision(width-3) << *(float *)buf;
   else if (!strcmp("double", name))
      cout <<   setw(width) << setprecision(width-3) << *(double *)buf;
}


ClassImp(St_Table)
 

//______________________________________________________________________________
Int_t St_Table::MakeWrapClass(Text_t *name)
{
 //
 // It is a static method wich must be ROOT macro
 // But I failed to pass through ROOT though and did this method instead
 //
  Char_t filename[500];
  if (!name) { return -1;}
  strcpy(filename,name); 
  strcat(filename,".h");
//  gROOT->LoadMacro(filename);
   G__loadfile(filename);
 // Pull the "structure name from the file
  Char_t *structname = strrchr(name,'/');
  if (structname) 
    structname++;
  else if (structname = strrchr(name,':'))
    structname++;
  else
   structname = name;
  St_Table t(structname,1);
  t.StafStreamer();
  return 0;
}
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Int_t size): St_DataSet()
{
   // Default St_Table ctor.
   s_TableHeader = new table_head_st;
   s_Size = &s_TableHeader->rbytes;
   s_MaxIndex = &s_TableHeader->nok;
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   s_Table = 0;
   *s_MaxIndex = 0;
   if (size == 0) Warning("St_Table(0)","Wrong table format");
}
 
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Int_t n,Int_t size) : St_DataSet(name)
{
   // Create St_Table object and set array size to n longs.
 
   s_Table = 0;
   s_TableHeader = new table_head_st;
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   if (n > 0) Set(n);
   *s_MaxIndex = 0;
}
 
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Int_t n, Char_t *table,Int_t size) : St_DataSet(name)
{
   // Create St_Table object and initialize it with values of array.
 
   s_Table = 0;
   s_TableHeader = new table_head_st; 
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   Set(n, table);
}
 
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Text_t *type, Int_t n, Char_t *array, Int_t size) 
         : St_DataSet(name)
{
   // Create St_Table object and initialize it with values of array.
 
   s_Table = array;
   s_TableHeader = new table_head_st; 
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);
   SetType(type);
   SetfN(n); 
}

//______________________________________________________________________________
St_Table::St_Table(const St_Table &table)
{
   // Copy constructor.
 
   s_Table = 0;
   *s_TableHeader = *(table.s_TableHeader);
   Set(table.fN, table.s_Table);
}
 
//______________________________________________________________________________
St_Table &St_Table::operator=(const St_Table &rhs)
{
   // St_Table assingment operator.
  if (this != &rhs){
      Set(rhs.fN, rhs.s_Table);
      SetUsedRows(rhs.GetNRows());
  }
  return *this;
}
 
//______________________________________________________________________________
St_Table::~St_Table()
{
   // Delete St_Table object.
   Delete();
   SafeDelete(s_TableHeader);
}
 
//______________________________________________________________________________
void St_Table::Adopt(Int_t n, void *arr)
{
   // Adopt array arr into St_Table, i.e. don't copy arr but use it directly
   // in St_Table. User may not delete arr, St_Table dtor will do it.
 
   Clear();
 
   SetfN(n);
   s_Table = (char *)arr;
}
 
//______________________________________________________________________________
void St_Table::AddAt(const void *row, Int_t i)
{
   // Add one element (row) of structure at position i. Check for out of bounds.
 
   if (!BoundsOk("St_Table::AddAt", i))
      i = 0;
   memcpy(s_Table+i*(*s_Size),row,*s_Size);
   *s_MaxIndex = TMath::Max((Int_t)i+1,Int_t(*s_MaxIndex));
}
 
//______________________________________________________________________________
void St_Table::CopyStruct(Char_t *dest, const Char_t *src)
{
    ::memcpy(dest,src,*s_Size*fN);
}
//______________________________________________________________________________
void St_Table::CopySet(St_Table &array)
{
  array.Set(fN); 
  CopyStruct(array.s_Table,s_Table); 
 *(array.s_TableHeader) = *s_TableHeader; 
}

//______________________________________________________________________________
void *St_Table::ReAllocate(Int_t newsize)
{
  if (s_Size && newsize > fN) {
   void *arr =  realloc(s_Table,*s_Size*newsize);
   SetfN(newsize);
   s_Table = (char *)arr;
 }  
 return (void *)s_Table;
}

//______________________________________________________________________________
Char_t *St_Table::Create()
{
  if (s_Size) 
       return (Char_t *)malloc(*s_Size*fN);
  return 0;
}
//______________________________________________________________________________
void St_Table::Browse(TBrowser *b){
  St_DataSet::Browse(b);
  Print(0,6);
}
//______________________________________________________________________________
void St_Table::Clear(Option_t *opt)
{
  if (s_Table)
  {
    if (! TestBit(kIsNotOwn)) free(s_Table);
    s_Table = 0;
   *s_MaxIndex = 0;
    fN = 0;
  } 
}

//______________________________________________________________________________
void St_Table::Delete(Option_t *opt)
{
  Clear();
  St_DataSet::Delete(opt);
}

#if 0
//______________________________________________________________________________
void St_Table::Dump()
{
   // Dump contents of object on stdout.
   // Using the information in the object dictionary (class TClass)
   // each data member is interpreted.
   // If a data member is a pointer, the pointer value is printed
   //
   // The following output is the Dump of a TArrow object:
   //   fAngle                   0           Arrow opening angle (degrees)
   //   fArrowSize               0.2         Arrow Size
   //   fOption.*fData
   //   fX1                      0.1         X of 1st point
   //   fY1                      0.15        Y of 1st point
   //   fX2                      0.67        X of 2nd point
   //   fY2                      0.83        Y of 2nd point
   //   fUniqueID                0           object unique identifier
   //   fBits                    50331648    bit field status word
   //   fLineColor               1           line color
   //   fLineStyle               1           line style
   //   fLineWidth               1           line width
   //   fFillColor               19          fill area color
   //   fFillStyle               1001        fill area style
 
   char parent[256];
   parent[0] = 0;
   TDumpMembers dm;
   ShowMembers(dm, parent);
}
#endif 
//______________________________________________________________________________
TClass  *St_Table::GetRowClass() const 
{
  // Return TClass object defining the origial STAF table

  //   TString buffer = Class()->GetName();
  //  TString buffer = GetTitle();
     TString buffer = IsA()->GetName();
     buffer.ReplaceAll("St_","");
     buffer += "_st";
   return gROOT->GetClass(buffer.Data());
}
//______________________________________________________________________________
Long_t St_Table::GetNRows() const { 
// Returns the number of the rows for the wrapped table
return *s_MaxIndex;
}
//______________________________________________________________________________
Long_t St_Table::GetRowSize() const { 
// Returns the size (in bytes) of one table row 
  return *s_Size;
}
//______________________________________________________________________________
Long_t St_Table::GetTableSize() const { 
// Returns the number of the used rows 
return fN;
}
//______________________________________________________________________________
void  *St_Table::GetArray() const {
//  return the void pointer to the C-structure
 return s_Table; }
//______________________________________________________________________________
const Char_t *St_Table::GetType() const { 
//Returns the type of the wrpped C-structure
  return GetTitle();
}

//______________________________________________________________________________
void St_Table::LinkHeader()
{
  // Link some class data members with the STAF table header
   if (s_TableHeader)
   {
     s_Size = &s_TableHeader->rbytes;
     s_MaxIndex = &s_TableHeader->nok;
   }
   else 
     Error("LinkHeader","wrong header !");
}

//______________________________________________________________________________
void St_Table::ls(Option_t *option)
{
  St_DataSet::ls(option);
  IncreaseDirLevel();
  IndentLevel();
  cout       <<Path() 
             <<"\t --> Allocated rows: "<<fN
             <<"\t Used rows: "<<*s_MaxIndex
             <<"\t Row size: "      << *s_Size << " bytes"
      <<endl;
  //  Print();
  DecreaseDirLevel();
}
//_____________________________________________________________________________
 void St_Table::ls(Int_t deep)
{
   St_DataSet::ls(deep);
   IncreaseDirLevel();
   IndentLevel();
   cout      <<Path() 
             <<"\t --> Allocated rows: "<<fN
             <<"\t Used rows: "<<*s_MaxIndex
             <<"\t Row size: " << *s_Size << " bytes"
      <<endl;
   //   Print();
   DecreaseDirLevel();
}

//______________________________________________________________________________
Char_t *St_Table::Print(Char_t *strbuf,Int_t lenbuf) const 
{
  Char_t buffer[100];
  strcpy(buffer,GetTitle());
  strcat(buffer,"_st");

//  ostrstream  out(strbuf,lenbuf);
  Int_t iOut = 0; 
 
  G__ClassInfo m(buffer);
  G__DataMemberInfo data(m);

  if (!m.Name()) {
      Error("Print"," No dictionary entry for <%s> structure", buffer);
      if (lenbuf>0) iOut += sprintf(strbuf+iOut," *** Errror ***");
      return strbuf;
  }
  IndentLevel();
 
  if (lenbuf>0) {
  // cut of the "_st" suffix 
     Char_t *typenam =  new Char_t [strlen(m.Name())+1];
     strcpy(typenam,m.Name());
  // look for the last "_"
     Char_t *last = strrchr(typenam,'_');
  // Check whether it is "_st"
     Char_t *eon = 0;
     if (last) eon = strstr(last,"_st");
  // Cut it off if any
     if (eon) *eon = '\0';
 //====      out << "struct " << typenam << " {";
     iOut += sprintf(strbuf+iOut,"struct %s {",typenam);
     delete [] typenam;
   }
   else
      cout << "struct " << m.Name() << " {" << endl;

  while(data.Next())
  {
    Int_t dim = data.ArrayDim();

    G__TypeInfo *t = data.Type();

    IndentLevel();

    if (lenbuf>0) {
//        out << " " << t->Name() << " " << data.Name();
       TString name = t->Name();
       name.ReplaceAll("unsigned char","octet");
       iOut += sprintf(strbuf+iOut," %s %s",name.Data(),data.Name());
    }
    else
        cout << '\t'<< t->Name() << '\t'<< data.Name();

    Int_t indx = 0;
    while (indx < dim) {
          if (lenbuf>0)
  //             out <<  "[" << data.MaxIndex(indx)<<"]";
                 iOut += sprintf(strbuf+iOut,"[%d]",data.MaxIndex(indx));
          else
               cout <<  "[" << data.MaxIndex(indx)<<"]";
          indx++;
    }
    if (lenbuf>0) 
//        out << "; " << data.Title();
//          iOut += sprintf(strbuf+iOut, "; %s", data.Title());
          iOut += sprintf(strbuf+iOut, ";");
    else 
        cout << ";\t//" <<  data.Title() << endl;
  }
  IndentLevel();
  if (lenbuf>0) {
//     out << "}";
          iOut += sprintf(strbuf+iOut, "}");
  }
  else
     cout << "}" << endl;
 
  return strbuf;
}
//______________________________________________________________________________
const Char_t *St_Table::Print(Int_t row, Int_t rownumber, const Char_t *colfirst, const Char_t *collast) const 
{
  //  
  //  Print the contents of STAF tables per COLUMN.
  //
  //  row       - the index of the first row to print (counting from ZERO)
  //  rownumber - the total number of rows to print out (=10 by default)
  //
  //  (No use !) Char_t *colfirst, *collast - the names of the first/last 
  //                                          to print out (not implemented yet)
  //
  //--------------------------------------------------------------
   // Check bounds and adjust it
   Int_t const width = 8;
   Int_t rowStep = 10; // The maximun values to print per line
   Int_t rowNumber = rownumber;
   if (row  > GetSize())  return 0;
   if (rowNumber > GetSize()-row) rowNumber = GetSize()-row;
   if (!rowNumber) return 0;
   rowStep = TMath::Min(rowStep,rowNumber);

   Int_t cdate = 0;
   Int_t ctime = 0;
   UInt_t *cdatime = 0;
   Bool_t isdate = kFALSE;
//   char *pname; 

//   if  (GetNRows() == 0) return 0;

   TClass *classPtr = GetRowClass();


   if (classPtr == 0) return 0;
   if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
   if (!classPtr->GetNdata()) return 0;

   TIter      next( classPtr->GetListOfDataMembers());

   //  3. Loop by "rowStep x lines"

   const Char_t  *startRow = (const Char_t *)GetArray() + row*GetRowSize(); 
   Int_t rowCount = rowNumber;
   Int_t thisLoopLenth = 0;
   const Char_t  *nextRow;
   while (rowCount) {
     cout << endl << " ---------------------------------------------------------------------------------------" << endl
         <<  " " << Path() 
                 <<"  Allocated rows: "<<fN
                 <<"\t Used rows: "<<*s_MaxIndex
                 <<"\t Row size: "      << *s_Size << " bytes"
      <<endl; 
     if  (GetNRows() == 0) {// to Print empty table header 
       cout << " ======================================================================================" << endl;
       return 0; 
     }
      cout << " Table: " << classPtr->GetName()<< "\t";
      for (Int_t j = row+rowNumber-rowCount; j<row+rowNumber-rowCount+rowStep && j < row+rowNumber ;j++)
      { 
         Int_t hW = width-2;
         if (j>=10) hW -= TMath::Log10(float(j))-1;
         cout  << setw(hW) << "["<<j<<"]";
         cout  << " :" ;
      }
      cout << endl
      <<       " ======================================================================================" << endl;
      next.Reset();
      TDataMember *member = 0;
      while (member = (TDataMember*) next()) {
         TDataType *membertype = member->GetDataType();
         isdate = kFALSE;
         if (strcmp(member->GetName(),"fDatime") == 0 && strcmp(member->GetTypeName(),"UInt_t") == 0) {
            isdate = kTRUE;
         }

//         cout << member->GetTypeName() << "\t" << member->GetName();
         cout << member->GetTypeName();

         // Add the dimensions to "array" members 
         Int_t dim = member->GetArrayDim();
         Int_t indx = 0;
         Int_t *arrayIndex = 0;
         Int_t *arraySize = 0;
         if (dim) {
           arrayIndex = new Int_t[dim];
           arraySize  = new Int_t[dim];          
           memset(arrayIndex,0,dim*sizeof(Int_t));
         }
         Int_t arrayLength  = 1;
         while (indx < dim ){
            arraySize[indx] = member->GetMaxIndex(indx);
            arrayLength *= arraySize[indx];
//            cout << "["<<  arraySize[indx] <<"]";
           // Take in account the room this index will occupy
           indx++;
         }

         // Encode data value or pointer value
         Int_t offset = member->GetOffset();
         Int_t thisStepRows;
         thisLoopLenth = TMath::Min(rowCount,rowStep);
         Int_t indexOffset;
         Bool_t breakLoop = kFALSE;

         for (indexOffset=0; indexOffset < arrayLength && !breakLoop; indexOffset++) 
         {
           nextRow = startRow;
           if (!indexOffset) cout << "\t" << member->GetName();
           else              cout << "\t" << setw(strlen(member->GetName())) << " ";
//           if (dim && indexOffset) {
           if (dim) {
                for (Int_t i=0;i<dim;i++) cout << "["<<arrayIndex[i]<<"]";
                ArrayIndex(arrayIndex,arraySize,dim);
           }
           cout << "\t";
           if ( strlen(member->GetName())+3*dim < 8) cout << "\t";

           for (thisStepRows = 0;thisStepRows < thisLoopLenth; thisStepRows++,nextRow += GetRowSize())
           {
             const char *pointer = nextRow + offset  + indexOffset*membertype->Size();
             const char **ppointer = (const char**)(pointer);
 
             if (member->IsaPointer()) {
                const char **p3pointer = (const char**)(*ppointer);
                if (!p3pointer) {
                   printf("->0");
                } else if (!member->IsBasic()) {
//                   if (pass == 1) tlink = new TLink(xvalue+0.1, ytext, p3pointer);
                   cout << "N/A :" ;
                } else if (membertype) {
                   if (!strcmp(membertype->GetTypeName(), "char"))
                      cout << *ppointer;
                   else {
                        if (dim == 1) {
                          char charbuffer[11];
                          strncpy(charbuffer,*p3pointer,TMath::Min(10,arrayLength));
                          charbuffer[10] = 0;
                          cout << "\"" << charbuffer;
                          if (arrayLength > 10) cout << " . . . ";
                          cout << "\"";
                          breakLoop = kTRUE;
                        }
                        else 
//                           cout << membertype->AsString(p3pointer) << " :";
                          ::AsString(p3pointer,membertype->GetTypeName(),width);
                           cout << " :";
                   }
                } else if (!strcmp(member->GetFullTypeName(), "char*") ||
                         !strcmp(member->GetFullTypeName(), "const char*")) {
                   cout << setw(width) << *ppointer;
                } else {
//                   if (pass == 1) tlink = new TLink(xvalue+0.1, ytext, p3pointer);
                   cout << setw(width) << " N/A ";
                }
             } else if (membertype)
                if (isdate) {
                   cdatime = (UInt_t*)pointer;
                   TDatime::GetDateTime(cdatime[0],cdate,ctime);
                   cout << cdate << "/" << ctime;
                } else if (strcmp(membertype->GetFullTypeName(),"char")==0 && dim == 1) {
                    char charbuffer[11];
                    strncpy(charbuffer,pointer,TMath::Min(10,arrayLength));
                    charbuffer[10] = 0;
                    cout << "\"" << charbuffer;
                    if (arrayLength > 10) cout << " . . . ";
                    cout << "\"";
                    breakLoop = kTRUE;
                 }
                 else{
                    ::AsString((void *)pointer,membertype->GetTypeName(),width);
                    cout << " :";
//                    cout << membertype->AsString((void *)pointer) <<" :";
                 }
             else
                cout << "->" << (Long_t)pointer;
           }
        // Encode data member title
           if (indexOffset==0) {
             Int_t ltit = 0;
             if (isdate == kFALSE && strcmp(member->GetFullTypeName(), "char*") &&
                 strcmp(member->GetFullTypeName(), "const char*")) {
                    cout << " " << member->GetTitle();
             }   
           }
           cout << endl;
         }
      }
      rowCount -= thisLoopLenth;
      startRow  = nextRow;
   }
  cout << "---------------------------------------------------------------------------------------" << endl;
  return 0;
 }

//______________________________________________________________________________
void St_Table::SavePrimitive(ofstream &out, Option_t *)
{
   // Save a primitive as a C++ statement(s) on output stream "out".
 
   Int_t rowStep =  1; // The maximun values to print per line
   Int_t rowNumber = GetSize();
//   Int_t rowNumber = 3; // Just to debug code only
   if (!rowNumber) return;

   Int_t cdate = 0;
   Int_t ctime = 0;
   UInt_t *cdatime = 0;
   Bool_t isdate = kFALSE;
//   char *pname; 

   if  (GetNRows() == 0) return;

   TClass *classPtr = GetRowClass();


   if (classPtr == 0) return;
   if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
   if (!classPtr->GetNdata()) return;

   TIter      next( classPtr->GetListOfDataMembers());

   //  3. Loop by "rowStep x lines"

   const Char_t  *startRow = (const Char_t *)GetArray();
   Int_t rowCount = rowNumber;
   Int_t thisLoopLenth = 0;
   const Char_t  *nextRow;
   const Char_t *rowId = "row";
   const Char_t *tableId = "tableSet";
   // Generate the header
   if (!rowCount) return;

    out << "St_DataSet *CreateTable() { " << endl
        << "// -----------------------------------------------------------------" << endl
        << "// "   << Path() 
                        << " Allocated rows: "<<fN
                        <<"  Used rows: "<<*s_MaxIndex
                        <<"  Row size: " << *s_Size << " bytes"
                        <<endl 
        << "// "  << " Table: " << classPtr->GetName()<<"[0]--> "
                         << classPtr->GetName()<<"["<<rowNumber-1 <<"]" 
      << endl
      << "// ====================================================================" << endl
      <<    classPtr->GetName() << " " << rowId << ";" << endl
      <<  "St_" <<  GetTitle() << " *" << tableId << " = new " 
                               << "St_" << GetTitle()
                               << "(\""<<GetName()<<"\"," << GetNRows() << ");" << endl 
      << "//" <<endl ;
 
   while (rowCount) {
      next.Reset();
      TDataMember *member = 0;
      while (member = (TDataMember*) next()) {
         TDataType *membertype = member->GetDataType();
         isdate = kFALSE;
         if (strcmp(member->GetName(),"fDatime") == 0 && strcmp(member->GetTypeName(),"UInt_t") == 0) {
            isdate = kTRUE;
         }

         // Add the dimensions to "array" members 
         Int_t dim = member->GetArrayDim();
         Int_t indx = 0;
         Int_t *arrayIndex = 0;
         Int_t *arraySize = 0;
         if (dim) {
           arrayIndex = new Int_t[dim];
           arraySize  = new Int_t[dim];          
           memset(arrayIndex,0,dim*sizeof(Int_t));
         }
         Int_t arrayLength  = 1;
         while (indx < dim ){
            arraySize[indx] = member->GetMaxIndex(indx);
            arrayLength *= arraySize[indx];
            // Take in account the room this index will occupy
            indx++;
         }

         // Encode data value or pointer value
         Int_t offset = member->GetOffset();
         Int_t thisStepRows;
         thisLoopLenth = TMath::Min(rowCount,rowStep);
         Int_t indexOffset;
         Bool_t breakLoop = kFALSE;

         for (indexOffset=0; indexOffset < arrayLength && !breakLoop; indexOffset++) 
         {
           nextRow = startRow;
//           out.setf(left);
           Int_t fieldLen = 20;
           fieldLen -= strlen(member->GetName())+strlen(rowId)+1;
           
           if (dim && !indexOffset) out << endl;
           out << setw(3) << " " ;  
           out << rowId << "." << member->GetName();

           if (dim) {
              for (Int_t i=0;i<dim;i++) {
                out << "["<<arrayIndex[i]<<"]";
                fieldLen -= 3;
             }
             ArrayIndex(arrayIndex,arraySize,dim);
           }

         // Generate "="
           out << setw(TMath::Max(fieldLen,1)) << " " << " = ";
//           out << " = ";

           for (thisStepRows = 0;thisStepRows < thisLoopLenth; thisStepRows++,nextRow += GetRowSize())
           {
             const char *pointer = nextRow + offset  + indexOffset*membertype->Size();
             const char **ppointer = (const char**)(pointer);
 
             if (member->IsaPointer()) {
                const char **p3pointer = (const char**)(*ppointer);
                if (!p3pointer) {
                   printf("->0");
                } else if (!member->IsBasic()) {
//                   if (pass == 1) tlink = new TLink(xvalue+0.1, ytext, p3pointer);
                   out << "N/A :" ;
                } else if (membertype) {
                   if (!strcmp(membertype->GetTypeName(), "char"))
                      out << *ppointer;
                   else {
                        if (dim == 1) {
                          out << "\"" << (char *)*p3pointer << "\"";
                          breakLoop = kTRUE;
                        }
                        else 
                           out << "char(" << membertype->AsString(p3pointer) << ")";
                   }
                } else if (!strcmp(member->GetFullTypeName(), "char*") ||
                         !strcmp(member->GetFullTypeName(), "const char*")) {
                   out << *ppointer;
                } else {
//                   if (pass == 1) tlink = new TLink(xvalue+0.1, ytext, p3pointer);
                   out << " N/A ";
                }
             } else if (membertype)
                if (isdate) {
                   cdatime = (UInt_t*)pointer;
                   TDatime::GetDateTime(cdatime[0],cdate,ctime);
                   out << cdate << "/" << ctime;
                } else if (strcmp(membertype->GetFullTypeName(),"char")==0 && dim == 1) {
                     out << "\"" << (char *)pointer << "\"";
                     breakLoop = kTRUE;
                }
                 else
                   out << setw(10) << membertype->AsString((void *)pointer) ;
             else
                  out << setw(10) << (Long_t)pointer;
           }
        // Encode data member title
           if (indexOffset==0) {
             Int_t ltit = 0;
             if (isdate == kFALSE && strcmp(member->GetFullTypeName(), "char*") &&
                 strcmp(member->GetFullTypeName(), "const char*")) {
                    out << "; // " << member->GetTitle();
             }   
           }
           out << ";" << endl;
         }
      }
      out << tableId << "->AddAt(&" << rowId << "," << rowNumber-rowCount <<");" 
          << endl; 
      rowCount -= thisLoopLenth;
      //      if (rowCount) cout << "// ------------------ next row -----------------" << endl;
      startRow  = nextRow;
   }
  out << "// ----------------- end of code ---------------" << endl
      << " return (St_DataSet *)tableSet;" << endl
      << "}"  << endl;
  return;
}
 
//______________________________________________________________________________
Int_t St_Table::ReadGenericArray(TBuffer &b, void *&ii, EBufSizes membersize)
{
   // Read array of ANY type with pre-defined size from the I/O buffer. 
   // Returns the number of that read. 
   // If argument is a 0 pointer then space will be allocated for the
   // array.

  switch ( membersize) {
    case kChar1Byte:    return b.ReadArray((Char_t *&)ii);
         break;
    case kShort2Bytes:  return b.ReadArray((Short_t *&)ii);
         break;
    case kLong4Bytes:   return b.ReadArray((Long_t *&)ii);
         break;
    case kDouble8Bytes: return b.ReadArray((Double_t *&)ii); 
         break;
   default: 
         break;
  };
  return 0;
}

//______________________________________________________________________________
void St_Table::Set(Int_t n)
{
   // Set array size of St_Table object to n longs. If n<0 leave array unchanged.
   if (n < 0) return;
   if (fN != n)  Clear();
   SetfN(n);
   if (fN == 0) return;
   if (!s_Table) s_Table = Create();
   Reset();
}
//______________________________________________________________________________
void St_Table::SetHeadFields(Text_t *name)
{
   if (name) SetName(name);      // Define "name" if supplied
   else SetName("unknown");      // Define the default name 
   SetType("unknown");           // Define the default type 
   s_TableHeader->dsl_pointer = (long)this;

}

//______________________________________________________________________________
void St_Table::SetName(const Char_t *name)
{
   SetTableName(name);
   St_DataSet::SetName(s_TableHeader->name);
}
//______________________________________________________________________________
void   St_Table::SetTableName(const Char_t *name){
// Fill staf table header with the table name information
 if (s_TableHeader)
 {
    s_TableHeader->name[0] = '\0';
    strncat(s_TableHeader->name,name,20); 
 }
}
//______________________________________________________________________________
void   St_Table::SetTableType(const Char_t *type){
// Fill staf table header with the table type information
   if (s_TableHeader) {
     s_TableHeader->type[0] = '\0';
     strncat(s_TableHeader->type,type,20); 
   }
}

//______________________________________________________________________________
void St_Table::SetTablePointer(void *table)
{ 
   if (s_Table) free(s_Table);
   s_Table = (Char_t *)table;
}

//______________________________________________________________________________
void St_Table::SetType(const Text_t *const type)
{
   SetTableType(type);
   St_DataSet::SetTitle(s_TableHeader->type);
}

//______________________________________________________________________________
int St_Table::PointerToPointer(G__DataMemberInfo &m)
{
   if (strstr(m.Type()->Name(), "**")) return 1;
   return 0;
}

//______________________________________________________________________________
void St_Table::MakeHeader(const Char_t *prefix,const Char_t *tablename,
                         const Char_t *suffix, FILE *fl)
{
  Char_t *include = new Char_t [strlen(prefix)+strlen(tablename)+strlen(suffix)+5];
  sprintf(include,"%s%s%s.h",prefix,tablename,suffix);
  //*-*
  //*-* Open file if that is not supplied via parameter
  //
   FILE *fp = (fl == 0 ? fopen(include, "w") : fl) ;
   fprintf(fp, "#ifndef STAF_%s%s_Table\n",prefix,tablename);
   fprintf(fp, "#define STAF_%s%s_Table\n",prefix,tablename);
   fprintf(fp, "\n");
   fprintf(fp, "#include \"St_Table.h\"\n");
   fprintf(fp, "\n");
   fprintf(fp, "#include \"%s.h\"\n",tablename);
   fprintf(fp, "\n");
   fprintf(fp, "class St_%s : public St_Table\n",tablename);
   fprintf(fp, "{\n");
   fprintf(fp, "public:\n");
   fprintf(fp, "  %s%s() : St_Table(\"%s\",sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Text_t *name) : St_Table(name,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Int_t n): St_Table(\"%s\",n,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Text_t *name,Int_t n): St_Table(name,n,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename);
   fprintf(fp, "  %s_st *GetTable(){ return (%s_st *)s_Table;}\n"
          ,tablename,tablename);
   fprintf(fp, "\n");
   fprintf(fp, "  ClassDef(%s%s,0) // class particle STAF tables\n"
          ,prefix,tablename);
   fprintf(fp, "};\n");
   fprintf(fp, "\n");
   fprintf(fp, "#endif  \n");
  fclose(fp);
  delete [] include;
}
//______________________________________________________________________________
void St_Table::StafStreamer(Char_t *structname, FILE *fl)
{

  ///////////////////////////////////////////////////////////////////////////////
  //
  // Method StafStreamer(Char_t *structname, FILE *fl)
  // Creates the implemetation of the class to wrap the "plain"
  // C data strucutre.
  //
  //  It is assumes if "name" is the name of the C-structure
  // 
  //    1. The name of this class is created as "prefix"+"name"
  //    2. The real name of the base C-structure is "name"+"tabsuffux"
  //    3. The of the generetared file with this class implementation
  //       is "prefix" + "name" + "suffix" + ".cxx" if any
  //
  //  Parameters:
  //      Char_t *structname  - The "name" of the "extrernal C data structure
  //                            (see p.1 above)
  //                      = 0  - The name is defined with GetType() method
  //      FILE *fl            - the FILE handler used to write the generated 
  //                            C++ code out, otherwise this method opens file
  //                            as p.3 defines alone and close it before return
  //
  ///////////////////////////////////////////////////////////////////////////////

   Char_t datamem[256];
   Char_t *tablename=0;;
   Char_t datatype[256];
   Char_t *thisclassname=0;
   const Char_t *prefix = "St_";      // Prefix for all STAF wrapper (table & modules) classes
   const Char_t *suffix = "_Table";   // Siffix to distinguish "module" and "table" wrappers
   const Char_t *tabsuffix ="_st";    // Suffix defined with STAF C structures
   Bool_t thisclassbase;
   const Char_t row[]="row";
   
   Bool_t typeunknown = strcmp(GetType(),"unknown") == 0;

   if (structname || typeunknown ) 
   {
        if (!structname) {
    //
    // The name of the original structure has not been supplied
    // Try to guess it
    //
          tablename = new Char_t [strlen(GetName())+1];
          strcpy(tablename,GetName());
        }
        else {
    //
    // The name of the original structure has been supplied
    //
          tablename = new Char_t [strlen(structname)+1];
          strcpy(tablename,structname);
        }
        strcpy(datamem, prefix);
        strcat(datamem, tablename);
        thisclassbase = kTRUE;
   }
   else if (!typeunknown)
   {
       tablename = new Char_t[strlen(GetType())+1];
       strcpy(tablename,GetType());
       strcpy(datamem, ClassName());
       thisclassbase = kFALSE;
   }
   else
   {
     Error("StafStreamer", "Can't guess the C-structure typedef");
     return;
   }
   thisclassname = new Char_t[strlen(datamem)+1]; 
   strcpy(thisclassname,datamem);
   strcat(datamem,suffix);
   strcat(datamem,".cxx");
  //*-*
  //*-* Open file if that is not supplied via parameter
  //
   FILE *fp = (fl == 0 ? fopen(datamem, "w") : fl) ;

//    Create ClassInfo
   strcpy(datatype,tablename);
   strcat(datatype,tabsuffix);

   G__ClassInfo cl(datatype);

   strcpy(datamem,"((");
   strcat(datamem,tablename);
   strcat(datamem,tabsuffix);
   strcat(datamem," *)s_Table)");

   // Create include file

   MakeHeader(prefix,tablename,suffix);

   fprintf(fp, "#include \"%s%s%s.h\"\n",prefix,tablename,suffix);
   fprintf(fp, "#include \"Stypes.h\"\n");
   fprintf(fp, "/////////////////////////////////////////////////////////////////////////\n");
   fprintf(fp, "//\n");
   fprintf(fp, "//  Class %s wraps the STAF table %s \n",thisclassname,datatype);
   fprintf(fp, "//  It has been generated \"by automatic\". Please don\'t change it \"by hand\"\n");
   fprintf(fp, "//\n");
   fprintf(fp, "///////////////////////////////////////////////////////////////////////// \n\n");
   fprintf(fp, "TableImp(%s)\n",tablename);

   fprintf(fp, "//_______________________________________");
   fprintf(fp, "_______________________________________\n");
   fprintf(fp, "void %s::Streamer(TBuffer &R__b)\n{\n", thisclassname);
   fprintf(fp, "   // Stream an array of the \"plain\" C-structures <%s>.\n\n", cl.Name());
 
   // In case of VersionID<=0 write dummy streamer only calling
   // its base class Streamer(s). If no base class(es) let Streamer
   // print error message, i.e. this Streamer should never have been called.
   char a[80];
//   int i = pass;
//   sprintf(a, "%s::Class_Version()", cl.Name());
//   version = (int)G__int(G__calc(a));
 
   // loop twice: first time write reading code, second time writing code
   for (int i = 0; i < 2; i++) {
 
      int decli = 0;

      if (i == 0) {
         fprintf(fp, "   if (R__b.IsReading()) {\n");
         fprintf(fp, "      Version_t R__v = R__b.ReadVersion(); if (R__v) { }\n");
      } else {
         fprintf(fp, "   } else {\n");
         fprintf(fp, "      R__b.WriteVersion(%s::IsA());\n",thisclassname);
      }

      // Stream base classes if any
      if (!thisclassbase)
      {
        TIter next(IsA()->GetListOfBases());
        TClass *obj;
        while (obj = (TClass *)next()) 
           fprintf(fp, "      %s::Streamer(R__b);\n", obj->GetName());
      }
      else 
           fprintf(fp, "      %s::Streamer(R__b);\n", IsA()->GetName());

     // Create a loop for IMaxIndex

      fprintf(fp, "      if (*s_MaxIndex <= 0) return; \n\n");
      fprintf(fp, "      for (Int_t indx=0;indx<*s_MaxIndex;indx++) {\n");
      fprintf(fp, "        %s &%s=*(%s+indx);\n",datatype, row, datamem);

      // Stream base class(es) when they have the Streamer() method
      G__BaseClassInfo b(cl);
 
      while (b.Next())
         if (b.HasMethod("Streamer"))
            fprintf(fp, "        %s::Streamer(R__b);\n", b.Name());
 
      // Stream data members
      G__DataMemberInfo m(cl);
 
      while (m.Next()) {
 
         // we skip:
         //  - static members
         //  - members with an ! as first character in the title (comment) field
         //  - the member G__virtualinfo inserted by the CINT RTTI system
 
         if (!(m.Property() & G__BIT_ISSTATIC) &&
             strncmp(m.Title(), "!", 1)        &&
             strcmp(m.Name(), "G__virtualinfo")) {
 
            // fundamental type: short, int, long, etc....
            if (((m.Type())->Property() & G__BIT_ISFUNDAMENTAL) ||
                ((m.Type())->Property() & G__BIT_ISENUM)) {
               if (m.Property() & G__BIT_ISARRAY &&
                   m.Property() & G__BIT_ISPOINTER) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                 if (i == 0) {
                     fprintf(stderr,"*** Datamember %s::%s: array of pointers to fundamental type (need manual intervention)\n", cl.Name(), m.Name());
                     fprintf(fp, "           ;//R__b.ReadArray(%s.%s);\n",row, m.Name());
                  } else {
                     fprintf(fp, "           ;//R__b.WriteArray(%s.%s, __COUNTER__);\n",row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISPOINTER) {
                  if (i == 0) {
                     fprintf(stderr,"*** Datamember %s::%s: pointer to fundamental type (need manual intervention)\n", cl.Name(), m.Name());
                     fprintf(fp, "        //R__b.ReadArray(%s.%s);\n",row, m.Name());
                  } else {
                     fprintf(fp, "        //R__b.WriteArray(%s.%s, __COUNTER__);\n",row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISARRAY) {
                  if (i == 0) {
                     if (m.ArrayDim() > 1) {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.ReadStaticArray((Int_t*)(%s.%s));\n", row,m.Name());
                        else
                           fprintf(fp, "        R__b.ReadStaticArray((%s*)(%s.%s));\n", m.Type()->TrueName(), row, m.Name());
                     } else {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.ReadStaticArray((Int_t*)(%s.%s));\n", row, m.Name());
                        else
                           fprintf(fp, "        R__b.ReadStaticArray(%s.%s);\n",row, m.Name());
                      }
                  } else {
                     int s = 1;
                     for (int dim = 0; dim < m.ArrayDim(); dim++)
                        s *= m.MaxIndex(dim);
                     if (m.ArrayDim() > 1) {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.WriteArray((Int_t*)(%s.%s), %d);\n", row,m.Name(), s);
                        else
                           fprintf(fp, "        R__b.WriteArray((%s*)(%s.%s), %d);\n", m.Type()->TrueName(), row, m.Name(), s);
                     } else {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.WriteArray((Int_t*)(%s.%s), %d);\n", row, m.Name(), s);
                        else
                           fprintf(fp, "        R__b.WriteArray(%s.%s, %d);\n", row, m.Name(), s);
                     }
                  }
               } else if ((m.Type())->Property() & G__BIT_ISENUM) {
                  if (i == 0)
                     fprintf(fp, "        R__b >> (Int_t&)(%s.%s);\n", row, m.Name());
                  else
                     fprintf(fp, "        R__b << (Int_t)(%s.%s);\n", row, m.Name());
               } else {
                  if (i == 0)
                     fprintf(fp, "        R__b >> %s.%s;\n", row, m.Name());
                  else
                     fprintf(fp, "        R__b << %s.%s;\n", row, m.Name());
               }
            } else {
               // we have an object
               if (m.Property() & G__BIT_ISARRAY &&
                   m.Property() & G__BIT_ISPOINTER) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                  if (i == 0) {
                     fprintf(fp, "           R__b >> (%s.%s)[R__i];\n", row, m.Name());
                  } else {
                     fprintf(fp, "           R__b << (%s.%s)[R__i];\n", row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISPOINTER) {
                  // This is always good. However, in case of a pointer
                  // to an object that is guarenteed to be there and not
                  // being referenced by other objects we could use
                  //     xx->Streamer(b);
                  // Optimize this with control statement in title.
                  if (PointerToPointer(m)) {
                     if (i == 0) {
                        fprintf(stderr,"*** Datamember %s::%s: pointer to pointer (need manual intervention)\n", cl.Name(), m.Name());
                        fprintf(fp, "        //R__b.ReadArray(%s.%s);\n", row, m.Name());
                     } else {
                        fprintf(fp, "        //R__b.WriteArray(%s.%s, __COUNTER__);\n", row, m.Name());
                     }
                  } else {
                     if (strstr(m.Type()->Name(), "TClonesArray")) {
                        fprintf(fp, "        (%s.%s)->Streamer(R__b);\n", row, m.Name());
                     } else {
                        if (i == 0)
                           fprintf(fp, "        R__b >> %s.%s;\n", row, m.Name());
                        else {
                           if (m.Type()->IsBase("TObject") && m.Type()->IsBase("TArray"))
                              fprintf(fp, "        R__b << (TObject*)(%s.%s);\n", row, m.Name());
                           else
                              fprintf(fp, "        R__b << %s.%s;\n", row, m.Name());
                        }
                     }
                  }
               } else if (m.Property() & G__BIT_ISARRAY) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                  fprintf(fp, "           (%s.%s)[R__i].Streamer(R__b);\n", row, m.Name());
               } else {
                  if ((m.Type())->HasMethod("Streamer"))
                     fprintf(fp, "        (%s.%s).Streamer(R__b);\n", row, m.Name());
                  else {
                     if (i == 0)
                        fprintf(stderr, "*** Datamember %s::%s: object has no Streamer() method (need manual intervention)\n",
                                cl.Name(), m.Name());
                     fprintf(fp, "        //(%s.%s).Streamer(R__b);\n", row, m.Name());
                  }
               }
            }
         }
      }

   fprintf(fp, "     }\n");
   }
   fprintf(fp, "   }\n");
   fprintf(fp, "}\n\n");
   if (!fl) fclose(fp);  // Close file if that was opened with this method

   // Clean things

   if (thisclassname) delete [] thisclassname;
   if (tablename)     delete [] tablename;
}

//______________________________________________________________________________
void St_Table::Reset(Int_t c)
{
  if (s_Table) ::memset(s_Table,c,*s_Size*fN);
}

//______________________________________________________________________________
void St_Table::Set(Int_t n, Char_t *array)
{
   // Set array size of St_Table object to n longs and copy array.
   // If n<0 leave array unchanged.
 
   if (n < 0) return;
   if (fN != n) Clear();

   SetfN(n);

   if (fN == 0) return;
   if (!s_Table) s_Table = Create();
   CopyStruct(s_Table,array);
   *s_MaxIndex = n;
}
 
//_______________________________________________________________________
void St_Table::Streamer(TBuffer &b)
{
   // Stream a St_Table object.
   // Stream an object of class St_Table.


   if (b.IsReading()) {
     St_DataSet::Streamer(b);
     b >> fN;
     StreamerHeader(b);
     LinkHeader();
    //   Create a table to fit nok rows
     Set(*s_MaxIndex);
     //     printf(" Read:s_Size = %i fN = %i \n", *s_Size, fN);
   } else {
      St_DataSet::Streamer(b);
      b << fN;
      StreamerHeader(b);
      //      printf(" Write: s_Size = %i  fN = %i head size =  %i \n", *s_Size, fN, sizeof(table_head_st));
   }
}
 
//_______________________________________________________________________
void St_Table::StreamerHeader(TBuffer &b)
{
  if (b.IsReading()) 
  {
   b.ReadStaticArray(s_TableHeader->name);         /* table name */
   b.ReadStaticArray(s_TableHeader->type);         /* table type */
   b >> s_TableHeader->maxlen;       /* # rows allocated */
   b >> s_TableHeader->nok;          /* # rows filled */
   b >> s_TableHeader->rbytes;       /* number of bytes per row */
   b >> s_TableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
   b >> s_TableHeader->data_pointer; /* swizzled (char*) */
  }
  else {
   b.WriteArray(s_TableHeader->name,20);         /* table name */
   b.WriteArray(s_TableHeader->type,20);         /* table type */
   b << s_TableHeader->maxlen;       /* # rows allocated */
   b << s_TableHeader->nok;          /* # rows filled */
   b << s_TableHeader->rbytes;       /* number of bytes per row */
   b << s_TableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
   b << s_TableHeader->data_pointer; /* swizzled (char*) */
  }
}

#if 0
//_______________________________________________________________________
TBuffer &operator>>(TBuffer &buf, St_Table *&obj)
{
   // Read St_Table object from buffer. Declared in ClassDef.
 
   obj = (St_Table *) TArray::ReadArray(buf, St_Table::Class());
   return buf;
}

#endif

//_______________________________________________________________________
Int_t St_Table::SetfN(Long_t len)
{
   fN = len;
   if (s_TableHeader)
   {
//     s_TableHeader->nok = fN;
     s_TableHeader->maxlen = fN;
   } 
   return fN;
}
//_______________________________________________________________________
void St_Table::Update(){;}
//_______________________________________________________________________
void St_Table::Update(St_DataSet *set, UInt_t opt)
{
 // Kill the table current data
 // and adopt those from set
  if (set->HasData()) 
  {
    // Check whether the new table has the same type 
    if (strcmp(GetTitle(),set->GetTitle()) == 0 ) 
    {
      St_Table *table =  (St_Table *)set;
      Adopt(table->GetSize(),table->GetArray());
     *s_TableHeader   = *(table->GetHeader());
      // mark that object lost the STAF table and can not delete it anymore
      table->SetBit(kIsNotOwn);
      // mark we took over of this STAF table
      ResetBit(kIsNotOwn);
    }
    else
       Error("Update",
             "This table is <%s> but the updating one has a wrong type <%s>",GetTitle(),set->GetTitle());
  }
  St_DataSet::Update(set,opt);
}

