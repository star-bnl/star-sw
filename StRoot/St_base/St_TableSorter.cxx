//*-- Author :    Valery Fine   26/01/99  (E-mail: fine@bnl.gov)
// 
// 

#include <stdlib.h> 
#include "St_TableSorter.h"
#include "St_Table.h"
#include <TClass.h>
#include <TDataMember.h>
#include <TDataType.h>

ClassImp(St_TableSorter)
//_____________________________________________________________________________
St_TableSorter::St_TableSorter() : m_ParentTable(0)
{
  m_SortIndex  = 0;
  m_searchMethod = 0;
  m_numberOfRows = 0;
  m_colType = kNAN;
}
//_____________________________________________________________________________
St_TableSorter::St_TableSorter(St_Table &table, TString &colName,Int_t firstRow
                               ,Int_t numberRows):m_ParentTable(table)
{
  m_SortIndex  = 0;
  m_searchMethod = 0;
  m_colType      = kNAN;

  Char_t *name = (Char_t *) colName.Data();
  if (!(name || strlen(colName.Data()))) return;
  name = StrDup(colName.Data());

  // check bounds:
  if (firstRow > m_ParentTable.GetNRows()) return; 
  m_firstRow = firstRow;

  m_numberOfRows = m_ParentTable.GetNRows()- m_firstRow;
  if (numberRows > 0)  m_numberOfRows = TMath::Min(numberRows,m_numberOfRows);

  // Allocate index array
  if (m_numberOfRows) m_SortIndex = new void*[m_numberOfRows];

  // define dimensions if any;
  // count the open "["
  m_colDimensions = 0;
  Char_t *br = name - 1;
  while(br = strchr(br+1,'[')) {
    if (!m_colDimensions) *br = 0; 
    m_colDimensions++;
  }

  // Define the column name
  m_colName = name;
  delete [] name;

  m_IndexArray = 0;
  if (m_colDimensions) {
     m_IndexArray = new Int_t[m_colDimensions];
     memset(m_IndexArray,0,m_colDimensions*sizeof(Int_t));
     // Define the index
     char *openBracket  = colName.Data()-1;
     char *closeBracket = colName.Data()-1;
     for (Int_t i=0; i< m_colDimensions; i++) 
     {
          openBracket  = strchr(openBracket+1, '[');
          closeBracket = strchr(closeBracket+1,']');
          if (closeBracket > openBracket) 
             m_IndexArray[i] = atoi(openBracket+1);
          else {
            Error("St_Tabel ctor", "Wrong parethethis <%s>",colName.Data());
            return;
          }
     }      
  }
  LearnTable();
  FillIndexArray();
  SortArray();
}
//_____________________________________________________________________________
St_TableSorter::~St_TableSorter() 
{ if (m_SortIndex) delete [] m_SortIndex; m_SortIndex = 0; m_numberOfRows=0; }
//_____________________________________________________________________________
//_____________________________________________________________________________
#define COMPAREVALUES(valuetype)  \
int St_TableSorter::Search##valuetype  (const void *elem1, const void **elem2) { \
         valuetype *value1 = (valuetype *)(elem1);   \
         valuetype *value2 = (valuetype *)(*elem2);   \
         valuetype diff = *value1-*value2;            \
         Int_t res = 0;                               \
         if (diff > 0)      res =  1;                 \
         else if (diff < 0) res = -1;                 \
         return res;                                  \
}                                                     \
int St_TableSorter::Compare##valuetype  (const void **elem1, const void **elem2) { \
         valuetype *value1 = (valuetype *)(*elem1);   \
         valuetype *value2 = (valuetype *)(*elem2);   \
         valuetype diff = *value1-*value2;            \
         Int_t res = 0;                               \
         if (diff > 0  )    res =  1;                 \
         else if (diff < 0) res = -1;                 \
         if (res) return res;                         \
         return value1-value2;                        \
}                                                     

  COMPAREVALUES(Float_t)
  COMPAREVALUES(Int_t) 
  COMPAREVALUES(Long_t)
  COMPAREVALUES(ULong_t)
  COMPAREVALUES(UInt_t)
  COMPAREVALUES(Short_t)
  COMPAREVALUES(Double_t)
  COMPAREVALUES(UShort_t)
  COMPAREVALUES(UChar_t)
  COMPAREVALUES(Char_t)

#define COMPAREORDER(valuetype) Compare##valuetype
#define SEARCHORDER(valuetype) Search##valuetype

//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t St_TableSorter::BSearch(Float_t value)
{
  return BSearch(&value);
}
//_____________________________________________________________________________
Int_t St_TableSorter::BSearch(Int_t value)
{
  return BSearch(&value);
}

//_____________________________________________________________________________
Int_t St_TableSorter::BSearch(Double_t value)
{
  return BSearch(&value);
}

//_____________________________________________________________________________
Int_t St_TableSorter::BSearch(const Char_t *value)
{
  return BSearch(value);
}

//_____________________________________________________________________________
Int_t St_TableSorter::BSearch(TString &value)
{
  return BSearch(value.Data());
}
//_____________________________________________________________________________
Int_t St_TableSorter::BSearch(const void *value){
  if (!m_searchMethod) {
     switch (m_colType) {
         case  kFloat:
           m_searchMethod = SEARCHORDER(Float_t);
           break;
         case  kInt :
           m_searchMethod = SEARCHORDER(Int_t);
           break;
         case  kLong :
           m_searchMethod = SEARCHORDER(Long_t);
           break;
         case  kShort :
           m_searchMethod = SEARCHORDER(Short_t);
           break;
         case  kDouble :  
           m_searchMethod = SEARCHORDER(Double_t);
           break;
         case  kUInt: 
           m_searchMethod = SEARCHORDER(UInt_t);
           break;
         case  kULong : 
           m_searchMethod= SEARCHORDER(ULong_t);
           break;
         case  kUShort: 
           m_searchMethod = SEARCHORDER(UShort_t);
           break;
         case  kUChar:
           m_searchMethod = SEARCHORDER(UChar_t);
           break;
         case  kChar:
           m_searchMethod = SEARCHORDER(Char_t);
           break;
         default:
            break;

      };
  }
  Int_t index = -1;
  if (m_searchMethod) {
    void **p = (void **)bsearch( value,  // Object to search for
                   m_SortIndex,     // Pointer to base of search data
                   m_numberOfRows,  // Number of elements
                   sizeof(void *),  // Width of elements
                   CALLQSORT(m_searchMethod));
      if (p) {
         const Char_t *res = (const Char_t *)(*p);
         // calculate index:
         index =  m_firstRow + (res - (((const Char_t *)m_ParentTable[m_firstRow]) + m_colOffset))/m_ParentTable.GetRowSize();
      }
    }
    return index;  
}

//_____________________________________________________________________________
Int_t St_TableSorter::GetIndex(UInt_t index)
{
   Int_t indx = -1;
   if (index < m_numberOfRows )  {
     void *p = m_SortIndex[index];
     if (p) {
         const Char_t *res = (const Char_t *)p;
         // calculate index:
         indx = m_firstRow + (res - (((const Char_t *)m_ParentTable[m_firstRow]) + m_colOffset))/m_ParentTable.GetRowSize();
     }
  }
  return indx;
}

#if 0
//_____________________________________________________________________________
int St_TableSorter::CompareUChar  (const void *elem1, const void *elem2)
{
  UChar_t *value1 = (UChar_t *)(*elem1);
  UChar_t *value2 = (UChar_t *)(*elem2);
  COMPAREVALUES(value1,value2)
}

//_____________________________________________________________________________
int St_TableSorter::CompareChar   (const void *elem1, const void *elem2)
{
  Char_t *value1 = (Char_t *)(*elem1);
  Char_t *value2 = (Char_t *)(*elem2);
  COMPAREVALUES(value1,value2)
}
#endif
//_____________________________________________________________________________
void St_TableSorter::FillIndexArray(){
  if (!m_SortIndex) return;
  for (Int_t i=m_firstRow; i < m_firstRow+m_numberOfRows;i++) 
           m_SortIndex[i-m_firstRow] = ((Char_t *)(m_ParentTable[i])) + m_colOffset;
 
}
//_____________________________________________________________________________
void  St_TableSorter::SortArray(){
   COMPAREMETHOD compare=0;
   switch (m_colType) {
       case  kFloat:
         compare = COMPAREORDER(Float_t);
         break;
       case  kInt :
         compare = COMPAREORDER(Int_t);
         break;
       case  kLong :  
         compare = COMPAREORDER(Long_t);
         break;
       case  kShort :  
         compare = COMPAREORDER(Short_t);
         break;
       case  kDouble:  
         compare = COMPAREORDER(Double_t);
         break;
       case  kUInt:  
         compare = COMPAREORDER(UInt_t);
         break;
       case  kULong:
         compare = COMPAREORDER(ULong_t);
         break;
       case  kUShort:
         compare = COMPAREORDER(UShort_t);
         break;
       case  kUChar:
         compare = COMPAREORDER(UChar_t);
         break;
       case  kChar:
         compare = COMPAREORDER(Char_t);
         break;
       default:  
         break;
    };
 
   if (compare)  
           qsort(m_SortIndex,  //Start of target array
                m_numberOfRows,       //Array size in elements
                sizeof(void *),       //Element size in bytes
                CALLQSORT(compare));           
}
  
//_____________________________________________________________________________
void St_TableSorter::LearnTable()
{
//
// LearnTable() allows the St_TableSorter to learn the structure of the
// tables used to fill the ntuple.
//  table     - the name of the table
//  buildTree - if kTRUE, then add TBranches to the TTree for each table
//              column (default=kFALSE)
//
  TClass *classPtr = m_ParentTable.GetRowClass();
  if (!classPtr) return;

  if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
  if (!(classPtr->GetNdata())) return;

  Int_t rowSize = m_ParentTable.GetRowSize();

  const Char_t *types;
  Char_t *varname;
  Int_t count = 0;

  TIter next(classPtr->GetListOfDataMembers());
  TDataMember *member = 0;
  while (member = (TDataMember *) next()) {
    varname = (Char_t *) member->GetName();   

    if (strcmp(varname,m_colName.Data())) continue;

    TDataType *memberType = member->GetDataType();
    types = memberType->GetTypeName();
    if (!strcmp("float", types)) 
      m_colType = kFloat ;  
    else if (!strcmp("int", types)) 
      m_colType = kInt   ;  
    else if (!strcmp("long", types)) 
      m_colType = kLong  ;  
    else if (!strcmp("short", types)) 
      m_colType = kShort ;  
    else if (!strcmp("double", types)) 
      m_colType = kDouble;  
    else if (!strcmp("unsigned int", types)) 
      m_colType = kUInt  ;  
    else if (!strcmp("unsigned long", types)) 
      m_colType = kULong ; 
    else if (!strcmp("unsigned short", types)) 
      m_colType = kUShort; 
    else if (!strcmp("unsigned char", types))
      m_colType = kUChar;
    else if (!strcmp("char", types))
      m_colType= kChar;

    if (m_colType != kNAN) {
      Int_t dim = 0;
      Int_t globalIndex = 0;
      if (dim = member->GetArrayDim()) {
      // Check dimensions
        if (dim != m_colDimensions) {
           Error("LearnTable","Wrong dimension");
           m_ParentTable.Print();
           return;
        }
        // Calculate the global index
        for( Int_t indx=0; indx < m_colDimensions; indx++ ){
           globalIndex *= member->GetMaxIndex(indx);
           globalIndex += m_IndexArray[indx];
        } 
      }
      m_colSize   = memberType->Size();
      m_colOffset = member->GetOffset() + memberType->Size() * globalIndex;
    }
    break;
  }
}

#undef COMPAREVALUES
#undef COMPAREORDER

