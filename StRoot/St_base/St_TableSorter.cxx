//*-- Author :    Valery Fine   26/01/99  (E-mail: fine@bnl.gov)
// 
// 

#include <stdlib.h> 
#include "St_TableSorter.h"
#include "St_Table.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "TMemberInspector.h"

/////////////////////////////////////////////////////////////////////////////////////////
//
//  St_TableSorter  - Is an "observer" class to sort the St_Table objects
//                    The class provides an interface to the standard "C/C++"
//
// qsort and bsearch subroutines (for further information see your local C/C++ docs)
// =====     =======
//
//  - This class DOESN'T change / touch the "host" table  itself
//    For any St_Table object one can create as many different "sorter"
//    as he/she finds useful for his/her code
//  - Any instance of this class is meaningful as long as the "host" object
//    "St_Table" does exist and is not changed
//  - Any attempt to access this St_TableSorter after the "host" object deleted
//    causes the program abnormal termination
//  - Any attempt to access this St_TableSorter after the "host" object been changed
//    causes an unpredictable result
//  - Any instance (object) of this class is NOT deleted "by automatic" just
//    the "host object "St_Table" deleted. It is the responsibility of the user's code
//    keeping St_TableSorter and the the "host" St_Table objects consistent.
//
// "To do" list
//
//  1. A separate method to provide lexicographical sort if the "sorted" column is a kind of array
//
//  Usage: 
//    1. Create an instance of the sorter for the selected column of your table
//
//        new St_TableSorter(St_Table &table, TString &colName,Int_t firstRow,Int_t numberRows)
//
//        All sort actions are performed within St_TableSorter ctor. 
//        This means one needs no extra effort to SORT table. "Sorter" contains 
//        the "sorted index array" as soon as you create the sorter
//
//        St_TableSorter sorter(MyTable,"id",20, 34);
//          - Creates a sorter for MyTable column "id" ordering
//            its 34 rows from 20 row with standard "C" qsort subroutine
//
//    2.  You may use this instance to search any "id" value with operator [] 
//          to get the table row index as follows:
//
//          Int_t id = 5;
//          Int_t index =  sorter[id]; // Look for the row index with id = 5
//                                     // using the standard "C"  "bsearch" binary search 
//                                     // subroutine
//          Int_t index =  sorter(id); // Look for the row index with id "nearest" to 5
//                                     // using the internal "BinarySearch" method
//
//    3. Some useful methods of this class:
//
//        3.1. CountKeys()
//        3.2  CountKey(const void *key, Int_t firstIndx=0,Bool_t bSearch=kTRUE,Int_t *firstRow=0)
//        3.3. FindFirstKey(const void *key)
//        3.4. GetIndex(UInt_t sortedIndex)    
//
/////////////////////////////////////////////////////////////////////////////////////////


static const St_Table *dummy= 0;
static const St_Table &dummyTable = *dummy;

ClassImp(St_TableSorter)
//_____________________________________________________________________________
//St_TableSorter::St_TableSorter() : m_simpleArray(0),m_ParentTable(*((const St_Table *)0))
St_TableSorter::St_TableSorter() : m_simpleArray(0),m_ParentTable(dummyTable)
{
  // default ctor for RootCint dictionary
  m_LastFound    = -1;
  m_SortIndex    = 0;
  m_searchMethod = 0;
  m_numberOfRows = 0;
  m_colType = kNAN;
  m_simpleArray=0;
}
//_____________________________________________________________________________
St_TableSorter::St_TableSorter(const St_Table &table, TString &colName,Int_t firstRow
                               ,Int_t numberRows):m_simpleArray(0),m_ParentTable(table)
{
  //
  // St_TableSorter ctor sort the input table along its column defined with colName
  //
  //    - colName    - may be followed by the square brackets with integer number inside, 
  //                   if that columm is an array (for example "phys[3]").
  //                   NO expression inside of [], only a single integer number allowed !
  //    - firstRow   - the first table row to sort from (=0 by default)
  //    - numberRows - the number of the table rows to sort (=0 by default)
  //                   = 0 means sort all rows from the "firstRow" by the end of table
  //

  m_LastFound    = -1;
  m_numberOfRows = 0;
  m_colType      = kNAN;
  m_simpleArray  = 0;

  TString n = table.GetName();
  n += ".";
  n += colName;
  SetName(n);

  m_SortIndex    = 0;
  m_searchMethod = 0;
  m_colType      = kNAN;

  Char_t *name = (Char_t *) colName.Data();
  if (!(name || strlen(colName.Data()))) { MakeZombie(); return; }
  name = StrDup(colName.Data());

  // check bounds:
  if (firstRow > m_ParentTable.GetNRows()) { MakeZombie(); return; }
  m_firstRow = firstRow;

  m_numberOfRows = m_ParentTable.GetNRows()- m_firstRow;
  if (numberRows > 0)  m_numberOfRows = TMath::Min(numberRows,m_numberOfRows);

  // Allocate index array
  if (m_numberOfRows <=0 ) { MakeZombie(); return; }
  m_SortIndex = new void*[m_numberOfRows];

  // define dimensions if any;
  // count the open "["
  m_colDimensions = 0;
  Char_t *br = name - 1;
  while((br = strchr(br+1,'['))) {
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
     const char *openBracket  = colName.Data()-1;
     const char *closeBracket = colName.Data()-1;
     for (Int_t i=0; i< m_colDimensions; i++) 
     {
          openBracket  = strchr(openBracket+1, '[');
          closeBracket = strchr(closeBracket+1,']');
          if (closeBracket > openBracket) 
             m_IndexArray[i] = atoi(openBracket+1);
          else {
            Error("St_Table ctor", "Wrong parethethis <%s>",colName.Data());
            MakeZombie();
            return;
          }
     }      
  }
  LearnTable();
  FillIndexArray();
  SortArray();
  SetSearchMethod();
}

//_____________________________________________________________________________
St_TableSorter::St_TableSorter(const Float_t *simpleArray, Int_t arraySize, Int_t firstRow
                               ,Int_t numberRows)
                               :m_simpleArray((const Char_t*)simpleArray)
                               ,m_ParentTable(dummyTable)
{
  //
  // St_TableSorter ctor sort the input "simpleArray" 
  //
  //    - arraySize  - the size of the full array
  //    - firstRow   - the first table row to sort from (=0 by default)
  //    - numberRows - the number of the table rows to sort (=0 by default)
  //                   = 0 means sort all rows from the "firstRow" by the end of table
  //

  m_LastFound    = -1;

  SetSimpleArray(arraySize,firstRow,numberRows);
  if (!m_simpleArray) { MakeZombie(); return; }

 //  LearnTable();

      m_colName = "Float";
      m_colType   = kFloat;
      m_colSize   = sizeof(Float_t);

  // FillIndexArray();

    Float_t *p = ((Float_t *)m_simpleArray) + m_firstRow;
    for (Int_t i=0; i < m_numberOfRows;i++,p++) m_SortIndex[i-m_firstRow] = p;
  
  SortArray();

  SetSearchMethod();
}
//_____________________________________________________________________________
St_TableSorter::St_TableSorter(const Double_t *simpleArray, Int_t arraySize, Int_t firstRow
                               ,Int_t numberRows)
                               :m_simpleArray((const Char_t*)simpleArray)
                               ,m_ParentTable(dummyTable)
{
  //
  // St_TableSorter ctor sort the input "simpleArray" 
  //
  //    - arraySize  - the sie of the full array
  //    - firstRow   - the first table row to sort from (=0 by default)
  //    - numberRows - the number of the table rows to sort (=0 by default)
  //                   = 0 means sort all rows from the "firstRow" by the end of table
  //

  m_LastFound    = -1;

  SetSimpleArray(arraySize,firstRow,numberRows);
  if (!m_simpleArray)  {MakeZombie(); return; }

 //  LearnTable();

      m_colName = "Double";
      m_colType = kDouble;
      m_colSize = sizeof(Double_t);

  // FillIndexArray();

    Double_t *p = ((Double_t *)simpleArray) + m_firstRow;
    for (Int_t i=0; i < m_numberOfRows;i++,p++) m_SortIndex[i-m_firstRow] = p;
  
  SortArray();

  SetSearchMethod();
}
//_____________________________________________________________________________
St_TableSorter::St_TableSorter(const Long_t *simpleArray, Int_t arraySize, Int_t firstRow
                               ,Int_t numberRows)
                               :m_simpleArray((const Char_t*)simpleArray)
                               ,m_ParentTable(dummyTable)
{
  //
  // St_TableSorter ctor sort the input "simpleArray" 
  //
  //    - arraySize  - the sie of the full array
  //    - firstRow   - the first table row to sort from (=0 by default)
  //    - numberRows - the number of the table rows to sort (=0 by default)
  //                   = 0 means sort all rows from the "firstRow" by the end of table
  //

  m_LastFound    = -1;

  SetSimpleArray(arraySize,firstRow,numberRows);
  if (!simpleArray) { MakeZombie(); return; }

 //  LearnTable();

      m_colName = "Long";
      m_colType = kLong;
      m_colSize = sizeof(Long_t);

  // FillIndexArray();

    Long_t *p = ((Long_t *)simpleArray) + m_firstRow;
    for (Int_t i=0; i < m_numberOfRows;i++,p++) m_SortIndex[i-m_firstRow] = p;
  
  SortArray();

  SetSearchMethod();
}

//_____________________________________________________________________________
void St_TableSorter::SetSimpleArray(Int_t arraySize, Int_t firstRow,Int_t numberRows)
{
  // Set some common parameteres for the "simple" arrays
  SetName("Array");

  m_SortIndex     = 0;
  m_searchMethod  = 0;
  m_colDimensions = 0;
  m_IndexArray    = 0;
  m_colOffset     = 0;

  // check bounds:
  if (firstRow > arraySize) return; 
  m_firstRow = firstRow;

  m_numberOfRows = arraySize - m_firstRow;
  if (numberRows > 0)  m_numberOfRows = TMath::Min(numberRows,m_numberOfRows);

  // Allocate index array
  if (m_numberOfRows > 0) m_SortIndex = new void*[m_numberOfRows];
}
//_____________________________________________________________________________
St_TableSorter::~St_TableSorter() 
{ if (m_SortIndex) delete [] m_SortIndex; m_SortIndex = 0; m_numberOfRows=0; }

//_____________________________________________________________________________
//______________________________________________________________________________
//*-*-*-*-*-*-*Binary search in an array of n values to locate value*-*-*-*-*-*-* 
//*-*          ==================================================   
//*-*  If match is found, function returns position of element.     
//*-*  If no match found, function gives nearest element smaller than value. 
//*-*                                                               
//*-* This method is based on TMath::BinarySearch                   
//*-*                                                               
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  

#define BINARYSEARCH(valuetype) Int_t St_TableSorter::BinarySearch(valuetype value) {\
   switch (m_colType) {                               \
         case  kFloat:                                \
           return SelectSearch(Float_t(value));       \
         case  kInt :                                 \
           return SelectSearch(Int_t(value));         \
         case  kLong :                                \
           return SelectSearch(Long_t(value));        \
         case  kShort :                               \
           return SelectSearch(Short_t(value));       \
         case  kDouble :                              \
           return SelectSearch(Double_t(value));      \
         case  kUInt:                                 \
           return SelectSearch(UInt_t(value));        \
         case  kULong :                               \
           return SelectSearch(ULong_t(value));       \
         case  kUShort:                               \
           return SelectSearch(UShort_t(value));      \
         case  kUChar:                                \
           return SelectSearch(UChar_t(value));       \
         case  kChar:                                 \
           return SelectSearch(Char_t(value));        \
         default:                                     \
           return -1;                                 \
           break;                                     \
      };                                              \
}                                                     \
Int_t St_TableSorter::SelectSearch(valuetype value) {               \
   valuetype **array = (valuetype **)m_SortIndex;                   \
   Int_t nabove, nbelow, middle;                                    \
   nabove = m_numberOfRows+1;                                       \
   nbelow = 0;                                                      \
   while(nabove-nbelow > 1) {                                       \
      middle = (nabove+nbelow)/2;                                   \
      if (value == *array[middle-1]) { nbelow = middle; break; }    \
      if (value  < *array[middle-1]) nabove = middle;               \
      else                           nbelow = middle;               \
   }                                                                \
   nbelow--;                                                        \
   m_LastFound    = nbelow;                                         \
   if (nbelow < 0) return nbelow;                                   \
   return GetIndex(nbelow);                                         \
}

#define COMPAREFLOATVALUES(valuetype)  \
int St_TableSorter::Search##valuetype  (const void *elem1, const void **elem2) { \
         valuetype *value1 = (valuetype *)(elem1);    \
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
}                                                     \
BINARYSEARCH(valuetype)

//_____________________________________________________________________________
#define COMPAREVALUES(valuetype)  \
int St_TableSorter::Search##valuetype  (const void *elem1, const void **elem2) { \
         valuetype *value1 = (valuetype *)(elem1);    \
         valuetype *value2 = (valuetype *)(*elem2);   \
         return    *value1-*value2;                   \
}                                                     \
int St_TableSorter::Compare##valuetype  (const void **elem1, const void **elem2) { \
         valuetype *value1 = (valuetype *)(*elem1);   \
         valuetype *value2 = (valuetype *)(*elem2);   \
         valuetype diff = *value1-*value2;            \
         if (diff ) return diff;                      \
         return value1-value2;                        \
}                                                     \
BINARYSEARCH(valuetype)

  COMPAREFLOATVALUES(Float_t)
  COMPAREVALUES(Int_t) 
  COMPAREVALUES(Long_t)
  COMPAREVALUES(ULong_t)
  COMPAREVALUES(UInt_t)
  COMPAREVALUES(Short_t)
  COMPAREFLOATVALUES(Double_t)
  COMPAREVALUES(UShort_t)
  COMPAREVALUES(UChar_t)
  COMPAREVALUES(Char_t)

#define COMPAREORDER(valuetype) Compare##valuetype
#define SEARCHORDER(valuetype) Search##valuetype

//_____________________________________________________________________________
#ifdef NOINLINES
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
Int_t St_TableSorter::BSearch(Long_t value)
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
#endif

//_____________________________________________________________________________
Int_t St_TableSorter::BSearch(const void *value) {
  Int_t index = -1;
  if (m_searchMethod) {
    void **p = (void **)bsearch( value,  // Object to search for
                   m_SortIndex,          // Pointer to base of search data
                   m_numberOfRows,       // Number of elements
                   sizeof(void *),       // Width of elements
                   CALLQSORT(m_searchMethod));
    m_LastFound = -1;
    if (p) {
       const Char_t *res = (const Char_t *)(*p);
       m_LastFound = ((Char_t *)p - (Char_t *)m_SortIndex)/sizeof(void *);
        // calculate index:
       if (!m_simpleArray) 
          index =  m_firstRow + 
                   (res - (((const Char_t *)m_ParentTable.At(m_firstRow))+ m_colOffset))
                  /m_ParentTable.GetRowSize();
       else
         index = ULong_t(res) - ULong_t(m_simpleArray)/m_colSize;
    }
  }
  return index;  
}

//_____________________________________________________________________________
Int_t St_TableSorter::GetIndex(UInt_t sortedIndex) const
{
  // returns the original index of the row by its sorted index
   Int_t indx = -1;
   if (sortedIndex < UInt_t(m_numberOfRows) )  {
     void *p = m_SortIndex[sortedIndex];
     if (p) {
         const Char_t *res = (const Char_t *)p;
         // calculate index:
       if (!m_simpleArray) 
         indx = m_firstRow + (res - (((const Char_t *)m_ParentTable.At(m_firstRow)) + m_colOffset))/m_ParentTable.GetRowSize();
       else
         indx = (ULong_t(res) - ULong_t(m_simpleArray))/m_colSize;
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
Int_t St_TableSorter::CountKey(const void *key, Int_t firstIndx, Bool_t bSearch, Int_t *firstRow)
{
 //
 //  CountKey counts the number of rows with the key value equal "key"
 //
 //  key      - it is a POINTER to the key value
 //  fistIndx - the first index within sorted array to star search
 //              = 0 by default
 //  bSearch  = kTRUE - binary search (by default) is used otherwise linear one
 //

  Int_t count = 0;
  if (firstRow) *firstRow = -1;         
  if (m_searchMethod) {
    Int_t indx = firstIndx;  
    Int_t nRows = GetNRows();
    if (!bSearch) {
       while ( indx < nRows && m_searchMethod(key,(const void **)&m_SortIndex[indx])){indx++;}
       // Remember the first row been asked:
    } else {
       indx = FindFirstKey(key);
       if (indx >= 0 ) {  // Key was found let's count it
         count = TMath::Max(0,GetLastFound() - indx + 1);
         indx  = TMath::Max(GetLastFound()+1,firstIndx);
         // Forward pass
       }
    }
    while ( indx < nRows &&!m_searchMethod(key,(const void **)&m_SortIndex[indx])){indx++; count++;}
    if (firstRow && count) *firstRow = indx-count;
  }
  return count;
}

//_____________________________________________________________________________
Int_t St_TableSorter::CountKeys()
{
 //
 // Counts the number of different key values 
 //
  Int_t count = 0;
  if (m_SortIndex && m_SortIndex[0]) {
    void *key = m_SortIndex[0];
    Int_t indx = 0;
    while (indx < GetNRows()){
      indx += CountKey(key,indx,kFALSE); 
      count++; 
      key = m_SortIndex[indx];
    }
  }
  return count;
}
//_____________________________________________________________________________
void St_TableSorter::FillIndexArray(){
  if (!m_SortIndex) return;
  for (Int_t i=m_firstRow; i < m_firstRow+m_numberOfRows;i++) 
           m_SortIndex[i-m_firstRow] = ((Char_t *)(m_ParentTable.At(i))) + m_colOffset;
 
}
//_____________________________________________________________________________
Int_t St_TableSorter::FindFirstKey(const void *key)
{
 //
 // Looks for the first index of the "key" 
 // within SORTED table AFTER sorting
 //
 // Returns: = -1 if the "key" was not found
 //
 // Note: This method has no sense for 
 // ====  the float and double key
 // 
 //       To get the index within the original
 //       unsorted table the GetIndex() method
 //       may be used like this:
 //       GetIndex(FindFirstKey(key))
 //
  Int_t indx = -1;
  if (BSearch(key)>=0) 
  {
    indx = GetLastFound();
    if (indx >=0) 
        while (indx > 0 && !m_searchMethod(key,(const void **)&m_SortIndex[indx-1])) indx--;  
  }
  return indx;
}
//_____________________________________________________________________________
const Text_t * St_TableSorter::GetTableName() const { return m_ParentTable.GetName();}
//_____________________________________________________________________________
const Text_t * St_TableSorter::GetTableTitle() const { return m_ParentTable.GetTitle();}
//_____________________________________________________________________________
const Text_t * St_TableSorter::GetTableType() const { return m_ParentTable.GetType();}
//_____________________________________________________________________________
St_Table *St_TableSorter::GetTable() const { return (St_Table *)&m_ParentTable;}

//_____________________________________________________________________________
void  St_TableSorter::SetSearchMethod()
{
  // Select search function at once 
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
  
//____________________________________________________________________________
void St_TableSorter::LearnTable()
{
//
//  LearnTable() allows the St_TableSorter to learn the structure of the
//  tables used to fill the ntuple.
//  table     - the name of the table
//  buildTree - if kTRUE, then add TBranches to the TTree for each table
//              column (default=kFALSE)
//
  TClass *classPtr = m_ParentTable.GetRowClass();
  if (!classPtr) return;

  if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
  if (!(classPtr->GetNdata())) return;

  const Char_t *types;
  Char_t *varname;

  TIter next(classPtr->GetListOfDataMembers());
  TDataMember *member = 0;
  while ( (member = (TDataMember *) next()) ) {
    varname = (Char_t *) member->GetName();   

    if (strcmp(varname,m_colName.Data())) continue;

    TDataType *memberType = member->GetDataType();
    types = memberType->GetTypeName();
    SetTitle(types);
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
      if ( (dim = member->GetArrayDim()) ) {
      // Check dimensions
        if (dim != m_colDimensions) {
           Error("LearnTable","Wrong dimension");
           St_Table *t = (St_Table *)&m_ParentTable;
           t->Print();
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
#if 0
//______________________________________________________________________________
void St_TableSorter::ShowMembers(TMemberInspector &R__insp, char *R__parent)
{
   // Inspect the data members of an object of class St_TableSorter.

   TClass *R__cl  = St_TableSorter::IsA();
   Int_t   R__ncp = strlen(R__parent);
   if (R__ncp || R__cl || R__insp.IsA()) { }
   R__insp.Inspect(R__cl, R__parent, "*m_SortIndex", &m_SortIndex);
   R__insp.Inspect(R__cl, R__parent, "m_firstRow", &m_firstRow);
   R__insp.Inspect(R__cl, R__parent, "m_numberOfRows", &m_numberOfRows);
   m_colName.ShowMembers(R__insp, strcat(R__parent,"m_colName.")); R__parent[R__ncp] = 0;
   R__insp.Inspect(R__cl, R__parent, "m_colOffset", &m_colOffset);
   R__insp.Inspect(R__cl, R__parent, "m_colSize", &m_colSize);
   R__insp.Inspect(R__cl, R__parent, "*m_IndexArray", &m_IndexArray);
   R__insp.Inspect(R__cl, R__parent, "m_colDimensions", &m_colDimensions);
   R__insp.Inspect(R__cl, R__parent, "*m_simpleArray", &m_simpleArray);
   R__insp.Inspect(R__cl, R__parent, "*m_searchMethod", &m_searchMethod);
   R__insp.Inspect(R__cl, R__parent, "m_colType", &m_colType);
   TNamed::ShowMembers(R__insp, R__parent);
}
#endif
#undef COMPAREVALUES
#undef COMPAREORDER
#undef COMPAREFLOATVALUES
#undef BINARYSEARCH
