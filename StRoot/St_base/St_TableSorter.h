//*-- Author :    Valery Fine   26/01/99  (E-mail: fine@bnl.gov)
// $Id: St_TableSorter.h,v 1.19 1999/12/01 01:40:04 fine Exp $
// $Log: St_TableSorter.h,v $
// Revision 1.19  1999/12/01 01:40:04  fine
// new access method with the Long_t parameter has been introduced to avoid the wrong cast from (long) to (double) in CINT
//
// Revision 1.18  1999/08/09 01:38:55  fine
// New method GetKeyAddress has been introduced
//
// Revision 1.17  1999/05/18 17:59:22  fine
// Clean up and some comments
//
// Revision 1.16  1999/05/18 14:41:29  fine
// New methods: CountKey(), CountKeys(), FindFirstKey() have beent introduced
//
// Revision 1.15  1999/05/14 22:20:56  fine
// CountKey and CountKeys methods have been introduced
//
// Revision 1.14  1999/05/14 00:30:38  fine
// GetLastFound method has been introduced
//  

#ifndef STAR_St_TableSorter
#define STAR_St_TableSorter

#include "TNamed.h"

////////////////////////////////////////////////////////////////////////////////////////
//
//  St_TableSorter  - Is an "observer" class to sort the St_Table objects
//                    The class provides an interface to the standard "C/C++"
//
// qsort and bsearch subroutine (for further information see your local C/C++ docs)
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
////////////////////////////////////////////////////////////////////////////////////////

class St_Table;
class St_TableSorter : public TNamed {
 private:
   union {  Char_t   m_Char; 
            Int_t    m_Int;  
            Long_t   m_Long;
            Float_t  m_Float; 
            Double_t m_Double;
         } m_Value;

 protected:
    typedef Int_t (*COMPAREMETHOD)(const void **, const void **);
    typedef Int_t (*SEARCHMETHOD)(const void *, const void **);
    typedef Int_t (*CALLQSORT)(const void *, const void *);
    enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                           ,kULong, kUShort, kUChar, kChar };
    void    **m_SortIndex;    // Array of pointers to columns of the sorted table
    Int_t     m_LastFound;    // The index of the last found index within m_SortIndex
    Int_t     m_firstRow;     // first row of the table to be sorted
    Int_t     m_numberOfRows; // number of rows of the table to be sorted
    TString   m_colName;      //
    Int_t     m_colOffset;    //
    Int_t     m_colSize;      // The size of the selected column in bytes
    Int_t    *m_IndexArray;   // "parsed" indecis
    Int_t     m_colDimensions;// The number of the dimensions for array (=-1 means it is a "simple" array)
    const Char_t *m_simpleArray;    // Pointer to the "simple" array;
#ifndef __CINT__
    const St_Table &m_ParentTable;  // the back pointer to the sorted table
#else
    //    const St_Table *m_ParentTable;  //!- the back pointer to the sorted table
#endif
    SEARCHMETHOD  m_searchMethod;   // Function selected to search values
    EColumnType   m_colType;        // data type of the selected column

    static int CompareFloat_t     (const void **, const void **);
    static int CompareInt_t       (const void **, const void **);
    static int CompareLong_t      (const void **, const void **);
    static int CompareULong_t     (const void **, const void **);
    static int CompareUInt_t      (const void **, const void **);
    static int CompareShort_t     (const void **, const void **);
    static int CompareDouble_t    (const void **, const void **);
    static int CompareUShort_t    (const void **, const void **);
    static int CompareUChar_t     (const void **, const void **);
    static int CompareChar_t      (const void **, const void **);

    Int_t  BSearch(const void *value);

    Int_t BSearch(Float_t value)      { return BSearch(&value);}
    Int_t BSearch(Int_t value)        { return BSearch(&value);}
    Int_t BSearch(Long_t value)       { return BSearch(&value);}
    Int_t BSearch(Double_t value)     { return BSearch(&value);}
    Int_t BSearch(const Char_t *value){ return BSearch(value); }
    Int_t BSearch(TString &value)     { return BSearch(value.Data()); }

    void   FillIndexArray();
    void   SortArray();
    void   LearnTable();

    static int SearchFloat_t     (const void *, const void **);
    static int SearchInt_t       (const void *, const void **);
    static int SearchULong_t     (const void *, const void **);
    static int SearchLong_t      (const void *, const void **);
    static int SearchUInt_t      (const void *, const void **);
    static int SearchShort_t     (const void *, const void **);
    static int SearchDouble_t    (const void *, const void **);
    static int SearchUShort_t    (const void *, const void **);
    static int SearchUChar_t     (const void *, const void **);
    static int SearchChar_t      (const void *, const void **);

    Int_t SelectSearch(Float_t  value );
    Int_t SelectSearch(Int_t    value );
    Int_t SelectSearch(ULong_t  value );
    Int_t SelectSearch(Long_t   value );
    Int_t SelectSearch(UInt_t   value );
    Int_t SelectSearch(Short_t  value );
    Int_t SelectSearch(Double_t value );
    Int_t SelectSearch(UShort_t value );
    Int_t SelectSearch(UChar_t  value );
    Int_t SelectSearch(Char_t   value );

    void  SetSearchMethod();
    void  SetSimpleArray(Int_t arraySize, Int_t firstRow,Int_t numberRows);

 public:
    St_TableSorter();
    St_TableSorter(const St_Table &table, TString &colName, Int_t firstRow=0,Int_t numbeRows=0);

    St_TableSorter(const Float_t  *simpleArray, Int_t arraySize, Int_t firstRow=0,Int_t numberRows=0);
    St_TableSorter(const Double_t *simpleArray, Int_t arraySize, Int_t firstRow=0,Int_t numberRows=0);
    St_TableSorter(const Long_t   *simpleArray, Int_t arraySize, Int_t firstRow=0,Int_t numberRows=0);
    virtual ~St_TableSorter();
    
    virtual Int_t CountKey(const void *key, Int_t firstIndx=0,Bool_t bSearch=kTRUE,Int_t *firstRow=0);
    virtual Int_t CountKeys();
    virtual Int_t FindFirstKey(const void *key);
 
    Int_t BinarySearch(Float_t  value );
    Int_t BinarySearch(Int_t    value );
    Int_t BinarySearch(ULong_t  value );
    Int_t BinarySearch(Long_t   value );
    Int_t BinarySearch(UInt_t   value );
    Int_t BinarySearch(Short_t  value );
    Int_t BinarySearch(Double_t value );
    Int_t BinarySearch(UShort_t value );
    Int_t BinarySearch(UChar_t  value );
    Int_t BinarySearch(Char_t   value );
 
    virtual const Text_t   *GetColumnName() const { return m_colName.Data();}
    virtual       Int_t     GetIndex(UInt_t sortedIndex) const;
    virtual const void     *GetKeyAddress(Int_t indx) { return (m_SortIndex && indx >= 0) ?m_SortIndex[indx]:(void *)(-1);}
    virtual       Int_t     GetLastFound()  const { return m_LastFound; }
    virtual const Text_t   *GetTableName()  const;
    virtual const Text_t   *GetTableTitle() const;
    virtual const Text_t   *GetTableType()  const;
    virtual       St_Table *GetTable()      const;
    virtual       Int_t     GetNRows()      const { return m_numberOfRows;}
    virtual       Int_t     GetFirstRow()   const { return m_firstRow;}

    Int_t operator[](Int_t value)    { return BSearch(value); }
    Int_t operator[](Long_t value)   { return BSearch(value); }
    Int_t operator[](Double_t value) { return BSearch(value); } 
    Int_t operator[](const Char_t *value) { return BSearch(value); }
//    Int_t operator[](TString &value) { return BSearch(value); }  // to be implemented

    Int_t operator()(Float_t value)  { return BinarySearch(value); }
    Int_t operator()(Int_t value)    { return BinarySearch(value); }
    Int_t operator()(Long_t value)   { return BinarySearch(value); }
    Int_t operator()(Double_t value) { return BinarySearch(value); }
//    Int_t operator()(const Char_t *value) { return BinarySearch(*value); } // to be implemented
//    Int_t operator()(TString &value)    { return *this(value.Data());  }   // to be implemented

    ClassDef(St_TableSorter,0)
};
#endif
