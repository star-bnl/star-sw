//*-- Author :    Valery Fine   26/01/99  (E-mail: fine@bnl.gov)
// 
// 

#ifndef STAR_St_TableSorter
#define STAR_St_TableSorter

#include "TObject.h"
#include "TString.h"

class St_Table;


class St_TableSorter : public TObject {
    typedef Int_t (*COMPAREMETHOD)(const void **, const void **);
    typedef Int_t (*SEARCHMETHOD)(const void *, const void **);
    typedef Int_t (*CALLQSORT)(const void *, const void *);
    enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                           ,kULong, kUShort, kUChar, kChar };
    void    **m_SortIndex;    // Array of pointers to columns of the sorted table
    Int_t     m_firstRow;     // first row of the table to be sorted
    Int_t     m_numberOfRows; // number of rows of ttable to be sorted
    TString   m_colName;      //
    Int_t     m_colOffset;    //
    Int_t     m_colSize;      // The size of the selected columen in bytes
    Int_t    *m_IndexArray;   // "parsed" indecis
    Int_t     m_colDimensions;// The number of the dimensions for array
    St_Table &m_ParentTable;  // the back pointer to the sorted table
    SEARCHMETHOD m_searchMethod; // Function selected to serach values
    EColumnType m_colType;   // data type of the selected column

 public:
    St_TableSorter();
    St_TableSorter(St_Table &table, TString &colName, Int_t firstRow=0,Int_t numbeRows=0);
    virtual ~St_TableSorter();
    
    Int_t BSearch(const void *value);
    Int_t BSearch(Float_t value);
    Int_t BSearch(Int_t value);
    Int_t BSearch(Double_t value);
    Int_t BSearch(const Char_t *value);
    Int_t BSearch(TString &value);
 
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
    void   FillIndexArray();
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
    void   SortArray();
    void   LearnTable();
    Int_t  LFind(Float_t value);
    Int_t  LFind(Int_t value);
    Int_t  LFind(Double_t value);
    Int_t  LFind(const Char_t *value);
    Int_t  LFind(TString &value);

    Int_t Next(Int_t index);

    Int_t operator()(Float_t value) { return BSearch(value); }
    Int_t operator()(Int_t value)   { return BSearch(value); }
    Int_t operator()(Double_t value){ return BSearch(value); } 
    Int_t operator()(const Char_t *value) { return BSearch(value); }
    Int_t operator()(TString &value) { return BSearch(value); }

    Int_t operator[](Float_t value)  { return LFind(value); }
    Int_t operator[](Int_t value)    { return LFind(value); }
    Int_t operator[](Double_t value) { return LFind(value); }
    Int_t operator[](const Char_t *value) { return LFind(value); }
    Int_t operator[](TString &value) { return LFind(value); }

    ClassDef(St_TableSorter,0)
};
#endif
