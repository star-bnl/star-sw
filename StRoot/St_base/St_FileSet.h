//*CMZ :          05/08/98  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   05/08/98 

#ifndef ROOT_St_FileSet
#define ROOT_St_FileSet
  
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_FileSet                                                           //
//                                                                      //
// St_FileSet class is a class to convert the                           // 
//      "native file system structure"                                  //
// into an instance of the St_DataSet class                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include "St_DataSet.h"
#include "TString.h"
  
class St_FileSet : public St_DataSet
{
 public: 
    St_FileSet();
    St_FileSet(const TString &dirname, const Char_t *filename=".",Bool_t expand=kTRUE);
    virtual ~St_FileSet();
    virtual Long_t HasData() const;
    virtual Bool_t IsEmpty() const;
    virtual Bool_t IsFolder();
    ClassDef(St_FileSet,1)
};

#endif
