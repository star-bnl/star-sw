#ifndef St_FilePathC_h
#define St_FilePathC_h

#include "TChair.h"
#include "tables/St_FilePath_Table.h"
#include "TString.h"
#include "TSystem.h"
class St_FilePathC : public TChair {
 public:
  FilePath_st 	*Struct(Int_t i = 0) 	const {return ((St_FilePath*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Char_t* 	file(Int_t i = 0) 	const {return Struct(i)->file;}
  const Char_t* GetPath(const Char_t *prepend = "$STAR/StarDb/Calibrations/tpc/");
 protected:
  St_FilePathC(St_FilePath *table=0) : TChair(table) {}
  virtual ~St_FilePathC() {}
 private:
  ClassDefChair(St_FilePath, FilePath_st )
  ClassDef(St_FilePathC,1) //C++ TChair for FilePath table class
};
#endif
