// Author: Victor Perev   08/04/01


#ifndef ROOT_TTreeIter
#define ROOT_TTreeIter


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TTreeIter                                                            //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TTree.h"
#include "TString.h"
class TChain;
class TTreeIterMem;
class TTreeIterCast
{
 public:
  TTreeIterCast(Int_t *nerr){fV=0;fN=0;fT=0;fE=nerr;}
  ~TTreeIterCast(){;}
  void Set(void* v,Int_t t,const char* name=0);
  
  operator const Char_t  		&();	//type 1
  operator const Short_t 		&();	//type 2
  operator const Int_t 		&();		//type 3
  operator const Long_t		&();		//type 4
  operator const Float_t 	&();		//type 5
  operator const Double_t 	&();		//type 8
  // 	operator const Double32_t 	&();	//type 9
  operator const UChar_t  	&();		//type 11
  operator const UShort_t 	&();		//type 12
  operator const UInt_t 	&();		//type 13
  operator const ULong_t	&();		//type 14
  
  operator const Char_t*  	&();		//type 21
  operator const Short_t* 	&();		//type 22
  operator const Int_t* 	&();		//type 23
  operator const Long_t*	&();		//type 24
  operator const Float_t* 	&();		//type 25
  operator const Double_t* 	&();		//type 28
  //  	operator const Double32_t* 	&();	//type 29
  operator const UChar_t*  	&();		//type 31
  operator const UShort_t* 	&();		//type 32
  operator const UInt_t* 		&();	//type 33
  operator const ULong_t*		&();	//type 34
  void *Addr(){return fV;}
  
 private:
  void *Addr(Int_t outType);
  void   *fV;
  Int_t 	fT;
  Int_t  *fE;
  const char   *fN;
};


class TTreeIter : public TNamed{
  
 protected:
  Int_t         fEntry;         //!  current entry number
  TChain       *fTree;	      	//!  pointer to TTree/TChain object
  TObjArray     fMemList;	//!  list of mem objects
  TObjArray     fBraList;	//!  list of used branches
  Int_t         fUnits;		//!  current number of units
  Int_t         fNErr;		//|
  Int_t         fCint;		//|
  Int_t         fNFiles;	//|
  
  TTreeIterCast   fCast;	//!
 public:
  
  TTreeIter(TTree *tree);
  TTreeIter(TChain *tree);
  TTreeIter(const char *treeName="");
  virtual ~TTreeIter();
  TChain   *Chain() {return fTree;}
  TTreeIterCast &operator() (const TString varname);
  void **Void(const TString varname);
#ifndef __CINT__
  TTreeIterCast &operator() (const char   *varname);
  void **Void(const char   *varname);
#endif
  Int_t             AddFile(const char *file);
  TBranch          *GetBranch(int idx) const;     
  Int_t             Next(Int_t ient=-1);
  virtual void      Print(Option_t* option) const;
  virtual void      ls(Option_t* option) const;
  void              Reset(){fEntry=0;}
  Bool_t            Notify();
  const char       *IsCorrupted() const;
  static const char*    TypeName(Int_t ity);
  static Int_t          TypeSize(Int_t ity);
  static Int_t          TypeCode(const char *typeName);
  static void Print(const TBranch *tb,Int_t lvl=0, Option_t* option="");
  static void ls(const TTree *tt, Option_t* option="");
  static void ls(const TObjArray *brList,Int_t lvl,Option_t* option="");
  static void GetInfo(const TBranch *tb, const char *&tyName,Int_t &units,void  *&add,Int_t &brType);
  
  
 private:
  void Init();
  void WhichTree(const char *fileName);
  
  ClassDef(TTreeIter,0)    
};
#endif //ROOT_TTreeIter
