#ifndef ROOT_StTree
#define ROOT_StTree
 

////#ifdef __CINT__
////
////#pragma link off all globals;
////#pragma link off all classes;
////#pragma link off all functions;
////#pragma link C++ class StBranch;
////#pragma link C++ class StTree;
////#endif

#include "TSystem.h"
#include "TROOT.h"
#include "TFile.h"
#include "TList.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

const ULong_t kUMAX = (ULong_t)(-1);

class StIO 
{
enum {kStSorted = 0x00100000};

 public:
 
 static Int_t    Write    (TFile *file, const Char_t *name, ULong_t  ukey, TObject  *obj);
 static TObject *Read     (TFile *file, const Char_t *name, ULong_t  ukey);
 static ULong_t GetNextKey(TFile *file, const Char_t *name, ULong_t  ukey);
 static TObject *ReadNext (TFile *file, const Char_t *name, ULong_t &ukey);
 static TString &MakeKey  (const Char_t *name, ULong_t ukey);
};

class StTree;
class StBranch : public St_DataSet {
//friend class StBranch;
friend class StTree;
public:
  StBranch(const Char_t *name="", StTree* parent=0);
 ~StBranch();

  virtual void SetIOMode(Option_t *iomode="0");
  virtual Option_t *GetIOMode();
  virtual void SetOption(Option_t *opt);
  virtual Option_t     *GetOption() const {return fOption;};
  virtual Int_t UpdateFile(const Char_t *file);
  virtual const Char_t *GetFile();
  virtual Int_t SetFile(const Char_t *file,const Char_t *iomode=0,int insist=0);
  virtual Int_t SetTFile(TFile *tfile);
  virtual TFile        *GetTFile(){return fTFile;};
  virtual void SetUKey(ULong_t ukey){if (ukey!=kUMAX) fUKey=ukey;};
  virtual ULong_t GetUKey()  const {return fUKey;};
  virtual Int_t GetNEvents() const {return fNEvents;};
  virtual Int_t GetEvent(Int_t mode);
  virtual Int_t ReadEvent (ULong_t  ukey=kUMAX);
  virtual Int_t NextEvent (ULong_t &ukey);
  virtual Int_t NextEvent ();
  virtual Int_t WriteEvent(ULong_t  ukey=kUMAX);
  virtual void Clear(const char *opt=0);
  virtual void Close(const char *opt=0);
  virtual Int_t Open();
  virtual Int_t GetDebug(){return fDebug;};
  virtual void  SetDebug(int dbl=1){fDebug=dbl;};

protected:
  virtual void OpenTFile();
  void SetParAll(St_DataSet *par,TList *savList);
  void SetParAll(TList *savList);
  Int_t fNEvents; 		//  Number of written events in file
  ULong_t fUKey;          	//! Current RunEvent number 
  Char_t fIOMode;		//! r=ReadOnly; w=WriteOnly; u=Update;0=do nothing
  TString fFile;		//  File name
  TString fOption;		//  Option string
  TFile   *fTFile;		//! Opened TFile
  Int_t   fDebug;		//! debug level
ClassDef(StBranch,1)
};  


class StTree : public StBranch {
public:
  StTree(const Char_t *name="");
 ~StTree();

  virtual void  SetIOMode (Option_t *iomode="0");			//Set for all branches
  virtual Int_t ReadEvent (ULong_t  ukey=kUMAX);
  virtual Int_t NextEvent (ULong_t &ukey);
  virtual Int_t NextEvent ();
  virtual Int_t WriteEvent(ULong_t  ukey=kUMAX);
  virtual void  Close(const char *opt=0);
  virtual Int_t Open();
  virtual void Clear(Option_t *opt="");
  virtual Int_t SetFile(const Char_t *file,const Char_t *iomode=0,int insist=0);
  virtual void SetBaseName(const char* basename);
  virtual const char *GetBaseName() 
          {return (fBaseName.IsNull()) ? 0:(const char*)fBaseName;};
  static StTree *GetTree(TFile *file, const char *treeName);
  virtual Int_t UpdateFile(const Char_t *file);

protected:
  TString fBaseName;		//base name to construct branch file name
				// as <basename>.<branchname>.root
ClassDef(StTree,1)
};  

//	Auxiliary class for StIO only.
class StIOEvent : public TObject 
{
public:
  StIOEvent();
 ~StIOEvent(){};
TObject *fObj;	// Pointer to full tree
 virtual void  Browse(TBrowser *b);
  virtual Bool_t IsFolder(){ return fObj ? kTRUE: kFALSE; }

ClassDef(StIOEvent,1)
};

class StFile : public St_DataSet
{
public:
  StFile(Int_t nbranches=1);
 ~StFile(){};
 
  Int_t AddFile(const Char_t *file,const Char_t *branch=0);
  Int_t AddWild(const Char_t *file);
  Int_t GetNBranches();
  const Char_t *NextFileName();
  const Char_t *GetBraName(){return GetAttr("branch=");}; 
  const Char_t *GetFormat() {return GetAttr("format=");};
  //  Int_t Check(){return 0;};
protected:
  void SetInfo();
  const Char_t *GetAttr(const char *att); 

  ClassDef(StFile,1)
};
#endif
