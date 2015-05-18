#ifndef ROOT_StTree
#define ROOT_StTree
 


#include "TSystem.h"
#include "TROOT.h"
#include "TFile.h"
#include "TList.h"
#include "TDataSet.h"
#include "St_ObjectSet.h"
#include "TDataSetIter.h"
#include "StFileI.h"



class StIO 
{

 public:
 
 static Int_t    Write    (TFile *file, const StUKey &ukey, TObject  *obj);
 static TObject *Read     (TFile *file, const char *name);
 static TObject *Read     (TFile *file, const StUKey &ukey);
 static Int_t   GetNextKey(TFile *file,       StUKey &ukey, ULong_t &handle);
 static TObject *ReadNext (TFile *file,       StUKey &ukey, ULong_t &handle);
 static TString  RFIOName (const char *name);
 static TFile   *Open     (const char *name, Option_t *option="",const char *title="",Int_t compress=1);
 static Int_t    IfExi(const char *file);
 static void    SetDebug(Int_t dblev=1){fgDebug=dblev;}
 static Int_t   fgDebug;
};

class StTree;
class StBranch : public TDataSet {
//friend class StBranch;
friend class StTree;
public:
  StBranch(const char *name="", StTree* parent=0,Option_t *opt=0);
 ~StBranch();

  virtual void SetIOMode(Option_t *iomode="0");
  virtual Option_t *GetIOMode();
  virtual void SetOption(Option_t *opt);
  virtual Option_t *GetOption() const {return fOption;};
  virtual Bool_t IsOption(Option_t *opt) const 
          {return fOption.Contains(opt,TString::kIgnoreCase);};
  virtual Int_t UpdateFile(const char *file);
  virtual const char *GetFile();
  virtual Int_t SetFile(const char *file,const char *iomode=0,int insist=0);
  virtual Int_t SetTFile(TFile *tfile);
  virtual TFile        *GetTFile(){return fTFile;};
  virtual void SetName(const char *name){fUKey=name;TDataSet::SetName(name);};
  virtual void SetUKey(Int_t ukey){fUKey=ukey;};
  virtual StUKey GetUKey()  const {return fUKey;};
  virtual Int_t GetNEvents() const {return fNEvents;};
  virtual Int_t GetEvent(Int_t mode);
  virtual Int_t ReadEvent (const StUKey &ukey);
  virtual Int_t NextEvent (      StUKey &ukey);
  virtual Int_t NextEvent ();
  virtual Int_t WriteEvent(const StUKey &ukey);
  virtual void Clear(const char *opt=0);
  virtual void Close(const char *opt=0);
  virtual Int_t Open();
  virtual Int_t GetDebug(){return fDebug;};
  virtual void  SetDebug(int dbl=1){fDebug=dbl;};

protected:
  virtual void OpenTFile();
  void SetParAll(TDataSet *parNew,TDataSet *parOld, TList *savList);
  void SetParAll(TList *savList);
  Int_t fNEvents; 		//  Number of written events in file
  StUKey  fUKey;          	//! Current RunEvent number 
  char fIOMode;		        //! 1=ReadOnly; 2=WriteOnly; 1+2=Update;0=do nothing
  TString fFile;		//  File name
  TString fOption;		//  Option string
  TFile   *fTFile;		//! Opened TFile
  Int_t   fDebug;		//! debug level
  ULong_t fHandle;              //!
ClassDef(StBranch,1)
};  


class StTree : public StBranch {
public:
  StTree(const char *name="");
 ~StTree();

  virtual void  SetIOMode (Option_t *iomode="0");			//Set for all branches
  virtual Int_t ReadEvent (const StUKey &ukey);
  virtual Int_t NextEvent (      StUKey &ukey);
  virtual Int_t NextEvent ();
          Int_t NextKey();
          Int_t Skip(int nskip);
  virtual Int_t WriteEvent(const StUKey &ukey);
  virtual void  Close(const char *opt=0);
  virtual Int_t Open();
  virtual void Clear(Option_t *opt="");
  virtual Int_t SetFile(const char *file,const char *iomode=0,int insist=0);
  virtual void SetBaseName(const char* basename,const char* dirname=0);
  virtual const char *GetBaseName() 
          {return (fBaseName.IsNull()) ? 0:(const char*)fBaseName;};
  static StTree *GetTree(TFile *file, const char *treeName);
  virtual Int_t UpdateFile(const char *file);

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
 virtual Bool_t IsFolder() const { return kTRUE; }

ClassDef(StIOEvent,1)
};

class StFile : public StFileI
{
public:
  StFile(const char** fileList=0);
  virtual ~StFile();

  virtual void  ls(Option_t *opt="") ;
  virtual void  ls(Option_t *opt="") const{ ((StFile*)this)->ls(opt);}
  virtual Int_t AddFile(const char *file,const char *opt=0); //opt="remove" or "exclude" remove file from list
  virtual Int_t AddFile(const char **fileList);
  virtual Int_t AddWild(const char *file,const char *opt=0);
  virtual Int_t AddEvent(UInt_t r,UInt_t e=0);
  virtual Int_t GetNBundles();
  virtual Int_t GetNFiles();
  virtual Int_t GetBundleSize(){return 1;};
  virtual StUKey GetNextEvent();
  virtual Int_t GetNextEvent(UInt_t *NextEventNumber){return StFileI::GetNextEvent(NextEventNumber);}

  virtual const char *GetFileName(Int_t idx=-1);
  virtual const char *GetCompName(Int_t idx=0);
  virtual const char *GetFormat(Int_t idx=0);
  virtual Int_t GetNextBundle();
  virtual void Rewind(){fIter = -1; delete fKeyIter; fKeyIter =0;}
  
protected:
  void  lsFull(Option_t *opt="") ;
  void SetInfo(TDataSet *ds);
  const char *GetAttr(TDataSet *ds,const char *att);
  TDataSet *GetFileDS(int idx);
  TDataSet *fDS;
  Int_t    fIter; 
  TDataSetIter *fKeyIter; 
  ClassDef(StFile,1)
};
#endif
