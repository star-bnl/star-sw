/*!
 * \class StFileI
 */
#ifndef ROOT_StFileI
#define ROOT_StFileI

#include "TNamed.h"
#include "assert.h"
#include "string.h"
 
const UInt_t kUMAX = (UInt_t)(-1);

class StUKey
{
public:
  StUKey(const char *name=0,UInt_t *uk=0,int nk=1);
  StUKey(const char *name,UInt_t uk);
  StUKey(UInt_t uRun,UInt_t uEvent=0);
  virtual ~StUKey(){}
  virtual  StUKey &operator=( const StUKey &from);
  virtual  StUKey &operator=( UInt_t from);
  virtual  StUKey &operator=( Int_t from){return *this=(UInt_t)from;}
  virtual  StUKey &operator=( const char *from);
  virtual  void    Update(const StUKey &from,const char *name=0);
  virtual  void    SetName(const char *name){fName=name;} 
  virtual  const char *GetName() const {return fName;} 
  virtual  TString GetKey() const; 
  virtual  Int_t   GetNInt() const{ return fNUrr;} 
  virtual  const UInt_t *GetUrr() const{ return fUrr;} 
  virtual  void    SetKey(const char *key);
  virtual  void    SetUrr(const UInt_t *key,int nk);
  virtual  UInt_t  GetSum() const;
  virtual  Int_t   EOK()    const { return fUrr[0]==kUMAX;}
  virtual  Int_t   IsNull() const { return !fUrr[0];}
  virtual  Int_t   Compare(StUKey &uk2) const 
                  { return strcmp(GetKey().Data(),uk2.GetKey().Data());}
  virtual  Int_t GetRunId()   const { return fUrr[0]; }
  virtual  Int_t GetEventId() const { return fUrr[1]; }
private:
  TString fName;
  Int_t fNUrr;
  UInt_t fUrr[9];
};


class StFileI : public TNamed
{
public:
  StFileI(const char *name="",const char *titl=""):TNamed(name,titl){SetDebug(0);}
  virtual ~StFileI(){}
  virtual Int_t SetDebug(Int_t dbl=1){fDebug=dbl; return fDebug;}
  virtual Int_t GetDebug() const {return fDebug;}
  virtual Int_t Init(int /* Argc=0 */, const char** /* Argv=0 */){return 0;}
  virtual Int_t Init(const char *argv)
                {const char *Argv[1]; Argv[0]=argv; return Init(1,Argv);};
 
  virtual void  ls(Option_t *opt="") const {if(opt){};};
  virtual Int_t AddFile(const char */* file */,const char * opt=0){if (opt) {}; return 0;};
  virtual Int_t AddFile(const char **/* fileList */){return 0;};
  virtual Int_t AddWild(const char */* file */,const char */* opt=0 */){return 0;};
  virtual Int_t GetNFiles()=0;
  virtual Int_t GetNBundles()=0;
  virtual Int_t GetBundleSize()=0;
  virtual const char *GetFileName(Int_t idx=-1)=0;
  virtual const char *GetCompName(Int_t idx=0)=0;
  virtual const char *GetFormat(Int_t idx=0)=0;
  virtual Int_t GetNextBundle()=0;
  virtual Int_t GetNextEvent(UInt_t *NextEventNumber)
                {*NextEventNumber=0;return 0;}
  virtual StUKey GetNextEvent();
  virtual  void Rewind(){assert(0);};
protected:
 Int_t fDebug;
 
ClassDef(StFileI,0)
};
#endif
