#ifndef ROOT_StFileI
#define ROOT_StFileI
 


#include "TNamed.h"


class StFileI : public TNamed
{
public:
  StFileI(const char *name="",const char *titl=""):TNamed(name,titl){SetDebug(0);}
  virtual ~StFileI(){}
  virtual Int_t SetDebug(Int_t dbl=1){fDebug=dbl; return fDebug;}
  virtual Int_t GetDebug() const {return fDebug;}
  virtual Int_t Init(int Argc=0, const char** Argv=0){return 0;}
  virtual Int_t Init(const char *argv)
                {const char *Argv[1]; Argv[0]=argv; return Init(1,Argv);};
 
  virtual void  ls(Option_t *opt=""){if(opt){};};
  virtual Int_t AddFile(const Char_t *file,const Char_t *comp=0){return 0;};
  virtual Int_t AddFile(const Char_t **fileList){return 0;};
  virtual Int_t AddWild(const Char_t *file){return 0;};
  virtual Int_t GetNFiles()=0;
  virtual Int_t GetNBundles()=0;
  virtual Int_t GetBundleSize()=0;
  virtual const Char_t *GetFileName(Int_t idx=-1)=0;
  virtual const Char_t *GetCompName(Int_t idx=0)=0;
  virtual const Char_t *GetFormat(Int_t idx=0)=0;
  virtual Int_t GetNextBundle()=0;
  virtual Int_t GetNextEvent(UInt_t *NextEventNumber)
                {*NextEventNumber=0;return 0;}
protected:
 Int_t fDebug;
 
//ClassDef(StFileI,1)
};
#endif
