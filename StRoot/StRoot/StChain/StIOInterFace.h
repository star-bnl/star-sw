#ifndef STAR_StIOInterFace
#define STAR_StIOInterFace

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StIOInterFace 			                            	        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StRTSBaseMaker_H
#include "StRTSBaseMaker.h"
#endif
#include "StTree.h"
class StUKey;

class StIOInterFace : public  StRTSBaseMaker {
private:
TString fFileName;
public:
   StIOInterFace(const char *name="",const char *iomode="r");
   virtual        ~StIOInterFace(){};
   virtual  Int_t MakeRead() {return Make();};
   virtual  Int_t MakeRead(const StUKey &/* RunEvent */) {return MakeRead();};
   virtual  Int_t Make() {assert(0);return 1999;};
   virtual  Int_t Skip(int nskip);
   virtual  Int_t MakeWrite(){assert(0);return 1999;};
   virtual  Int_t Open(const char *filename=0){assert(0&&filename);return 1999;};
   virtual  void  Close(Option_t *opt=0){assert(0&&opt);};
   virtual  Int_t  Finish();
   TString         fIOMode;	//!r=read,w=write,u=update
   TString         fTreeName;	//!Tree name
   TString         fFile;	//!Main file name name
   Int_t           fNIO;	//!number of transactions

   virtual void  SetIOMode(Option_t *iomode="w") {fIOMode=tolower(iomode[0]);};
   virtual void  SetTreeName(const Char_t *treeName="bfcTree"){fTreeName=treeName;};
   virtual const Char_t  *GetTreeName() const {return (const Char_t*)fTreeName;};
   virtual void  SetFileName(const char *fileName){fFile = fileName;};
   virtual void  SetFile(const char *fileName)    {fFile = fileName;};
   virtual const char *GetFile()     const   {return (const char*)fFile;};
   virtual const char *GetFileName() const   {return GetFile();};

   virtual void SetBranch (const Char_t *brName,const Char_t *file=0,const Char_t *iomode="w",Option_t *opt=0);
   virtual void IntoBranch(const Char_t *brName,const Char_t *logNames);

   ClassDef(StIOInterFace, 1)   //StAR chain virtual base class for Makers
};

#endif
