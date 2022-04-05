/*!
 * \class StIOMaker 
 */
#ifndef STAR_StIOMaker
#define STAR_StIOMaker


#include "StMaker.h"
#include "StTree.h"
#include "StIOInterFace.h"

//class St_io_Maker;
//class St_xdfin_Maker;
//class StTreeMaker;

class StIOMaker : public StIOInterFace {
public:
   StIOMaker(const char *name="StIO",const char *iomode="r", const char *ioFile="",const char *treeName="bfcTree");
   StIOMaker(const char *name,       const char *iomode,     StFileI  *fileSet ,const char *treeName="bfcTree");
   virtual       ~StIOMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
           Int_t  MakeRead();
           Int_t  MakeRead(const StUKey &);
           Int_t  MakeWrite();
           Int_t  Open(const char * = 0);
           Int_t  OpenRead();
           Int_t  OpenWrite();
           void   Close(Option_t *opt=0);
   virtual Int_t  Finish();
   virtual void   Clear(Option_t *opt);
   virtual void   SetFile(const char *file);   
           void   SetMaxEvent(Int_t mx=10000000){fMaxEvent=mx;fNumEvent=0;};
   void   SetFileSet(StFileI *fileSet){fFileSet = fileSet;};
   virtual Int_t Skip(int nskip){fSkip=nskip;return 0;}
   virtual Int_t Skip();
   virtual void  Rewind();
   virtual void  NotifyMe(const char *about,const void *info);
   

protected:

void Build(StFileI *fileSet,const char *ioFile,const char *treeName);
StIOInterFace *Load();


//	Data members


   StFileI        *fFileSet;    //!Chain of files
   TString         fNextFile;	//!next file from file set
   StIOInterFace  *fCurrMk;	//!Pointer to Current Maker
   StIOInterFace  *fFmtMk[9];	//!Pointers to TreeMaker,xdfin_Maker,St_io_Maker,StDAQMaker
   Int_t  fMaxEvent;		//! for debug only
   Int_t  fNumEvent;    	//! for debug only
   Int_t  fCase    ;		//! case 1=root,2=xdf,3=mdc2,4=daq
   Int_t  fSkip    ;		//! 

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StIOMaker.h,v 1.15 2014/08/06 11:43:21 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

   ClassDef(StIOMaker,0)   //StAR chain virtual base class for Makers
};

#endif
