#ifndef STAR_StIOMaker
#define STAR_StIOMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StIOMaker 			                            	        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif
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
           Int_t  MakeWrite();
           Int_t  Open();
           void   Close(Option_t *opt=0);
   virtual Int_t  Finish();
   virtual void Clear(Option_t *opt);
   virtual void   SetFile(const char *file);   
           void   SetMaxEvent(Int_t mx=10000000){fMaxEvent=mx;fNumEvent=0;};
   void   SetFileSet(StFileI *fileSet){fFileSet = fileSet;};
   virtual Int_t Skip(int nskip);
protected:

void Build(StFileI *fileSet,const char *treeName);
StIOInterFace *Load();


//	Data members


   StFileI      *fFileSet;    //!Chain of files
   const char     *fNextFile;	//!next file from file set
   StIOInterFace  *fCurrMk;	//!Pointer to Current Maker
   StIOInterFace  *fFmtMk[9];	//!Pointers to TreeMaker,xdfin_Maker,St_io_Maker,StDAQMaker
   Int_t  fMaxEvent;		//! for debug only
   Int_t  fNumEvent;    	//! for debug only
   Int_t  fCase    ;		//! case 1=root,2=xdf,3=mdc2,4=daq

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StIOMaker.h,v 1.7 2000/04/03 23:54:41 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StIOMaker, 1)   //StAR chain virtual base class for Makers
};

#endif
