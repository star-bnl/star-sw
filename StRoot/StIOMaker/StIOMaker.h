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
   StIOMaker(const char *name,const char *iomode, const char *ioFile="",const char *treeName="bfcTree");
   StIOMaker(const char *name,const char *iomode, StFile      *fileSet ,const char *treeName="bfcTree");
   virtual       ~StIOMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
           Int_t  MakeRead();
           Int_t  MakeWrite();
           Int_t  Open();
           void   Close(Option_t *opt=0);
   virtual Int_t  Finish();
   virtual void Clear(Option_t *opt);
   
   virtual void   PrintInfo();
           void   SetMaxEvent(Int_t mx=10000000){fMaxEvent=mx;fNumEvent=0;};

   StFile         *fFileSet;    //!Chain of files

   StIOInterFace  *fCurrMk;	//!Pointer to Current Maker
   StIOInterFace  *fFmtMk[9];	//!Pointers to TreeMaker,xdfin_Maker,St_io_Maker


   Int_t  fMaxEvent;		//! for debug only
   Int_t  fNumEvent;    	//! for debug only


   void   SetFileSet(StFile *fileSet){fFileSet = fileSet;};

protected:


   Int_t  fMaxEvt;
   Int_t  fNumEvt;


void Build(StFile *fileSet,const char *treeName);
StIOInterFace *Load();


//	Data members

Int_t  fCase    ;		//! case 1=root,2=xdf,3=mdc2


   ClassDef(StIOMaker, 1)   //StAR chain virtual base class for Makers
};

#endif
