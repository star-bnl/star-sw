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
   StIOMaker(const char *name,       const char *iomode,     StFile      *fileSet ,const char *treeName="bfcTree");
   virtual       ~StIOMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
           Int_t  MakeRead();
           Int_t  MakeWrite();
           Int_t  Open();
           void   Close(Option_t *opt=0);
   virtual Int_t  Finish();
   virtual void Clear(Option_t *opt);
   
           void   SetMaxEvent(Int_t mx=10000000){fMaxEvent=mx;fNumEvent=0;};

   StFile         *fFileSet;    //!Chain of files

   StIOInterFace  *fCurrMk;	//!Pointer to Current Maker
   StIOInterFace  *fFmtMk[9];	//!Pointers to TreeMaker,xdfin_Maker,St_io_Maker,StDAQMaker


   Int_t  fMaxEvent;		//! for debug only
   Int_t  fNumEvent;    	//! for debug only


   void   SetFileSet(StFile *fileSet){fFileSet = fileSet;};

protected:


   Int_t  fMaxEvt;
   Int_t  fNumEvt;


void Build(StFile *fileSet,const char *treeName);
StIOInterFace *Load();


//	Data members

Int_t  fCase    ;		//! case 1=root,2=xdf,3=mdc2,4=daq

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StIOMaker.h,v 1.3 1999/07/15 13:57:12 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StIOMaker, 1)   //StAR chain virtual base class for Makers
};

#endif
