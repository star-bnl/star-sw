#ifndef STAR_StTreeMaker
#define STAR_StTreeMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTreeMaker 			                            	//
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StTree.h"

class StTreeMaker : public StMaker {
private:
TString fFileName;
public:
   StTreeMaker(const char *name,const char *ioFile=0);
   virtual       ~StTreeMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
           Int_t  MakeRead();
           Int_t  MakeWrite();
   virtual Int_t  Finish();
   virtual void Clear(Option_t *opt);
   
   virtual void   PrintInfo();
   TString        fIOMode;	//!r=read,w=write,u=update
   StTree        *fTree;	//!
   St_DataSet    *fMakers;	//!
   StMaker       *fTopMaker;	//!
   TString       fDefaultBranch;//!
   void  SetIOMode(Option_t *iomode="w"){fIOMode=iomode;};
private:
   
   StBranch *BranchOfMaker(StMaker* mk,int doit=0);
   ClassDef(StTreeMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
