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
#include "StIOInterFace.h"
#include "StTree.h"

class StTreeMaker : public StIOInterFace {
private:
   Int_t  fFinished;
public:
   StTreeMaker(const char *name="",const char *ioFile="",const char *treeName=0);
   virtual       ~StTreeMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  MakeRead(UInt_t *RunEvent);
   virtual Int_t  MakeRead(){return MakeRead(0);};
   virtual Int_t  MakeWrite();
   virtual Int_t  Finish();
   virtual Int_t  Save();
   virtual void   Clear(Option_t *opt);
   virtual Int_t  Open(const Char_t *ioFile=0);
   virtual void   Close(Option_t *opt=0);
           void   UpdateTree(Int_t flag);
           void   UpdateHddr();
   virtual void   FillHistBranch(StBranch *histBr);
  

   StTree        *fTree;	//!
   StTree *GetTree(){return fTree;};
   StBranch *GetBranch(const Char_t *brName)
     {if(!fTree)return 0;return (StBranch*)fTree->Find(brName);};   

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTreeMaker.h,v 1.12 2000/04/07 15:30:29 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTreeMaker, 1)   //StAR chain virtual base class for Makers
};

#endif
