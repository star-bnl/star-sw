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
   StTreeMaker(const char *name,const char *ioFile=0,const char *treeName="bfcTree");
   virtual       ~StTreeMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
           Int_t  MakeRead();
           Int_t  MakeWrite();
   virtual Int_t  Finish();
   virtual void Clear(Option_t *opt);
   virtual Int_t  Open(const Char_t *ioFile=0);
   virtual Int_t  Close();
   
   virtual void   PrintInfo();

   TString        fIOMode;	//!r=read,w=write,u=update
   TString        fTreeName;	//!Tree name
   StTree        *fTree;	//!

   void  SetIOMode(Option_t *iomode="w") {fIOMode=iomode;};
   void  SetTreeName(const Char_t *treeName="bfcTree")
     {fTreeName=treeName;if (fTree) fTree->SetName(fTreeName);};
   const Char_t  *GetTreeName()
     {return (const Char_t*)fTreeName;};
   StTree *GetTree(){return fTree;};
   StBranch *GetBranch(const Char_t *brName)
     {if(!fTree)return 0;return (StBranch*)fTree->Find(brName);};
   void SetBranch(const Char_t *brName,const Char_t *file=0,const Char_t *iomode="w");
   void IntoBranch(const Char_t *brName,const Char_t *logNames);
   
   ClassDef(StTreeMaker, 1)   //StAR chain virtual base class for Makers
};

#endif
