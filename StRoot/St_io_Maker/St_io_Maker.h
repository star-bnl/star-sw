//  
//  
//
#ifndef STAR_St_io_Maker
#define STAR_St_io_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_io_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class  TBranch;
class  TTree;

class St_io_Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: St_io_Maker.h,v 1.1 1999/01/16 01:01:52 fine Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
   TObjArray     *m_ListOfBranches; //!

 
 public: 
            St_io_Maker(const char *name="output", const char *title="io_something",TTree *tree=0);
   virtual  ~St_io_Maker();
   virtual  TObjArray *GetListOfBranches(){return m_ListOfBranches;}
   virtual void Add(const Char_t *dataName);
   virtual void Add(TString &dataName);
   virtual void Add(TBranch *branch,const Char_t *dataName=0);
   virtual TObjArray *CreateBranchList() {if (!m_ListOfBranches) m_ListOfBranches = new TObjArray; return m_ListOfBranches;}
   virtual Int_t  Finish();
   virtual Int_t GetEvent(Int_t nevent=0);
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual TTree *MakeTree(const char* name, const char*title);
   Int_t   NextEventPut();
   Int_t   NextEventGet(Int_t nevent);
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_io_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
