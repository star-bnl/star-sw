//*CMZ :          23/02/99  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@bnl.gov)   03/07/98
//
//  
//  
//
#ifndef STAR_St_io_Maker
#define STAR_St_io_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_io_Maker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html  //
//
//////////////////////////////////////////////////////////////////////////
#ifndef StIOInterFace_H
#include "StIOInterFace.h"
#endif
 
class  TBranch;
class  TTree;
class  TFile;

class St_io_Maker : public StIOInterFace {
 private:
   Bool_t fSplit;    // flag of the "split" mode
// static Char_t  m_VersionCVS = "$Id: St_io_Maker.h,v 1.15 1999/12/29 22:21:15 fine Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
   TFile         *m_TreeRootFile;   //! ROOT file to keep TTRee object in their.
   TObjArray     *m_ListOfBranches; //!
   TObjArray     *m_ListOfFiles;    //!
   TIter         *m_FileIterator;   //!
   Int_t          m_OffSet;         // Event offset for multi-volumes tree's
   Stat_t         m_Entries;        // Number of the events of the current tree.
   Int_t          m_MaxEventToProcess; // Max # if events to rpocess from each files
   TTree         *m_Tree;           //! Local Tree of this maker.
   TString        m_BranchName;      //?????????
 protected:
   virtual TTree *SetNextTree();
   virtual void   BuildBranchList(TTree *tree);
   virtual TObjArray *CreateBranchList() {if (!m_ListOfBranches) m_ListOfBranches = new TObjArray; return m_ListOfBranches;}
   virtual void DestroyBranchList();
 
 public: 
            St_io_Maker(const char *name="output", const char *title="io_something",Bool_t split=kTRUE,TTree *tree=0);
           void Destructor();
   virtual  ~St_io_Maker(){Destructor();};
   virtual TObjArray *GetListOfBranches(){return m_ListOfBranches;}
   virtual void       Add(const Char_t *dataName,const Char_t *fileName=0);
   virtual void       Add(TString &dataName,const Char_t *fileName=0);
   virtual void       Add(TBranch *branch,const Char_t *dataName=0,const Char_t *fileName=0);
   virtual void       AddFile(const Char_t *fileName);
   virtual Int_t      AddFilesFromFile(const Char_t *fileName);
   virtual void       Clear(Option_t *option="");
//VP   virtual St_DataSet *DataSet(const Char_t *set);
   virtual Int_t      Finish();
   virtual Stat_t     GetEvent(Int_t nevent=0);
   virtual Stat_t     GetMaxEvent(){ return Stat_t(m_MaxEventToProcess);} 
   virtual TTree     *GetTree();  
   virtual Int_t      Init();
   virtual Bool_t     IsNewTree(Int_t nevent);
   virtual Int_t      Make();
   virtual TTree     *MakeTree(const char* name, const char*title);
           Int_t      NextEventPut();
           Int_t      NextEventGet(Int_t nevent);
   virtual Int_t      SetActive();
   virtual void       SetMaxEvent(Int_t events=10000000){ m_MaxEventToProcess = events;} 
   virtual void       SetTree(TTree *tree=0){ m_Tree = tree;}
   virtual TTree     *Tree() { return m_Tree;}
//   virtual Int_t      SetFile(const Char_t *rootFileName);

// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*

//VP		StIOInterFace part
   
   virtual Int_t Open (const char *dummy=0){return Init();};
   virtual void  Close(const char *dummy=0){Finish();};
   virtual void  SetFile(const char *file){ AddFile(file);};
   virtual void  SetBranch(const char *br,const char *file=0,const char *iomode="w")
   { AddFile(file);};


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_io_Maker.h,v 1.15 1999/12/29 22:21:15 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_io_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
