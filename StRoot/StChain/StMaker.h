// $Id: StMaker.h,v 1.11 1999/01/02 19:08:13 fisyak Exp $
// $Log: StMaker.h,v $
// Revision 1.11  1999/01/02 19:08:13  fisyak
// Add ctf
//
// Revision 1.10  1998/12/21 19:42:51  fisyak
// Move ROOT includes to non system
//
// Revision 1.9  1998/11/19 01:23:57  fine
// StChain::MakeDoc has been introduced, StChain::MakeDoc has been fixed (see macros/bfc_doc.C macro
//
// Revision 1.8  1998/11/18 22:46:10  fine
// The lost MakeDoc method has been re-introduced
//
// Revision 1.7  1998/10/06 18:00:27  perev
// cleanup
//
// Revision 1.5  1998/08/18 14:05:02  fisyak
// Add to bfc dst
//
// Revision 1.4  1998/07/20 15:08:09  fisyak
// Add tcl and tpt
//
#ifndef STAR_StMaker
#define STAR_StMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMaker virtual base class for Makers                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TStopwatch.h"
#include "Stypes.h"
#include "St_DataSet.h"
#include "TString.h"
#ifndef ROOT_TClonesArray
#include "TClonesArray.h"
#endif

class TList;
class TBrowser;
class TChain;
class TTree;

class StMaker : public TNamed {

protected:

   Bool_t         m_IsClonable;  //!True if Maker objects are clonable
   Int_t          m_Save;        // = 1 if m-Maker to be saved in the Tree
   TObject       *m_Fruits;      //Pointer to maker fruits (result)
   //   TClonesArray  *m_Clones;      //Pointer to clones of fruits
   TString        m_BranchName;  //Name of branch (if any)
   St_DataSet    *m_DataSet;     //Pointer to the Maker's dataset
   TString        m_BranchFile;  //
   TTree         *m_Tree;        //! TTree to write this branch out
   TList         *m_Histograms;  // Pointer to list supporting Maker histograms
   TStopwatch     m_Timer;       // Timer object
   
public:

   enum {kSTAFCV_BAD, kSTAFCV_OK} EModule_return_Status;

                  StMaker();
                  StMaker(const char *name, const char *title="");
   virtual       ~StMaker();
   virtual void   Browse(TBrowser *b);
   virtual void Clear(Option_t *option="");
   virtual St_DataSet *DataSet() const {return m_DataSet;}
   void           SetDataSet (St_DataSet *set);
   virtual void   Draw(Option_t *option="");
   virtual Int_t  Finish();
   TList         *Histograms() {return m_Histograms;}
   virtual TTree *GetTree();  
   virtual Int_t  Init();
   Bool_t         IsFolder() {return kTRUE;}
   TObject       *Fruit()  {return m_Fruits;}
   St_DataSet    *Fruits() {return (St_DataSet*)m_Fruits;}
   //   TObject       *Clones() {return m_Clones;}
   virtual void   FillClone();
   virtual Int_t  IsToSave(){return m_Save;}
   virtual Int_t  Make() = 0;
   virtual void   MakeDoc(const TString &stardir="$(afs)/rhic/star/packages/dev",const TString &outdir="$(star)/StRoot/html");
   virtual void   PrintInfo();
   virtual void   MakeBranch();
   virtual void   Save(Int_t save=1) {m_Save = save;}
   virtual void   SetBranch();
   virtual void   SetTree(TTree *tree=0){ m_Tree = tree;}
   virtual void   StartTimer(Bool_t reset = kFALSE){m_Timer.Start(reset);}
   virtual void   StopTimer(){m_Timer.Stop();}
   virtual void   PrintTimer(Option_t *option="");
   virtual void   SetChainAddress(TChain *chain);
   virtual void   SetBranchFile (TString &name){m_BranchFile = name;}  // *MENU*
           TTree *Tree(){return GetTree(); }

   virtual TString  GetBranchFile (){return m_BranchFile;}
   ClassDef(StMaker, 1)   //StChain virtual base class for Makers
};

#endif
