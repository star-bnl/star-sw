#ifndef STAR_StMaker
#define STAR_StMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMaker virtual base class for Makers                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_DataSet.h"

#ifndef ROOT_TClonesArray
#include <TClonesArray.h>
#endif

class TList;
class TBrowser;
class TChain;

class StMaker : public TNamed {

protected:

   Bool_t         m_IsClonable;  //!True if Maker objects are clonable
   Int_t          m_Save;        // = 1 if m-Maker to be saved in the Tree
   TObject       *m_Fruits;      //Pointer to maker fruits (result)
   TObject       *m_Clones;      //Pointer to clones of fruits
   TString        m_BranchName;  //Name of branch (if any)
   TList         *m_Histograms;  //Pointer to list supporting Maker histograms
   St_DataSet    *m_DataSet;     //Pointer to the Maker's dataset

public:

   enum {kSTAFCV_BAD, kSTAFCV_OK} EModule_return_Status;

                  StMaker();
                  StMaker(const char *name, const char *title="");
   virtual       ~StMaker();
   virtual void   Browse(TBrowser *b);
   virtual void   Clear(Option_t *option="");
   virtual St_DataSet *DataSet() {return m_DataSet;}
   virtual void   Draw(Option_t *option="");
   virtual void   Finish();
   TList         *Histograms() {return m_Histograms;}
   virtual void   Init();
   Bool_t         IsFolder() {return kTRUE;}
   TObject       *Fruit()  {return m_Fruits;}
   St_DataSet    *Fruits() {return (St_DataSet*)m_Fruits;}
   TObject       *Clones() {return m_Clones;}
   virtual void   FillClone();
   virtual Int_t  Make() = 0;
   virtual void   PrintInfo();
   virtual void   MakeBranch();
   virtual void   Save(Int_t save=1) {m_Save = save;}
   virtual void   SetChainAddress(TChain *chain);

   ClassDef(StMaker, 1)   //ATLFast virtual base class for Makers
};

#endif
