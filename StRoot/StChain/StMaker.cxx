
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChain virtual base class for StMaker                              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <TChain.h>
#include <TTree.h>
#include <TList.h>
#include <TClonesArray.h>
#include <TBrowser.h>

#include "StMaker.h"
#include "StChain.h"

ClassImp(StMaker)

//_____________________________________________________________________________
StMaker::StMaker()
{
   m_BranchName = "";
   m_Save       = 0;
   m_Histograms = 0;
   m_Fruits     = 0;
   m_Clones     = 0;
   m_IsClonable = kTRUE;
   m_DataSet    = 0;
}

//_____________________________________________________________________________
StMaker::StMaker(const char *name, const char *title)
       :TNamed(name,title)
{
   m_BranchName = "";
   m_Save       = 0;
   m_Histograms = new TList();
   m_Clones     = 0;
   m_IsClonable = kTRUE;
   m_DataSet    = 0;
   gStChain->Makers()->Add(this);  
}

//_____________________________________________________________________________
StMaker::~StMaker()
{
  if (m_Fruits)  {delete m_Fruits;  m_Fruits = 0;}
  if (m_Clones)  {delete m_Clones;  m_Clones = 0;}
  if (m_DataSet) {delete m_DataSet; m_DataSet = 0;}
}

//______________________________________________________________________________
void StMaker::Browse(TBrowser *b)
{
//  Insert Maker objects in the list of objects to be browsed.

  char name[64];
  if( b == 0) return;
  if (m_Histograms) b->Add(m_Histograms,"Histograms");
  if (m_Fruits == 0) return;
   TObject *obj;

// If m_Fruits is a ClonesArray, insert all the objects in the list
// of browsable objects
  if (m_Fruits->InheritsFrom("TClonesArray")) {
     TClonesArray *clones = (TClonesArray*)m_Fruits;
     Int_t nobjects = clones->GetEntries();
     for (Int_t i=0;i<nobjects;i++) {
        obj = clones->At(i);
        sprintf(name,"%s_%d",obj->GetName(),i);
        if (strstr(name,"St")) b->Add(obj, &name[4]);
        else                   b->Add(obj, &name[0]);
     }
// m_Fruits points to an object in general. Insert this object in the browser
  } else {
      b->Add( m_Fruits, m_Fruits->GetName());
  }

}

//_____________________________________________________________________________
void StMaker::Clear(Option_t *option)
{
  if (m_Fruits) m_Fruits->Clear(option);
  delete m_Clones;
  m_Clones = 0;
}

//_____________________________________________________________________________
void StMaker::Draw(Option_t *)
{
//    Insert products of this maker in graphics pad list

  TObject *obj;

// If m_Fruits is a ClonesArray, insert all the objects in the list
// of objects to be painted
  if (m_Fruits->InheritsFrom("TClonesArray")) {
     TClonesArray *clones = (TClonesArray*)m_Fruits;
     Int_t nobjects = clones->GetEntries();
     for (Int_t i=0;i<nobjects;i++) {
        obj = clones->At(i);
        if (obj) obj->AppendPad();
     }
// m_Fruits points to an object in general. Insert this object in the pad
  } else {
     m_Fruits->AppendPad();
  }
}

//_____________________________________________________________________________
void StMaker::FillClone()
{
//   Copy original fruits in a separate list (clones)

   if (!m_IsClonable || m_Fruits == 0) return;
   m_Clones = m_Fruits->Clone();
}

//_____________________________________________________________________________
void StMaker::Init()
{
 
   //dummy
}

//_____________________________________________________________________________
void StMaker::Finish()
{

   //dummy
}

//_____________________________________________________________________________
Int_t StMaker::Make()
{
   Warning("Make","Dummy function called");
   return 0;
}

//_____________________________________________________________________________
void StMaker::PrintInfo()
{
   printf("*********************************\n");
   printf("*                               *\n");
   printf("*     %23s   *\n",GetName());
   printf("*                               *\n");
   printf("*********************************\n");

   Dump();
}

//_____________________________________________________________________________
void StMaker::MakeBranch()
{
//   Adds the list of physics objects to the ATLFast tree as a new branch

   if (m_Save == 0) return;

   TTree *tree = gStChain->Tree();
   if (tree == 0  || m_Fruits == 0  || m_BranchName.Length() == 0) return;

//  Make a branch tree if a branch name has been set
   Int_t buffersize = 4000;
   if (m_Fruits->InheritsFrom("TClonesArray")) {
      tree->Branch(m_BranchName.Data(), &m_Fruits, buffersize);
   } else {
      tree->Branch(m_BranchName.Data(),m_Fruits->ClassName(), &m_Fruits, buffersize);
   }
}

//_____________________________________________________________________________
void StMaker::SetChainAddress(TChain *chain)
{
//   Set branch address in a chain of files

   if (chain == 0) return;

   chain->SetBranchAddress(m_BranchName.Data(), &m_Fruits);
}

//______________________________________________________________________________
#ifdef WIN32
void StMaker::Streamer(TBuffer &R__b)
{
   // Stream an object of class StMaker.

   if (R__b.IsReading()) {
      R__b.ReadVersion(); // Version_t R__v = R__b.ReadVersion();
      TNamed::Streamer(R__b);
      R__b >> m_Save;
      R__b >> m_Fruits;
      m_BranchName.Streamer(R__b);
      R__b >> m_Histograms;
          //this is an addition to the standard rootcint version of Streamer
          //branch address for this maker is set automatically
      TTree *tree = gStChain->Tree();
      if (tree == 0  || m_Fruits == 0  || m_BranchName.Length() == 0) return;
      TBranch *branch = tree->GetBranch(m_BranchName.Data());
      if (branch)  branch->SetAddress(&m_Fruits);
   } else {
      R__b.WriteVersion(StMaker::IsA());
      TNamed::Streamer(R__b);
      R__b << m_Save;
      R__b << m_Fruits;
      m_BranchName.Streamer(R__b);
      R__b << m_Histograms;
   }
}
#endif
