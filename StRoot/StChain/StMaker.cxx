// $Id: StMaker.cxx,v 1.18 1999/02/27 19:09:40 fine Exp $
// $Log: StMaker.cxx,v $
// Revision 1.18  1999/02/27 19:09:40  fine
// St_Maker::MakeDoc() name of the directory has been adjusted from base to St_base
//
// Revision 1.17  1999/02/22 22:25:10  fisyak
// No Fruits(=0) by default
//
// Revision 1.16  1999/01/20 23:44:48  fine
// The special Input/Output makers and the static variable StChain::g_Chain have been introduced
//
// Revision 1.15  1999/01/02 19:08:12  fisyak
// Add ctf
//
// Revision 1.14  1998/12/21 19:42:51  fisyak
// Move ROOT includes to non system
//
// Revision 1.13  1998/11/19 01:23:57  fine
// StChain::MakeDoc has been introduced, StChain::MakeDoc has been fixed (see macros/bfc_doc.C macro
//
// Revision 1.12  1998/11/18 22:46:09  fine
// The lost MakeDoc method has been re-introduced
//
// Revision 1.9  1998/09/23 20:22:52  fisyak
// Prerelease SL98h
//
// Revision 1.10  1998/10/06 18:00:27  perev
// cleanup
// Revision 1.8  1998/09/22 01:39:07  fine
// Some make up
//
// Revision 1.6  1998/08/18 14:05:02  fisyak
// Add to bfc dst
//
// Revision 1.5  1998/07/20 15:08:09  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChain virtual base class for StMaker                              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "TSystem.h"
#include "TClass.h"
#include "TROOT.h"
#include "THtml.h"

#include "TChain.h"
#include "TTree.h"
#include "TList.h"
#include "TClonesArray.h"
#include "TBrowser.h"

#include "StMaker.h"
#include "StChain.h"

static StMaker *thisMakers[100];
static Int_t     thisMakerIndx=0;

StChain *StMaker::g_Chain = 0;

ClassImp(StMaker)

//_____________________________________________________________________________
StMaker::StMaker()
{
  //  cout << "Default ctor for " << GetName() << " - " << GetTitle() << endl;
   m_BranchName = "";
   m_Save       = 0;
   m_Histograms = 0;
   m_Fruits     = 0;
   //   m_Clones     = 0;
   m_IsClonable = kTRUE;
   m_DataSet    = 0;
}

//_____________________________________________________________________________
StMaker::StMaker(const char *name, const char *title)
       :TNamed(name,title)
{
  //   cout << "ctor for " << GetName() << " - " << GetTitle() << endl;   
   m_BranchName = "";
   m_Save       = 0;
   m_Histograms = new TList();
   m_Fruits     = 0;
   //   m_Clones     = 0;
   m_IsClonable = kTRUE;
   m_DataSet    = 0;
   if (!g_Chain) g_Chain = gStChain;
   TList *list =  g_Chain->Makers();
   if (list) {
     StMaker *maker =  (StMaker *) list->FindObject(name);
     if (!maker) list->Add(this);
     //   gStChain->Makers()->Add(this);  
   }
}

//_____________________________________________________________________________
StMaker::~StMaker()
{
  //  cout << "Default destructor for " << GetName() << " - " << GetTitle() << endl;
  Finish();
}

//______________________________________________________________________________
void StMaker::Browse(TBrowser *b)
{
//  Insert Maker objects in the list of objects to be browsed.

  char name[64];
  if( b == 0) return;
  if (m_Histograms) b->Add(m_Histograms,"Histograms");
  if (m_DataSet) b->Add(m_DataSet,m_DataSet->GetName()); 
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
 SafeDelete(m_DataSet);
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
   if (!m_Save) return;


   TTree *tree = GetTree();
#if 0
// Fill the maker's owned tree if any
//   if (!tree) {
//     TString name = GetName();
//     name += "_Tree";
//     tree = new TTree(name.Data(),GetTitle());
//     thisMakers[thisMakerIndx]=this;     
//     tree->Branch(m_DataSet->GetName(),m_DataSet->IsA()->GetName(),&m_DataSet,0);
/     name.ReplaceAll("Tree","Branch");
//     printf(" Branch %s \n", tree->Branch(name.Data(),IsA()->GetName(),&thisMakers[thisMakerIndx])->GetName());
//     printf(" Branch %s \n", tree->Branch(name.Data(),IsA()->GetName(),&thisMakers[thisMakerIndx],32000,0)->GetName());
//     printf(" Maker number %d \n",thisMakerIndx++);
//     SetTree(tree);
//     SetBranch();
//   }
#else
   if (tree) 
     tree->Fill();
#endif

}

//_____________________________________________________________________________
Int_t StMaker::Init()
{   
  return kStOK; //dummy
}

//_____________________________________________________________________________
Int_t StMaker::Finish()
{
 Clear();
 return kStOK;
}

//_____________________________________________________________________________
Int_t StMaker::Make()
{
   Warning("Make","Dummy function called");
   return kStOK;
}

//_____________________________________________________________________________
void StMaker::MakeDoc(const TString &stardir,const TString &outdir)
{
 //
 // MakeDoc - creates the HTML doc for this class and for the base classes:
 //         *  St_XDFFile  St_Module      St_Table       *
 //         *  St_DataSet  St_DataSetIter St_FileSet     *
 //         *  StMaker     StChain                       *
 //
 // stardir - the "root" directory to lookup the subdirectories as follows.
 // outdir  - directory to write the generated HTML and Postscript files into
 //
 //            The following subdirectories are used to look it up:
 //            $(stardir) + "StRoot/St_base"
 //            $(stardir) + "StRoot/StChain"
 //            $(stardir) + "StRoot/xdf2root"
 //            $(stardir) + ".share/tables"
 //            $(stardir) + "inc",
 //            $(stardir) + "StRoot/<this class name>",
 //
 //   where $(stardir) is the input parameter (by default = "$(afs)/rhic/star/packages/dev/")
 //

  // Define the type of the OS
  TString STAR= stardir;
  TString delim = ":";
  Bool_t NT=kFALSE;

  if (strcmp(gSystem->GetName(),"WinNT") == 0 ) {
     NT=kTRUE;
     delim = ";";
     STAR.ReplaceAll("$(afs)","//sol/afs");
  }
  else 
     STAR.ReplaceAll("$(afs)","/afs");

  TString classname = ClassName();

  THtml html;

  // Define the set of the subdirectories with the STAR class sources
  const Char_t *source[] = {"StRoot/St_base"
                           ,"StRoot/StChain"
                           ,"StRoot/xdf2root"
                           ,".share/tables"
                           ,"inc"
                           };
  const Int_t lsource = 5;
 
  TString lookup = STAR;
  lookup += "StRoot/";
  lookup += classname;
  Int_t i = 0;
  for (i=0;i<lsource;i++) {
    lookup += delim;
    lookup += STAR;
    lookup += "/";
    lookup += source[i];
  }
  
  const Char_t *c = ClassName();  // This tric has to be done since a bug within ROOT

    lookup += delim;
    lookup += STAR;
    lookup += "/StRoot/";
    lookup += c;

  html.SetSourceDir(lookup);

  TString odir = outdir;
  odir.ReplaceAll("$(star)",STAR);
   
  html.SetOutputDir(odir);

  // Create the list of the classes defined with the loaded DLL's to be documented

  Char_t *classes[] = { "St_XDFFile",  "St_Module",      "St_Table"
                       ,"St_DataSet",  "St_DataSetIter", "St_FileSet"
                       ,"StMaker",     "StChain"
                       ,"table_head_st"
                      };
  Int_t nclass = 9;
  // Create the definitions of the classes not derived from TObjects
  TString header = STAR;
  header += "/inc/table_header.h";

  gROOT->LoadMacro(header);

  TClass header1("table_head_st",1,"table_header.h","table_header.h");

  // Update the docs of the base classes
  for (i=0;i<nclass;i++) 
                   html.MakeClass(classes[i]);

  // Create the doc for this class
  printf(" Making html for <%s>\n",c);
  html.MakeClass((Char_t *)c);
}


//_____________________________________________________________________________
TTree *StMaker::MakeTree(const char* name, const char*title)
{
   TTree *tree = Tree();
  // Fill the maker's owned tree if any
   if (!tree) {
     TString name = GetName();
     name += "_Tree";
     tree = new TTree(name.Data(),GetTitle());
//     thisMakers[thisMakerIndx]=this;     
//     tree->Branch(m_DataSet->GetName(),m_DataSet->IsA()->GetName(),&m_DataSet,0);
//     name.ReplaceAll("Tree","Branch");
//     printf(" Branch %s \n", tree->Branch(name.Data(),IsA()->GetName(),&thisMakers[thisMakerIndx])->GetName());
     //     printf(" Branch %s \n", tree->Branch(name.Data(),IsA()->GetName(),&thisMakers[thisMakerIndx],32000,0)->GetName());
//     printf(" Maker number %d \n",thisMakerIndx++);
     SetTree(tree);
//     SetBranch();
   }
  return tree;
}

//_____________________________________________________________________________
void StMaker::PrintInfo()
{
   printf("*********************************\n");
   printf("*                               *\n");
   printf("*     %23s   *\n",GetName());
   printf("*                               *\n");
   printf("*********************************\n");

   if (g_Chain->Debug()) Dump();
}
//_____________________________________________________________________________
void StMaker::PrintTimer(Option_t *option)
{
   Printf("%-10s: Real Time = %6.2f seconds Cpu Time = %6.2f seconds",GetName(),m_Timer.RealTime(),m_Timer.CpuTime());
}
//_____________________________________________________________________________
void StMaker::MakeBranch()
{
//   Adds the list of physics objects to the STAR tree as a new branch

   if (m_Save == 0) return;

   TTree *tree = GetTree();
   if (!tree) return;

//   if (tree == 0  || m_Fruits == 0  || m_BranchName.Length() == 0) return;

//  Make a branch tree if a branch name has been set
   Int_t buffersize = 4000;

//   thisMakers[thisMakerIndx]=this;     
//   printf(" Branch %s \n", tree->Branch(GetName(),IsA()->GetName(),&thisMakers[thisMakerIndx])->GetName());

//   return;

   if (m_Fruits){
     if (m_Fruits->InheritsFrom("TClonesArray")) {
       tree->Branch(m_BranchName.Data(), &m_Fruits, buffersize);
     } else {
       tree->Branch(m_BranchName.Data(),m_Fruits->ClassName(), &m_Fruits, buffersize);
     }
   }
///==   if (m_DataSet)
   {
//     m_BranchName = m_DataSet->GetName();
     m_BranchName = GetName();
     m_BranchName += "_Branch";
     TBranch *b = tree->Branch(m_BranchName.Data(),m_DataSet->ClassName(), &m_DataSet, buffersize,0); 
   }
}

//_____________________________________________________________________________
void  StMaker::SetBranch(){
    TTree *tree = GetTree();
    if (!tree) return;
//    m_BranchName = GetName();
    TBranch *dstBranch = tree->GetBranch(m_BranchName.Data());
    printf(" ---- Branch name %s, address %x, %x \n", m_BranchName.Data(), dstBranch,&m_DataSet);
    if (dstBranch) dstBranch->SetAddress(&m_DataSet);
}
//_____________________________________________________________________________
void StMaker::SetChainAddress(TChain *chain)
{
//   Set Chain address

   if (chain == 0) return;

   chain->SetBranchAddress(m_BranchName.Data(), &m_Fruits);
}
//_____________________________________________________________________________
void StMaker::SetDataSet(St_DataSet *set)
{
//   Set m_DataSet
  if (m_DataSet != set) {
    if (m_DataSet) delete m_DataSet;
    m_DataSet = set;
  }  
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
      TTree *tree = g_Chain->Tree();
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

//_____________________________________________________________________________
TTree *StMaker::GetTree(){
  return m_Tree ? m_Tree : g_Chain->Tree();
}
