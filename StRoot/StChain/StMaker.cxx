// $Id: StMaker.cxx,v 1.24 1999/03/28 02:57:51 perev Exp $
// $Log: StMaker.cxx,v $
// Revision 1.24  1999/03/28 02:57:51  perev
// Add .const in searching path in GetDataSet
//
// Revision 1.23  1999/03/20 20:57:35  perev
// add StEvtHddr.h and fix Get/SetNumber in maker
//
// Revision 1.22  1999/03/19 20:30:49  perev
// GetCVSTag introduced
//
// Revision 1.21  1999/03/11 01:23:59  perev
// new schema StChain
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
#include "St_Table.h"

StMaker *StMaker::fgStChain = 0;

ClassImp(StMaker)

const char  *StMaker::GetCVSIdC()
{static const char cvs[]="$Id: StMaker.cxx,v 1.24 1999/03/28 02:57:51 perev Exp $";
return cvs;};

//_____________________________________________________________________________
StMaker::StMaker()
{
}

//_____________________________________________________________________________
StMaker::StMaker(const char *name,const char *):St_DataSet(name,".maker")
{
   m_Inputs = 0;
   if (!fgStChain) {	// it is first maker, it is chain
     fgStChain = this;
     AddData(0,".make");
   } else         {	// add this maker to chain  
     fgStChain->AddData(this,".make");
   }
   m_DataSet  = new St_ObjectSet(".data") ;Add(m_DataSet);
   m_ConstSet = new St_ObjectSet(".const");Add(m_ConstSet);
   m_GarbSet  = new St_ObjectSet(".garb" );Add(m_GarbSet);
   AddHist(0); m_Histograms = GetHistList();
   gStChain = this; //?????????????????????????????????????????????????????

}

//_____________________________________________________________________________
StMaker::~StMaker()
{
  Finish();
}
//______________________________________________________________________________
void StMaker::SetNumber(Int_t number)
{
 StMaker *par = GetParentMaker();
 if (par) par->SetNumber(number);
 m_Number = number;
}
//______________________________________________________________________________
Int_t StMaker::GetNumber() const
{
 StMaker *par = GetParentMaker();
 if (par) return par->GetNumber();
 return m_Number;
}
//______________________________________________________________________________
StMaker *StMaker::GetParentMaker() const
{ 
  St_DataSet *par = GetParent(); if (!par) return 0;
  return (StMaker*)par->GetParent();
}
//______________________________________________________________________________
StMaker *StMaker::GetMaker(const char *mkname) 
{ 
  TString path(".make/"); path+=mkname;
  return (StMaker*)GetDataSet((const char*)path);
}
//______________________________________________________________________________
TObject *StMaker::GetDirObj(const char *dir) const
{
  St_ObjectSet *h = (St_ObjectSet*)Find(dir);
  if (!h) return 0;
  return h->GetObject();
}
//______________________________________________________________________________
void StMaker::SetDirObj(TObject *obj,const char *dir)
{ 
  St_ObjectSet *set = (St_ObjectSet *)Find(dir);
  if (!set) { // No dir, make it
    set = new St_ObjectSet(dir); Add(set);}
  set->SetObject(obj);
}
//______________________________________________________________________________
void   StMaker::AddObj(TObject *obj,const char *dir)
{ 
  assert (dir[0]=='.');
  St_ObjectSet *set = (St_ObjectSet*)Find(dir);
  if (!set) { // No dir, make it
    set = new St_ObjectSet(dir); Add(set);}

  TList *list = (TList *)set->GetObject();
  if (!list) {// No list, make it
    list = new TList();
    set->SetObject((TObject*)list);}
  if (obj) list->Add(obj);
}
//______________________________________________________________________________
void   StMaker::AddData(St_DataSet *ds, const char* dir)
{ 
  assert(!m_DataSet || ds!=m_DataSet);
  St_DataSet *set = GetDataSet(dir);
  if (!set) { // No dir, make it
    set = (St_ObjectSet*) new St_ObjectSet(dir);
    Add(set);}
  set->Add(ds);
}
//______________________________________________________________________________
TList *StMaker::GetMakeList() const
{ St_DataSet *ds = Find(".make");
  if (!ds) return 0;
  return ds->GetList();
}
//______________________________________________________________________________
void StMaker::SetInput(const char* logInput,const char* actInput)
{
//	defines relations between logical input and actual input names
  TNamed *inp = new TNamed(logInput,actInput);
  if (!m_Inputs) m_Inputs = new TList();
  m_Inputs->Add(inp);
}
//______________________________________________________________________________
TString StMaker::GetInput(const char* logInput) const
{
  TString actInput;
  if (! m_Inputs) return actInput;
  int nspn = strcspn(logInput," /");
  actInput.Prepend(logInput,nspn);
  TNamed *in = (TNamed*)m_Inputs->FindObject(actInput);
  actInput ="";
  if (in) {actInput = in->GetTitle(); actInput += logInput+nspn;}
  return actInput;
}
//______________________________________________________________________________
St_DataSet *StMaker::GetDataSet(const char* logInput) const
{
const char *subdirs[] = {".data",".const",0};

TString actInput,makerName,findString;
St_DataSet *dataset;
StMaker    *parent;
int icol,idat,nspn;
  actInput = GetInput(logInput);
  if (actInput.IsNull()) actInput = logInput;

//		Direct try
  dataset = Find(actInput);
  if (dataset) return dataset;

//		Not so evident, do some editing
  icol = actInput.Index(":");
  idat = actInput.Index("/.");

  const char *dir;
  for ( int idir=0;(dir=subdirs[idir]);idir++)
  { 

    if (icol<0 && idat < 0) {

  //		MAKER/Dataset -> .make/MAKER/.data/Dataset
      findString = ".make/";
      nspn = strcspn((const char*)actInput,"/ ");
      findString.Append(actInput,nspn);
      findString +="/"; findString +=dir;
      findString.Append((const char*)actInput+nspn);
      dataset = Find(findString);  
      if (dataset) return dataset;
    }


  //		AAA/BBB/CCC -> .make/*/.data/AAA/BBB/CCC
  //		AAA:BBB/CCC -> .make/AAA/.data/BBB/CCC
    makerName="*";
    if (icol>=0) makerName.Replace(0,9999,actInput,icol);
    findString = ".make/"; findString += makerName; 
    findString+="/";findString+=dir; findString+="/";
    findString += (const char*)actInput+icol+1;

    dataset = Find(findString);  
    if (dataset) return dataset;
  }//end for over dirs

  parent = GetMaker(this);         if (!parent) return 0;
  dataset = parent->GetInputDS(actInput);
  if (!dataset && GetDebug()) //Print Warning message
    Warning("GetInputDS"," \"%s\" Not Found ***\n",(const char*)actInput);
  return dataset;

}
//______________________________________________________________________________
St_DataSet *StMaker::GetDataBase(const char* logInput)
{
  St_DataSet *ds;
  StMaker *mk;
  ds = GetInputDS(logInput);
  if (!ds) return 0;
  mk = GetMaker(ds); if (!mk) return 0;
  return mk->UpdateDB(ds);
}
//_____________________________________________________________________________
void StMaker::Clear(Option_t *option)
{
if(option){};
if (m_DataSet) m_DataSet->Delete();

}

//_____________________________________________________________________________
Int_t StMaker::Init()
{   
   TObject *objfirst, *objlast;
   TList *tl = GetMakeList();
   if (!tl) return kStOK;
   
   TIter nextMaker(tl);
   StMaker *maker;

   while ((maker = (StMaker*)nextMaker())) {
     // save last created histogram in current Root directory
      objlast = gDirectory->GetList()->Last();

     // Initialise maker
      gBenchmark->Start((const char *)maker->GetName());
      if (GetDebug()) printf("\n*** Call %s::Init() ***\n\n",maker->ClassName());
      if ( maker->Init()) return kStErr;
      gBenchmark->Stop((const char *) maker->GetName());
     // Add the Maker histograms in the Maker histograms list
      if (objlast) objfirst = gDirectory->GetList()->After(objlast);
      else         objfirst = gDirectory->GetList()->First();
      while (objfirst) {
         maker->AddHist((TH1*)objfirst);
         objfirst = gDirectory->GetList()->After(objfirst);
      }
   }
  return kStOK; 
}
void StMaker::StartMaker()
{
  if (GetDebug()) printf("\n*** Call %s::Make() ***\n\n", ClassName());
  gBenchmark->Start(GetName());
}
void StMaker::EndMaker(int ierr)
{
  if (ierr){};
  St_DataSet *dat = Find(".data");
  if (dat) dat->Pass(ClearDS,0);

  gBenchmark->Stop(GetName());
}

//_____________________________________________________________________________
Int_t StMaker::Finish()
{
//    Terminate a run
//   place to make operations on histograms, normalization,etc.
   int nerr = 0;

   TIter next(GetMakeList());
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      if ( maker->Finish() ) nerr++;
      gBenchmark->Print((char *) maker->GetName());
   }
  Clear();
 return nerr;
}

//_____________________________________________________________________________
Int_t StMaker::Make()
{
//   Loop on all makers
   Int_t ret;
   TList *tl = GetMakeList();
   if (!tl) return kStOK;
   
   TIter nextMaker(tl);
   StMaker *maker;
   while ((maker = (StMaker*)nextMaker())) {
  // Call Maker
     maker->StartMaker();
     ret = maker->Make();
     maker->EndMaker(ret);
     
     if (Debug()) printf("*** %s::Make() == %d ***\n",maker->ClassName(),ret);

     if (ret==kStErr) { if (Debug()) maker->ls(3); return ret;}
   }
   return kStOK;
}
void StMaker::Fatal(int Ierr, const char *com)
{
   printf("%s::Fatal: Error %d %s\n",GetName(),Ierr,com);
   StMaker *parent = (StMaker *)GetParent();
   if (parent) ((StMaker*)parent)->Fatal(Ierr,com);
   fflush(stdout);
}
//_____________________________________________________________________________
StMaker *StMaker::GetMaker(const St_DataSet *ds) 
{ 
  const St_DataSet *par = ds;
  while (par && (par = par->GetParent()) && strcmp(".maker",par->GetTitle())) {}
  return (StMaker*)par;
}
//_____________________________________________________________________________
EDataSetPass StMaker::ClearDS (St_DataSet* ds,void * )
{  ds->Clear("Garbage");
   return kContinue; 
}
//_____________________________________________________________________________
void StMaker::PrintInfo() const
{
   printf("*********************************\n");
   printf("*                               *\n");
   printf("*     %23s   *\n",GetName());
   printf("*                               *\n");
   printf("*********************************\n");

}
//_____________________________________________________________________________
Int_t        StMaker::GetEventNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (hd) return hd->GetEventNumber();
   Warning("GetEventNumber"," EvtHddr not found");
   return 0;
}
//_____________________________________________________________________________
Int_t        StMaker::GetRunNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (hd) return hd->GetRunNumber();
   Warning("GetRunNumber"," EvtHddr not found");
   return 0;
}


//_____________________________________________________________________________
TDatime  StMaker::GetDateTime() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (hd) return hd->GetDateTime();
   Warning("GetDateTime"," EvtHddr not found");
   TDatime td; return td;   
}
Int_t    StMaker::GetDate()  const {return GetDateTime().GetDate();}
Int_t    StMaker::GetTime()  const {return GetDateTime().GetTime();}
//_____________________________________________________________________________
const Char_t *StMaker::GetEventType() const
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (hd) return hd->GetEventType();
   Warning("GetEventType"," EvtHddr not found");
   return 0;
}

//_____________________________________________________________________________
void StMaker::PrintTimer(Option_t *option) 
{
   if(option){};
   Printf("%-10s: Real Time = %6.2f seconds Cpu Time = %6.2f seconds",GetName(),m_Timer.RealTime(),m_Timer.CpuTime());
}
