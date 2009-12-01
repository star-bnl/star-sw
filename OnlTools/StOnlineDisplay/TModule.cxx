
// $Id: TModule.cxx,v 1.1 2009/12/01 01:33:33 fine Exp $
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TModule is a  base class to define the member of the chain           //
// to perform some complex action                                       //
//                                                                      //
// ("Root" service within Athena framework, for example)                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "Riostream.h"
#include <stdio.h>
#include <stdlib.h>

#include <TSystem.h>
#include <TClass.h>
#include <TROOT.h>
#include <THtml.h>
#include <TH1.h>

#include <TChain.h>
#include <TTree.h>
#include <TList.h>
#include <TClonesArray.h>
#include <TBrowser.h>

#include <TTable.h>
#include "TModule.h"
#include "StMemStat.h"
#include "StEvtHddr.h"
#include "TMemoryInfo.h"

#if 0
  StMaker *TModule::fgStChain = 0;
  StMaker *TModule::fgFailedModule = 0;
  Int_t    TModule::fgTallyModule[kFatal+1] = {0,0,0,0,0};
  Int_t MaxWarnings = 26;
#endif
  
ClassImp(TModule)

#if 1
//_____________________________________________________________________________
TModule::TModule(const char *name,const char *t):StMaker(name,t) , fNxtModule(0)
, fCurrentMaker(0){ fCurrentMaker=this ;}
//_____________________________________________________________________________
TModule::~TModule()
{   fCurrentMaker = 0;  ResetModule (true);  } 
//_____________________________________________________________________________
void  TModule::ResetModule(bool deleteOnly)
{
   if (fNxtModule) delete fNxtModule; 
   fNxtModule    = 0; 
   fCurrentMaker = 0;
   TList *tl     = 0;
   if (!deleteOnly && (tl = GetMakeList()))
      fNxtModule = new TIter(tl);
}

//_____________________________________________________________________________
Int_t TModule::Make()
{
//   Loop on all makers
   Int_t ret = kStOK;
   Int_t run=-1;

   if (!Iter()) return kStOK;

   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");   

   TIter &nextModule= *Iter();
   StMaker *maker=0;
   fgFailedMaker = 0;
   if ((maker = (StMaker*)nextModule()) && (ret==kStOK) && maker->IsActive() ) {

     if (hd && hd->IsNewRun()) {
       run = hd->GetOldRunNumber();  
       if (run>-1) maker->FinishRun(run);
       run = hd->GetRunNumber();  
       // do not call InitRun VF 21.10.2008 maker->InitRun(run);
     }

// 		Call Module
//     maker->StartModule();
     ret = maker->Make();
     assert(ret>=0 && ret<=kStFatal);     
     fgTallyMaker[ret]++;
  //   maker->EndModule(ret);
    // vf 18.04.2007  if (!gROOT->IsBatch()) gSystem->DispatchOneEvent(kTRUE);

//     if (Debug() ) 
        printf("*** %s::Make() == %d ***\n",maker->GetName(),ret);

     if (ret>kStWarn) { 
       fCurrentMaker =  maker;
       if (GetDebug() > 1) maker->ls(3);
       fgFailedMaker = maker; return ret;}
     
   } 
   if (!maker) {
      ResetModule (true); 
   }
   fCurrentMaker =  maker;
   return ret;
}

//_____________________________________________________________________________
Int_t        TModule::GetIventNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return -1;
   return hd->GetIventNumber();
}
//_____________________________________________________________________________
void         TModule::SetIventNumber(Int_t iv)  
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return;
   hd->SetIventNumber(iv);
}
//_____________________________________________________________________________
Int_t        TModule::GetEventNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return -1;
   return hd->GetEventNumber();
}
//_____________________________________________________________________________
Int_t        TModule::GetRunNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return -1;
   return hd->GetRunNumber();
}
//_____________________________________________________________________________
const Char_t *TModule::GetEventType() const
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return 0;
   return hd->GetEventType();
}
//_____________________________________________________________________________
UInt_t TModule::GetTriggerMask() const
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return 0;
   return hd->GetTriggerMask();
}
#else
static void doPs(const char *who,const char *where);

//_____________________________________________________________________________
TModule::TModule(const char *name,const char *):TDataSet(name,".maker"),fActive(kTRUE)
{
  // Steering TDataSet to control the program flow control
  // and pass data between program modules the standard way
   SetMode();
   m_DebugLevel=0;
   m_MakeReturn=0;
   m_Inputs = 0;
   if (!fgStChain) {	// it is first maker, it is chain
     fgStChain = this;
     AddData(0,".make");
   } else         {	// add this maker to chain  
     fgStChain->AddData(this,".make");
   }
   m_DataSet  = new TObjectSet(".data") ;Add(m_DataSet);
   m_ConstSet = new TObjectSet(".const");Add(m_ConstSet);
   m_GarbSet  = new TObjectSet(".garb" );Add(m_GarbSet);
   m_Inputs   = new TObjectSet(".aliases" );Add(m_Inputs);
   m_Runco  = new TObjectSet(".runco" );Add(m_Runco);
   AddHist(0); m_Histograms = GetHistList();
   ::doPs(GetName(),"constructor");
   fMemStatMake  = 0;
   fMemStatClear = 0;
   m_Timer.Stop();
}

//_____________________________________________________________________________
void TModule::AddModule(StMaker *mk)
{
  TDataSet *dotmk = Find(".make");
  if (!dotmk) dotmk = new TDataSet(".make",this);
  mk->Shunt(dotmk);
}
//_____________________________________________________________________________
TModule::~TModule()
{
  if (fgStChain == this) fgStChain = 0;
  delete fMemStatMake;	fMemStatMake  = 0;
  delete fMemStatClear;	fMemStatClear = 0;
}
//______________________________________________________________________________
void TModule::SetNumber(Int_t number)
{
StMaker *par = GetParentModule();
 if (par) par->SetNumber(number);
 m_Number = number;

 SetIventNumber(number);

}
//______________________________________________________________________________
Int_t TModule::GetNumber() const
{
StMaker *par = GetParentModule();
 if (par) return par->GetNumber();
 return m_Number;
}
//______________________________________________________________________________
StMaker *TModule::GetParentModule() const
{ 
  TDataSet *par = GetParent(); if (!par) return 0;
  return (StMaker *)par->GetParent();
}
//______________________________________________________________________________
StMaker  *TModule::GetModule(const char *mkname) 
{ 
  TString path(".make/"); path+=mkname;
  return (StMaker*)GetDataSet((const char*)path);
}
//______________________________________________________________________________
TObject *TModule::GetDirObj(const char *dir) const
{
  TObjectSet *h = (TObjectSet*)Find(dir);
  if (!h) return 0;
  return h->GetObject();
}
//______________________________________________________________________________
void TModule::SetDirObj(TObject *obj,const char *dir)
{ 
  TObjectSet *set = (TObjectSet *)Find(dir);
  if (!set) { // No dir, make it
    set = new TObjectSet(dir); Add(set);}
  set->SetObject(obj);
}
//______________________________________________________________________________
TObjectSet *TModule::AddObj(TObject *obj,const char *dir)
{ 
  assert (dir[0]=='.');
  TObjectSet *set = (TObjectSet*)Find(dir);
  if (!set) { // No dir, make it
    set = new TObjectSet(dir); Add(set);}

  TList *list = (TList *)set->GetObject();
  if (!list) {// No list, make it
    list = new TList();
    set->SetObject((TObject*)list);}
  if (!obj) return set;
  if(!list->FindObject(obj)) list->Add(obj);
  return set;
}
//______________________________________________________________________________
 void TModule::AddHist(TH1 *h,const char *dir)
{  
  if (dir){/*unused*/}
  if (!h) {AddObj(0,".hist");return;}
  if (h->InheritsFrom(TH1::Class())) h->SetDirectory(0);
  AddObj(h,".hist");
}    
//______________________________________________________________________________
 void TModule::AddRunco (double par,const char* name,const char* comment)
{
   assert (name && name && comment[0]); 

   TDataSet *dp = new TDataSet(name,m_Runco);
   TString ts("  // "); ts += comment;
   char buf[40];
   sprintf(buf,"%f",par);
   ts.Replace(0,0,buf);
   dp->SetTitle((const char*)ts);
}
//______________________________________________________________________________
TDataSet *TModule::AddData(TDataSet *ds, const char* dir)
{ 
  assert (dir); assert(dir[0]=='.');
  TDataSet *set = Find(dir);
  if (!set) { // No dir, make it
    set = new TObjectSet(dir); Add(set);}
  if (!ds) return set;
  TList *tl = set->GetList();
  if (!tl || !tl->FindObject(ds)) set->Add(ds);
  return set;
}
//______________________________________________________________________________
TDataSet  *TModule::GetData(const char *name, const char* dir) const
{ 
  TDataSet *set = Find(dir);
  if (!set) return 0;
  return set->Find(name);
}
//______________________________________________________________________________
void TModule::AddAlias(const char* log, const char* act,const char* dir)
{
  TDataSet *ali = new TDataSet(log); 
  ali->SetTitle(act);
  AddData(ali,dir);
}
//______________________________________________________________________________
void TModule::SetAlias(const char* log, const char* act,const char* dir)
{ 
  TDataSet *ali = GetData(log,dir);
  if (ali) {
    if (!strcmp(act,ali->GetTitle())) return;
  } else {
    ali = new TDataSet(log); AddData(ali,dir);
  }
  ali->SetTitle(act);

  if (GetDebug()) 
    printf("<%s(%s)::SetAlias> %s = %s\n",ClassName(),GetName(),log,act);
}
//______________________________________________________________________________
void TModule::SetOutput(const char* log,TDataSet *ds)
{
  int idx;
  const char* logname = log;
  if (!logname || !logname[0]) logname = ds->GetName();
  TString act = ds->Path();
  while ((idx=act.Index(".make/"))>=0) act.Replace(0,idx+6,"");  
  SetOutput(logname,act); 
}

//______________________________________________________________________________
void TModule::SetOutputAll(TDataSet *ds, Int_t level)
{
  TDataSet *set;
  TDataSetIter next(ds,level);
  while ((set = next())) SetOutput(set);
}

//______________________________________________________________________________
TList *TModule::GetMakeList() const
{ TDataSet *ds = Find(".make");
  if (!ds) return 0;
  return ds->GetList();
}
//______________________________________________________________________________
TString TModule::GetAlias(const char* log,const char* dir) const
{
  TString act;
  int nspn = strcspn(log," /");
  act.Prepend(log,nspn);
  TDataSet *in = GetData(act,dir);
  act ="";
  if (in) {act = in->GetTitle(); act += log+nspn;}
  return act;
}
//______________________________________________________________________________
TDataSet *TModule::GetDataSet(const char* logInput,
                                const StMaker *uppMk,
                                const StMaker  *dowMk) const
{
TDataSetIter nextMk(0);
TString actInput,findString,tmp;
TDataSet *dataset,*dir;
TModule    *parent,*mk;
int icol,islas;
  
  actInput = GetInput(logInput);
  if (actInput.IsNull()) actInput = logInput;
  

//		Direct try
  dataset = 0;
  if (actInput.Contains("."))  dataset = Find(actInput);
  if (dataset) goto FOUND;
  
  if (actInput==GetName()) dataset = m_DataSet;
  if (dataset) goto FOUND;

//		Not so evident, do some editing
  
  
  icol = actInput.Index(":");
  if (icol>=0) {//there is maker name is hidden
    tmp = actInput; 
    tmp.Replace(0,0,".make/"); icol +=6;
    tmp.Replace(icol,1,"/.data/");
    dataset = Find((const char*)tmp);  		// .make/MAKER/.data/...
    if (dataset) goto FOUND;
    dataset = Find((const char*)tmp+6);		//       MAKER/.data/...
    if (dataset) goto FOUND;
    tmp.Replace(icol,7,"/.const/");
    dataset = Find((const char*)tmp);		// .make/MAKER/.const/...
    if (dataset) goto FOUND;
    dataset = Find((const char*)tmp+6);		//       MAKER/.const/...
    if (dataset) goto FOUND;
    goto DOWN;
  }

  assert(m_DataSet);
  islas = actInput.Index("/");
  if (islas>0) {
    tmp.Replace(0,999,actInput,islas);
    if (tmp == GetName()) { // 
      tmp = actInput;
      tmp.Replace(0,islas+1,"");
      dataset = m_DataSet->Find(tmp);
      if (dataset) goto FOUND;
      dataset = m_ConstSet->Find(tmp);
      if (dataset) goto FOUND;
    }
  }

  dataset = m_DataSet->Find(actInput);
  if (dataset) goto FOUND;
  dataset = m_ConstSet->Find(actInput);
  if (dataset) goto FOUND;


//	Try to search DOWN
DOWN: if (!(dir = Find(".make"))) goto UP;

  nextMk.Reset(dir);
  while ((mk = (TModule*)nextMk()))
  {
    if (mk==dowMk) continue;
    dataset = mk->GetDataSet(actInput,this,0);
    if (dataset) goto FOUND;
  }

//     Try to search UP
UP: if (uppMk) return 0;

  parent = GetModule(this); if (!parent) goto NOTFOUND;
  dataset = parent->GetDataSet(actInput,0,this);
  if (dataset) goto FOUND;

//		Not FOUND
NOTFOUND:
  if (!dowMk && GetDebug()) //Print Warning message
    if ((MaxWarnings--) > 0) Warning("GetDataSet"," \"%s\" Not Found ***\n",(const char*)actInput);
  return 0;

//		DataSet FOUND
FOUND: if (uppMk || dowMk) 	return dataset;
       if (GetDebug()<2) 	return dataset;
  printf("Remark: <%s::%s> DataSet %s FOUND in %s\n"
  ,ClassName(),"GetDataSet",logInput,(const char*)dataset->Path());

  return dataset;

}
//______________________________________________________________________________
TDataSet *TModule::GetDataBase(const char* logInput)
{
  TDataSet *ds=0;
  StMaker *mk;
  //loop over makers
  TModuleIter mkiter(this);
  while ((mk = mkiter.NextModule())) {
    if (  (ds = mk->UpdateDB(logInput)) ) break;
  }
  return ds;
}
//_____________________________________________________________________________
TDataSet *TModule::UpdateDB(const char* logInput)
{  if (logInput); return 0; }

#if 0
//______________________________________________________________________________
Int_t   TModule::GetValidity(const TTable *tb, TDatime *val) const
{
   StMaker *mk = GetModule(tb);
   if (!mk) 					return 10;
   if (!mk->IsDbProvider())	return 11;
   return mk->GetValidity(tb,val);
}
 #endif 

//_____________________________________________________________________________
void TModule::Clear(Option_t *option)
{
  m_MakeReturn = 0;
  if(option){};
  if (m_DataSet) m_DataSet->Delete();
//    Reset lists of event objects
   
   TIter next(GetMakeList(), kIterBackward);
   StMaker *maker;
   while ((maker = (TModule*)next())) {
      maker->StartTimer();
      if (maker->fMemStatClear && GetNumber()>20) maker->fMemStatClear->Start();
         maker->Clear(option);
      if (maker->fMemStatClear && GetNumber()>20) maker->fMemStatClear->Stop();
      maker->StopTimer();
   }
   TCollection::EmptyGarbageCollection();
   ::doPs(GetName(),"Clear");
   return;
}
//_____________________________________________________________________________
Int_t TModule::Init()
{   
   TObject  *objLast,*objHist;
   TList *tl = GetMakeList();
   if (!tl) return kOK;
   
   TIter nextModule(tl);
   StMaker *maker;

   while ((maker = (TModule*)nextModule())) {

     // save last created histogram in current Root directory
      gROOT->cd();
      objLast = gDirectory->GetList()->Last();

// 		Initialise maker

      maker->StartTimer();
      if (GetDebug()) printf("\n*** Call %s::Init() ***\n\n",maker->ClassName());
      TString ts1(maker->ClassName()); ts1+="("; ts1+=maker->GetName(); ts1+=")::";
      TString ts2 = ts1; ts2+="Make ";
      maker->fMemStatMake  = new StMemStat(ts2);
      ts2 = ts1; ts2+="Clear";
      maker->fMemStatClear = new StMemStat(ts2);
      
      if ( maker->Init()) return kErr;
      maker->StopTimer();

// 		Add the Module histograms in the Module histograms list
// 		and remove it from the ROOT system directory
      gROOT->cd();
      TIter nextHist(gDirectory->GetList());
      int ready = !objLast;
      while((objHist=nextHist())) {// loop over gDirectory
        if (!ready && objHist!=objLast)		continue;
        ready = 1999;
        if (objHist==objLast)			continue;
        if (!objHist->InheritsFrom("TH1")) 	continue;

// 		Move the histogram from the ROOT list into the "maker's" list
        ((TH1*)objHist)->SetDirectory(0);
        maker->AddHist((TH1*)objHist);
      }
    ::doPs(maker->GetName(),"Init");
    }
  return kOK; 
}
//_____________________________________________________________________________
void TModule::StartModule()
{
  if (!m_DataSet) {//Keep legacy code
    m_DataSet = Find(".data");
    if (!m_DataSet) {m_DataSet = new TObjectSet(".data"); Add(m_DataSet);}
  }
  if (fMemStatMake && GetNumber()>20) fMemStatMake->Start();
  else 
   if (GetDebug()>1) {
    printf("\n*** Call %s::Make() ***\n\n", ClassName());
   }
  StartTimer();
}
//_____________________________________________________________________________
void TModule::EndModule(int ierr)
{
  SetMakeReturn(ierr);
  if (m_DataSet) m_DataSet->Pass(ClearDS,0);
  if (m_GarbSet) m_GarbSet->Delete();
  ::doPs(GetName(),"EndModule");
  
  if (fMemStatMake && GetNumber()>20) fMemStatMake->Stop();
  else if (GetDebug()>1) {
    printf("\n*** End of %s::Make() ***\n\n", ClassName());
  } 
  StopTimer();
}

//_____________________________________________________________________________
Int_t TModule::Finish()
{
//    Terminate a run
//   place to make operations on histograms, normalization,etc.
   int nerr = 0;

   int run = GetRunNumber();
   if (run>-1) FinishRun(run);   


   TIter next(GetMakeList());
   StMaker *maker;
   Double_t totalCpuTime = 0;
   Double_t totalRealTime = 0;   
   while ((maker = (TModule*)next())) 
   {
      if ( maker->Finish() ) nerr++;
      maker->PrintTimer();
      totalCpuTime  += maker->CpuTime();
      totalRealTime += maker->RealTime();
   }

   // Print relative time
   if (totalCpuTime && totalRealTime) {
     Printf("\n---------------------------------------------------------------------------------");
     Printf("QAInfo: Total: %-12s: Real Time = %6.2f seconds Cpu Time = %6.2f seconds"
                               ,GetName(),totalRealTime,totalCpuTime);
     Printf("---------------------------------------------------------------------------------");
     next.Reset();
     while ((maker = (TModule*)next())) {
        Printf("QAInfo:%-20s: Real Time = %5.1f %%        Cpu Time = %5.1f %% "
               ,maker->GetName()
               ,100*maker->RealTime()/totalRealTime
               ,100*maker->CpuTime()/totalCpuTime);
     }
     if (!GetParent()) {// Only for top maker

       printf("\n--------------Error Codes-------------------------\n");
       printf("     nStOK   nStWarn    nStEOF    nStErr  nStFatal  \n");
       for( int i=0; i<=kFatal; i++) printf("%10d",fgTallyModule[i]); 
       printf("\n--------------------------------------------------\n");
     }  
     Printf("=================================================================================\n");
   }
   
   Clear("finish");
   if (GetParent()==0) StMemStat::Summary();
   return nerr;
}

//_____________________________________________________________________________
Int_t TModule::Make()
{
//   Loop on all makers
   Int_t ret = kOK;

   Int_t run=-1;

   TList *tl = GetMakeList();
   if (!tl) return kOK;

   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");   

   TIter nextModule(tl);
   StMaker *maker;
   fgFailedMaker = 0;
   while ((maker = (TModule*)nextModule()) && (ret==kOK)) {
     if (!maker->IsActive()) continue;

     if (hd && hd->IsNewRun()) {
       run = hd->GetOldRunNumber();  
       if (run>-1) maker->FinishRun(run);
       run = hd->GetRunNumber();  
       maker->InitRun(run);
     }

// 		Call Module
     maker->StartModule();
     ret = maker->Make();
     assert(ret>=0 && ret<=kFatal);     
     fgTallyModule[ret]++;
     maker->EndModule(ret);
    // vf 18.04.2007  if (!gROOT->IsBatch()) gSystem->DispatchOneEvent(kTRUE);

     if (Debug() ) printf("*** %s::Make() == %d ***\n",maker->ClassName(),ret);

     if (ret>kWarn) { 
       if (GetDebug() > 1) maker->ls(3);
       fgFailedModule = maker; return ret;}
     
   }
   return ret;
}
//_____________________________________________________________________________
void TModule::FatalErr(int Ierr, const char *com) const
{
   printf("QAInfo:%s::FatalM: Error %d %s\n",GetName(),Ierr,com);
   StMaker *parent = (StMaker *)GetParent();
   if (parent) ((StMaker*)parent)->FatalErr(Ierr,com);
   fflush(stdout);
}
//_____________________________________________________________________________
StMaker *TModule::GetModule(const TDataSet *ds) 
{ 
  const TDataSet *par = ds;
  while (par && (par = par->GetParent()) && strncmp(".maker",par->GetTitle(),6)) {}
  return (StMaker*)par;
}
//_____________________________________________________________________________
TDataSet::EDataSetPass TModule::ClearDS (TDataSet* ds,void * )
{
  // Reduce the size of the table to the used rows + 1
  // and filll the last empty row awith a special pattern
  // Check the table for NaN floating cells if any

  if (ds->InheritsFrom(TTable::Class())){
     TTable *table = (TTable *)ds;
     Int_t setSize =  table->GetTableSize();
     table->ReAllocate();
     memset((void *)table->At(table->GetNRows()),127,table->GetRowSize());
     //yf     if (setSize && (setSize - table->GetTableSize() > 100))
     if (setSize && table->GetTableSize() == 0)
        table->Warning("ReAllocate"," Table %s has purged from %d to %d "
               ,table->GetName(),setSize,table->GetTableSize());
     table->NaN();
  }
  return kContinue; 
}
//_____________________________________________________________________________
void TModule::PrintInfo() const
{
   const char *cvs = GetCVS();
   const char *built = strstr(cvs,"built");
   printf("QAInfo:%-20s %s from %.*s\n",ClassName(),built,built-cvs,cvs);
//     Print info for all defined Modules
   TIter next(GetMakeList());
   StMaker *maker;
   while ((maker = (TModule*)next())) {
      maker->PrintInfo();
   }
}

//_____________________________________________________________________________
Int_t        TModule::GetIventNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return -1;
   return hd->GetIventNumber();
}
//_____________________________________________________________________________
void         TModule::SetIventNumber(Int_t iv)  
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return;
   hd->SetIventNumber(iv);
}
//_____________________________________________________________________________
Int_t        TModule::GetEventNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return -1;
   return hd->GetEventNumber();
}
//_____________________________________________________________________________
Int_t        TModule::GetRunNumber() const 
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return -1;
   return hd->GetRunNumber();
}


//_____________________________________________________________________________
TModule     *TModule::GetParentChain() const 
{
    const StMaker *mk = GetParentModule();
    while(mk && !mk->IsChain()) {mk = mk->GetParentModule();}
    return (TModule*)mk;
}
//_____________________________________________________________________________
TDatime  TModule::GetDateTime() const 
{    
   TDatime td;    

   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return td;
   return hd->GetDateTime();

}
//_____________________________________________________________________________
Int_t    TModule::GetDate()  const {return GetDateTime().GetDate();}
//_____________________________________________________________________________
const char *TModule::GetDateString()  const { return GetDateTime().AsString();}
//_____________________________________________________________________________
Int_t    TModule::GetTime()  const {return GetDateTime().GetTime();}


//_____________________________________________________________________________
const Char_t *TModule::GetEventType() const
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return 0;
   return hd->GetEventType();
}
//_____________________________________________________________________________
UInt_t TModule::GetTriggerMask() const
{
   StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
   if (!hd) return 0;
   return hd->GetTriggerMask();
}


//_____________________________________________________________________________
void TModule::PrintTimer(Option_t *option) 
{
  // Print timer information of this maker
  // Entries counts how many times the methods:
  //    Init(), Make() and Finish () 
  // were called
   if(option){};
   Printf("QAInfo:%-20s: Real Time = %6.2f seconds Cpu Time = %6.2f seconds, Entries = %d",GetName()
           ,m_Timer.RealTime(),m_Timer.CpuTime(),m_Timer.Counter());
}

//_____________________________________________________________________________
static void doPs(const char *who, const char *where)
{
  static const char *ps =0;
  if (!ps) {
//		execute shell      
    ps = gSystem->Getenv("StarEndMakerShell"); 
    ps = (ps) ? "yes" : "";
  }
  if (!ps[0]) return;
#ifdef STAR_LOGGER_BUG  
  LOG_QA << Form("QAInfo: doPs for %20s:%12s \t",who,where);
#else
  printf("QAInfo: doPs for %20s:%12s \t",who,where);
#endif
  StMemStat::PrintMem(0);
#ifdef STAR_LOGGER_BUG  
  LOG_QA << endm;
#else  
  printf("\n");
#endif  
}
//_____________________________________________________________________________
void TModule::DoPs(const char*opt) { ::doPs(GetName(),opt); }

#if 0
//_____________________________________________________________________________
void TModule::Streamer(TBuffer &)
{ Error("Streamer"," attempt to write %s\n ",GetName());
  assert(0);
}
#endif

//______________________________________________________________________________
StMaker *TModule::New(const Char_t *classname, const Char_t *name, void *title)
{
  // This static method creates a new TModule object if provided 
  
  StMaker *maker = 0;
  if (classname) 
  {
    TClass *cl = gROOT->GetClass(classname);
    if (cl) {
      maker = (StMaker *)cl->New();
      if (maker) {
	if (name && strlen(name)) maker->SetName(name);
	if (title) maker->SetTitle((Char_t *) title);
      }
    } 
  }
  return maker; 
}

//_____________________________________________________________________________
void TModule::SetDEBUG(Int_t l)
{
   m_DebugLevel = l;
   //   Loop on all makers
   TList *tl = GetMakeList();
   if (!tl) return;

   TIter nextModule(tl);
   StMaker *maker;
   while ((maker = (TModule*)nextModule())) maker->SetDEBUG(l);
}
//_____________________________________________________________________________
Int_t TModule::InitRun(int runumber) {if (runumber); return 0;}
//_____________________________________________________________________________
Int_t TModule::FinishRun(int runumber) {if (runumber); return 0;}

//_____________________________________________________________________________
Int_t  TModule::Remake() {  return kOK; }

//_____________________________________________________________________________
TModuleIter::TModuleIter(StMaker *mk,int secondary)
{
   fState = 0;
   fModule = mk;
   fModuleIter = 0;
   fIter  = new TDataSetIter(fModule->Find(".make"));
   fItWas = (TDataSet*)(-1);
   fSecond = secondary;
}
//_____________________________________________________________________________
TModuleIter::~TModuleIter()
{
   delete fIter; 	fIter = 0; 
   delete fModuleIter;	fModuleIter = 0;
   fModule=0; fState = 0;
}
//_____________________________________________________________________________
StMaker  *TModuleIter::NextModule()
{
  TDataSet *ds;
  if (!fModule)	return 0;

AGAIN: switch (fState) {

    case 0: 					//current maker 
      ds = fIter->Next();
      if (ds == fItWas) 	goto AGAIN;	//used already, go to Next
      fState = 2;  if (!ds) 	goto AGAIN;	//no more,      go to UP
      fState = 1;				//go to Down
      delete fModuleIter; 
      fModuleIter = new TModuleIter((TModule*)ds,1);
      goto AGAIN;

    case 1: 	// Recursive iteration
      ds = fModuleIter->NextModule();
      if (ds) return (TModule*)ds;
      fState = 0;		goto AGAIN;	//no more in downstaires,go curren
     
    case 2:
      delete fModuleIter; fModuleIter=0;
      delete fIter; 	 fIter = 0;
      fState = 3;
      return fModule;

    case 3:					// go upper when started
      if (fSecond) return 0;
      TDataSet *par = fModule->GetParent();
      fItWas = fModule; fModule = 0;
      if (!par) 				return 0;
      if (strcmp(".make",par->GetName()))	return 0;
      fModule = (TModule*)par->GetParent();
      if (!fModule)				return 0;
      delete fIter; fIter = new TDataSetIter(par);
      fState = 0; goto AGAIN;
  }
  assert(0); return 0;
}
#endif
//_____________________________________________________________________________
// $Log: TModule.cxx,v $
// Revision 1.1  2009/12/01 01:33:33  fine
// Move online display udner OnlTools
//
// Revision 1.7  2009/02/24 22:42:08  fine
// change the base class pointer
//
// Revision 1.6  2009/02/17 20:42:18  fine
// Change the class name to face the new ROOT API
//
// Revision 1.5  2008/10/22 22:25:38  fine
// Disable make::InitRun from TModule::Init
//
// Revision 1.4  2008/03/11 13:35:45  fine
// remove the RUN CPP flag
//
// Revision 1.3  2008/03/07 22:53:09  fine
// multithreaded version
//
// Revision 1.2  2007/12/28 15:27:05  fine
// version with the offline coordinate transformation
//
// Revision 1.1  2007/10/25 16:16:15  fine
// Add the lost components
//
// Revision 1.8  2003/04/03 17:39:40  fine
// Make merge with ROOT 3.05.03 and add TR package
