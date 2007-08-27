// $Id: StMaker.cxx,v 1.199 2007/08/27 19:54:41 fisyak Exp $
//
//
/*!
 * Base class for user maker class. Provide common functionality for all
 * makers(modules). Allows to arrange modules in a tree, consisting of modules
 * and produced datasets.
 *
 * User must provide at least its own methods
 * - Init()                     mandatory
 * - Make()                     mandatory
 * - InitRun(newRunNumber)      if needed
 * - FinishRun(oldRunNumber)    if needed
 * - Clear()                    if standard StMaker::Clear()  is not appropriate 
 * - Finish()                   if standard StMaker::Finish() is not appropriate 
 *
 * The method will be described.
 *                                                                     
 */
#define STAR_LOGGER 1
#include "Stiostream.h"
#include <stdio.h>
#include <stdlib.h>

#include "TSystem.h"
#include "TClass.h"
#include "TROOT.h"
#include "TError.h"
#if 0
#include "THtml.h"
#endif
#include "TH1.h"

#include "TChain.h"
#include "TTree.h"
#include "TList.h"
#include "TClonesArray.h"
#include "TBrowser.h"

#include "StMaker.h"
#include "StChainOpt.h"
#include "TObjectSet.h"
#include "StChain.h"
#include "TTable.h"

#include "TMemStat.h"
#include "TAttr.h"
#include "StMkDeb.h"
#include "StMessMgr.h"

StMaker     *StMaker::fgTopChain    = 0;
StMaker     *StMaker::fgStChain     = 0;
StMaker     *StMaker::fgFailedMaker = 0;
StTestMaker *StMaker::fgTestMaker   = 0;
Int_t    StMaker::fgTallyMaker[kStFatal+1] = {0,0,0,0,0};
Int_t MaxWarnings = 26;
/* geometry version from Maxim 09/29/2005
year2000                     VMC
year_2a         
year_2b
year2001                     VMC
year2002                     VMC
year2003                     VMC
y2003a                       VMC
y2003b                       VMC
y2003x                       VMC
y2004                        VMC
y2004a                       VMC
y2004b                       VMC
y2004c                       VMC
y2004x                       VMC
y2004y                       VMC
y2005                        VMC  
y2005b                       VMC     
y2005c                       VMC
y2005x                       VMC
dev2005   - non-production
complete   - non-production
ist1   - non-production      VMC
pix1   - non-production
----------------
old version of db tags 
{"sd97",        19970101,     0},
{"sd98",        19980101,     0},
{"year_1a",     19990101,     0},
{"year_1b",     19990501,     0},
{"es99",        19990615,     0},
{"er99",        19990616,120000},
{"year_1c",     19991001,     0},
{"year_1d",     19991101,     0},
{"year_1e",     19991201,     0},
{"dc99",        19991206, 80000},
{"y2000",       20000614,175430},
{"year_2b",     20010501,     0},
{"year_2a",     20010610,     0},
{"year2001",    20010615,     0},
{"year2003",    20021115,     0},
{"y2003b",      20021115,     0},
{"y2003a",      20021115,     0},
{"y2003x",      20021115,     0},
{"y2004b",      20031120,     0},
{"y2004a",      20031120,     0},
{"y2004x",      20031120,     0},
{"y2004",       20031120,     0},
{"y2004c",      20031125,     0},
{"y2005x",      20041030,     0},
{"y2005",       20041030,     0},
{"y2005b",      20041101,     0},
{"y2005c",      20041201,     0},


*/
static const DbAlias_t fDbAlias[] = {// geometry  Comment            old 
  {"sd97",        19970101,     0, "year2000", ""},                   //       {"sd97",        19970101,     0}
  {"sd98",        19980101,     0, "year2000", ""},                   //       {"sd98",        19980101,     0}
  {"y1a",         19990101,     0, "year2000", ""}, 
  {"year_1a",     19990101,     0, "year2000", ""},                   //       {"year_1a",     19990101,     0}
  {"y1b",         19990501,     0, "year2000", ""}, 
  {"year_1b",     19990501,     0, "year2000", ""},                   //       {"year_1b",     19990501,     0}
  {"y1s",         19990501,     0, "year2000", ""}, 
  {"year_1s",     19990501,     0, "year2000", ""}, 
  {"es99",        19990615,     0, "year2000", ""},                   //       {"es99",        19990615,     0}
  {"er99",        19990616,120000, "year2000", ""},                   //       {"er99",        19990616,120000}
  {"year_1c",     19991001,     0, "year2000", ""},                   //       {"year_1c",     19991001,     0}
  {"y1d",         19991101,     0, "year2000", ""}, 
  {"year_1d",     19991101,     0, "year2000", ""},                   //       {"year_1d",     19991101,     0}
  {"y1e",         19991201,     0, "year2000", ""}, 
  {"year_1e",     19991201,     0, "year2000", ""},                   //       {"year_1e",     19991201,     0}
  {"dc99",        19991206, 80000, "year2000", ""},                   //       {"dc99",        19991206, 80000}
  {"y1h",         20000614,175430, "year2000", ""}, 
  {"y2000",       20000614,175430, "year2000", ""},                   //       {"y2000",       20000614,175430}
  {"y2b",         20010501,     0, "year_2b",  ""},
  {"y2001",       20010501,     0, "year2001", ""},
  {"year_2b",     20010501,     0, "year_2b",  ""},                   //       {"year_2b",     20010501,     0}
  {"year2002",    20010502,     0, "year2002", ""},
  {"y2a",         20010610,     0, "year_2a",  ""}, 
  {"year_2a",     20010610,     0, "year_2a",  ""},                   //       {"year_2a",     20010610,     0}
  {"y2001n",      20010615,     0, "year2001", ""}, 
  {"year2001",    20010615,     0, "year2001", ""},                   //       {"year2001",    20010615,     0}
  {"y2003",       20021115,     0, "year2003", ""}, 
  {"year2003",    20021115,     0, "year2003", ""},                   //       {"year2003",    20021115,     0}
  {"y2003x",      20021115,     1, "y2003x",   ""},                   //       {"y2003x",      20021115,     0}
  {"y2003a",      20021115,     2, "y2003a",   ""},                   //       {"y2003a",      20021115,     0}
  {"y2003b",      20021115,     3, "y2003b",   ""},                   //       {"y2003b",      20021115,     0}
  {"y2003c",      20021115,     4, "y2003c",   "new SVT"}, 
  {"y2004",       20031120,     0, "y2004",    ""},                   //       {"y2004",       20031120,     0}
  {"y2004x",      20031120,     1, "y2004x",   ""},                   //       {"y2004x",      20031120,     0}
  {"y2004y",      20031120,     2, "y2004y",   ""},  
  {"y2004a",      20031120,     3, "y2004a",   ""},                   //       {"y2004a",      20031120,     0}
  {"y2004b",      20031120,     4, "y2004b",   ""},                   //       {"y2004b",      20031120,     0}
  {"y2004c",      20031125,     0, "y2004c",   ""},                   //       {"y2004c",      20031125,     0}
  {"y2004d",      20031125,     1, "y2004d",   "new SVT"},
  // Dead area in SSD, in version y2005f
  {"y2005x",      20041030,     0, "y2005x",   ""},                   //       {"y2005x",      20041030,     0}
  {"y2005",       20041030,     0, "y2005",    ""},                   //       {"y2005",       20041030,     0}
  {"y2005b",      20041101,     0, "y2005b",   ""},                   //       {"y2005b",      20041101,     0}
  {"y2005c",      20041201,     0, "y2005c",   ""},                   //       {"y2005c",      20041201,     0}
  {"y2005d",      20041201,     1, "y2005d",   "y2005c + new SVT"},   //       {"y2005d",      20041201,     0}
  {"y2005e",      20041201,     2, "y2005e",   "y2005d + new SSD"},   //       {"y2005e",      20041201,     0}
  {"y2005f",      20041201,     3, "y2005f",   "y2005e + SSD5/CALB2"},//       {"y2005e",      20041201,     0}

  // Dead area in SSD, in version y2006b
  {"y2006",       20051201,     0, "y2006",    "base for y2006: y2005e+fixed TPC plane"},
  {"y2006a",      20051201,     1, "y2006a",   "y2006+new FPD"},
  {"y2006b",      20051201,     2, "y2006b",   "y2006+new FPD+SSD5/CALB2"},// code versions indicated
  {"y2006c",      20051201,     3, "y2006c",   "y2006+new FPD+SSD5/CALB2+noPMD"},// 

  // in preparation
  {"y2007",       20061105,     0, "y2007",    "base geometry for y2007"}, // advertized simu 20061101

  // development tags
  {"dev2005",     20190101,     0, "dev2005",  "non-production"},
  {"complete",    20190101,     1, "complete", "non-production"},
  {"ist1",        20190101,     2, "ist1",     "non-production"},
  //  {"pix1",        20190101,     3, "pix1",     "non-production, old is not in present starsim tags"},
  {"upgr01",      20190101,     4, "upgr01",   ""},
  {"upgr02",      20190101,     5, "upgr02",   ""},
  {"upgr03",      20190101,     6, "upgr03",   ""},
  {"upgr04",      20190101,     7, "upgr04",   ""},
  {"upgr05",      20190101,     8, "upgr05",   ""},
  {"upgr06",      20190101,     9, "upgr06",   ""},  // what happened to 6? Historical not re-used
  {"upgr07",      20190101,    10, "upgr07",   ""},
  {"upgr08",      20190101,    11, "upgr08",   ""},
  {"upgr09",      20190101,    12, "upgr09",   ""},
  {"upgr10",      20190101,    13, "upgr10",   ""},
  {"upgr11",      20190101,    14, "upgr11",   ""},
  {"upgr12",      20190101,    15, "upgr12",   ""},
  {"upgr13",      20190101,    16, "upgr13",   ""},
  // Future development:
  {"simpletpc",   20200102,    16, "simpletpc",""},
  {0,                    0,     0,        0,    0}
};

// Turn the logger of the current maker
#define TURN_LOGGER(maker)                                        \
        if (!fLogger) fLogger = StMessMgr::Instance(ClassName()); \
        StTurnLogger SaveRestoreLogger(maker->GetLogger());       
                                
        
ClassImp(StMaker)

static void doPs(const char *who,const char *where);

//_____________________________________________________________________________
StMaker::StMaker(const char *name,const char *):TDataSet(name,".maker"),
						m_Mode(0), m_Number(0), m_LastRun(-3),
						m_DebugLevel(0),m_MakeReturn(0),fStatus(0),
						fLogger(0),fLoggerHold(0)
{
   m_Attr=0;
   m_Inputs = 0;
   if (!fgStChain) {	// it is first maker, it is chain
     fgTopChain = fgStChain = this;
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
   m_Timer.Stop();
   fMemStatMake  = 0;
   fMemStatClear = 0;
   memset(fTallyMaker,0,(kStFatal+1)*sizeof(Int_t));
   SetActive();
   StMkDeb::Register(this);
}

//_____________________________________________________________________________
void StMaker::AddMaker(StMaker *mk)
{
  TDataSet *dotmk = Find(".make");
  if (!dotmk) dotmk = new TDataSet(".make",this);
  mk->Shunt(dotmk);
}
//_____________________________________________________________________________
StMaker::~StMaker()
{
  if (fgTopChain == this) fgTopChain = 0;
  if (fgStChain == this) fgStChain = 0;
  delete fMemStatMake;	fMemStatMake  = 0;
  delete fMemStatClear;	fMemStatClear = 0;
  TDataSet *ds = this;
  Cleanup(ds);
  StMkDeb::Cancel(this);
}
//_____________________________________________________________________________
const char *StMaker::GetName() const
{
  static int occ = 0;
  const char *name = TNamed::GetName();
  if (name && *name ) return name;
  TString ts(ClassName());
  ts+="#"; ts+=(occ++);		
  ((TNamed*)this)->SetName(ts.Data());		
  return GetName();
}
//______________________________________________________________________________
void StMaker::SetNumber(Int_t number)
{
 StMaker *par = GetParentMaker();
 if (par) par->SetNumber(number);
 m_Number = number;
 SetIventNumber(number);
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
  TDataSet *par = GetParent(); if (!par) return 0;
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
  TObjectSet *h = (TObjectSet*)Find(dir);
  if (!h) return 0;
  return h->GetObject();
}
//______________________________________________________________________________
void StMaker::SetDirObj(TObject *obj,const char *dir)
{ 
  TObjectSet *set = (TObjectSet *)Find(dir);
  if (!set) { // No dir, make it
    set = new TObjectSet(dir); Add(set);}
  set->SetObject(obj);
}
//______________________________________________________________________________
TObjectSet *StMaker::AddObj(TObject *obj,const char *dir)
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
 void StMaker::AddHist(TH1 *h,const char *dir)
{  
  if (dir){/*unused*/}
  if (!h) {AddObj(0,".hist");return;}
  if (h->InheritsFrom(TH1::Class())) h->SetDirectory(0);
  AddObj(h,".hist");
}    
//______________________________________________________________________________
 void StMaker::AddRunco (double par,const char* name,const char* comment)
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
TDataSet *StMaker::AddData(TDataSet *ds, const char* dir)
{ 
  assert (dir); assert(dir[0]=='.');
  TDataSet *set = Find(dir);
  if (!set) { // No dir, make it
    set = new TObjectSet(dir); Add(set);}
  if (!ds) return set;
  int dotMake = (strcmp(dir,".make")==0);
  int inhMake = ds->InheritsFrom(StMaker::Class());
  if (dotMake!=inhMake) {
     Error("AddData","Add to %s is NOT allowed: %s.%s\n"
             ,dir,ds->ClassName(),ds->GetName());
     return 0;}

  TList *tl = set->GetList();
  if (!tl || !tl->FindObject(ds->GetName())) {
    set->Add(ds);
  } else {
    Error("AddData","Data %s/%s is not added. ***Name clash***",dir,ds->GetName());
    return 0;
  }
  return set;
}
//______________________________________________________________________________
TDataSet  *StMaker::GetData(const char *name, const char* dir) const
{ 
  TDataSet *set = Find(dir);
  if (!set) return 0;
  return set->Find(name);
}
//______________________________________________________________________________
void StMaker::ToWhiteBoard(const char *name, void *dat)
{
   TObjectSet *envelop = new TObjectSet(name,(TObject*)dat,0);
   envelop->SetTitle(".envelop");
   AddData(envelop,".data");
}
//______________________________________________________________________________
void StMaker::ToWhiteConst(const char *name, void *dat)
{
   TObjectSet *envelop = new TObjectSet(name,(TObject*)dat,0);
   envelop->SetTitle(".envelop");
   AddData(envelop,".const");
}
//______________________________________________________________________________
void StMaker::ToWhiteBoard(const char *name, TObject *dat, int owner)
{
   TObjectSet *envelop = new TObjectSet(name,dat,owner);
   envelop->SetTitle(".envelop");
   AddData(envelop,".data");
}
//______________________________________________________________________________
void StMaker::ToWhiteConst(const char *name, TObject *dat, int owner)
{
   TObjectSet *envelop = new TObjectSet(name,dat,owner);
   envelop->SetTitle(".envelop");
   AddData(envelop,".const");
}
//______________________________________________________________________________
void *StMaker::WhiteBoard(const char *name, void *v) const
{
  void **dat = (void **)v;
  *dat = 0;
  TDataSet *ds = GetDataSet(name);
  if (!ds) return 0;
  if (strcmp(".envelop",ds->GetTitle())==0) {*dat = ds->GetObject();}
  else					    {*dat = ds             ;}
  return ds->GetObject();
}
//______________________________________________________________________________
void StMaker::AddAlias(const char* log, const char* act,const char* dir)
{
  TDataSet *ali = new TDataSet(log); 
  ali->SetTitle(act);
  AddData(ali,dir);
}
//______________________________________________________________________________
void StMaker::SetNotify(const char* about, StMaker *mk)
{
  TDataSet *ali = new TObjectSet(about,mk,0); 
  AddData(ali,".notify");
}
//______________________________________________________________________________
void StMaker::NotifyEm(const char* about, const void *ptr)
{
// Turn the logger of the current maker
  TURN_LOGGER(this);
  
  TDataSet *set = Find(".notify");
  if (!set) return;
  TDataSetIter iter(set);
  TObjectSet *os=0;
  while((os=(TObjectSet*)iter())) {
    if (strcmp(about,os->GetName()))	continue;
    StMaker *mk=(StMaker*)os->GetObject();
    if (!mk)				continue;
    TURN_LOGGER(mk);
    mk->NotifyMe(about,ptr);
  }

}
//______________________________________________________________________________
void StMaker::SetAlias(const char* log, const char* act,const char* dir)
{ 
  TDataSet *ali = GetData(log,dir);
  if (ali) {
    if (!strcmp(act,ali->GetTitle())) return;
  } else {
    ali = new TDataSet(log); AddData(ali,dir);
  }
  ali->SetTitle(act);

  if (GetDebug()) {
#ifdef STAR_LOGGER     
  LOG_DEBUG << "<" << ClassName() << "(" << GetName() << "::SetAlias> " 
            << log << " = " << act << endm;
#else  
    printf("<%s(%s)::SetAlias> %s = %s\n",ClassName(),GetName(),log,act);
#endif    
 }
}
//______________________________________________________________________________
void StMaker::SetOutput(const char* log,TDataSet *ds)
{
  int idx;
  const char* logname = log;
  if (!logname || !logname[0]) logname = ds->GetName();
  TString act = ds->Path();
  while ((idx=act.Index(".make/"))>=0) act.Replace(0,idx+6,"");  
  SetOutput(logname,act); 
}

//______________________________________________________________________________
void StMaker::SetOutputAll(TDataSet *ds, Int_t level)
{
  TDataSet *set;
  TDataSetIter next(ds,level);
  while ((set = next())) SetOutput(set);
}

//______________________________________________________________________________
TList *StMaker::GetMakeList() const
{ TDataSet *ds = Find(".make");
  if (!ds) return 0;
  return ds->GetList();
}
//______________________________________________________________________________
TString StMaker::GetAlias(const char* log,const char* dir) const
{
  int nspn = strcspn(log," /");
  TString act(log,nspn);
  TDataSet *in = GetData(act,dir);
  act ="";
  if (in) {act = in->GetTitle(); act += log+nspn;}
  return act;
}
//______________________________________________________________________________
TDataSet *StMaker::FindDataSet(const char* logInput,
                                const StMaker *uppMk,
                                const StMaker *dowMk) const
{
  TURN_LOGGER(this);

TDataSetIter nextMk(0);
TString actInput,findString,tmp;
TDataSet *dataset,*dir;
StMaker    *parent,*mk;
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

  if (m_DataSet) {
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
  }

//	Try to search DOWN
DOWN: if (!(dir = Find(".make"))) goto UP;

  nextMk.Reset(dir);
  while ((mk = (StMaker* )nextMk()))
  {
    if (mk==dowMk) continue;
    dataset = mk->FindDataSet(actInput,this,0);
    if (dataset) goto FOUND;
  }

//     Try to search UP
UP: if (uppMk) return 0;

  parent = GetMaker(this); if (!parent) goto NOTFOUND;
  dataset = parent->FindDataSet(actInput,0,this);
  if (dataset) goto FOUND;

//		Not FOUND
NOTFOUND:
  if (!dowMk && GetDebug()>1) //Print Warning message
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
TDataSet *StMaker::GetDataBase(const char* logInput,const TDatime *td)
{
  TURN_LOGGER(this);
  TDataSet *ds = 0;
  StMaker  *mk = GetMakerInheritsFrom("St_db_Maker");
  if (mk) ds = mk->GetDataBase(logInput,td);
  return ds;
}
//______________________________________________________________________________
StMaker *StMaker::GetMakerInheritsFrom (const char *mktype)
{
  TURN_LOGGER(this);
  StMaker  *mk = 0;
  StMakerIter mkiter(this);
  while ((mk = mkiter.NextMaker())) {//loop over makers
    if (mk->InheritsFrom(mktype))   break;
  }
  return mk;
}
//______________________________________________________________________________
void StMaker::SetFlavor(const char *flav,const char *tabname)
{
  StMaker *mk = GetMakerInheritsFrom("St_db_Maker");
  if (mk) mk->SetFlavor(flav,tabname);
}
//______________________________________________________________________________
Int_t   StMaker::GetValidity(const TTable *tb, TDatime *val) const
{
   StMaker *mk = GetMaker(tb);
   if (!mk) 					return 10;
   if (!mk->InheritsFrom("St_db_Maker"))	return 11;
   return mk->GetValidity(tb,val);
}
//_____________________________________________________________________________
void StMaker::Clear(Option_t *option)
{
  TURN_LOGGER(this);

  m_MakeReturn = 0;
  if(option){};
  if (m_DataSet) m_DataSet->Delete();

//  Reset lists of event objects
   
   TIter next(GetMakeList(),kIterBackward);
   StMaker *maker;
   int curr = StMkDeb::GetCurrent();
   while ((maker = (StMaker* )next())) {
      assert(maker->TestBIT(kCleaBeg)==0);
      StMkDeb::SetCurrent(maker,3);
      maker->SetBIT(kCleaBeg);
      maker->StartTimer();
      if (maker->fMemStatClear && GetNumber()>20) maker->fMemStatClear->Start();
      TURN_LOGGER(maker);
      maker->Clear(option);
      if (maker->fMemStatClear && GetNumber()>20) maker->fMemStatClear->Stop();
      maker->StopTimer();
      maker->ResetBIT(kCleaBeg);
      StMkDeb::SetCurrent(curr);
   }
   TCollection::EmptyGarbageCollection();
   doPs(GetName(),"Clear");
   // Maker=StChain or whatever is called "Eread" would
   // reach this with a NULL pointer when executed from a macro
   // such as doEvent(). Same reason for the patch below ...
   //else       printf("StMaker::Clear :: cannot call method doPs on NULL pointer [%s]\n",GetName()) ;
   return;

}
//_____________________________________________________________________________
Int_t StMaker::Init()
{   
   TURN_LOGGER(this);  
   TObject  *objLast,*objHist;
   TList *tl = GetMakeList();
   if (!tl) return kStOK;
   
   TIter nextMaker(tl);
   StMaker *maker;
   int curr = StMkDeb::GetCurrent();
   while ((maker = (StMaker* )nextMaker())) {

      TURN_LOGGER(maker);
      // save last created histogram in current Root directory
      gROOT->cd();
      objLast = gDirectory->GetList()->Last();

      // Initialise maker

      assert(maker->TestBIT(kInitBeg)|maker->TestBIT(kInitEnd)==0);
      StMkDeb::SetCurrent(maker,1);
      maker->SetBIT(kInitBeg);
      maker->StartTimer();
      
      if (GetDebug()) {
#ifdef STAR_LOGGER 
        LOG_DEBUG << "*** Call << " << maker->ClassName() << ":Init() ***" << endm;       
#else      
        printf("\n*** Call %s::Init() ***\n\n",maker->ClassName());
#endif      
     }
      TString ts1(maker->ClassName()); ts1+="("; ts1+=maker->GetName(); ts1+=")::";
      TString ts2 = ts1; ts2+="Make ";
      maker->fMemStatMake  = new TMemStat(ts2);
      ts2 = ts1; ts2+="Clear";
      maker->fMemStatClear = new TMemStat(ts2);
      
      if ( maker->Init()) {
#ifdef STAR_LOGGER 
        LOG_ERROR << "   Maker "<< maker->GetName() << " failed in Init" << endm;
#else        
        printf("   Maker %s failed in Init\n", maker->GetName());
#endif        
        return kStErr;
      }
      maker->StopTimer();

// 		Add the Maker histograms in the Maker histograms list
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
      maker->ResetBIT(kInitBeg);
      maker->SetBIT  (kInitEnd);
      StMkDeb::SetCurrent(curr);
    }
  return kStOK; 
}
//_____________________________________________________________________________
void StMaker::StartMaker()
{
  // Save the previous logger status
  if (!fLoggerHold) fLoggerHold = new StTurnLogger(GetLogger());
  if (!m_DataSet) {//Keep legacy code
    m_DataSet = Find(".data");
    if (!m_DataSet) {m_DataSet = new TObjectSet(".data"); Add(m_DataSet);}
  }
  /*if (GetNumber()>3)*/ 
  if (fMemStatMake) if (GetNumber()>20) fMemStatMake->Start();
  else              
    if (GetDebug()) {
#ifdef STAR_LOGGER 
        LOG_DEBUG << "StMaker::StartMaker : cannot use TMemStat (no Init()) in [" << 
              GetName() << "]" << endm;
#else        
        printf("StMaker::StartMaker : cannot use TMemStat (no Init()) in [%s]\n",GetName());
#endif        
     }

  

  StartTimer();
}
//_____________________________________________________________________________
void StMaker::EndMaker(int ierr)
{
  SetMakeReturn(ierr);
  fgTallyMaker[ierr%10]++;
  fTallyMaker [ierr%10]++;
  if (m_DataSet) m_DataSet->Pass(ClearDS,0);
  if (m_GarbSet) m_GarbSet->Delete();
  ::doPs(GetName(),"EndMaker");
  
  /*if (GetNumber()>3)*/ 
  if (fMemStatMake) if (GetNumber()>20) fMemStatMake->Stop();
  else               
    if (GetDebug()) {
#ifdef STAR_LOGGER     
     LOG_DEBUG << "StMaker::EndMaker : cannot use TMemStat (no Init()) in [" 
               <<   GetName() 
               << "]" 
               << endm ;    
#else     
        printf("StMaker::EndMaker : cannot use TMemStat (no Init()) in [%s]\n",GetName());
#endif
     }


  StopTimer();
  // Restore the previous logger status
  if (fLoggerHold) { delete fLoggerHold; fLoggerHold = 0;}
  
}

//_____________________________________________________________________________
/*!
  Terminate a run.
  Place to make operations on histograms, normalization,etc.
 */
Int_t StMaker::Finish()
{
   if (TestBIT(kFiniEnd)) return 1;
   TURN_LOGGER(this);

   int nerr = 0;
   int run = GetRunNumber();
   if (run>-1) FinishRun(run);   

   TIter next(GetMakeList(),kIterBackward);
   StMaker *maker;
   Double_t totalCpuTime = 0;
   Double_t totalRealTime = 0;   
   while ((maker = (StMaker* )next())) 
   {
      totalCpuTime  += maker->CpuTime();
      totalRealTime += maker->RealTime();      
   }

   // Print relative time
   if (!totalRealTime) totalRealTime = 1;
   if (!totalCpuTime ) totalCpuTime  = 1;

   next.Reset();
   int fst=1;
   while ((maker = (StMaker*)next())) {
 #ifdef STAR_LOGGER  
      TURN_LOGGER(maker);

      if (fst) {
        fst=0;
        LOG_QA <<
            Form("=================================================================================") << endm;
        LOG_QA <<
            Form("QAInfo:Chain %20s::%-20s Ast =%6.2f        Cpu =%6.2f "
                   ,ClassName(),GetName(),totalRealTime,totalCpuTime) << endm;
      }
        LOG_QA <<
           Form("QAInfo:Maker %20s::%-20s Ast =%6.2f(%4.1f%%) Cpu =%6.2f(%4.1f%%) "
             ,maker->ClassName(),maker->GetName()
             ,maker->RealTime()
             ,100*maker->RealTime()/totalRealTime
             ,maker->CpuTime()
             ,100*maker->CpuTime()/totalCpuTime) << endm;

      static const char *ee[]={"nStOK","nStWarn","nStEOF","nStErr","nStFatal"};
      TString tail("");
      for (int j=0;j<=kStFatal;j++) {
        if (fTallyMaker[j]) tail += Form(" %s=%d",ee[j],fTallyMaker[j]);}
      if (tail != "") LOG_QA << (const char *) tail << endm;     
#else
     if (fst) {
        fst=0;
        Printf("=================================================================================\n");
        Printf("QAInfo: Chain %20s::%-20s Ast =%6.2f        Cpu =%6.2f "
               ,ClassName(),GetName(),totalRealTime,totalCpuTime);
      }
      printf("QAInfo: Maker %20s::%-20s Ast =%6.2f(%4.1f%%) Cpu =%6.2f(%4.1f%%) "
             ,maker->ClassName(),maker->GetName()
             ,maker->RealTime()
             ,100*maker->RealTime()/totalRealTime
             ,maker->CpuTime()
             ,100*maker->CpuTime()/totalCpuTime);

      static const char *ee[]={"nStOK","nStWarn","nStEOF","nStErr","nStFatal"};
      for (int j=0;j<=kStFatal;j++) {
        if (fTallyMaker[j]) printf(" %s=%d",ee[j],fTallyMaker[j]);}
      printf("\n");
#endif      
   }

   next.Reset();
   int curr = StMkDeb::GetCurrent();
   while ((maker = (StMaker* )next())) 
   {
      TURN_LOGGER(maker);

      if (maker->TestBIT(kFiniEnd)) {
        maker->Warning("Finish","maker %s.%s Finished twice"
               ,maker->GetName(),maker->ClassName());
        continue;}
      StMkDeb::SetCurrent(maker,4);
      maker->SetBIT(kFiniBeg);
      if ( maker->Finish() ) nerr++;
      maker->ResetBIT(kFiniBeg);
      maker->SetBIT  (kFiniEnd);
      StMkDeb::SetCurrent(curr);
   }
   if (!GetParent()) {// Only for top maker
#ifdef STAR_LOGGER     
     LOG_INFO << "--------------Error Codes-------------------------" << endm;
     LOG_INFO << "     nStOK   nStWarn    nStEOF    nStErr  nStFatal" << endm;
     TString tail("");
     for( int i=0; i<=kStFatal; i++) tail += Form("%10d",fgTallyMaker[i]); 
     if (tail != "") 
       LOG_INFO << (const char *)tail << endm;
     LOG_INFO << "--------------------------------------------------" << endm;
#else
     printf("\n--------------Error Codes-------------------------\n");
     printf("     nStOK   nStWarn    nStEOF    nStErr  nStFatal  \n");
     for( int i=0; i<=kStFatal; i++) printf("%10d",fgTallyMaker[i]); 
     printf("\n--------------------------------------------------\n");
#endif          
   }  
//VP   Printf("=================================================================================\n");
   
   if (GetParent()==0) TMemStat::Summary();
   return nerr;
}

//_____________________________________________________________________________
/*!
  The Make() method is the one responsible for calling the
  maker's InitRun(). Note that that InitRun() is called for
  real data and if both of the following are true
  - a header exists
  - the run number changes

  This is the ONLY place calling the InitRun() routine.

 */
Int_t StMaker::Make()
{
   TURN_LOGGER(this);

//   Loop on all makers
   Int_t ret,run=-1,oldrun;
   TList *tl = GetMakeList();
   if (!tl) return kStOK;
   StEvtHddr *hd = GetEvtHddr();   
   TIter nextMaker(tl);
   StMaker *maker;
   fgFailedMaker = 0;
   int curr = StMkDeb::GetCurrent();
   while ((maker = (StMaker* )nextMaker())) {
     if (!maker->IsActive()) continue;
     TURN_LOGGER(maker);
     assert(maker->TestBIT(kMakeBeg)==0);
     maker->SetBIT(kMakeBeg);
     StMkDeb::SetCurrent(maker,2);
     oldrun = maker->m_LastRun;
     if (hd && hd->GetRunNumber()!=oldrun) {
       if (oldrun>-1) maker->FinishRun(oldrun);
       run = hd->GetRunNumber();  
       if (Debug() && this == fgStChain && m_LastRun!=run){
         m_LastRun = run;
#ifdef STAR_LOGGER     
        LOG_INFO << " +++ New RunNumber found=" << run << " (previous = " << oldrun << ")" << endm;
#else
        printf(" +++ New RunNumber found=%d (previous = %d)\n",run,oldrun);
#endif                
         hd->Print();
       }
       maker->InitRun(run);
       maker->m_LastRun=run;
     }
// 		Call Maker
     if (fgTestMaker) { fgTestMaker->SetNext(maker); fgTestMaker->Make();}

     maker->StartMaker();
     ret = maker->Make();
     assert((ret%10)>=0 && (ret%10)<=kStFatal);     
     maker->EndMaker(ret);
     
     if (Debug() || ret) 
#ifdef STAR_LOGGER     
        LOG_INFO << "*** " << maker->ClassName() << "::Make() == " 
                  << RetCodeAsString(ret) << "(" << ret << ") ***" 
                  << endm;
#else
        printf("*** %s::Make() == %s(%d) ***\n"
                        ,maker->ClassName(),RetCodeAsString(ret),ret);
#endif     
     maker->ResetBIT(kMakeBeg);
     StMkDeb::SetCurrent(curr);
     if ((ret%10)>kStWarn) { //something unusual
       if ((ret%10) != kStERR) 		return ret;
///	check privilege to skip event
       fgFailedMaker = maker;
       if (maker->IAttr(".Privilege"))	return ret;
       continue;
     }
     
   }
   return kStOK;
}
//_____________________________________________________________________________
void StMaker::FatalErr(int Ierr, const char *com)
{
#ifdef STAR_LOGGER     
    LOG_QA    << Form("QAInfo:%s::Fatal: Error %d %s",GetName(),Ierr,com) << endm;
    LOG_FATAL << Form("QAInfo:%s::Fatal: Error %d %s",GetName(),Ierr,com) << endm;
#else
    printf("QAInfo:%s::Fatal: Error %d %s\n",GetName(),Ierr,com);
#endif        
   StMaker *parent = (StMaker *)GetParent();
   if (parent) ((StMaker*)parent)->FatalErr(Ierr,com);
#ifndef STAR_LOGGER     
   fflush(stdout);
#endif        
}
//_____________________________________________________________________________
StMaker *StMaker::GetMaker(const TDataSet *ds) 
{ 
  const TDataSet *par = ds;
  while (par && (par = par->GetParent()) && strncmp(".maker",par->GetTitle(),6)) {}
  return ( StMaker*) par;
}

//_____________________________________________________________________________
/*!
  Reduce the size of the table to the used rows + 1
  and filll the last empty row awith a special pattern
  Check the table for NaN floating cells if any
*/
EDataSetPass StMaker::ClearDS (TDataSet* ds,void * )
{
  if (ds->InheritsFrom(TTable::Class())){
     TTable *table = (TTable *)ds;
     Int_t setSize =  table->GetTableSize();
     table->ReAllocate();
     memset((void *)table->At(table->GetNRows()),127,table->GetRowSize());
     //if (setSize && (setSize - table->GetTableSize() > 100)) {
      if (setSize && table->GetTableSize() == 0){
        table->Warning("ReAllocate"," Table %s has purged from %d to %d "
               ,table->GetName(),setSize,table->GetTableSize());
	       }
     table->NaN();
  }
  return kContinue; 
}
//_____________________________________________________________________________
void StMaker::PrintInfo() 
{
   const char *cvs = GetCVS();
   const char *built = strstr(cvs,"built");
#ifdef STAR_LOGGER       
   if (built > cvs) LOG_QA << Form("QAInfo:%-20s %s from %.*s",ClassName(),built,built-cvs,cvs)<< endm;
   else             LOG_QA << Form("QAInfo:%-20s    from %s",ClassName(),cvs) << endm;
#else   
   if (built > cvs) printf("QAInfo:%-20s %s from %.*s\n",ClassName(),built,built-cvs,cvs);
   else             printf("QAInfo:%-20s    from %s\n",ClassName(),cvs);
#endif   
//     Print info for all defined Makers
   TIter next(GetMakeList());
   StMaker *maker;
   while ((maker = (StMaker* )next())) {
      maker->PrintInfo();
   }
}

//_____________________________________________________________________________
/// Returns the current event number
Int_t        StMaker::GetIventNumber() const 
{
   StEvtHddr *hd = GetEvtHddr();
   return hd->GetIventNumber();
}

//_____________________________________________________________________________
void         StMaker::SetIventNumber(Int_t iv)  
{
   StEvtHddr *hd = GetEvtHddr();
   hd->SetIventNumber(iv);
}
//_____________________________________________________________________________
Int_t        StMaker::GetEventNumber() const 
{
   StEvtHddr *hd = GetEvtHddr();
   return hd->GetEventNumber();
}

//_____________________________________________________________________________
/// Returns the current RunNumber
Int_t        StMaker::GetRunNumber() const 
{
   StEvtHddr *hd = GetEvtHddr();
   return hd->GetRunNumber();
}
//_____________________________________________________________________________
StMaker     *StMaker::GetParentChain() const 
{
    const StMaker *mk = GetParentMaker();
    while(mk && !mk->IsChain()) {mk = mk->GetParentMaker();}
    return (StMaker*) mk;
}
//_____________________________________________________________________________
TDatime  StMaker::GetDateTime() const 
{    
   StEvtHddr *hd = GetEvtHddr();
   return hd->GetDateTime();
}
//_____________________________________________________________________________
Int_t    StMaker::GetDate()  const {return GetDateTime().GetDate();}
//_____________________________________________________________________________
Int_t    StMaker::GetTime()  const {return GetDateTime().GetTime();}
//_____________________________________________________________________________
const Char_t *StMaker::GetEventType() const
{
   StEvtHddr *hd = GetEvtHddr();
   return hd->GetEventType();
}

//_____________________________________________________________________________
/*!
  Print timer information of this maker.
  Entries counts how many times the methods: Init(), Make() and Finish ()
  were called.
 */
void StMaker::PrintTimer(Option_t *option) 
{
   if(option){};
#ifdef STAR_LOGGER       
   LOG_QA << Form("QAInfo:%-20s: Real Time = %6.2f seconds Cpu Time = %6.2f seconds, Entries = %d",GetName()
           ,m_Timer.RealTime(),m_Timer.CpuTime(),m_Timer.Counter()) << endm;
#else   
   Printf("QAInfo:%-20s: Real Time = %6.2f seconds Cpu Time = %6.2f seconds, Entries = %d",GetName()
           ,m_Timer.RealTime(),m_Timer.CpuTime(),m_Timer.Counter());
#endif
}
void StMaker::lsMakers(const StMaker *top)
{
  TDataSetIter   iter((TDataSet*)top,20);
  int N=0;
  for(const TDataSet *mk=top;mk;mk = iter.Next()) {
    if(strncmp(".maker",mk->GetTitle(),6)!=0) continue;
    int l=iter.GetDepth();
    N++;
    char space[20]; memset(space,' ',sizeof(space));space[l]=0;
    
    printf("%3d(%d) - %s %s::%s\n",N,l/2,space,mk->ClassName(),mk->GetName());
  }
}



#if 0
//_____________________________________________________________________________
static void MakeAssociatedClassList(const TObject *obj, const Char_t *classDir=0)
{
/*!
 * This function creates the html docs of the classes pointed within
 * <obj> class source directory
 *
 * classDir - the name of the directory to search in
 *
 * Search C++ class declarations like:
 *
 *   class <className> : 
 *   class <className> { 
 *   class <className> ;
 *
 * looping over all *.h within the "classDir" directory 
 * if provided otherwise within "this" class source directory.
 *
 */
  if (!obj) return;
  const Char_t *thisDir = classDir;
  if (thisDir == 0 || thisDir[0] == 0) 
       thisDir = gSystem->DirName(obj->IsA()->GetImplFileName());
  const Char_t *thisClassName = obj->IsA()->GetName();
  // Loop over all *.h files within <thisDir> to find
  // C++ class declarations
  void *dirhandle = 0;
  TString className;
  if ( (dirhandle = gSystem->OpenDirectory(thisDir)) ) 
  {
    const Char_t *n = 0;
    ifstream headerFile;
    Char_t inBuffer[128] = {0};
    Int_t lBuf = sizeof(inBuffer);
    Char_t *nextSym = inBuffer;
    Int_t status = 0;
    const Char_t keyWord[] = "class";
    const Int_t lKeyWord = sizeof(keyWord);

    while ( (n = gSystem->GetDirEntry(dirhandle)) ) {
      // look for *.h* files but *Cint.h
      if (!strstr(n,".h") || strstr(n,"Cint.h") ) continue; 
      Char_t *fullFile = gSystem->ConcatFileName(thisDir,n);
      headerFile.open(fullFile);
      if (headerFile.fail()) continue;
      while (headerFile.getline(inBuffer,lBuf) && !headerFile.eof()) {
        nextSym = inBuffer;
        if (status==0)  status = 1;
        do {
 /* 
  ************************************************************
  *
  *      |<===========================================+
  *      |                                            |
  * $-->(1)<===+                                      |
  *      | ' ' |                                      |
  *      |---->|                                      |
  *      |"class"   ' '                               |
  *      |------>(2)--->(3)<===+                      |
  *                      | ' ' |                      |
  *                      |---->|                      | 
  *                      | name                       |
  *                      |------>(4)<===+             |
  *                               | ' ' |             | 
  *                               |---->|             | 
  *                               | ";"               | 
  *                               |------>(5)-------->|
  *                               |        | add2list    
  *                               |  ":"   |
  *                               |------->|
  *
  ************************************************************
  */
          switch (status) {
            case 1: {
                if (*nextSym == ' ' || *nextSym == '\t') break;    
                const Char_t *classFound = strstr(nextSym,keyWord);
                if ( classFound && classFound == nextSym){
                                                        status = 2;
                   nextSym += lKeyWord-2;
                }
                else                         status = 0;             
                break;
              }
            case 2:                       status = 0;
              if (*nextSym == ' ' || *nextSym == '\t')  status = 3;
              break;
            case 3:
              if (*nextSym == ' ' || *nextSym == '\t') break;    
                                           status = 0;
              if (isalpha(*nextSym)) {
                 className = *nextSym;
                 nextSym++;
                 while (isalnum(*nextSym) || *nextSym == '_' ) { 
                    className += *nextSym++;            status = 4;
                 }
                 nextSym--;
              }
              break;
            case 4:
              if (*nextSym == ' ' || *nextSym == '\t') break;    
                                          status = 0;
              if (*nextSym == 0 || *nextSym == ':' || *nextSym == '{' ||
                  ( *nextSym == '/' && 
                    (*(nextSym+1) == '/' || *(nextSym+1) == '*') 
                  )
                 )                                      status = 5;                 
              break;
            case 5:
              if (strcmp(thisClassName,className.Data())) {
                printf(" MakeDoc: %s\n", className.Data());
                TClass *cl = gROOT->GetClass(className.Data());
                if (cl && !cl->InheritsFrom("StMaker") ) {
                    gHtml->MakeClass((Text_t *)className.Data());
                }
              }
            default:                                    status = 1;
               break;
          };
        }   while (*(++nextSym) && status ); // end of buffer
      } // eof()
      headerFile.close();
      delete [] fullFile;
    } 
  }  
}
//_____________________________________________________________________________
void StMaker::MakeDoc(const TString &stardir,const TString &outdir, Bool_t baseClasses)
{
/*!
 * MakeDoc - creates the HTML doc for this class and for the base classes
 *           (if baseClasses == kTRUE):
 *
 *         *                 St_Module      TTable       * 
 *         *  TDataSet   St_DataSetIter St_FileSet       *
 *         *  StMaker      StChain        StEvent        *
 *         *  St_TLA_Maker                               *
 *
 * stardir - the "root" directory to lookup the subdirectories as follows.
 *           = "$(STAR)"             by default
 * outdir  - directory to write the generated HTML and Postscript files into
 *           = "$(STAR)/StRoot/html" by default
 *
 *            The following subdirectories are used to look it up:
 *            $(stardir)
 *            $(stardir) + "StRoot/St_base"
 *            $(stardir) + "StRoot/StChain"
 *            $(stardir) + "StRoot/StarClassLibrary"
 *            $(stardir) + "StRoot/StEvent"
 *            $(stardir) + "include",
 *            $(stardir) + "include/tables",
 *            $(stardir) + "StRoot/<this class name>",
 *
 *   where $(stardir) is the input parameter (by default = "$STAR")
 *
 * baseClasses - flag to mark whether the base classes HTML docs will be created as well
 *               = kTRUE by default
 */
 // Define the type of the OS
  TString STAR = stardir;
  TString delim = ":";
  //Bool_t NT=kFALSE; variable assigned but never used

  if (strcmp(gSystem->GetName(),"WinNT") == 0 ) {
    //NT=kTRUE;
     delim = ";";
     STAR.ReplaceAll("$(afs)","//sol/afs");
  }
  else 
     STAR.ReplaceAll("$(afs)","/afs");

  TString classname = IsA()->GetName();

  THtml thisHtml;

  //  if (!gHtml) gHtml = new THtml;

  // Define the set of the subdirectories with the STAR class sources
  //                       | ----------------------  | ------------  | ------------------ |
  //                       | Directory name             Class name     Share library name |
  //                       | ----------------------  | ------------  | ------------------ |
  const Char_t *source[] = {"StRoot/St_base"         , "TDataSet"  ,      "St_base"
                           ,"StRoot/StChain"         , "StMaker"     ,    "StChain"
			    //,"StRoot/StUtilities"     , "StMessage"   ,    "StUtilities"
                           ,"StRoot/StarClassLibrary", ""            ,    ""
                           ,"StRoot/StEvent"         , "StEvent"     ,    "StEvent"
                           ,"StRoot/St_TLA_Maker"    , "St_TLA_Maker",    "St_TLA_Maker"
                           ,"include"                , ""            ,     ""
                           ,"include/tables"         , ""            ,     ""
                           };

  const Int_t lsource = sizeof(source)/sizeof(const Char_t *);
 
  TString classDir = gSystem->DirName(IsA()->GetImplFileName());
  TString lookup = STAR;
  lookup += delim;

  lookup += STAR;
  lookup += "/";
  lookup += classDir;
  lookup += delim;

  lookup += STAR;
  lookup += "/StRoot/";
  lookup += classname;

  // Add class name base
  
  Int_t i = 0;
  for (i=0;i<lsource-3;i+=3) {
    lookup += delim;
    lookup += STAR;
    lookup += "/";
    lookup += source[i];
    // Load extra share library if any
    const Char_t *cl = source[i+1];
    const Char_t *so = source[i+2];
    if (cl && cl[0] && so && so[0] && !gROOT->GetClass(cl)) 
    {
       if (gSystem->Load(so)) 
           printf(" Failed to load the share library %s for class %s\n",so,cl);
    }
  }

//  cout << lookup.Data() << endl << endl;
  
//  const Char_t *c = ClassName();  // This trick has to be done since a bug within ROOT

  lookup.ReplaceAll("//StRoot/","/StRoot/");
  thisHtml.SetSourceDir(lookup);

  TString odir = outdir;
//  odir.ReplaceAll("$(STAR)",STAR);
  gSystem->ExpandPathName(odir);
  thisHtml.SetOutputDir(odir);

  // Create the list of the classes defined with the loaded DLL's to be documented

  Char_t *classes[] = { 
			"St_Module",      "TTable"
                       ,"TDataSet",    "TDataSetIter",   "TFileSet"
                       ,"StMaker",     "StChain"
                       ,"table_head_st"
                      };
  Int_t nclass = sizeof(classes)/4;
  // Create the definitions of the classes not derived from TObjects
  TString header = "$STAF/inc/table_header.h";

  gSystem->ExpandPathName(header);
  header.ReplaceAll("//inc/","/inc/");
  gROOT->LoadMacro(header);

  TClass header1("table_head_st",1,"table_header.h","table_header.h");

  // Update the docs of the base classes
  static Bool_t makeAllAtOnce = kTRUE;
  if (makeAllAtOnce && baseClasses) { 
      makeAllAtOnce = kFALSE;
      //  thisHtml.MakeAll();  // VF 10/09/99
      for (i=0;i<nclass;i++) thisHtml.MakeClass(classes[i]);
      thisHtml.MakeIndex();
//      MakeAssociatedClassList(this, classDir.Data());
  }

  if (baseClasses) {gHtml= &thisHtml;  MakeAssociatedClassList(this, classDir.Data()); }
  // Create the doc for this class
  printf(" Making html for <%s>\n",classname.Data());
  thisHtml.MakeClass((Char_t *)classname.Data());
  // Create the associated classes docs
  //   Loop on all makers
   TList *tl = GetMakeList();
   if (tl) {
     TIter nextMaker(tl);
     StMaker *maker;
     while ((maker = (StMaker* )nextMaker())) 
         maker->MakeDoc(stardir,outdir,kFALSE);
   }
}
#endif
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
  TMemStat::PrintMem(0);
#if 0
#ifdef STAR_LOGGER_BUG  
  LOG_QA << endm;
#else  
  printf("\n");
#endif  
#endif
}

//_____________________________________________________________________________
void StMaker::Streamer(TBuffer &)
{ LOG_FATAL << Form("%s::Streamer - attempt to write %s",ClassName(),GetName()) << endm;
  assert(0);
}
//______________________________________________________________________________
StMaker *StMaker::New(const Char_t *classname, const Char_t *name, void *title)
{
  // This static method creates a new StMaker object if provided 
  
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
void  StMaker::SetDebug(Int_t l)
{
   m_DebugLevel = l;
   StMessMgr    *log = GetLogger();
   if (log) log->SetLevel(m_DebugLevel);
}

//_____________________________________________________________________________
void StMaker::SetDEBUG(Int_t l)
{
  SetDebug(l);
//   Loop on all makers
   TList *tl = GetMakeList();
   if (!tl) return;
   
   TIter nextMaker(tl);
   StMaker *maker;
   while ((maker = (StMaker* )nextMaker())) maker->SetDEBUG(l);
}
//_____________________________________________________________________________
/*!
 * SetAttr(const char *key, const char *val, const char *to)
 * key is a keyname, val is a value
 * key is any character string. Spaces and case are ignored
 * The "." at the beginning is reserved for the frame work only
 * Like ".privilege" makes maker to be priveleged, i.e to skip event
 * by return of kStERR or kStSKIP.
 * attribute  always inserted at the begining of existing list
 * so the last key value overwrites the previous one for the same key
 * to =="" or == 0 means  THIS maker
 * to =="*" means for all makers down of the first
 * to =="name" means for maker with this name only
 */
int StMaker::SetAttr(const char *key, const char *val, const char *to)
{

   int count = 0;
   TString tk(key);tk.ToLower();tk.ReplaceAll(" ","");tk.ReplaceAll("\t","");
   if (!val) val ="";
   TString tv(val);tv = tv.Strip(TString::kBoth)     ;tv.ReplaceAll("\t","");
   if (!to || !to[0]) to  =".";
   TString tt(to );             tt.ReplaceAll(" ","");tt.ReplaceAll("\t","");
   TString tn(tt),tc("*");
   int idx = tt.Index("::");
   if (idx>=0) {//Case with class name
     tn.Replace(0  ,idx+2,""); if (!tn.Length()) tn = "*";
     tc=tt;
     tc.Replace(idx,999  ,""); if (!tc.Length()) tc = "*";
   }  
   int act=0;
        if (tn==".") 			{act = 1;}
   else if (tn=="*") 			{act = 5;}
   else if (tn==GetName()) 		{act = 1;}
   else                  		{act = 4;}

        if (tc=="*")               	{act |=2;}
   else if (InheritsFrom(tc.Data()))	{act |=2;}

   TString fullName(ClassName()); fullName+="::"; fullName+=GetName();

   if ((act&3)==3) { // this attribute is for this maker 
     count++;
     if (tk == ".call") {
         TString command("(("); command += ClassName(); command+="*)";
         char buf[20]; sprintf(buf,"%p",(void*)this);
	 command +=buf; command +=")->"; command+=tv;command+=";";
	 gROOT->ProcessLineFast(command.Data(),0);}
     else {
       if (!m_Attr) m_Attr = new TAttr(GetName());
       m_Attr->SetAttr(tk.Data(), tv.Data());
       if (Debug() > 1) {
	 LOG_DEBUG << Form("SetAttr","(\"%s\",\"%s\",\"%s\")",tk.Data(),tv.Data(),fullName.Data()) << endm;
       }
     }
   }
   if (!(act&4)) return count;
     
   //   Loop on all makers
   TList *tl = GetMakeList();
   if (!tl) return count;
   
   TIter nextMaker(tl);
   StMaker *maker;
   while ((maker = (StMaker*)nextMaker())) count += maker->SetAttr(tk.Data(),tv.Data(),to);
   return count;
}
//_____________________________________________________________________________
int StMaker::SetAttr(const char *key, int val, const char *to)
{
   TString ts; ts+=val; return SetAttr(key, ts.Data(), to);
}
//_____________________________________________________________________________
int StMaker::SetAttr(const char *key, UInt_t val, const char *to)
{
   TString ts; ts+=val; return SetAttr(key, ts.Data(), to);
}
//_____________________________________________________________________________
int StMaker::SetAttr(const char *key, double val, const char *to)
{
   TString ts; ts+=val; return SetAttr(key, ts.Data(), to);
}

//_____________________________________________________________________________
const char *StMaker::SAttr(const char *key) const
{
   if (!m_Attr) return "";
   return m_Attr->SAttr(key);
}   
//_____________________________________________________________________________
int StMaker::IAttr(const char *key) const
{
   if (!m_Attr) return 0;
   return m_Attr->IAttr(key);
}
//_____________________________________________________________________________
UInt_t StMaker::UAttr(const char *key) const
{
   if (!m_Attr) return 0;
   return m_Attr->UAttr(key);
}
//_____________________________________________________________________________
double StMaker::DAttr(const char *key) const
{
   if (!m_Attr) return 0;
   return m_Attr->DAttr(key);
}
//_____________________________________________________________________________
void StMaker::PrintAttr() const
{
   if (!m_Attr) return ;
   m_Attr->PrintAttr();
}

//_____________________________________________________________________________
Int_t StMaker::InitRun  (int runumber) {return 0;}
//_____________________________________________________________________________
Int_t StMaker::FinishRun(int runumber) {return 0;}

//_____________________________________________________________________________
int StMaker::Cleanup(TDataSet *&ds) 
{

   if (!ds->TObject::TestBit(TObject::kNotDeleted)) {ds=0;return 0;}	
   TSeqCollection *list = ds->TDataSet::GetCollection();
   if (!list)                        		return 0;
   assert(list->IsA()==TList::Class() || list->IsA()==TObjArray::Class()); 

   int kount = 0;
   TIter iter(list);
   TDataSet *son;	
   int num = list->Capacity();
   for (int i=0; i<num; i++) {
     son = (TDataSet*)iter.Next();
     if (!son) continue;
     if (!son->TObject::TestBit(TObject::kNotDeleted)) 	{list->Remove(son); continue;}
     TDataSet* par = son->TDataSet::GetParent();
     if ( par != ds)				{list->Remove(son); continue;}
     assert (son->InheritsFrom(TDataSet::Class()));
     if (son->InheritsFrom(StMaker::Class())) 	continue;//Delay cleanup
     kount = Cleanup(son) + 1;
   }     
   if (!ds->InheritsFrom(TObjectSet::Class()))  return kount;
   TObjectSet *os = (TObjectSet*)ds;
   TObject *to = os->GetObject();
   if (!to)					return kount;
   if (!to->TObject::TestBit(TObject::kNotDeleted)) {
     os->DoOwner(0); os->SetObject(0); 		return kount+1;}
   if (!os->IsOwner()) {os->SetObject(0);	return kount;}
   if (!to->InheritsFrom(TDataSet::Class()))	return kount;	
   TDataSet *t = (TDataSet*)to;
   return kount + Cleanup(t);   
}
//_____________________________________________________________________________
StEvtHddr *StMaker::GetEvtHddr() const
{
  StEvtHddr *hddr = (StEvtHddr*)GetDataSet("EvtHddr");
  if(!hddr) hddr = new StEvtHddr((TDataSet*)m_ConstSet);
  return hddr;
}
//_____________________________________________________________________________
  void StMaker::SetDateTime(int idat,int itim)
{
  StEvtHddr *hddr = GetEvtHddr();
  hddr->SetDateTime(idat,itim);
}

//_____________________________________________________________________________
const char *StMaker::RetCodeAsString(int kode)
{
static const char* retCodes[] = {
  "StOK"  ,"StWarn"  ,"StEOF"  ,"StERR"  ,"StFATAL" ,0,0,0,0,0,	
  "StOK!" ,"StWarn!" ,"StEOF!" ,"StSKIP" ,"StSTOP"  ,0,0,0,0,0,
  "StOK!!","StWarn!!","StEOF!!","StSKIP!","StSTOP!" ,0,0,0,0,0};

  assert(kode>=0);
  if (kode>=30) kode = kode%10+20;
  const char *res = retCodes[kode];
  if (!res) res = "StUNKNOWN";	
  return res;	
	
}

//_____________________________________________________________________________
StMakerIter::StMakerIter(StMaker *mk,int secondary)
{
  fState = 0;
  fMaker = mk;
  fMakerIter = 0;
  fIter  = new TDataSetIter(fMaker->Find(".make"));
  fItWas = (TDataSet*)(-1);
  fSecond = secondary;
   
}
//_____________________________________________________________________________
StMakerIter::~StMakerIter()
{
  delete fIter; 	fIter = 0; 
  delete fMakerIter;	fMakerIter = 0;
  fMaker=0; fState = 0;
}
//_____________________________________________________________________________
StMaker  *StMakerIter::NextMaker()
{
  TDataSet *ds;
  if (!fMaker)	return 0;

AGAIN: switch (fState) {

    case 0: 					//current maker 
      ds = fIter->Next();
      if (ds == fItWas) 	goto AGAIN;	//used already, go to Next
      fState = 2;  if (!ds) 	goto AGAIN;	//no more,      go to UP
      fState = 1;				//go to Down
      delete fMakerIter; 
      fMakerIter = new StMakerIter((StMaker*)ds,1);
      goto AGAIN;

    case 1: 	// Recursive iteration
      ds = fMakerIter->NextMaker();
      if (ds) return (StMaker* )ds;
      fState = 0;		goto AGAIN;	//no more in downstaires,go curren
     
    case 2:
      delete fMakerIter; fMakerIter=0;
      delete fIter; 	 fIter = 0;
      fState = 3;
      return fMaker;

    case 3:					// go upper when started
      if (fSecond) return 0;
      TDataSet *par = fMaker->GetParent();
      fItWas = fMaker; fMaker = 0;
      if (!par) 				return 0;
      if (strcmp(".make",par->GetName()))	return 0;
      fMaker = (StMaker* )par->GetParent();
      if (!fMaker)				return 0;
      delete fIter; fIter = new TDataSetIter(par);
      fState = 0; goto AGAIN;
  }
  assert(0); return 0;
}
//_____________________________________________________________________________
Int_t StMaker::AliasDate(const char *alias)

{

  int n = strcspn(alias," ."); if (n<3) return 0;
  int i;
  for (i=0;fDbAlias[i].tag && strncmp(alias,fDbAlias[i].tag,n);i++) {} 
  return fDbAlias[i].date;
}
//_____________________________________________________________________________
Int_t StMaker::AliasTime(const char *alias)

{

  int n = strcspn(alias," ."); if (n<3) return 0;
  int i;
  for (i=0;fDbAlias[i].tag && strncmp(alias,fDbAlias[i].tag,n);i++) {} 
  return fDbAlias[i].time;
}
//_____________________________________________________________________________
Char_t *StMaker::AliasGeometry(const char *alias)

{

  int n = strcspn(alias," ."); if (n<3) return 0;
  int i;
  for (i=0;fDbAlias[i].tag && strncmp(alias,fDbAlias[i].tag,n);i++) {} 
  return fDbAlias[i].geometry;
}
//_____________________________________________________________________________
const DbAlias_t *StMaker::GetDbAliases() {return fDbAlias;}
//_____________________________________________________________________________
const StChainOpt *StMaker::GetChainOpt()    const
{
  StMaker *mk = GetMaker(this);
  if (!mk) return 0;
  return  mk->GetChainOpt();
}
//_____________________________________________________________________________
TFile *StMaker::GetTFile() const 			
{
  const static Char_t *mktype = "StBFChain";
  StMaker  *mk = 0;
  if (this->InheritsFrom(mktype)) {mk = (StMaker *) this;}
  else {
    StMakerIter mkiter(GetChain());
    while ((mk = mkiter.NextMaker())) {//loop over makers
      if (mk->InheritsFrom(mktype))   {// take first TFile in any BFC
	const StChainOpt *opt = mk->GetChainOpt();
	if (!opt) continue;
	if (opt->GetTFile()) break;
      }
    }
  }
  const StChainOpt *opt = mk->GetChainOpt();
  if (!opt) return 0;
  return opt->GetTFile();
}

ClassImp(StTestMaker)
//_____________________________________________________________________________
StTestMaker::StTestMaker(const char *name):StMaker(name)
{
   fNext=0; fLast=0;
   if (fgStChain == this ) {fgStChain=0;}
   else                    {Shunt()    ;}
   fgTestMaker = this;
}
//_____________________________________________________________________________
void StTestMaker::SetNext(StMaker *mk)
{
  fLast=fNext;
  fNext=mk;
}
//_____________________________________________________________________________
void StTestMaker::Print(const char *) const
{
#ifdef STAR_LOGGER    
   if (fLast) { LOG_INFO << Form("%s: Last Maker %s::%s(%p)",
              ClassName(),fLast->ClassName(),fLast->GetName(),(void*)fLast)<< endm;}
   if (fNext) { LOG_INFO << Form("%s: Next Maker %s::%s(%p)",
              ClassName(),fNext->ClassName(),fNext->GetName(),(void*)fNext) << endm;}
#else
   if (fLast) printf("%s: Last Maker %s::%s(%p)\n",
              ClassName(),fLast->ClassName(),fLast->GetName(),(void*)fLast);
   if (fNext) printf("%s: Next Maker %s::%s(%p)\n",
              ClassName(),fNext->ClassName(),fNext->GetName(),(void*)fNext);
#endif      
}

//_____________________________________________________________________________
// $Log: StMaker.cxx,v $
// Revision 1.199  2007/08/27 19:54:41  fisyak
// Just account that only StBFChain has TFile
//
// Revision 1.198  2007/08/24 23:57:24  perev
// More informative err message
//
// Revision 1.197  2007/07/12 19:17:20  fisyak
// Add fTopChain - a pointer to TopChain (for embedding), add method GetMakerInheritsFrom
//
// Revision 1.196  2007/04/26 20:36:49  perev
// Some ChainOpt fixes
//
// Revision 1.194  2007/04/26 03:59:16  perev
// new WhiteBoard methods
//
// Revision 1.193  2007/04/17 05:07:41  perev
// GetTFile()==>StMaker. Jerome request
//
// Revision 1.192  2007/04/13 17:48:11  potekhin
// Added a stub for y2006c
//
// Revision 1.191  2007/03/12 17:51:19  perev
// new signature of GetDataBase()
//
// Revision 1.190  2007/02/22 22:50:18  potekhin
// Added three geometry tags: Y2005F and Y2006B, due to the added dead area in the SSD,
// and also incorporating the updated Barrel EMC code.
//
// Revision 1.189  2007/02/05 20:57:10  potekhin
// a) corrected a few typos in the comments
// b) added y2006a to the list of geometries
// c) created a placeholder for the VMC test tag
//
// Revision 1.188  2007/01/25 06:28:02  fine
// connect Logger and Maker debug levels
//
// Revision 1.187  2006/12/21 23:13:06  potekhin
// Included the upgr12 tag (corrected IGT)
//
// Revision 1.186  2006/12/18 23:36:34  potekhin
// Removed an extraneous CVS tag from top
//
// Revision 1.185  2006/12/18 23:34:58  potekhin
// Adding the tags UPGR10 and UPGR11, recently
// introduced in the geometry.
//
// Revision 1.184  2006/12/14 23:44:09  fisyak
// Add upgr06 and upgr09
//
// Revision 1.183  2006/12/01 17:50:14  jeromel
// upgr08 added
//
// Revision 1.182  2006/11/21 16:34:38  fisyak
// remove geometry pix1 and add upgr07
//
// Revision 1.181  2006/11/03 16:24:56  jeromel
// Oops. hard-coded should be greater than advertized.
//
// Revision 1.180  2006/11/03 15:06:56  jeromel
// Added y2007 placeholder - timestamp for new run will start at 20061101 (simu)
//
// Revision 1.179  2006/10/09 19:39:52  fisyak
// Add geometry y2005e
//
// Revision 1.178  2006/10/04 18:51:25  fisyak
// Add new geometry tags: upgr04 and upgr04, remove rference to xdf
//
// Revision 1.177  2006/08/07 22:44:38  fisyak
// Assert => R__ASSERT for ROOT 5.12
//
// Revision 1.176  2006/05/08 15:15:37  jeromel
// upgr03
//
// Revision 1.175  2006/03/10 00:09:01  jeromel
// 2 options did not have the proper number of elements (unlikely worked / no-one used upgr02 for sure) + minor cosmetics
//
// Revision 1.174  2006/01/31 21:11:01  fisyak
// Add y2006,upgr01 and upgr02
//
// Revision 1.173  2005/12/18 23:17:02  perev
// uint attributes fix
//
// Revision 1.172  2005/12/07 18:56:16  perev
// PrintAttr() method added
//
// Revision 1.171  2005/11/22 21:37:04  fisyak
// add more Simu time stamps (reflecting new SVT), and clean up
//
// Revision 1.170  2005/10/06 18:55:45  fisyak
// Add all used simulation time stamps and geometries
//
// Revision 1.169  2005/09/09 21:32:32  perev
// ERROR message ==> INFO
//
// Revision 1.168  2005/08/29 21:42:21  fisyak
// switch from fBits to fStatus for StMaker control bits
//
// Revision 1.167  2005/07/18 19:04:53  fine
// get rid of an unvisible redundant blank after end of like. Caused ICC compilatiion error
//
// Revision 1.166  2005/06/13 03:03:43  fine
// fix cpp macro to save/restore maker logger
//
// Revision 1.165  2005/04/10 20:38:35  jeromel
// TimeStamp now corrected. Hopefully,
// http://www.star.bnl.gov/STAR/comp/prod/MCGeometry.html#Year5
// should reflect all timestamps correctely.
//
// Revision 1.164  2005/04/10 20:32:27  jeromel
// Expanded geo (several missing in the past / doc not accurate)
//
// Revision 1.163  2005/03/09 23:42:26  perev
// Clear() removed from Finish()
//
// Revision 1.162  2005/02/05 00:56:20  perev
// More tests for second call Finish()
//
// Revision 1.161  2004/11/16 17:48:10  fine
// fixed the doPs method printout
//
// Revision 1.160  2004/11/13 00:28:57  fine
// move the logger instantiation away of the ctor to be able to get the csubclass name
//
// Revision 1.159  2004/11/04 22:26:38  fine
// populate the package with save/restore the logger and edit some messages
//
// Revision 1.158  2004/11/03 22:30:12  fine
// Instantiate the logger per maker and clean up
//
// Revision 1.157  2004/11/03 16:41:21  fine
// add new logger invocation (optional)
//
// Revision 1.156  2004/11/02 02:11:15  jeromel
// Updated aliases for y2005 (note 5 days offset comparing to advertized value for breathing room margin)
//
// Revision 1.155  2004/09/07 18:42:19  fisyak
// Make icc happy
//
// Revision 1.154  2004/09/03 20:33:31  perev
// Attributes, cleanup
//
// Revision 1.153  2004/09/03 00:05:48  jeromel
// Comment block oxygenized, removed unused var
//
// Revision 1.152  2004/09/01 22:09:51  perev
// new methods SetAttr and IAttr,DAttr,SAttr added
//
// Revision 1.151  2004/08/03 00:49:03  perev
// bug fix, wrong maker for dops name
//
// Revision 1.150  2004/08/02 19:44:14  perev
// Bug fix, doPs Clear was not called
//
// Revision 1.149  2004/07/23 17:06:18  perev
// AliasDate & AliasTime moved fro db maker to StMaker
//
// Revision 1.148  2004/04/26 00:07:12  perev
// RetCodeAsString(kode) added. String form of STAR return codes
//
// Revision 1.147  2004/04/15 16:05:28  fine
// Add extra data-mmeber and method for the coming STAR logger
//
// Revision 1.146  2004/04/15 00:21:32  perev
// SetDateTime(int,int) added
//
// Revision 1.145  2004/04/09 21:10:20  jeromel
// Print Info only in debug mode
//
// Revision 1.144  2004/04/09 01:59:00  jeromel
// Bug fix.
//
// Revision 1.143  2004/04/08 21:32:41  perev
// MemStat improving
//
// Revision 1.142  2004/04/07 18:16:10  perev
// MemStat for Make bug fixed
//
// Revision 1.141  2004/03/15 23:57:01  jeromel
// Protect against NULL
//
// Revision 1.140  2004/02/17 19:53:14  perev
// Make more robust
//
// Revision 1.139  2004/01/28 04:37:26  perev
// Print of new Run added
//
// Revision 1.138  2004/01/26 22:47:26  perev
// Account stage (init,make,..)
//
// Revision 1.137  2004/01/14 22:33:12  fisyak
// restore built time
//
// Revision 1.136  2003/11/17 22:19:20  perev
// count memory only after 3 events, to avoid non event memory
//
// Revision 1.135  2003/11/13 02:54:34  perev
// Safe destructor of TDataSet like object added
//
// Revision 1.134  2003/11/05 19:56:32  perev
// Simple debugging class added
//
// Revision 1.133  2003/10/07 00:22:30  perev
// PrintInfo simplified
//
// Revision 1.132  2003/09/28 21:12:45  jeromel
// Unused var NT removed/commented
//
// Revision 1.131  2003/09/02 17:55:29  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.130  2003/07/03 19:40:14  perev
// Cleanup prints in Finish
//
// Revision 1.129  2003/07/01 16:59:16  perev
// error codes for Maker added
//
// Revision 1.128  2003/06/23 23:43:39  perev
// InitRun called even if no run at all
//
// Revision 1.127  2003/05/01 16:56:50  jeromel
// Extraneous declaration removed
//
// Revision 1.126  2003/04/30 20:36:23  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.125  2002/04/28 00:53:42  jeromel
// More doc added ...
//
// Revision 1.124  2002/04/14 21:51:12  perev
// Obsolete StBroadcast
//
// Revision 1.123  2002/03/12 21:19:00  fisyak
// Set only one StEvtHddr as default option (due to Embedding)
//
// Revision 1.122  2002/02/22 21:16:21  perev
// new method NotifyMe
//
// Revision 1.121  2002/02/02 23:31:14  jeromel
// doxygenized. Added some text for the Make() method.
//
// Revision 1.120  2001/11/18 00:58:07  perev
// Broadcast method added
//
// Revision 1.119  2001/10/13 20:23:45  perev
// SetFlavor  working before and after Init()
//
// Revision 1.118  2001/08/14 16:42:48  perev
// InitRun call improved
//
// Revision 1.117  2001/06/05 22:04:47  perev
// Summary only on top
//
// Revision 1.116  2001/06/01 02:47:31  perev
// Memory consumption measurement added
//
// Revision 1.115  2001/05/31 02:40:29  perev
// const(ing)
//
// Revision 1.114  2001/05/10 17:43:20  perev
// Defence against saving maker added
//
// Revision 1.113  2001/05/10 17:33:20  perev
// Defence against saving maker added
//
// Revision 1.112  2001/05/04 19:15:40  perev
// Fatal() -> FatalErr()
//
// Revision 1.111  2001/04/14 01:55:39  perev
// Reverse iter for Clear() and Finish()
//
// Revision 1.110  2001/04/12 22:23:22  perev
// Small bug fixed (fine found)
//
// Revision 1.109  2001/03/02 16:54:44  perev
// doPs fix
//
// Revision 1.108  2001/03/01 02:08:02  perev
// StMem into doPs
//
// Revision 1.107  2001/01/23 22:02:51  fine
// warning message has been re-introduced
//
// Revision 1.106  2000/11/25 18:59:48  fisyak
// Add warning for failed Initialization
//
// Revision 1.105  2000/08/07 22:41:37  perev
// remove redundant print in case of error
//
// Revision 1.104  2000/08/04 21:03:38  perev
// Leaks + Clear() cleanup
//
// Revision 1.103  2000/07/30 01:39:04  perev
// StMem::Print added
//
// Revision 1.102  2000/07/27 19:05:34  perev
// Small memleak in StMakerIter fixed, thanx Akio
//
// Revision 1.101  2000/07/21 21:54:43  fisyak
// Respore lost ps after memory leak correction
//
// Revision 1.100  2000/07/14 01:52:19  perev
// SetIvent called in SetNumber
//
// Revision 1.99  2000/07/04 02:36:01  perev
// AddMaker method added & gStChain removed
//
// Revision 1.98  2000/07/01 00:17:37  fisyak
// Remove memory leak
//
// Revision 1.97  2000/06/21 23:59:24  perev
// getDataBase loop over makers added
//
// Revision 1.96  2000/06/21 21:12:39  perev
// StMakerIter class added
//
// Revision 1.95  2000/06/09 22:12:29  fisyak
// Reduce level of noise
//
// Revision 1.94  2000/05/30 21:04:41  fine
// Fix typo in the ReAllocate message
//
// Revision 1.93  2000/05/20 01:17:54  perev
// NaN added
//
// Revision 1.92  2000/05/20 01:11:07  perev
// IventNumber and BfcStatus added
//
// Revision 1.91  2000/04/20 14:25:17  perev
// Minor simplification
//
// Revision 1.90  2000/04/13 02:53:35  perev
// StMaker::GetValidity added
//
// Revision 1.89  2000/04/07 15:41:42  perev
// Printout error codes improved
//
// Revision 1.88  2000/04/05 02:45:13  fine
// call-counter has been added
//
// Revision 1.87  2000/04/03 23:46:48  perev
// Increased error check
//
// Revision 1.86  2000/03/23 00:15:22  fine
// Adjusted to libSTAR for ROOT 2.24
//
// Revision 1.85  2000/03/01 22:56:25  fisyak
// Adjust ps for RedHat 6.1
//
// Revision 1.84  2000/01/07 22:31:43  perev
// one more argument for SetOutputAll
//
// Revision 1.83  2000/01/04 17:27:06  perev
// Use timestamp instead of current one
//
// Revision 1.82  1999/12/28 21:23:22  fine
// StChain::MakeDoc corrections
//
// Revision 1.81  1999/12/22 16:22:45  fine
// MakeIndex for html doc introduced. Thankls Art
//
// Revision 1.80  1999/12/06 01:57:30  fine
// Time statistic fixed
//
// Revision 1.77  1999/12/01 22:56:30  perev
// .runco directory & AddRunco method introduced
//
// Revision 1.76  1999/11/19 21:02:11  didenko
// valeri's changes
//
// Revision 1.75  1999/10/19 03:23:55  fine
// Some new comments
//
// Revision 1.74  1999/09/24 16:32:40  fisyak
// add return for Init/Finish Run
//
// Revision 1.73  1999/09/24 14:51:51  fisyak
// Add implementation for InitRun/FinishRun
//
// Revision 1.72  1999/09/23 21:24:57  perev
// recovered debug level init(lost)
//
// Revision 1.71  1999/09/21 15:05:17  perev
// InitRun & FinishRun added
//
// Revision 1.70  1999/09/14 17:30:37  fine
// some clean ups
//
// Revision 1.69  1999/09/13 23:22:53  fine
// improved version of MakeDoc with MakeAssociatedClassList function
//
// Revision 1.68  1999/09/13 16:39:24  fine
// MakeDoc ExpandPath removed to keep path short
//
// Revision 1.67  1999/09/13 13:30:46  fine
// non-active new method MakeAssociatedClassList to be introduced new release
//
// Revision 1.66  1999/09/12 16:54:50  fine
// StMaker::MakeDoc() adjusted to multi-level makers. Some bug fix also
//
// Revision 1.65  1999/09/12 15:02:53  fine
// Multi-level maker source dirs introduced for MakeDoc method
//
// Revision 1.64  1999/09/12 01:42:14  fine
// StMAker::MakeDoc has been adjusted to the new source tree
//
// Revision 1.63  1999/09/08 00:13:35  fisyak
// Add static *GetChain()
//
// Revision 1.62  1999/09/03 23:11:48  perev
// Add .runcont directory
//
// Revision 1.61  1999/09/02 22:27:11  fisyak
// Add SetDEBUG
//
// Revision 1.60  1999/08/06 13:01:37  fisyak
// Add Active flag
//
// Revision 1.59  1999/07/29 01:05:23  fisyak
// move bfc to StBFChain
//
// Revision 1.58  1999/07/17 23:29:22  fisyak
// Add Peter Jacobs QAInfo tag in printout
//
// Revision 1.57  1999/07/17 19:08:45  perev
// StMemoryInfo added
//
// Revision 1.55  1999/07/15 13:56:47  perev
// cleanup
//
// Revision 1.54  1999/07/13 02:19:34  perev
// GetCVS,StEvtHddr,etc...
//
// Revision 1.53  1999/07/12 02:33:09  perev
// Add SetMode
//
// Revision 1.52  1999/07/11 21:04:06  fisyak
// Clash resolion
//
// Revision 1.51  1999/07/11 20:40:35  perev
// Move Clear from StChain to StMaker
//
// Revision 1.50  1999/07/11 01:59:04  perev
// add GetCVSTag again
//
// Revision 1.49  1999/07/11 01:33:33  fine
// makedoc some corrections for MakeDoc
//
// Revision 1.48  1999/07/09 22:00:22  perev
// GetCVS into StMaker
//
// Revision 1.47  1999/06/11 23:45:31  perev
// cleanup
//
// Revision 1.46  1999/06/11 22:56:03  perev
// Merge 2 updates
//
// Revision 1.45  1999/06/11 21:50:47  perev
// garb->Delete()
//
// Revision 1.44  1999/06/11 17:45:57  perev
// assert StMaker::Streamer to forbid to write it
//
// Revision 1.43  1999/05/23 04:05:02  fine
// The lost since 1.35 Wed Mar 10 20:23:58 timer functions have been re-introduced
//
// Revision 1.42  1999/05/23 03:25:07  perev
// Start & Stop Timer instead of benchmark
//
// Revision 1.41  1999/05/22 17:50:18  perev
// StMaker::EndMaker ps added
//
// Revision 1.40  1999/05/13 20:56:50  perev
// Supress too much warnings
//
// Revision 1.39  1999/05/10 17:16:44  perev
// AddHist typo
//
// Revision 1.38  1999/05/10 15:37:51  perev
// Save of hisogramm in StMaker::Init
//
// Revision 1.37  1999/05/07 20:51:31  perev
// AddData bug fix
//
// Revision 1.36  1999/05/07 15:46:10  perev
// Added test for the same object into AddObj
//
// Revision 1.35  1999/05/06 22:15:32  perev
// fix objLast should not be included in StMaker::Init
//
// Revision 1.34  1999/05/06 21:27:10  perev
// StMaker remove his from hdirectory
//
// Revision 1.33  1999/05/06 00:47:43  fine
// maker's histogram is removed from the ROOT system gDirectory
//
// Revision 1.32  1999/05/06 00:23:45  fine
// StMaker::MakeDoc some extra comments have been introduced
//
// Revision 1.31  1999/05/06 00:19:04  fine
// StMaker::MakeDoc method has been re-introduced for the 3d time
//
// Revision 1.30  1999/05/05 16:23:07  perev
// add recreation of m_DataSet to keep old codes
//
// Revision 1.29  1999/05/03 22:29:28  perev
// Bug in GetDataSet fix. Thanks to Bill Love
//
// Revision 1.28  1999/05/01 00:53:38  perev
// GetDataSet bug fix NAME == NAME/.data
//
// Revision 1.26  1999/04/30 14:58:41  perev
//  cd() added to StMaker class
//
// Revision 1.25  1999/04/16 14:22:00  fisyak
// replace break in Makers loop from ==kStErr to >kStWarn to account EOF
//
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
