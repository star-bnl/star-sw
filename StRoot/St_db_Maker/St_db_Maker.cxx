//*-- Author :    Valery Fine(fine@bnl.gov)   10/08/98 
// 
// 
// 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_db_Maker class for Makers                                      //
//                                                                      //
// This class is C++ implementation of the Begin_html <a href="http://www.rhic.bnl.gov/afs/rhic/star/doc/www/packages_l/pro/pams/db/sdb/doc/index.html">Simple Database Manager</a> End_html    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "TBrowser.h"
#include "TDatime.h"
#include "TInterpreter.h"
#include "TSystem.h"
#include "TRegexp.h"
#include "St_db_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "St_XDFFile.h"

static Int_t AliasDate(const char *alias);
static Int_t AliasTime(const char *alias);
static const char *aliases[]={"sd97",   "sd98",   "year_1a","year_1b","year_1c"
			     ,"es99",   "er99"
                             ,"year_1d","year_1e","year_2a","nofield",      0};   

static const int   dates[]=  {19970101, 19980101, 19990101, 19990500, 19991001
			     ,19990615, 19990616
                             ,19991101, 19991201, 20000101, 1996010,        0};
static const int   times[]=  {       0,        0,        0,        0,        0
			     ,       0,   120000
                             ,       0,        0,        0,        0,        0};

//_________________________ class St_Validity ____________________________________
class St_ValiSet : public St_DataSet{
public:
   TDatime fTimeMin;
   TDatime fTimeMax;
   St_DataSet *fDat;
   St_ValiSet(const char *name,St_DataSet *parent);
   virtual ~St_ValiSet(){};
   virtual void ls(Int_t lev=1);
};

//_____________________________________________________________________________
St_ValiSet::St_ValiSet(const char *name,St_DataSet *parent): St_DataSet(name,parent)
{
  SetTitle(".Val");
  fTimeMin.Set(kMaxTime,0);
  fTimeMax.Set(kMinTime,0);
  fDat =0;
}

//_____________________________________________________________________________
void St_ValiSet::ls(Int_t lev)
{
  printf("  %s.Validity = %s ",GetName(),fTimeMin.AsString());
  printf(" <-> %s\n",     fTimeMax.AsString());
  if (fDat) printf("  Contains DataSet %s\n",fDat->GetName());
  St_DataSet::ls(lev);
}

//__________________________ class St_db_Maker  ____________________________
ClassImp(St_db_Maker)
//_____________________________________________________________________________
St_db_Maker::St_db_Maker(const char *name, const char *maindir,const char *userdir)
:StMaker(name)
{

   fIsDBTime = 0;
   m_MainDir = maindir;
   if (userdir) m_UserDir=userdir;
   m_DataBase = 0;
}
//_____________________________________________________________________________
St_db_Maker::~St_db_Maker(){
}
//______________________________________________________________________________
void St_db_Maker::Browse(TBrowser *b)
{
  StMaker::Browse(b);
}
//_____________________________________________________________________________
Int_t St_db_Maker::Init()
{
   TString fullpath,topdir;

   // recreate a memory resided data-structure
   m_CurrentDir = m_MainDir;
   St_FileSet *fileset = new St_FileSet(m_CurrentDir);
   fileset->Purge(); 
   fileset->Sort(); 
   fileset->Pass(PrepareDB,&m_CurrentDir);
   fileset->Purge(); 
   St_FileSet *Fileset = fileset;

   m_CurrentDir = m_UserDir; fileset = 0;
   if (!m_CurrentDir.IsNull()) {
     fileset = new St_FileSet(m_CurrentDir);
     fileset->Purge();
     fileset->Sort(); 
     fileset->Pass(PrepareDB,&m_CurrentDir);
     fileset->Purge();}

   if (fileset) {Fileset->Update(fileset); delete fileset;}

   Fileset->Sort();

   AddData(Fileset);
   m_DataBase = Fileset;
   SetOutput(Fileset); //  
// SetOutputAll(Fileset); //  

   if (Debug()) Fileset->ls("*");
   OnOff();
   return 0;
}
//_____________________________________________________________________________
TDatime St_db_Maker::Time(const char *filename)
{
  int lfilename,lname,idate,itime;

  TDatime time; time.Set(kMaxTime,0);

  lfilename = strlen(filename);
  lname = strcspn(filename,".");
  if (lname+2>lfilename) return time;
  idate = ::AliasDate(filename+lname+1);
  itime = ::AliasTime(filename+lname+1);

  if (idate) { time.Set(idate,itime);return time;}

  if (lname+20 <= lfilename    &&
      filename[lname+1 ]=='.'  && 
      filename[lname+9 ]=='.'  && 
      filename[lname+18]=='.'  ) {// file name format:  <name>.YYYYMMDD.hhmmss.<ext>
       idate  = atoi(filename+lname+ 2); 
       itime  = atoi(filename+lname+11);
   } else {			   // file name format:  <name>.<ext>
       idate = kMinTime;
       itime = 0;
   }
   time.Set(idate,itime); return time;

}

int St_db_Maker::Kind(const char *filename)
{
   int lfilename;
   
   lfilename = strlen(filename);
   if (!strcmp(filename+lfilename-4,".xdf" )) return 1;
   if (!strcmp(filename+lfilename-2,".C"   )) return 2;
   if (!strcmp(filename+lfilename-2,".c"   )) return 2;
   if (!strcmp(filename+lfilename-5,".root")) return 3;
   return 0;
}
//_____________________________________________________________________________
St_DataSet *St_db_Maker::UpdateDB(St_DataSet* ds)
{ 
  if(!ds) return 0;
  ds->Pass(UpdateDB,this);
  return ds;
}
//_____________________________________________________________________________
EDataSetPass St_db_Maker::UpdateDB(St_DataSet* ds,void *user )
{
  St_DataSet *set,*newdat,*bak,*ps;
  St_ValiSet *val;
  const char *filename;
  TDatime timeMin,timeMax;
   
  TList *list = ds->GetList();
  if (!list) 				return kContinue;
  if (strcmp(".Val",ds->GetTitle()))	return kContinue;
//
//	It is our place.
  val = (St_ValiSet*)ds;    
  St_db_Maker *mk = (St_db_Maker*)user;    
  UInt_t uevent = mk->GetDateTime().Get();

// 		Check validity
    if (val->fTimeMin.Get() <= uevent 
     && val->fTimeMax.Get() >  uevent) return kPrune;
    if (val->fDat) delete val->fDat; val->fDat=0;

//	Start loop
  bak = 0;  timeMin.Set(950101,0);
  TListIter next(list);
  while ((set = (St_DataSet*)next())) {
    filename = set->GetName();
    timeMax = St_db_Maker::Time(filename);
    if (uevent < timeMax.Get()) break;
    bak = set; timeMin=timeMax;
  }   


  if (!set) timeMax.Set(kMaxTime,0);
  val->fTimeMin=timeMin;
  val->fTimeMax=timeMax;
  if (!bak) return kContinue;

  TString dbfile = bak->GetTitle()+5;

  ps = ds->GetParent();
  dbfile += strchr(strstr(ps->Path(),"/.data/")+7,'/');
  dbfile += "/"; dbfile += bak->GetName();
  gSystem->ExpandPathName(dbfile);
  newdat = 0;
  TString command; 
  switch (mk->Kind(bak->GetName())) {
  
    case 1: // .xdf file
    newdat = St_XDFFile::GetXdFile(dbfile);assert (newdat);
    break;

    case 2: // .C file

    command = ".L "; command += dbfile;
    gInterpreter->ProcessLine(command);
    newdat = (St_DataSet *) gInterpreter->Calc("CreateTable()");
    command.ReplaceAll(".L ",".U "); 
    gInterpreter->ProcessLine(command);
    break;
    
    default: assert(0);
  }
  val->fDat = newdat;
  ds->GetParent()->AddFirst(newdat);
  
  return kPrune;  
}
//_____________________________________________________________________________
EDataSetPass St_db_Maker::PrepareDB(St_DataSet* ds, void *user)
{
  St_DataSet *set;
  St_ValiSet *pseudo;
  const char *dsname,*filename,*dot;     
  char psname[100];
  int ldsname,lpsname;
  
  TList *list = ds->GetList();
  if (!list) return kContinue;
  if (strcmp("directory",ds->GetTitle())) return kPrune;
  dsname = ds->GetName(); ;
  if (!strcmp("CVS",dsname)) { delete ds; return kPrune;}
  ldsname= strlen(dsname);

  TString newTitle = "file ";
  newTitle += *((TString*)user);

//	Start loop
  pseudo = 0; psname[0]='.'; psname[1]=0;
  TListIter next(list);
  while ((set = (St_DataSet*)next())) {
    filename = set->GetTitle();
    if (strncmp("file",filename,4))		continue;  
    filename = set->GetName();
    if (!(dot = strchr(filename,'.'))) 		continue;
    if (!Kind(filename)){ delete set;		continue;}
    set->SetTitle(newTitle);
    lpsname = dot - filename;
    if (strncmp(filename,psname+1,lpsname)) {// make new pseudo directory
      psname[1]=0; strncat(psname,filename,lpsname);
      pseudo = new St_ValiSet(psname,ds); strcat(psname,".");}

    set->Shunt(pseudo);}   
  return kContinue;
}
//_____________________________________________________________________________
void    St_db_Maker::SetMainDir(const Char_t *db)
{m_MainDir = db; gSystem->ExpandPathName(m_MainDir);}
//_____________________________________________________________________________
void    St_db_Maker::SetUserDir(const Char_t *db)
{m_UserDir = db; gSystem->ExpandPathName(m_UserDir);}
//_____________________________________________________________________________
void St_db_Maker::PrintInfo(){
  printf("***************************************************************\n");
  printf("* $Id: St_db_Maker.cxx,v 1.8 1999/07/08 19:12:22 fisyak Exp $\n");
  printf("***************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
TDatime St_db_Maker::GetDateTime()
{ 
  if (!fIsDBTime) return StMaker::GetDateTime();
  return fDBTime;
}
//_____________________________________________________________________________
void St_db_Maker::SetDateTime(Int_t idat,Int_t itim)
{ 
  fIsDBTime=1;
  fDBTime.Set(idat,itim);
}
//_____________________________________________________________________________
void   St_db_Maker::SetDateTime(const char *alias)
{ 
  fIsDBTime=1;
  int idat = AliasDate(alias);// <name>.YYYYMMDD.hhmmss.<ext>
  int itim = AliasTime(alias);
  assert(idat);
  fDBTime.Set(idat,itim);
}
//_____________________________________________________________________________
void   St_db_Maker::SetOn(const char *path)
{ AddAlias("On" ,path,".onoff"); OnOff();}
//_____________________________________________________________________________
void   St_db_Maker::SetOff(const char *path)
{ AddAlias("Off",path,".onoff"); OnOff();}
//_____________________________________________________________________________
void St_db_Maker::OnOff()
{
  int Off,len;
  if (!m_DataBase) return;
  St_DataSet *onoff = Find(".onoff");
  if (!onoff) return;
  
  TString tsBase,tsDir,tsTit;
  St_DataSet *ono;  
  St_DataSetIter onoffNext(onoff);
  while((ono=onoffNext())) {// loop onoffs
    Off = (strcmp("Off",ono->GetName())==0);
    tsDir  = gSystem->DirName(ono->GetTitle());
    tsBase = gSystem->BaseName(ono->GetTitle());
    TRegexp rex(tsBase,1);
    St_DataSet *dsDir = GetDataSet(tsDir);
    if (!dsDir) continue;
    if (GetMaker(dsDir) != this) continue;
    St_DataSetIter nextVal(dsDir);
    St_DataSet *val;
    while ((val=nextVal())) {//loop over val's  
      const char *name = val->GetName();
      if(name[0]!='.') 				continue;
      tsTit = val->GetTitle();
      int ival = tsTit.Index(".Val");
      if (ival<0) 				continue;		
      if (Off != (ival==0))			continue;
      if (rex.Index(name+1,&len)!=0)		continue;
      if ( Off) tsTit.Replace(0,0,".Off");
      if (!Off) tsTit.Replace(0,4,""    );
      val->SetTitle(tsTit);
      printf("<%s::Set%s>   %s/%s\n"
      ,ClassName(),ono->GetName(),(const char*)tsDir,val->GetName()+1);
    }//end loop over val's 
  }// end loop onoffs
}
//_____________________________________________________________________________
static Int_t AliasDate(const char *alias)

{

  int n = strcspn(alias," ."); if (n<4) return 0;
  int i;
  for (i=0;aliases[i] && strncmp(alias,aliases[i],n);i++) {} 
  return dates[i];
}
//_____________________________________________________________________________
static Int_t AliasTime(const char *alias)

{

  int n = strcspn(alias," ."); if (n<4) return 0;
  int i;
  for (i=0;aliases[i] && strncmp(alias,aliases[i],n);i++) {} 
  return times[i];
}
