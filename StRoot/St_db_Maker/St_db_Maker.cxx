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
#include "St_db_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "St_XDFFile.h"

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

   m_MainDir = maindir;
   if (userdir) m_UserDir=userdir;
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
   fileset->Purge(); ;fileset->Pass(PrepareDB,&m_CurrentDir);
   St_FileSet *Fileset = fileset;

   m_CurrentDir = m_UserDir; fileset = 0;
   if (!m_CurrentDir.IsNull()) {
     fileset = new St_FileSet(m_CurrentDir);
     fileset->Purge();fileset->Pass(PrepareDB,&m_CurrentDir);}

   if (fileset) {Fileset->Update(fileset); delete fileset;}

   Fileset->Sort();

   AddData(Fileset);
   if (Debug()) Fileset->ls("*");
// Create Histograms    
   return StMaker::Init();
}


//_____________________________________________________________________________
TDatime St_db_Maker::Time(const char *filename)
{
   int lfilename,lname,idate,itime;
   
   TDatime time; time.Set(kMaxTime,0);
   
   lfilename = strlen(filename);
   lname = strcspn(filename,".");
   if (lname+2>lfilename) return time;
   
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
    time.Set(idate,itime); 
    return time;

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
    if (!Kind(filename))			continue;
    set->SetTitle(newTitle);
    lpsname = dot - filename;
    if (strncmp(filename,psname+1,lpsname+1)) {// make new pseudo directory
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
  printf("* $Id: St_db_Maker.cxx,v 1.3 1999/03/11 01:32:56 perev Exp $\n");
  printf("***************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

