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
#include "TSortedList.h"
#include "TDatime.h"
#include "St_db_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "St_XDFFile.h"

//_________________________ class St_Validity ____________________________________
class St_Validity : public TNamed, public TDatime  {
private:
   const St_FileSet *fFile;
   Bool_t      fValid;
   Int_t       fDate;
   Int_t       fTime;
public:
   St_Validity(const St_FileSet *file);
   virtual ~St_Validity();
   Int_t    GetBegin() const { return Get(); }
   const    St_FileSet *GetFile() const { return fFile; }
   Int_t    GetDate(){return fDate;}
   Int_t    GetTime(){return fTime;}
   Bool_t   GetValid(){ return fValid;}
   Bool_t   IsValid(Int_t time){ return  fValid ; }
};

//_____________________________________________________________________________
St_Validity::St_Validity(const St_FileSet *file):fFile(file)
{
// fFile = (St_FileSet *) file;
   fValid = kFALSE;
   Char_t *buffer = 0;
   Char_t *point1 = 0;
   Char_t *point2 = 0;
   buffer = StrDup(fFile->GetName());
   if (point1 = strchr(buffer,'.')) {
    // Check file name format:  <name>.YYYYMMDD.hhmmss or <name>.C
     if (point2 = strchr(point1+1,'.')) {
        const Char_t *date  = point1+1;
        const Char_t *hours = point2+1;
        *point1 = 0;
        *point2 = 0;
        Int_t idate  = atoi(date);
        Int_t ihours = atoi(hours);
        if (idate) {
           Set(idate-1900,ihours);
           fDate = idate;
           fTime = ihours;
           fValid = kTRUE;
        }
     }
     else {
       if (point2 = strchr(point1+1,'C')){
	 fDate = 199820101;
	 fTime = 0;
	 Set(fDate-1900,fTime);
	 fValid = kTRUE;
       }
     }
   }
   if (fValid) { 
      const Text_t *name = (const Char_t *)(fFile->Path());
      SetName(name);
      SetTitle("SDBMList");
   }
   else 
      Error("St_Validity ctor"," Wrong file name %s for the period %s ",fFile->GetName(),AsString());
   if (buffer) delete [] buffer;
}

//_____________________________________________________________________________
St_Validity::~St_Validity(){
}
//_____________________________________________________________________________

//___________________________ class St_DBList _________________________________
class St_DBList : public TSortedList
{
 private:
    TObjLink *m_ActiveLink;  // pointer to the active valid DB object
    TString   m_Name;        // 
 public:
    St_DBList(const Char_t *name=""):m_ActiveLink(0),m_Name(name) {;}
    virtual      ~St_DBList(){}
    TObjLink     *GetActive() const { return m_ActiveLink;}
    const Text_t *GetName() const{ return m_Name;}
    St_DataSet   *GetValidFile(TDatime &time);
    St_DataSet   *GetValidFile(Int_t date, Int_t time);
};

//_____________________________________________________________________________
St_DataSet *St_DBList::GetValidFile(TDatime &time)
{
 Error("GetValidFile(TDatime &time)","Wrong call");
 return 0;
}
//_____________________________________________________________________________
St_DataSet *St_DBList::GetValidFile(Int_t date, Int_t time)
{

  // return the pointer to the new valid dataset
  //        = 0 if there is no valid name yet
  TObjLink *lnk = FirstLink();
  if (!lnk) return 0;

//++  UInt_t validtime = time.Get();
//++   UInt_t settime   = 0;

  Int_t validate   = date;  
  Int_t validtime  = time;

  Int_t setdate    = 0;
  Int_t settime    = 0;

  St_Validity *validfile = 0;
  TObjLink *nxt = 0;
  while (lnk) {
     St_Validity *val = (St_Validity *)lnk->GetObject();         
     if (val) { 
//++             settime = val->Get();
         setdate = val->GetDate();
         settime = val->GetTime();
      }
      if (validate < setdate) break;
      if (validate == setdate && validtime < settime) break;
      lnk = lnk->Next();
  }

  if (!lnk) {
     Error("GetValidFile(...)","no validity for <%s>:%d %d",GetName(),date,time);
     return 0;
  }

  if (lnk->Prev()) lnk = lnk->Prev();

  validfile = (St_Validity *)lnk->GetObject();

  if (m_ActiveLink != lnk  && validfile ) {
    St_DataSet *set = 0;
    const St_FileSet *file = validfile->GetFile();
    if (file) {
      const Char_t *p = 0;
      St_DataSet *parent = file->GetParent();
      if (parent) {
        TString dbfile = "/afs/rhic/star/";
//        dbfile += file->Path();
        dbfile += validfile->GetName();
        p=dbfile;
        set = St_XDFFile::GetXdFile(p);
        if (set) 
          parent->Update(set);
      }
      else 
        Error("GetValidFile"," No parent to update with <%s>",file->GetName());
      m_ActiveLink = lnk;
    }
    return set;  
  }
  return 0;
}

//__________________________ class St_db_Maker  ____________________________
ClassImp(St_db_Maker)

//_____________________________________________________________________________
St_db_Maker::St_db_Maker(const char *name, const char *title,
                               const TString &rootdir):StMaker(name,title)
{
   drawinit    = kFALSE;
   m_DBList    = 0;
   m_ValidTime.Set(19950000,0);
   SetDbDir(rootdir);
}
//_____________________________________________________________________________
St_db_Maker::~St_db_Maker(){
  // SafeDelete(m_DataSet);
   if (m_DBList) {
       m_DBList->Delete();
       delete m_DBList;
       m_DBList = 0;
   }
}
//______________________________________________________________________________
void St_db_Maker::Browse(TBrowser *b)
{
  StMaker::Browse(b);
  if (m_DBList) b->Add(m_DBList,"Simple DataBase Manager");
}
//_____________________________________________________________________________
void St_db_Maker::Clear(Option_t *option){
  // SafeDelete(m_DataSet);
}
//_____________________________________________________________________________
Int_t St_db_Maker::Init()
{
   const Char_t *f = m_RootDbDirectory;

   // recreate a memory resided data-structure
   St_FileSet fileset(f,"db");

   // Create a list of directories holding those "file"s

   fileset.Purge();
   St_DataSetIter next(&fileset,0);
   St_FileSet *dir = 0;
   Int_t depth = 0;
   TList garbage;
   while (  dir = (St_FileSet *)next() ) 
   {
     if ( strcmp(dir->GetTitle(),"file") == 0)
     {
       // Check all "file"'s and 
       // leave those fit the pattern "name.<date>.<time>.C" or "name.C"
       St_Validity *valid = new St_Validity(dir);
       if ( valid->GetValid() ) {
          if (!m_DBList) m_DBList = new TObjArray;
          // Check whether this directory has been registered
          Char_t *fullpath =  StrDup(dir->Path());
          Char_t *point = strrchr(fullpath,'.');
          if (point) { 
             *point = 0;
              point = strrchr(fullpath,'.');
              if (point) {
                 *point = 0;
                 St_DBList *db = (St_DBList *)m_DBList->FindObject(fullpath);
                 if (!db) {
                    db = new St_DBList(fullpath);
                    m_DBList->Add(db);
                 }
                 db->Add(valid);
              }
              else 
                  Error("Init","Path name has no second \".\" %s",(const Char_t *)(dir->Path()));
           }
           else 
                   Error("Init","Path name has no  \".\" %s",(const Char_t *)(dir->Path()));
       }
       else {
          // Collect the "bad" dir's to delete it later
          garbage.Add(dir);
          delete valid;
       }
     }
   }
   // delete the garbage
   garbage.Delete();
   //   gStChain->GetDb()->Update(&fileset);
   m_DataSet->Update(&fileset);
// Create Histograms    
   return StMaker::Init();
}

//_____________________________________________________________________________
Int_t St_db_Maker::Make(){
  if (gStChain->Debug()) PrintInfo();
// Look for the validity

 if (!m_DBList) return 0;

 TIter nextdb(m_DBList);
 St_DBList *db = 0;
 while (db =  (St_DBList *)nextdb()) 
    St_DataSet *set = db->GetValidFile(m_ValidDate,m_ValidHours);
 return kStOK;
}
//_____________________________________________________________________________
void St_db_Maker::PrintInfo(){
  printf("***************************************************************\n");
  printf("* $Id: St_db_Maker.cxx,v 1.1 1999/01/02 19:08:15 fisyak Exp $\n");
  printf("***************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

