// 
//  
//
#ifndef STAR_St_db_Maker
#define STAR_St_db_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_db_Maker virtual base class for Maker                          //
//                                                                      //
// This class is C++ implementation of the Begin_html <a href="http://www.rhic.bnl.gov/afs/rhic/star/doc/www/packages_l/pro/pams/db/sdb/doc/index.html">Simple Database Manager</a> End_html    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif 

#include "TDatime.h"

class St_FileSet;
class St_DataSet;
class TList;
class TBrowser;

enum DBConst {kMinTime = 19950101, kMaxTime = 20380101};


class St_db_Maker : public StMaker {
private:

  Bool_t drawinit;
  TObjArray  *m_DBList;        // List of the nodes containing the active validation tables
  
  TString     m_MainDir;       // The main root directory for the calibrarion data
  TString     m_UserDir;       // The user root directory for the calibrarion data
  TString     m_CurrentDir;    // The current root directory for the calibrarion data

//  static Char_t m_VersionCVS = "$Id: St_db_Maker.h,v 1.2 1999/03/11 01:32:56 perev Exp $";
 protected:
 public: 
                   St_db_Maker(const char *name,const char *maindir,const char *userdir=0);
   virtual        ~St_db_Maker();
   virtual void    Browse(TBrowser *b);
   virtual TString GetMainDir(){ return m_MainDir;}
   virtual TString GetUserDir(){ return m_UserDir;}
   virtual Int_t   Init();
   virtual Int_t   Make(){return kStOK;};                                                               // *MENU*
   virtual void    PrintInfo();
   virtual void    SetMainDir(const Char_t *db);
   virtual void    SetUserDir(const Char_t *db);
   virtual St_DataSet* UpdateDB (St_DataSet* ds);
   static EDataSetPass UpdateDB (St_DataSet* ds,void *user );
   static EDataSetPass PrepareDB(St_DataSet* ds,void *user );
   static TDatime  Time(const char *filename);
   static int      Kind(const char *filename);
   virtual void Clear(Option_t *opt=""){if(opt){/*unused*/}};
   ClassDef(St_db_Maker, 0)   //StAF chain virtual base class for Makers
};

#endif
