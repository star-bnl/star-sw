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
#include "St_Table.h"                      

#include "TDatime.h"

class St_FileSet;
class St_DataSet;
class TList;
class TBrowser;
class StDbBroker;
class St_tables_hierarchy;
class St_ValiSet;

enum DBConst {kMinTime = 19950101, kMaxTime = 20380101};


class St_db_Maker : public StMaker {
private:

  St_DataSet *m_DataBase;       // DB structure
  StDbBroker *m_DBBroker;	//!MySql broker 
  St_tables_hierarchy *m_Hierarchy; //!
  TString     m_MainDir;        // The main root directory for the calibrarion data
  TString     m_UserDir;        // The user root directory for the calibrarion data
  TString     m_CurrentDir;     // The current root directory for the calibrarion data
  Int_t       fIsDBTime;	//! flag to use owb time stamp
  TDatime     fDBTime;		//! Own DB time stamp

//  static Char_t m_VersionCVS = "$Id: St_db_Maker.h,v 1.6 1999/09/14 15:22:37 perev Exp $";
 protected:
 public: 
                   St_db_Maker(const char *name,const char *maindir,const char *userdir=0);
   virtual        ~St_db_Maker();
   virtual TString GetMainDir(){ return m_MainDir;}
   virtual TString GetUserDir(){ return m_UserDir;}
   virtual TDatime GetDateTime();
   virtual void    SetDateTime(int idat,int itim);
   virtual void    SetDateTime(const char *datalias);
   virtual Int_t   Init();
   virtual Int_t   Make(){return kStOK;};      // *MENU*
   virtual void    SetMainDir(const Char_t *db);
   virtual void    SetUserDir(const Char_t *db);
   virtual void    SetOff(const Char_t *path);
   virtual void    SetOn (const Char_t *path);
   virtual void    OnOff();
   virtual void Clear(Option_t *opt=""){if(opt){/*unused*/}};

protected:
   virtual St_DataSet* UpdateDB (St_DataSet* ds);
   virtual int UpdateTable(St_Table* dat, TDatime val[2]);
   virtual St_DataSet *LoadTable(St_DataSet* left);
   virtual St_DataSet *FindLeft(St_ValiSet *val, TDatime vals[2]);
   virtual St_DataSet *OpenMySQL(const char* dbname);

   static EDataSetPass UpdateDB (St_DataSet* ds,void *user );
   static EDataSetPass PrepareDB(St_DataSet* ds,void *user );
public:
   static TDatime  Time(const char *filename);
   static int      Kind(const char *filename);

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_db_Maker.h,v 1.6 1999/09/14 15:22:37 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_db_Maker, 0)   //StAF chain virtual base class for Makers
};
                                           

struct  tables_hierarchy_st {
char tabname[20];
char tabtype[20];
char parname[20];
};                                          
                                           
class St_tables_hierarchy : public St_Table   
{                                          
protected:                                 
  static St_tableDescriptor *fgColDescriptors;    
  virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}       
  virtual void  SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}  
public:                                    
  St_tables_hierarchy() : St_Table("tables_hierarchy",sizeof(tables_hierarchy_st)) {SetType("tables_hierarchy");}           
  St_tables_hierarchy(Text_t *name) : St_Table(name,sizeof(tables_hierarchy_st)) {SetType("tables_hierarchy");}          
  St_tables_hierarchy(Int_t n): St_Table("tables_hierarchy",n,sizeof(tables_hierarchy_st)) {SetType("tables_hierarchy");}   
  St_tables_hierarchy(Text_t *name,Int_t n): St_Table(name,n,sizeof(tables_hierarchy_st)) {SetType("tables_hierarchy");} 
  tables_hierarchy_st *GetTable(){ return (tables_hierarchy_st *)s_Table;}                                            
                                           
  ClassDef(St_tables_hierarchy,0) //  
};                                                            
                                                              

#endif
