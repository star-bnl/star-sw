// $Id: St_db_Maker.h,v 1.10 2000/02/26 01:45:04 fine Exp $
// $Log: St_db_Maker.h,v $
// Revision 1.10  2000/02/26 01:45:04  fine
// CVS id has been introduced
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
#include "StDbBroker/dbConfig.h"

class St_FileSet;
class St_DataSet;
class TList;
class TBrowser;
class StDbBroker;
class St_dbConfig;
class St_ValiSet;

enum DBConst {kMinTime = 19950101, kMaxTime = 20380101};


class St_db_Maker : public StMaker {
private:

  St_DataSet *fDataBase;       	//! DB structure
  StDbBroker *fDBBroker;	//!MySql broker 
  St_dbConfig *fHierarchy; //!
  TString     fMainDir;        	//! The main root directory for the calibrarion data
  TString     fUserDir;        	//! The user root directory for the calibrarion data
  TString     fCurrentDir;     	//! The current root directory for the calibrarion data
  Int_t       fIsDBTime;	//! flag to use owb time stamp
  TDatime     fDBTime;		//! Own DB time stamp
  Int_t       fUpdateMode;	//! 

//  static Char_t fVersionCVS = "$Id: St_db_Maker.h,v 1.10 2000/02/26 01:45:04 fine Exp $";
 protected:
 public: 
                   St_db_Maker(const char *name,const char *maindir,const char *userdir=0);
   virtual        ~St_db_Maker();
   virtual TString GetMainDir(){ return fMainDir;}
   virtual TString GetUserDir(){ return fUserDir;}
   virtual TDatime GetDateTime();
   virtual void    SetDateTime(int idat,int itim);
   virtual void    SetDateTime(const char *datalias);
   virtual Int_t   Init();
   virtual Int_t   Make();
   virtual void    SetMainDir(const Char_t *db);
   virtual void    SetUserDir(const Char_t *db);
   virtual void    SetOff(const Char_t *path);
   virtual void    SetOn (const Char_t *path);
   virtual void    OnOff();
   virtual void Clear(Option_t *opt=""){if(opt){/*unused*/}};

protected:
   virtual St_DataSet* UpdateDB (St_DataSet* ds);
   virtual int UpdateTable(UInt_t parId, St_Table* dat, TDatime val[2]);
   virtual St_DataSet *LoadTable(St_DataSet* left);
   virtual St_DataSet *FindLeft(St_ValiSet *val, TDatime vals[2]);
   virtual St_DataSet *OpenMySQL(const char* dbname);

   static EDataSetPass UpdateDB (St_DataSet* ds,void *user );
   static EDataSetPass PrepareDB(St_DataSet* ds,void *user );
public:
   static TDatime  Time(const char *filename);
   static int      Kind(const char *filename);

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_db_Maker.h,v 1.10 2000/02/26 01:45:04 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_db_Maker, 0)   //StAF chain virtual base class for Makers
};

class St_dbConfig : public St_Table   
{                                          
protected:                                 
  static St_tableDescriptor *fgColDescriptors;    
  virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}       
  virtual void  SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}  
public:                                    
  St_dbConfig() : St_Table("dbConfig",sizeof(dbConfig_st)) {SetType("dbConfig");}           
  St_dbConfig(Text_t *name) : St_Table(name,sizeof(dbConfig_st)) {SetType("dbConfig");}          
  St_dbConfig(Int_t n): St_Table("dbConfig",n,sizeof(dbConfig_st)) {SetType("dbConfig");}   
  St_dbConfig(Text_t *name,Int_t n): St_Table(name,n,sizeof(dbConfig_st)) {SetType("dbConfig");} 
  dbConfig_st *GetTable(){ return (dbConfig_st *)GetArray();}                                            
                                           
  ClassDef(St_dbConfig,0) //  
};                                                            
                                           
#endif
