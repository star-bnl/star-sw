// $Id: St_db_Maker.h,v 1.11 2000/03/23 14:55:56 fine Exp $
// $Log: St_db_Maker.h,v $
// Revision 1.11  2000/03/23 14:55:56  fine
// Adjusted to libSTAR and ROOT 2.24
//
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
#include "TTable.h"                      

#include "TDatime.h"
#include "StDbBroker/dbConfig.h"

class TFileSet;
class TList;
class TBrowser;
class StDbBroker;
class St_dbConfig;
class St_ValiSet;

enum DBConst {kMinTime = 19950101, kMaxTime = 20380101};


class St_db_Maker : public StMaker {
private:

  TDataSet *fDataBase;       	//! DB structure
  StDbBroker *fDBBroker;	//!MySql broker 
  St_dbConfig *fHierarchy; //!
  TString     fMainDir;        	//! The main root directory for the calibrarion data
  TString     fUserDir;        	//! The user root directory for the calibrarion data
  TString     fCurrentDir;     	//! The current root directory for the calibrarion data
  Int_t       fIsDBTime;	//! flag to use owb time stamp
  TDatime     fDBTime;		//! Own DB time stamp
  Int_t       fUpdateMode;	//! 

//  static Char_t fVersionCVS = "$Id: St_db_Maker.h,v 1.11 2000/03/23 14:55:56 fine Exp $";
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
   virtual TDataSet* UpdateDB (TDataSet* ds);
   virtual int UpdateTable(UInt_t parId, TTable* dat, TDatime val[2]);
   virtual TDataSet *LoadTable(TDataSet* left);
   virtual TDataSet *FindLeft(St_ValiSet *val, TDatime vals[2]);
   virtual TDataSet *OpenMySQL(const char* dbname);

   static EDataSetPass UpdateDB (TDataSet* ds,void *user );
   static EDataSetPass PrepareDB(TDataSet* ds,void *user );
public:
   static TDatime  Time(const char *filename);
   static int      Kind(const char *filename);

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_db_Maker.h,v 1.11 2000/03/23 14:55:56 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_db_Maker, 0)   //StAF chain virtual base class for Makers
};

class St_dbConfig : public TTable   
{                                          
protected:                                 
  static TTableDescriptor *fgColDescriptors;    
  virtual TTableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}       
  virtual void  SetDescriptorPointer(TTableDescriptor *list) const { fgColDescriptors = list;}  
public:                                    

  St_dbConfig() : TTable("dbConfig",sizeof(dbConfig_st)) {SetType("dbConfig");}           
  St_dbConfig(Text_t *name) : TTable(name,sizeof(dbConfig_st)) {SetType("dbConfig");}          
  St_dbConfig(Int_t n): TTable("dbConfig",n,sizeof(dbConfig_st)) {SetType("dbConfig");}   
  St_dbConfig(Text_t *name,Int_t n): TTable(name,n,sizeof(dbConfig_st)) {SetType("dbConfig");} 
  dbConfig_st *GetTable(){ return (dbConfig_st *)GetArray();}                                            
                                           
  ClassDef(St_dbConfig,0) //  
};                                                            
                                           
#endif
