// $Id: St_db_Maker.h,v 1.14 2000/06/26 20:58:41 perev Exp $
// $Log: St_db_Maker.h,v $
// Revision 1.14  2000/06/26 20:58:41  perev
// multiple DBs
//
// Revision 1.13  2000/05/20 01:00:43  perev
// SetFlavor() added
//
// Revision 1.12  2000/04/13 02:58:47  perev
// Method Save is added & default CintDB loaded if exists
//
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

  TDataSet    *fDataBase;       //! DB structure
  StDbBroker  *fDBBroker;	//!MySql broker 
  St_dbConfig *fHierarchy; 	//!
  TString     fDirs[10];        //! Array of dirs with DBs
  TString     fFlavor;     	//! Flavor selection
  Int_t       fIsDBTime;	//! flag to use own time stamp
  TDatime     fDBTime;		//! Own DB time stamp
  Int_t       fUpdateMode;	//! 

//  static Char_t fVersionCVS = "$Id: St_db_Maker.h,v 1.14 2000/06/26 20:58:41 perev Exp $";
 protected:
 public: 
                   St_db_Maker(const char *name
			      ,const char *dir0
			      ,const char *dir1 = ""
			      ,const char *dir2 = ""
			      ,const char *dir3 = ""
                   );
   virtual        ~St_db_Maker();
   virtual const char *GetFlavor(){ return fFlavor;}
   virtual TDatime GetDateTime();
   virtual Int_t   GetValidity(const TTable *tb, TDatime *val) const;
   virtual void    SetDateTime(int idat,int itim);
   virtual void    SetDateTime(const char *datalias);
   virtual Int_t   Init();
   virtual Int_t   Make();
   virtual Int_t   Save(const char *path);
   virtual void    SetOff(const Char_t *path);
   virtual void    SetOn (const Char_t *path);
   virtual void    SetFlavor(const char *flav){fFlavor=flav;}
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
  {static const char cvs[]="Tag $Name:  $ $Id: St_db_Maker.h,v 1.14 2000/06/26 20:58:41 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

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
