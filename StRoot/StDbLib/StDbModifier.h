/***************************************************************************
 *
 * $Id: StDbModifier.h,v 1.2 2007/08/20 18:21:29 deph Exp $
 *
 * Author: Masashi Kaneta, updated by R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Class to act between Root-Cint files and database
 *
 ***************************************************************************
 *
 * $Log: StDbModifier.h,v $
 * Revision 1.2  2007/08/20 18:21:29  deph
 * New Version of Load Balancer
 *
 * Revision 1.1  2000/08/15 22:51:52  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 **************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StDbModifier
#define STAR_StDbModifier

#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbDefs.hh"

class StDbModifier
{

 private:
   char*   fDbName;         // Database bame
   Int_t   fDebug;          // Debug flag 
   char*   fTableName;      // c-structure name that is same as table in database
   unsigned int funixTime;  // unix timestamp
   char*   fTimestamp;      // Timestamp of the data requested
   char*   fVersionName;    // version name of table
   char*   fOutputFileName; // file name for output
   char*   fInputFileName;  // file name for inputt
   char*   fFlavorName;     // flavor name, like 'ofl', 'sim'  
 
 protected:

 public: 
                 StDbModifier();
   virtual      ~StDbModifier();

   virtual Int_t ReadDataFromDB();
   virtual Int_t WriteDataToDB();

   virtual void  SetTime(unsigned int timestamp);
   virtual void  SetDateTime(const char* timestamp);
   virtual void  SetDbName(const char* dbname);
   virtual void  SetFlavor(const char* flavorname);
   virtual void  SetInputFileName(const char* inputfilename);
   virtual void  SetOutputFileName(const char* outputfilename);
   virtual void  SetTableName(const char* tablename);
   virtual void  SetVersionName(const char* versionname);

   virtual void  SetDebug()           { fDebug = 1    ; }
   virtual void  SetDebug(Int_t debug){ fDebug = debug; }

   ClassDef(StDbModifier, 1)

};

#endif
#endif

