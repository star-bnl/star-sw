/***************************************************************************
 *
 * $Id: StDbModifier.cxx,v 1.11 2016/05/25 20:57:11 dmitry Exp $
 *
 * Author: Masashi Kaneta, updated by R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Class to act between Root-Cint files and database
 *
 ***************************************************************************
 *
 * $Log: StDbModifier.cxx,v $
 * Revision 1.11  2016/05/25 20:57:11  dmitry
 * coverity - resource leakage
 *
 * Revision 1.10  2015/05/15 19:56:09  dmitry
 * more cleanup
 *
 * Revision 1.9  2015/05/15 19:47:16  dmitry
 * proper delete added before overwrite
 *
 * Revision 1.8  2015/05/15 18:34:39  dmitry
 * now deallocating memory in destructor of StDbModifier + cleanup
 *
 * Revision 1.7  2007/08/20 18:21:29  deph
 * New Version of Load Balancer
 *
 * Revision 1.6  2007/05/16 22:48:10  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.5  2005/09/07 22:04:02  deph
 * update to correct padding issue for packed tables
 *
 * Revision 1.4  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2002/03/15 22:08:07  porter
 * fixed retval in modifier
 *
 * Revision 1.2  2002/03/13 22:14:53  porter
 * added variable length table defautls for simplifying writes of l3 counters to db
 *
 * Revision 1.1  2000/08/15 22:51:52  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 **************************************************************************/
#ifdef __ROOT__
#include <stdio.h>
#include <stdlib.h>


#include "TROOT.h"
#include "TInterpreter.h"

#ifndef __STDB_STANDALONE__
#include "Stiostream.h"
#include "StMessMgr.h"
#else
#define LOG_DEBUG cout
#define LOG_INFO cout
#define LOG_WARN cout
#define LOG_ERROR cerr
#define LOG_FATAL cerr
#define LOG_QA cout
#define endm "\n"
#endif


#include "TTable.h"

#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbDefs.hh"
#include "StDbTableDescriptor.h"

#include "StDbModifier.h"


ClassImp(StDbModifier)

//_____________________________________________________________________________
  StDbModifier::StDbModifier() : fDbName(0), fDebug(0), fTableName(0), funixTime(0), fTimestamp(0), fVersionName(0),
	fOutputFileName(0), fInputFileName(0), fFlavorName(0)
{
  // constructor of StDbModifier


  //  fTimestamp = new char[30];
  //  strcpy(fTimestamp,"2038-01-01 04:59:59");
  // Timestamp of the data requested.
  // To get recent one, here, it is assigned as end of UNIX time.

  //fDebug = 0;        // set No-debug mode.

  //fDbName    = 0;    // set Database name on DB server  as brank.
  //fTableName = 0;    // set Table name on DB server  as brank.
                     // If either fDbName or fTableName is still 0 
                     // in ReadDataFromBD() and WriteDataToDB(),
                     // the program will be terminated.
  //fVersionName = 0;

  //fFlavorName = 0;

  fOutputFileName = new char[200];
  strcpy(fOutputFileName,"./database_data.C");
  // default name of a file to be filled data from database.

  fInputFileName = new char[200];
  strcpy(fInputFileName,"./database_data.C");
  // default name of a file to be read data from ascii file.
}

//_____________________________________________________________________________
StDbModifier::~StDbModifier()
{
  // destructor of StDbModifier
  delete [] fTimestamp;
  delete [] fDbName;
  delete [] fTableName;
  delete [] fVersionName;
  delete [] fFlavorName;
  delete [] fOutputFileName;
  delete [] fInputFileName;
}

//_____________________________________________________________________________
Int_t StDbModifier::ReadDataFromDB()
{
  // This funciton will connect database server, get informaion of table and
  // make a file that contains data of table in database.

  if( funixTime==0 && !(fTimestamp)) {
    cout<< " No timestamp specified " << endl;
    return 0;
  }

  if ( fDbName == 0 ){
    cout << "  StDbModifier; Set Database name by " << endl;
    cout << "                StDbModifier::setDbName(TString dbname)";
    cout << endl;
    return 0;
  }
  if ( fTableName == 0 ){
    cout << "  StDbModifier; Set table name by " << endl;
    cout << "                StDbModifier::setTableName(TString tablename)";
    cout << endl;
    return 0;
  }

  StDbManager* mgr = StDbManager::Instance();               // Get the singleton manager
  if ( fDebug == 1 ){ 
    bool isVerbose = true;
    mgr->setVerbose(isVerbose);                             // Set verbose mode for debuging for fDebug=1
  }

  StDbConfigNode* configNode = mgr -> initConfig(fDbName);  // Connect to the db & get an empty container

  StDbTable* dbtable;
  if ( fVersionName == 0 ){
    dbtable = configNode -> addDbTable(fTableName);
    // Add a table to the container with descriptor given by Database with wsing version name as "default" 
  }else{
    dbtable = configNode -> addDbTable(fTableName,fVersionName);
    // Add a table to the container with descriptor given by Database with wsing version name specified .
  }
  if ( dbtable == 0 ){                              // If table asigned by fTableName does not exist in Dababase
    cout << " No Table : " << fTableName << endl;   // program is stoped and exit from this function.
    return 0;                                       //
  }

  if ( fFlavorName != 0 ){
    dbtable -> setFlavor(fFlavorName);
    cout << "Flavor is set as " << fFlavorName << " by StDbTable::setFlavor." << endl;
  }else{
    cout << "Flavor is NOT assigned. Default value is set as 'ofl'. " << endl;
    dbtable -> setFlavor("ofl");
  }


  if(funixTime) {
    mgr->setRequestTime(funixTime);
  } else {
    mgr->setRequestTime(fTimestamp);
  }
 
  //  mgr -> setRequestTime(fTimestamp);          // Set the request time
  mgr -> fetchDbTable(dbtable);               // Fetch the data from Database 

  void* cstruct = dbtable -> GetTableCpy();   // Get pointer of table and copy to c-structure
  Int_t   nrows = dbtable -> GetNRows();      // Get number of raws in the table

  TTable* table = TTable::New(fTableName,fTableName,cstruct,nrows);
  // Create new TTable object for c-structure

  ofstream ofs(fOutputFileName);  // Open a file 
  table -> SavePrimitive(ofs,0);  // Write information of c-structure from object of TTable to the file
  ofs.close();                    // Close the file

  return 1;
}


//_____________________________________________________________________________
Int_t StDbModifier::WriteDataToDB()
{
  // This funciton will make a table as object of TTable.
  // Data of the table will be read from a file that is generated from
  // StDbModifier::ReadDataFromDB().
  // After data is read, connection to database server is made and data will
  // be stored in database.


  if( funixTime==0 && !(fTimestamp)) {
    cout<< " No timestamp specified " << endl;
    return 0;
  }

  if ( fDbName == 0 ){
    cout << "  StDbModifier; Set Database name by " << endl;
    cout << "                StDbModifier::setDbName(TString dbname)";
    cout << endl;
    return 0;
  }
  if ( fTableName == 0 ){
    cout << "  StDbModifier; Set table name by " << endl;
    cout << "                StDbModifier::setTableName(TString tablename)";
    cout << endl;
    return 0;
  }

  TString command;                  // Make a string to load Root C macro generated
  command = ".L ";                  // by ReadDataFromDB()
  command += fInputFileName;

  if ( fDebug == 1 ) LOG_INFO<<"   LoadTable: "<<(const char*)command<<endm;

  gInterpreter->ProcessLine(command);                                 // Load the file in CINT
  TTable* table = (TTable*) gInterpreter->Calc("CreateTable()");      // execute comand in the file
  command.ReplaceAll(".L ",".U ");                                    // chage comand .L to .U
  gInterpreter->ProcessLine(command);                                 // unload file from CINT

  void* cstruct = table -> GetArray();      // Get pointer of table and copy to c-structure
  Int_t   nrows = table -> GetNRows();      // Get number of raws in the table
 //MPD - below is need TTable info to correct c-array 8 byte padding issue
 //could have broke it out - but I chose to keep all the TTable (root) stuff together

   Int_t  rowSize  = table -> GetRowSize();   // Get the size (in bytes) of each row - fixes gap/padding problem
//   Int_t nCols  = table -> GetNumberOfColumns();
  
 StDbTableDescriptor* TD = new StDbTableDescriptor();
  TD->storeRowSize(rowSize); 
   //table -> Print(0,2);  //uncomment for debugging



  StDbManager* mgr = StDbManager::Instance();               // Get the singleton manager
  if ( fDebug == 1 ){ 
    bool isVerbose = true;
    mgr->setVerbose(isVerbose);                             // Set verbose mode for debuging for fDebug=1
  }
  StDbConfigNode* configNode = mgr -> initConfig(fDbName);  // Connect to the db & get an empty container

  StDbTable* dbtable;
  if ( fVersionName == 0 ){
    dbtable = configNode -> addDbTable(fTableName);
    // Add a table to the container with descriptor given by Database with wsing version name as "default" 
  }else{
    dbtable = configNode -> addDbTable(fTableName,fVersionName);
    // Add a table to the container with descriptor given by Database with wsing version name specified .
  }

     

  if ( dbtable == 0 ){                            // If table asigned by fTableName does not exist in Dababase,
    cout << " No Table : " << fTableName << endl; // program is stoped and exit from this function.
	delete TD;
    return 0;                                     //
  }

  if ( fFlavorName != 0 ){
    cout << "set Flavor" << endl;
    dbtable -> setFlavor(fFlavorName);
    cout << "Flavor is set as " << fFlavorName << " by StDbTable::setFlavor." << endl;
  }else{
    cout << "Flavor is NOT assigned. Default value is set as 'ofl'. " << endl;
    dbtable -> setFlavor("ofl");
  }

  int dbnrows=dbtable->GetNRows();
  int* eidList=0;
  int tmp;
  int* dbeidList=dbtable->getElementID(tmp);
  if(nrows>dbnrows){
    eidList=new int[nrows];
    int i;
    for(i=0;i<dbnrows;i++)eidList[i]=dbeidList[i];
    for(i=dbnrows;i<nrows;i++)eidList[i]=eidList[dbnrows-1]+i;
  }
      
 
  dbtable -> SetTable((char*)cstruct,nrows,eidList);      // Put data in local table on memory
  //  dbtable -> SetNRows(nrows);                     // Set number of rows on table in database

  //  mgr -> setStoreTime(fTimestamp);          // Set the time stamp 
  if(funixTime) {
    mgr->setStoreTime(funixTime);
  } else {
    mgr->setStoreTime(fTimestamp);
  }

  int retVal=0;
  if(mgr -> storeDbTable(dbtable)) retVal=1;             // Fetch the data 
  delete [] eidList;
  delete TD;

  return retVal;
}

//_____________________________________________________________________________
void StDbModifier::SetDateTime(const char* timestamp)
{
  delete [] fTimestamp;
  fTimestamp = new char[strlen(timestamp)+1];
  strcpy(fTimestamp,timestamp);
}

void StDbModifier::SetTime(unsigned int time){ funixTime=time;}

//_____________________________________________________________________________
void StDbModifier::SetDbName(const char* dbname)
{
  delete [] fDbName;
  fDbName = new char[strlen(dbname)+1];
  strcpy(fDbName,dbname);
}

//_____________________________________________________________________________
void StDbModifier::SetInputFileName(const char* inputfilename)
{
  delete [] fInputFileName;
  fInputFileName = new char[strlen(inputfilename)+1];
  strcpy(fInputFileName,inputfilename);
}

//_____________________________________________________________________________
void StDbModifier::SetOutputFileName(const char* outputfilename)
{
  delete [] fOutputFileName;
  fOutputFileName = new char[strlen(outputfilename)+1];
  strcpy(fOutputFileName,outputfilename);
}

//_____________________________________________________________________________
void StDbModifier::SetTableName(const char* tablename)
{
  delete [] fTableName;
  fTableName = new char[strlen(tablename)+1];
  strcpy(fTableName,tablename);
}

//_____________________________________________________________________________
void StDbModifier::SetVersionName(const char* versionname)
{
  delete [] fVersionName;
  fVersionName = new char[strlen(versionname)+1];
  strcpy(fVersionName,versionname);
}

//_____________________________________________________________________________
void StDbModifier::SetFlavor(const char* flavorname)
{
  delete [] fFlavorName;
  fFlavorName = new char[strlen(flavorname)+1];
  strcpy(fFlavorName,flavorname);
  cout << "       Flavor is set " << flavorname  << endl;
  cout << "       Flavor is set " << fFlavorName << endl;
}

#endif
