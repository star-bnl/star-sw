/***************************************************************************
 *
 * $Id: StDbModifier.cxx,v 1.1 2000/08/15 22:51:52 porter Exp $
 *
 * Author: Masashi Kaneta, updated by R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Class to act between Root-Cint files and database
 *
 ***************************************************************************
 *
 * $Log: StDbModifier.cxx,v $
 * Revision 1.1  2000/08/15 22:51:52  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 **************************************************************************/

#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>

#include "TROOT.h"
#include "TInterpreter.h"

#include "TTable.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbDefs.hh"

#include "StDbModifier.h"


ClassImp(StDbModifier)

//_____________________________________________________________________________
  StDbModifier::StDbModifier() : funixTime(0), fTimestamp(0)
{
  // constructor of StDbModifier


  //  fTimestamp = new char[30];
  //  strcpy(fTimestamp,"2038-01-01 04:59:59");
  // Timestamp of the data requested.
  // To get recent one, here, it is assigned as end of UNIX time.

  fDebug = 0;        // set No-debug mode.

  fDbName    = 0;    // set Database name on DB server  as brank.
  fTableName = 0;    // set Table name on DB server  as brank.
                     // If either fDbName or fTableName is still 0 
                     // in ReadDataFromBD() and WriteDataToDB(),
                     // the program will be terminated.
  fVersionName = 0;

  fFlavorName = 0;

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
  if(fTimestamp) delete [] fTimestamp;

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

  if ( fDebug == 1 ) printf("   LoadTable: %s\n",(const char*)command);

  gInterpreter->ProcessLine(command);                                 // Load the file in CINT
  TTable* table = (TTable*) gInterpreter->Calc("CreateTable()");      // execute comand in the file
  command.ReplaceAll(".L ",".U ");                                    // chage comand .L to .U
  gInterpreter->ProcessLine(command);                                 // unload file from CINT

  void* cstruct = table -> GetArray();      // Get pointer of table and copy to c-structure
  Int_t   nrows = table -> GetNRows();      // Get number of raws in the table

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


  dbtable -> SetTable((char*)cstruct,nrows);      // Put data in local table on memory
  //  dbtable -> SetNRows(nrows);                     // Set number of rows on table in database

  //  mgr -> setStoreTime(fTimestamp);          // Set the time stamp 
  if(funixTime) {
    mgr->setStoreTime(funixTime);
  } else {
    mgr->setStoreTime(fTimestamp);
  }

  mgr -> storeDbTable(dbtable);             // Fetch the data 

  return 1;
}

//_____________________________________________________________________________
void StDbModifier::SetDateTime(const char* timestamp)
{
  fTimestamp = new char[strlen(timestamp)+1];
  strcpy(fTimestamp,timestamp);
}

void StDbModifier::SetTime(unsigned int time){ funixTime=time;}

//_____________________________________________________________________________
void StDbModifier::SetDbName(const char* dbname)
{
  fDbName = new char[strlen(dbname)+1];
  strcpy(fDbName,dbname);
}

//_____________________________________________________________________________
void StDbModifier::SetInputFileName(const char* inputfilename)
{
  fInputFileName = new char[strlen(inputfilename)+1];
  strcpy(fInputFileName,inputfilename);
}

//_____________________________________________________________________________
void StDbModifier::SetOutputFileName(const char* outputfilename)
{
  fOutputFileName = new char[strlen(outputfilename)+1];
  strcpy(fOutputFileName,outputfilename);
}

//_____________________________________________________________________________
void StDbModifier::SetTableName(const char* tablename)
{
  fTableName = new char[strlen(tablename)+1];
  strcpy(fTableName,tablename);
}

//_____________________________________________________________________________
void StDbModifier::SetVersionName(const char* versionname)
{
  fVersionName = new char[strlen(versionname)+1];
  strcpy(fVersionName,versionname);
}

//_____________________________________________________________________________
void StDbModifier::SetFlavor(const char* flavorname)
{
  fFlavorName = new char[strlen(flavorname)+1];
  strcpy(fFlavorName,flavorname);
  cout << "       Flavor is set " << flavorname  << endl;
  cout << "       Flavor is set " << fFlavorName << endl;
}

