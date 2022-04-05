/***************************************************************************
 *
 * $Id: StDbDefaults.hh,v 1.1 2001/01/22 18:37:53 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Defualt values -> version, flavor, endTime, dbserver file
 *         -> Used to be imbedded in code & in StDbDefs header.
 *            now (Dec2000) separate file.
 *
 ***************************************************************************
 *
 * $Log: StDbDefaults.hh,v $
 * Revision 1.1  2001/01/22 18:37:53  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 *
 *
 ***************************************************************************/
#ifndef STDBDEFAULTS_HH
#define STDBDEFAULTS_HH

#include "StDbDefs.hh"
#include <string.h>

class StDbDefaults {

private:

  char mversion[64];
  char mflavor[16];
  unsigned int mprodTime;
  unsigned int mendTime;
  char mdbServerVar[16];
  char mdbServerFile[16];

  StDbDefaults();
 
  static StDbDefaults* mInstance;

public:

  static StDbDefaults* Instance(){
         if(!mInstance)mInstance = new StDbDefaults;
         return mInstance;
  }

  ~StDbDefaults() {};

  bool         IsDefaultVersion(const char* version);
  bool         IsDefaultFlavor(const char* flavor);
  char*        printVersion();
  char*        printFlavor();
  unsigned int getProdTime() const;
  unsigned int getEndTime() const;
  char*        printDbServerVar();
  char*        printDbServerFile();

  char*        getServerFileName(const dbFindServerMode mode);
  char*        getFileName(const char* fileName);

};

inline
bool StDbDefaults::IsDefaultVersion(const char* version){
if(strcmp(version,mversion)==0)return true;
return false;
}

inline
bool StDbDefaults::IsDefaultFlavor(const char* flavor){
if(strcmp(flavor,mflavor)==0)return true;
return false;
}

inline
char* StDbDefaults::printVersion(){ return mversion; }

inline
char* StDbDefaults::printFlavor() { return mflavor; }

inline
unsigned int StDbDefaults::getProdTime() const { return mprodTime; }

inline
unsigned int StDbDefaults::getEndTime() const { return mendTime; }

inline
char* StDbDefaults::printDbServerVar() { return mdbServerVar; };

inline
char* StDbDefaults::printDbServerFile() { return mdbServerFile; };
#endif





