//******************************************************************************

/*! \class StEmcDBHandler
    \author Marcia Maria de Moura
		
		  2003/01/02
		  Class to retrieve Emc database tables
*/		
//******************************************************************************

#ifndef StEmcDbHandler_HH
#define StEmcDbHandler_HH

#include <iostream>
using namespace std;


#include <strstream>

#include "TObject.h"
#include "TString.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDataBaseI.hh"


class StEmcDbHandler : public TObject 
{
  public:

                   StEmcDbHandler();  ///< Class ctor
        virtual    ~StEmcDbHandler();  ///< Class dtor
     StDbTable*    getDbTable();
        TString    getTimeStamp() { return mTimeStamp; }
       TString*    getTimeStampList();
   unsigned int    getListSize() { return mListSize; }
	   void    setTableName(char* name) { mTableName = name; }
	   void    setTableName(char*, char* );
	   void    setTableNode(char* node = "Calibrations_emc") { mTableNode = node; }
	   void    setTimeStamp(char* stamp = "2030-01-01 00:00:00") { mTimeStamp = stamp; }
	   void    setTimeStampLimits(char* stamp1 = "2000-01-01 00:00:00", char* stamp2 = "2030-01-01 00:00:00");

  protected:

    unsigned int    mDet;
    unsigned int    mParam;
      StDbTable*    mTable;                  ///< EMC database table
         TString    mTableNode;              ///< Database node for EMC tables
         TString    mTableName;              ///< EMC database table name, according to input
         TString    mTimeStamp;              ///< EMC database table time stamp
         TString    mFstTimeStamp;           ///< First time stamp of list
         TString    mLstTimeStamp;           ///< Last time stamp of list
    unsigned int    mListSize;
         TString    mTimeStampList[1000];    ///< Time stamp list of specified EMC database table
      
         TString    timeToSqlTime(const char*);

  ClassDef(StEmcDbHandler,1)
  
};
#endif
