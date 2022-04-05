/***************************************************************************
 *
 * $Id: DbRead.cxx,v 1.10 2016/05/24 18:02:27 dmitry Exp $
 *
 * Author: S. Vanyashin 
 * Updated by:  R. Jeff Porter
 ***************************************************************************
 *
 * Description: Temporary C-code to call the C++ DB-API
 *              
 *
 ***************************************************************************
 *
 * $Log: DbRead.cxx,v $
 * Revision 1.10  2016/05/24 18:02:27  dmitry
 * final fix for StDbBroker
 *
 * Revision 1.9  2015/05/21 18:29:06  dmitry
 * small memory leak and type conversion warnings fixed
 *
 * Revision 1.8  2007/05/16 22:47:54  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.7  2001/01/22 18:40:24  porter
 * Added a wrapper for StMessage so one can use it in StDbLib
 *
 * Revision 1.6  2000/04/13 20:22:56  porter
 * - reconnected tableDescriptor that had been broken via St_tableDescriptor.
 * - added unix timestamp as standard
 * - top node returned via InitConfig will be a database type
 *
 * Revision 1.5  2000/01/10 20:31:16  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/
// #include <stdio.h>
#include <stdlib.h>
// #include <string.h>

#include "StDbLib/StDbManager.hh"
#include "StDbBroker.h"
#include "StDbLib/StDbDefs.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbTableDescriptor.h"
#include "StMessMgr.h"

// modified for general access

extern "C" void * DbRead(unsigned int *nRows,
			unsigned int *datetime,
			const char * tableName,
            const char * structName,
			unsigned int nVar,
			unsigned int sizeOfStruct,
			StDbBroker::oldDescriptor *d,
            const char* database,
            const char* tableVersion)
{

 
char validFrom[20];
char validTo[20];
char temps[128];
char row[128];
char currentDateTime[20];
char time[7];

sprintf(currentDateTime,"%.8d",(int)datetime[0]);
sprintf(time,"%.6d",(int)datetime[1]);
strcat(currentDateTime,time);

currentDateTime[19]='\0';
currentDateTime[18]=currentDateTime[13];
currentDateTime[17]=currentDateTime[12];
currentDateTime[16]=':';
currentDateTime[15]=currentDateTime[11];
currentDateTime[14]=currentDateTime[10];
currentDateTime[13]=':';
currentDateTime[12]=currentDateTime[9];
currentDateTime[11]=currentDateTime[8];
currentDateTime[10]=' ';
currentDateTime[9]=currentDateTime[7];
currentDateTime[8]=currentDateTime[6];
currentDateTime[7]='-';
currentDateTime[6]=currentDateTime[5];
currentDateTime[5]=currentDateTime[4];
currentDateTime[4]='-';

//cout<<"currentDateTime: \""<<currentDateTime<<"\""<<endl;
StDbManager * mgr = StDbManager::Instance();
 

char dbType[64];
char dbDomain[64];
StDbType type;
StDbDomain domain;
char version[128];

if(!tableVersion)strcpy((char*)version,"default");
 if(!database){
   strcpy((char*)dbType,"TestScheme");
   strcpy((char*)dbDomain,"Star");
   type = mgr->getDbType(dbType);
   domain = mgr->getDbDomain(dbDomain);
 } else {
   char* atype;
   char* adomain;
   if(!mgr->getDataBaseInfo(database, atype, adomain)){
     LOG_ERROR << "StDbManager:: Database specified incorrectly" << endm;
     *nRows=0;
     return NULL;
   } 
   //  cout << "Returned DbType = "<<atype<<" & DbDomain= "<<adomain<< endl;
   type = mgr->getDbType(atype);
   domain = mgr->getDbDomain(adomain);

 }
  

StDbConfigNode* node=mgr->initConfig(type,domain);
// now try without descriptor --> can't
StDbTable* mtable=node->addDbTable(tableName,version);
//StDbTable* mtable=node->addTable(tableName,version);

 if(!mtable){
   *nRows=0;
    return NULL;
 }


 // --> can't do it 'cause St_base descriptor's limits 
 //_Descriptor* _d = (_Descriptor*)d;
 //StDbTableDescriptor* dbDescr = new StDbTableDescriptor(_d,nVar,sizeOfStruct);
 //mtable->setDescriptor((StTableDescriptorI*)dbDescr);

mgr->setRequestTime(currentDateTime);
mgr->fetchDbTable(mtable);


if (mtable->GetTable()==NULL)
  {
    *nRows=0;
    return NULL;
  }
 
*nRows = mtable->GetNRows();

//  cout<<"DbRead: nRows"<<*nRows<<endl;
 
//fill return datetime values 
int latestDirDate;
int latestDirTime;
 int ic;
 int i2, i3;

//convert hhmmss from: 1999-06-17 12:48:33
 strncpy(row,mtable->getBeginDateTime(),19); row[19] = '\0';
 
                strncpy(validFrom,row,19);validFrom[19]='\0';
                //start from blank at position row[0][10] 
                ic=10;
                for(i3=0;i3<3;i3++,++ic) {
		  for(i2=0;i2<2;i2++,++ic) {
		    temps[ic]=row[ic];
		  }
		}
 		temps[6]='\0';
		latestDirTime = atoi(temps);
        
  		//get date from: 1999-06-17 12:48:33

                strncpy(temps,validFrom,10);
                temps[4]=temps[5];
                temps[5]=temps[6];
                temps[6]=temps[8];
                temps[7]=temps[9];
		temps[8]='\0';

		latestDirDate = atoi(temps);

int nextDirDate;
int nextDirTime;
 
//convert hhmmss from: 1999-06-17 12:48:33
 strncpy(row,mtable->getEndDateTime(),19); row[19] = '\0';
                strncpy(validTo,row,19);validTo[19]='\0';
                //start from blank at position row[0][10] 
                ic=10;
                for(i3=0;i3<3;i3++,++ic) {
		  for(i2=0;i2<2;i2++,++ic) {
		    temps[ic]=row[ic];
		  }
		}
 		temps[6]='\0';
		nextDirTime = atoi(temps);
        
  		//get date from: 1999-06-17 12:48:33
                strncpy(temps,validTo,10);
                temps[4]=temps[5];
                temps[5]=temps[6];
                temps[6]=temps[8];
                temps[7]=temps[9];
		temps[8]='\0';

		nextDirDate = atoi(temps);

 datetime[0] = latestDirDate;
 datetime[1] = latestDirTime;
 datetime[2] = nextDirDate;
 datetime[3] = nextDirTime;

 if (datetime[2]==19691231) datetime[2]=20380101;

 void* data = mtable->GetTableCpy();
 delete node;


return data;
}










