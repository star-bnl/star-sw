// #include <stdio.h>
// #include <stdlib.h>
// #include <string.h>

#include "StDbLib/StDbManager.hh"
#include "StDbBroker.h"
#include "StDbLib/StDbDefs.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbTableDescriptor.h"

// modified for general access

extern "C" void * DbRead(unsigned int *nRows,
			unsigned int *datetime,
			const char * tableName,
            const char * structName,
			unsigned int nVar,
			unsigned int sizeOfStruct,
			StDbBroker::Descriptor *d,
            const char* database,
            const char* tableVersion)
{

 
char validFrom[20];
char validTo[20];
char temps[128];
char row[128];
char currentDateTime[20];
char time[7];

sprintf(currentDateTime,"%.8d",datetime[0]);
sprintf(time,"%.6d",datetime[1]);
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
 } else {
   char* type=(char*)dbType;
   char* domain = (char*)dbDomain;
   if(!mgr->getDataBaseInfo(database, type, domain)){
     cerr << "StDbManager:: Database specified incorrectly" << endl;
     *nRows=0;
     return NULL;
   }
 }
  
   type = mgr->getDbType(dbType);
   domain = mgr->getDbDomain(dbDomain);

StDbConfigNode* node=mgr->initConfig(type,domain);
// now try without descriptor --> can't
StDbTable* mtable=node->addDbTable(tableName,version,1);
//StDbTable* mtable=node->addTable(tableName,version,1);

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
 strcpy(row,mtable->getBeginDateTime());
 
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
 strcpy(row,mtable->getEndDateTime());
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







