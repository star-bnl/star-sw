#include "StDbFactoryI.hh"
#include "StDbTable.h"

///////////////////////////////////////////////////////////////////////

StDbTable*
StDbFactoryI::getDbTable(const char* tableName, int option){

if(!isloaded)initIDList();

int tableID = findTableID(tableName);
StDbTable* dbTable = new StDbTable(tableName,tableID);

return dbTable;
}

///////////////////////////////////////////////////////////////////////
int
StDbFactoryI::findTableID(const char* tableName){

int retVal=0;

if(!isloaded)initIDList();

for(IDList::iterator itr = mIDList.begin();
    itr != mIDList.end(); ++itr){
  if(!(*itr)){cout << "Look out Null Pointer in list" << endl; continue;}
  if((*itr)->checkName(tableName)){
     retVal = (*itr)->getID();
     break;
  }
}

if(!retVal) cerr << "StDbFactoryI:: No TableID for Table= "<<tableName<< endl;

return retVal;
}

//////////////////////////////////////////////////////////////

void
StDbFactoryI::deleteIDList(){

  StDbTableID* table;
  IDList::iterator itr;

  do {
      for(itr = mIDList.begin(); itr != mIDList.end(); ++itr){
         table = *itr;
         mIDList.erase(itr);
         delete table;
         break;
        }
     } while( mIDList.begin() != mIDList.end() );

isloaded=false;
}

/////////////////////////////////////////////////////////

void
StDbFactoryI::initIDList(ifstream& is){


char line[256];
char line2[256];
char tmp[2];
bool done = false;
char* name;
char* ptr;
int ID;
int j,k,icount;

 while(!done){
   if(is.eof()){
     done = true;
   } else {
     is.getline(line,255);
     if(strstr(line,"//"))continue;  // comment line
     ptr=strstr(line,":");
     if(!ptr)continue;  // no Name:Value pair incountered

     // --> copy all non-blank spaces from line to line2
     k = strlen(line);
     j = 0;
     line2[j]='\0';
     for(icount=0;icount<k;icount++){
       if(!(line[icount]==' ')){
          *tmp=line[icount];
          tmp[1]='\0';
          j++;
          strcat(line2,tmp);
       }
     }
     line2[j]='\0';

     // --> now read  Name:Value pair from line2

     ptr=strstr(line2,":");
     if(!ptr)continue;
     ptr[0]='\0';

     name=new char[strlen(line2)];
     strcpy(name,line2);
     ID=atoi(ptr);

     // --> now add to IDList
     mIDList.push_back(new StDbTableID(name,ID));
   }
 }     

}












