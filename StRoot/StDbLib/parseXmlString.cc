/***************************************************************************
 *
 * $Id: parseXmlString.cc,v 1.2 1999/09/30 02:06:15 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  parses Xml file & returns contents between request TAGs
 *
 ***************************************************************************
 *
 * $Log: parseXmlString.cc,v $
 * Revision 1.2  1999/09/30 02:06:15  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "parseXmlString.hh"
#include <iostream.h>
#include <strstream.h>
#include <strings.h>

/////////////////////////////////////////////////////////////////

char*
parseXmlString::getString(char* line, char* key1, char* key2){


char* p1 = 0;
char* p = &line[0];

int i1 = getIndexAfter(line, key1);
if(!i1) return p1;
int i2 = getIndexBefore(line, key2);
if(!i2) return p1;


int size = i2-i1+1;
if(i2<i1){
  cerr << " Missing Key " << endl;
  return p1;
}

for (int i=0; i<i1-1; i++)p++;

p1 = new char[size+1];
strncpy(p1,p,size);
p1[size]='\0';

// cout << p1 << endl;
 char* retVal = removeBlankEnds(p1);
 delete [] p1;
 return retVal;
 // return removeBlankEnds(p1); 

}

/////////////////////////////////////////////////////////////////

char*
parseXmlString::getStringAfter(char* line, char* key){

 int i = getIndexAfter(line,key);
 char* tmp =0;

 if(!i)return tmp;

 char* p1 = &line[i];

 int k = strlen(line);
 int size = k-i;

 tmp = new char[size+1];
 strncpy(tmp,p1,size);

 tmp[size] = '\0';
 char* retVal = removeBlankEnds(tmp);
 delete [] tmp;
 return retVal;
 // return removeBlankEnds(tmp); 

}

/////////////////////////////////////////////////////////////////

char*
parseXmlString::getStringBefore(char* line, char* key){

 int i = getIndexBefore(line,key);

 char * tmp = 0;
 if(!i)return tmp;
 char* p1 = &line[0];
 int size = i;
 tmp = new char[size+1];
 strncpy(tmp,p1,size);
 tmp[size] = '\0';

 char* retVal = removeBlankEnds(tmp);
 delete [] tmp;
 return retVal;
 // return removeBlankEnds(tmp); 
}

/////////////////////////////////////////////////////////////////

int
parseXmlString::getIndexAfter(char* line, char* key){

 if(!line || !key)return 0;
 char* id = strstr(line,key);
 if(!id)return 0;  //use as bool
 
 int i = id - line;
 i += strlen(key)+1;
 return i; 
}

/////////////////////////////////////////////////////////////////

int
parseXmlString::getIndexBefore(char* line, char* key){

 if(!line || !key) return 0;
 char* id = strstr(line,key);
 if(!id) return 0; //use as bool
 int i = id - line; 
 return i--;

}

/////////////////////////////////////////////////////////////////

char*
parseXmlString::removeBlankEnds(char* line){

 char* p1 = &line[0];
 int k = strlen(line);
 int i,j;

 for(i=0; i<k; i++){
   if(line[i]!=' ')break;
   p1++;
  }

 for(j=k; j>0; j--){
   if(line[j]=='\0')continue;
   if(line[j]!=' ')break;
   }

 int size = j-i+1;
 char* tmp = new char[size+1];
 strncpy(tmp,p1,size);
 tmp[size] = '\0';

 // delete [] line;
 return tmp;
}







