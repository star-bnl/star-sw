/***************************************************************************
 *
 * $Id: parseXmlString.cc,v 1.8 2012/06/11 14:33:47 fisyak Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  parses Xml file & returns contents between request TAGs
 *
 ***************************************************************************
 *
 * $Log: parseXmlString.cc,v $
 * Revision 1.8  2012/06/11 14:33:47  fisyak
 * std namespace
 *
 * Revision 1.7  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.6  2003/09/16 22:44:18  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.5  2003/09/02 17:57:50  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.3  1999/12/28 21:31:42  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.2  1999/09/30 02:06:15  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "parseXmlString.hh"
#include "stdb_streams.h"
#include <string.h>
using namespace std;
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

 //delete [] line;
 return tmp;
}







