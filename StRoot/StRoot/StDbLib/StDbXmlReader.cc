/***************************************************************************
 *
 * $Id: StDbXmlReader.cc,v 1.19 2016/05/27 16:26:23 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  implement typeAcceptor for READING XML files of DB-tables
 *
 ***************************************************************************
 *
 * $Log: StDbXmlReader.cc,v $
 * Revision 1.19  2016/05/27 16:26:23  dmitry
 * more coverity
 *
 * Revision 1.18  2016/05/25 21:01:30  dmitry
 * coverity - buffer size
 *
 * Revision 1.17  2015/05/15 19:11:44  dmitry
 * typo, now doing proper deallocation
 *
 * Revision 1.16  2015/05/15 19:07:09  dmitry
 * small memory leak fixed
 *
 * Revision 1.15  2012/06/11 14:33:47  fisyak
 * std namespace
 *
 * Revision 1.14  2007/08/20 18:21:31  deph
 * New Version of Load Balancer
 *
 * Revision 1.13  2007/05/16 22:48:10  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.12  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.11  2003/09/16 22:44:18  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.10  2003/09/02 17:57:50  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.9  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.8  2001/01/22 18:38:02  porter
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
 * Revision 1.7  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.6  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.5  1999/12/03 17:03:22  porter
 * added multi-row support for the Xml reader & writer
 *
 * Revision 1.4  1999/09/30 02:06:11  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbXmlReader.h"
#include "stdb_streams.h"
#include <string.h>

#include "dbStruct.hh"

#ifndef __STDB_STANDALONE__
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
using namespace std;
template<class T>
static void passAux(elem* e, T*& i,  int& len)
{
  len = e->size.isize;
  i = new T[len];
  memset(i,0,sizeof(T)*len);
  const char* p1 = e->val.data;
  const char* p2=p1;
  int iloop = 0;
  for(;1;p2++) {
     if (p2[0] && p2[0]!=',') 		continue;
     i[iloop++] = (T)atof(p1);
     p1=p2+1;
     if (iloop==len || p2[0]==0) 	break;
  }
}

//----------------------------------------------------
StDbXmlReader::StDbXmlReader(){ tab = new dbTable(); }

//----------------------------------------------------

StDbXmlReader::~StDbXmlReader() { delete tab; }

//----------------------------------------------------
elem*
StDbXmlReader::findElement(char* name){

elem* e = 0;
elem* retVal = 0;

int k;
// check accessor 1st
// here it's a bit scary : if accessor element has same name as data-element
 if(tab->curRow==-1){
 for(k=0;k<tab->a.nelems;k++){
    e=tab->a.e[k];
    if(strcmp(name,e->name)==0){
      retVal=e;
      break;
    }
 }
 }

 if(!retVal){ // now check rows
   if(tab->curRow==-1)tab->curRow=0; // init row
   if(tab->curRow==tab->numRows)return retVal;
  for(k=0;k<tab->row[tab->curRow]->nelems;k++){
   //   cout <<k<<" elements = " << tab->nelems << endl;
    e=tab->row[tab->curRow]->e[k];
    if(strcmp(name,e->name)==0){
      retVal=e;
      break;
    }
 }
 if(k==tab->row[tab->curRow]->nelems-1)tab->curRow++;
 }

return retVal;
}

//----------------------------------------------------

//----------------------------------------------------

void
StDbXmlReader::readTable(ifstream &is){

 int done = 0;
 int i = 0;
 int j,icount,k;
 char tmp[2];
 char line[256];
 char line2[256];

 while(!done){
   if(is.eof()){
     done = 1;
   } else {
     //     cout << "line . " << i << endl;   
     is.getline(line,255);
     k = strlen(line);
     j = 0;
     line2[j]='\0';
     char* ptr=&line2[0];
     for(icount=0;icount<k;icount++){
       if(!(line[icount]==' ')){
          *tmp=line[icount];
          tmp[1]='\0';
          j++;
          strncpy(ptr,tmp,1);
          ptr++;
       }
     }
     line2[j]='\0';
     //     cout << line << endl;
     loca[i] = new char[strlen(line2)+1];
     strcpy(loca[i],line2);
     //     cout << "line i = "<< loca[i] << endl;
     i++;
   }
 }

maxlines = i-1;

buildDbTable();

}

//-----------------------------------------------------------


void 
StDbXmlReader::buildDbTable(){

 int done = 0;
 int j=0;

 // find table start
 while (!done) {
   if(strstr(loca[j],tab->startKey)){
     tab->istart = j;
     done = 1;
   }
   j++;
 }

 // find accessor start
 j=tab->istart+1;
 done = 0;
 while (!done) {
   //   cout << loca[j] << endl;
   if(strstr(loca[j],(tab->a).startKey)){
     (tab->a).istart = j;
     done = 1;
   }
   j++;
 }

 // if(j>=maxlines)cout << "1 j to big " << j << " max= " << maxlines << endl;

 elem* e0;
 elem *e = new elem(); // key holder
 // (tab->a).e.push_back(e)
 done = 0;
 int nelems = 0;
 j=(tab->a).istart+1;
 // if(j>=maxlines)cout << "2 j to big " << j << " max= " << maxlines << endl;
 while (!done) {
   if(strstr(loca[j],e->startKey)){
     e0 = new elem();
     e0->istart = j;
     (tab->a).e.push_back(e0);
     nelems++;
     if(strstr(loca[j],e->endKey))e0->iend = j;
   } else if(strstr(loca[j],e->endKey)){
     e0->iend = j;
   } else if(strstr(loca[j],(tab->a).endKey)){
     if(strstr(loca[j],e->endKey))e0->iend = j;
     (tab->a).iend = j;
     (tab->a).nelems = nelems;
     done = 1;
   }
   j++;
 }
 
 // end of accessor
 // find  all rows
 j=(tab->a).iend+1;
 // if(j>=maxlines)cout << "3 j to big " << j << " max= " << maxlines << endl;
 dbRow row;
 dbRow* row0;
 
 done = 0;
 int numRows = 0;
 while (!done) {
   if(strstr(loca[j],row.startKey)){
       row0 = new dbRow();
       row0->istart = j;
       if(strstr(loca[j],row.endKey))row0->iend = j;
       tab->row.push_back(row0);
       row0->rowNumber = numRows;
       numRows++;
     } else if(strstr(loca[j],tab->endKey)){
       tab->iend = j;
       tab->numRows = numRows;
       tab->curRow = -1;
       done = 1;
     }
   j++;
 }

 // cout << "Numrows = " << numRows<< endl;
 // cout << "Current Row = " << tab->curRow << endl;
   //
   // Loop over rows
   //
 for(int nrows=0; nrows<tab->numRows; nrows++){ 
 done = 0;
 nelems = 0;
 j=tab->row[nrows]->istart;
 while (!done) {
   //   cout << loca[j] << endl;
   if(strstr(loca[j],e->startKey)){
     e0 = new elem();
     e0->istart = j;
     tab->row[nrows]->e.push_back(e0);
     nelems++;
     if(strstr(loca[j],e->endKey))e0->iend = j;
     //     cout<<"estart"<< loca[j] <<endl;
   } else if(strstr(loca[j],e->endKey)){
     e0->iend = j;
     //     cout<<"eend"<< loca[j] <<endl;
   } else if(strstr(loca[j],row.endKey)){
     tab->row[nrows]->nelems = nelems;
     done = 1;
     //     cout<<"rowend"<< loca[j] <<endl;
   }
   j++;
 }
 }

buildStruct();
	delete e;
}

//-----------------------------------------------------------

void
StDbXmlReader::buildStruct(){

 char * p1 = strstr(loca[tab->istart],tab->startKey);
 int len = strlen(tab->startKey);
 int k;
 for(k=0;k<len+1;k++)p1++;

 char* tmpName = new char[strlen(p1)+1];
 strcpy(tmpName,p1);
 char* id;

 if((id=strstr(tmpName,"<")))*id='\0';
 tab->name=new char[strlen(tmpName)+1];
 strcpy(tab->name,tmpName);
 delete [] tmpName;

 accessor * a = &(tab->a);
 fillElements(a);
 // a = (accessor*)(tab);

 len = strlen(tab->row[0]->startKey);
 
 for(int n=0; n<tab->numRows;n++){

   // 1st get rowID (elementID) of this row
   p1 = strstr(loca[tab->row[n]->istart],tab->row[n]->startKey);
   for(k=0;k<len+1;k++)p1++;
   tmpName = new char[strlen(p1)+1];
   strcpy(tmpName,p1);
   if((id=strstr(tmpName,"<")))*id='\0';
   tab->row[n]->rowID = atoi(tmpName);
   delete [] tmpName;

   // now get row values

    a = (accessor*)(tab->row[n]);
    fillElements(a);

 }

}

//-----------------------------------------------------------

void
StDbXmlReader::fillElements(accessor* a){

elem* e;
int len,j;
char *p1 = 0, *p2 = 0, *hlen = 0;
int ilist;
int iline;

 for(int k=0;k<a->nelems;k++){

   e=a->e[k];

     // get Type

   p1=strstr(loca[e->istart],e->startKey) + strlen(e->startKey);

   p2=strstr(loca[e->istart],">");
   len = strlen(p1) - strlen(p2);
   e->type = new char[len+1];
   memcpy(e->type,p1,len);
   e->type[len]='\0';

   // get Name
   p2++;
   while(p2[0]==' ')p2++;
   p1 = strstr(p2,"<");
   len = strlen(p2) - strlen(p1);
   e->name = new char[len+1];
   memcpy(e->name,p2,len);
   e->name[len]='\0';

   // get size

   p1=strstr(loca[e->istart],e->size.startKey);

if(p1){
   p1+=strlen(e->size.startKey);
   p2=strstr(loca[e->istart],e->size.endKey);
   len = strlen(p1) - strlen(p2)+1;
   hlen = new char[len+1];
   memcpy(hlen,p1,len);
   hlen[len]='\0';
   e->size.isize = atoi(hlen);
   delete [] hlen;
} 
 if(e->size.isize==0)e->size.isize=1; // <length> not required for len=1

  
   // get values : if array it is on separate lines

   p1=strstr(loca[e->istart],(e->val).startKey);
   if(p1){
     // <element Type> ...     <value> data </value>
     // </element Type>
      p1+=strlen((e->val).startKey);
      while(p1[0]==' ')p1++;
      p2=strstr(loca[e->istart],(e->val).endKey);
      len = strlen(p1) - strlen(p2);
      (e->val).data = new char[len];
      memcpy((e->val).data,p1,len);
      (e->val).data[len]='\0';
   } else {
     // <element Type> ...
     // <value>
     // data
     // data
     // </value> </element Type>
     ilist =e->iend - e->istart;
     iline =0;
     for(j=2;j<ilist;j++)iline += strlen(loca[e->istart+j]);
     StString fs;
     for(j=2;j<ilist;j++) fs<<loca[e->istart+j]; //<<endl;
     string fs2=fs.str();
     e->val.data = new char[fs2.length()+1];
     //cout << "TEST:: " << fullLine << " len = " << strlen(fullLine) << endl;
     strcpy(e->val.data,fs2.c_str());

   }

   //   cout << "Test:: " << endl;
   // cout << e->name << " "<<e->type << " " << e->val.data << endl;
 }

}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, char*& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"String")==0){
   i=new char[strlen((e->val).data)+1];
   strcpy(i,(e->val).data);
  }
}

//----------------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned char& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"Char")==0)i=atoi((e->val).data);
}

////////////////////////////////////////////////////////////////////////

void
StDbXmlReader::pass(char* name, unsigned char*& i,  int& len)
{
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"UChar")) return;
 passAux(e,i,len);
}


//----------------------------------------------------


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, short& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"Short")==0)i=atoi((e->val).data);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, short*& i,  int& len){

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"Short"))return;
 passAux(e,i,len);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned short& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"UShort")==0)i=atoi((e->val).data);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned short*& i,  int& len){

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"UShort"))return;
 passAux(e,i,len);
}

void
StDbXmlReader::pass(char* name, int& i,  int& len){
  //  cout << "In Int " << endl;

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"Int")==0)i=atoi((e->val).data);
}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, int*& i,  int& len){

  //  cout << "In IntArray " << endl;
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"Int")) return; 
 passAux(e,i,len);
}
void
StDbXmlReader::pass(char* name,unsigned int& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"UInt")==0)i=atoi((e->val).data);
}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name,unsigned int*& i,  int& len){

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"UInt"))return;
 passAux(e,i,len);
}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, long& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"Long")==0)i=atol((e->val).data);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, long*& i,  int& len){

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"Long")) return;
 passAux(e,i,len);
}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned long& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"ULong")==0)i=atol((e->val).data);
}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, long long& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
#ifndef __osf__
 if(strcmp(e->type,"LongLong")==0)i=atoll((e->val).data);
#else
 if(strcmp(e->type,"LongLong")==0)i=atol((e->val).data);
#endif
}
void
StDbXmlReader::pass(char* name, long long*& i,  int& len){
  //
  cout<<"Not Yet Implemented"<<endl;
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned long*& i,  int& len){

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"ULong"))return;
 passAux(e,i,len);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, float*& i,  int& len){

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"Float")) return; 
 passAux(e,i,len);
}
//----------------------------------------------------

void
StDbXmlReader::pass(char* name, double*& i,  int& len){

 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(!strstr(e->type,"Double"))return;
 passAux(e,i,len);
}
//----------------------------------------------------

void
StDbXmlReader::pass(char* name, float& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"Float")==0)i=(float)atof((e->val).data);

}


void
StDbXmlReader::pass(char* name, double& i,  int& len){
 elem* e = findElement(name); if(!e){ LOG_ERROR<<name<<" not found"<<endm; return;}
 if(strcmp(e->type,"Double")==0)i=atof((e->val).data);

}








