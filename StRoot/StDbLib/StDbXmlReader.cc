/***************************************************************************
 *
 * $Id: StDbXmlReader.cc,v 1.4 1999/09/30 02:06:11 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  implement typeAcceptor for READING XML files of DB-tables
 *
 ***************************************************************************
 *
 * $Log: StDbXmlReader.cc,v $
 * Revision 1.4  1999/09/30 02:06:11  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbXmlReader.h"
#include <iostream.h>
#include <strings.h>
#include <fstream.h>
#include <strstream.h>
#include "dbStruct.hh"

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
 for(k=0;k<tab->a.nelems;k++){
    e=tab->a.e[k];
    if(strcmp(name,e->name)==0)retVal=e;
 }
 if(!retVal){
 for(k=0;k<tab->nelems;k++){
   //   cout <<k<<" elements = " << tab->nelems << endl;
    e=tab->e[k];
    if(strcmp(name,e->name)==0)retVal=e;
 }
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
     cout << "line i = "<< loca[i] << endl;
     i++;
   }
 }
buildDbTable();

}

//-----------------------------------------------------------


void 
StDbXmlReader::buildDbTable(){

 int done = 0;
 int j=0;
 while (!done) {
   if(strstr(loca[j],tab->startKey)){
     tab->istart = j;
     done = 1;
   }
   j++;
 }

 // find accessor start
 j=tab->istart+1;
 while (!done) {
   if(strstr(loca[j],(tab->a).startKey)){
     (tab->a).istart = j;
     done = 1;
   }
   j++;
 }

 elem* e0;
 elem *e = new elem(); // key holder
 // (tab->a).e.push_back(e)
 done = 0;
 int nelems = 0;
 j=(tab->a).istart+1;
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
 
 // 
 j=(tab->a).iend+1;
 nelems = 0;
 done = 0;  
 // e = new elem();
 while (!done) {
   if(strstr(loca[j],e->startKey)){
     e0 = new elem();
     e0->istart = j;
     tab->e.push_back(e0);
     nelems++;
     if(strstr(loca[j],e->endKey))e0->iend = j;
   } else if(strstr(loca[j],e->endKey)){
     e0->iend = j;
   } else if(strstr(loca[j],tab->endKey)){
     tab->iend = j;
     tab->nelems = nelems;
     done = 1;
   }
   j++;
 }

buildStruct();

}

//-----------------------------------------------------------

void
StDbXmlReader::buildStruct(){

 char * p1 = strstr(loca[tab->istart],tab->startKey);
 int len = strlen(tab->startKey);
 for(int k=0;k<len+1;k++)p1++;
 tab->name=new char[strlen(p1)+1];
 strcpy(tab->name,p1);

 accessor * a = &(tab->a);
 fillElements(a);
 a = (accessor*)(tab);
 fillElements(a);

}

//-----------------------------------------------------------

void
StDbXmlReader::fillElements(accessor* a){

elem* e;
int len,j;
char *p1,*p2,*hlen;
int ilist;
int iline;
char* fullLine;

 for(int k=0;k<a->nelems;k++){

   e=a->e[k];

     // get Type

   p1=strstr(loca[e->istart],e->startKey);
   for(j=0;j<strlen(e->startKey);j++)p1++;
   p2=strstr(loca[e->istart],">");
   len = strlen(p1) - strlen(p2);
   e->type = new char[len];
   memcpy(e->type,p1,len);
   e->type[len]='\0';

   // get Name
   p2++;
   while(p2[0]==' ')p2++;
   p1 = strstr(p2,"<");
   len = strlen(p2) - strlen(p1);
   e->name = new char[len];
   memcpy(e->name,p2,len);
   e->name[len]='\0';

   // get size

   p1=strstr(loca[e->istart],e->size.startKey);
if(p1){
   for(j=0;j<strlen(e->startKey);j++)p1++;
   p2=strstr(loca[e->istart],e->size.endKey);
   len = strlen(p1) - strlen(p2);
   hlen = new char[len];
   memcpy(hlen,p1,len);
   hlen[len-1]='\0';
   e->size.isize = atoi(hlen);
}
  
   // get values : if array it is on separate lines

   p1=strstr(loca[e->istart],(e->val).startKey);
   if(p1){
     // <element Type> ...     <value> data </value>
     // </element Type>
      for(j=0;j<strlen((e->val).startKey);j++)p1++;
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
     fullLine=new char[iline+1];
     ostrstream fs(fullLine,iline+1);
     for(j=2;j<ilist;j++) fs<<loca[e->istart+j]<<endl;
     fs << ends;
     e->val.data = new char[strlen(fullLine)+1];
     //     cout << "TEST:: " << fullLine << endl;
     strcpy(e->val.data,fullLine);
     delete [] fullLine;

   }

   //   cout << "Test:: " << endl;
   // cout << e->name << " "<<e->type << " " << e->val.data << endl;
 }

}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, char* i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"String")==0){
   i=new char[strlen((e->val).data)+1];
   strcpy(i,(e->val).data);
  }
}


void
StDbXmlReader::pass(char* name, unsigned char* i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"UString")==0); // i=atoi((e->val).data);
}


//----------------------------------------------------


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, short& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"Short")==0)i=atoi((e->val).data);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, short* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"ShortArray")==0){

   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //      cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = atoi(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned short& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"UShort")==0)i=atoi((e->val).data);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned short* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"UShortArray")==0){

   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //      cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = atoi(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}



void
StDbXmlReader::pass(char* name, int& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"Int")==0)i=atoi((e->val).data);
}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, int* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"IntArray")==0){

   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = atoi(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}
void
StDbXmlReader::pass(char* name,unsigned int& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"UInt")==0)i=atoi((e->val).data);
}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name,unsigned int* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"UIntArray")==0){

   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = atoi(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, long& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"Long")==0)i=atol((e->val).data);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, long* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"LongArray")==0){

   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //      cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = atol(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned long& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"ULong")==0)i=atol((e->val).data);
}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, unsigned long* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"ULongArray")==0){

   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //      cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atoi(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = atol(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}


//----------------------------------------------------

void
StDbXmlReader::pass(char* name, float* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"FloatArray")==0){


   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //      cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = (float)atof(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = (float)atof(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, double* i,  int len){

 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"DoubleArray")==0){


   char* p1;
   char* p2;
   char* dstring;
   int idata =strlen((e->val).data)+1;
   char* tmp = new char[idata];
   char* dataString = new char[idata];
   ostrstream os(dataString,idata);
   os << (e->val).data <<ends;
   int iloc;
   int ilen;
   for(int iloop=0; iloop<len-1; iloop++){
     p1 = strstr(dataString,","); 
     iloc = p1-dataString;
      p2 = &dataString[0];
      strncpy(tmp,p2,iloc); tmp[iloc]='\0';
      //      cout << "Data passed " << tmp << " from " << dataString << endl;
      i[iloop] = atof(tmp);
      p1++; 
      ilen = strlen(dataString)-iloc;
      dstring=new char[ilen];
      strncpy(dstring,p1,ilen);
      delete [] dataString;
      dataString = new char[strlen(dstring)+1];
      strcpy(dataString,dstring);
      delete [] dstring;                  
   }
      i[len-1] = atof(dataString);
      delete [] dataString;
      delete [] tmp;

 }// strcmp==0

}

//----------------------------------------------------

void
StDbXmlReader::pass(char* name, float& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"Float")==0)i=(float)atof((e->val).data);

}


void
StDbXmlReader::pass(char* name, double& i,  int len){
 elem* e = findElement(name); if(!e){ cerr<<name<<" not found"<<endl; return;}
 if(strcmp(e->type,"Double")==0)i=atof((e->val).data);

}


