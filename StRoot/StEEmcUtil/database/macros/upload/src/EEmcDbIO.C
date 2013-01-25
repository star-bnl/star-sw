// $Id: EEmcDbIO.C,v 1.1 2013/01/25 16:46:48 stevens4 Exp $:

#include <stdlib.h>
#include <stdio.h>

#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <ctype.h>

#include "EEmcDbIO.h"
#include "eemcDb.h"


static const size_t MinLine =   4; // min length of a inp file line
static const size_t MaxLine = 256; // max length of a inp file line

inline int min(int a,int b) { return (a<b) ? a : b; } 

// ----------------------------------------------------------------
// a terrible hack for our name array
// we pack a string no longer than maxlen into char[] buffer
// and then "terminate" it with a $ sing (sounds f77 like )
// ----------------------------------------------------------------
char *
resetString(char *buf, int nelem, int maxlen) 
{
  memset(buf,0x20,nelem*maxlen);
  for(int i=0; i<nelem; i++ ) memset(buf+i*maxlen,EEMCDbStringDelim,0x01); 
  return buf;
}

char *
packString(char *buf, int pos, int maxlen, char *str) 
{
  int len = min(maxlen,strlen(str));
  memcpy(buf+pos*maxlen,str,len);
  memset(buf+pos*maxlen+len,EEMCDbStringDelim,0x01);
  return str;
}


char *
unpackString(char *buf, int pos, int maxlen, char *str) {
  memcpy(str,buf+pos*maxlen,maxlen);
  char *dollar = (char *)memchr(str,EEMCDbStringDelim,maxlen);
  if( dollar == NULL ) *(str+maxlen) = 0; else *dollar = 0x0;
  return str;
}


// THIS IS A PIECE OF 'EINE GROSSE SCHEISSE'
// WRITTEN ON 01/24/2003 IN PAIN BY PAZ (IU)
int
indexFromString(const char *str, const int MaxIndex)
{
  //const int   MaxSect=12;
  //const int   MaxInp =12;
  char  sbox[3] ="";
  short sec=-1,inp=-1,box=-1;
  int   index=0;

  // more scheisse
  if(str==NULL) {
    fprintf(stderr,"indexFromString(): invalid string\n");
    return -1;
  }
  // skip leading non digits
  while(str[index]!=0x00 && ! isdigit(str[index]) ) index++;
  
  int   ns=sscanf(str+index,"%2hd%2s%2hd",&sec,sbox,&inp);

  if(sec<=0  || kEEmcMaxSect < sec) {
    fprintf(stderr,"indexFromString(): sector %d out of bounds (%s)\n",sec,str);
    return -2;
  }

  if(sbox!=NULL && strlen(sbox)>=2) { 
    for(int i=0; kEEmcBoxInp[i]!="" ; i++) {
      if(strncmp(kEEmcBoxInp[i],sbox,2)==0) { box=i; break; }
    }
  }

  if(box<0 || kEEmcMaxBox<box) {
    fprintf(stderr,"indexFromString(): box %.2s is invalid (%s)\n",sbox,str); 
    return(-3); 
  }
  	
  if(ns==3 && inp>0) { // eg. 05TA11
    if(inp<=0  || kEEmcMaxInput  < inp ) {
      fprintf(stderr,"indexFromString(): input %d out of bounds (%s)\n",inp,str);
      return -4;
    }
    index = ((sec-1)*12+box)*12+inp;
  } else {
    index =  (sec-1)*12+box ;
  }
  if(index<=0 || MaxIndex<index) { 
    fprintf(stderr,"indexFromString():  index %d out of bounds (%s)\n",index,str);
    return -5;
  }
  //dprintf("STR: %s : %02hd-%s-%02hd %d\n",str,sec,sbox,inp,index);
  return (index);
}



// ================================================================
//
// ================================================================
EEmcDbIOBase::EEmcDbIOBase (int nelem, int size)
{
  bytes    = nelem*size;
  nElem    = nelem;
  bytePtr  = new char[bytes];
  indexArr = new int[nElem];
  comment  = NULL;
  for(int i=0;i<nElem;i++) indexArr[i]=i; // default index
}

EEmcDbIOBase::~EEmcDbIOBase() { 
  if(bytePtr ) delete [] bytePtr ; 
  if(indexArr) delete [] indexArr; 
};


bool
EEmcDbIOBase::checkLine(const char *line) {
  if(strlen(line)<=MinLine) return false;
  if(line[0]=='#'         ) return false;
  return true;
}



// ================================================================
//
// ================================================================
template <class T> 
int 
EEmcDbCCIO<T>::read(FILE *f)
{
  char line[MaxLine];
  char tn  [EEMCDbMaxName];
  T*   s = data();

  memset(bytePtr,0x00,bytes);
  resetString(s->name,getSize(),EEMCDbMaxName);
  memset(s->comment,0x20,sizeof(s->comment));

  int i=0;
  while ( fgets(line,MaxLine,f) != NULL && i<getSize() )  { 
    if( ! checkLine(line) ) continue;
    memset(tn,0x00,EEMCDbMaxName);    
    if( ! scan(line,tn,i) ) *tn=0x00; 
    packString(s->name,i,EEMCDbMaxName,tn);
    i++;
  };
  s->name   [sizeof(s->name)   -1]=0x00; // *** potential bug for last name ***
  if(comment) strncpy(s->comment,comment,EEMCDbMaxComment);
  return (i>0);
}

template <class T>
int 
EEmcDbCCIO<T>::write(FILE *f)
{
  char line[MaxLine];
  T *s = data();
  for(int i=0;i<getSize();i++) {
    char tn[EEMCDbMaxName];
    unpackString(s->name   ,i,EEMCDbMaxName   ,tn);
    if(*tn==0x00) break;
    print(line,tn,i);
    fputs(line,f);
  }
  setComment(s->comment);
  return 1;
}


// ================================================================
//
// ================================================================
template <class T> 
int 
EEmcDbQAIO<T>::read(FILE *f)
{
  int  i=0;
  char line[MaxLine];
  memset(bytePtr,0x00,bytes);
  while(fgets(line,MaxLine,f) != NULL && i<getSize()) { 
    if( ! checkLine(line) ) continue;
    //if(strlen(line)<=MinLine) continue;
    if(! scan(line,i)      ) break;
    if(comment) strncpy(data(i)->comment,comment,EEMCDbMaxComment-1);
    i++;
  }
  return i;
}

template <class T>
int 
EEmcDbQAIO<T>::write(FILE *f) 
{
  int i;
  char line[MaxLine];
  for(i=0;i<getSize();i++) {
    if(! print(line,i)    ) break;
    fputs(line,f); 
    setComment(data(i)->comment);
  }
  return i;
}

// ================================================================
//
// ================================================================
template <class T> 
int 
EEmcDbHVIO<T>::read(FILE *f)
{
  int  i=0;
  char line[MaxLine];
  memset(bytePtr,0x00,bytes);
  while(fgets(line,MaxLine,f) != NULL && i<getSize()) { 
    if( ! checkLine(line) ) continue;
    //if(strlen(line)<=MinLine) continue;
    if(! scan(line,i)     ) break;
    if(comment && data(i)->comment[0]==0) strncpy(data(i)->comment,comment,EEMCDbMaxComment-1);
    i++;
  }
  return i;
}

template <class T>
int 
EEmcDbHVIO<T>::write(FILE *f) 
{
  int i;
  char line[MaxLine];
  for(int i=0;i<getSize();i++) indexArr[i]=i; // reset index array
  for(i=0;i<getSize();i++) {
    memset(line,0x00,MaxLine);
    int r = print(line,i);
    if(r<0) break;
    if(r>0) {
      fputs(line,f); 
      setComment(data(i)->comment);
    }
  }
  return i;
}

// ================================================================
//
// ================================================================
template <class T> 
int 
EEmcDbXML<T>::read(FILE *f)
{
  char line[MaxLine];
  char *ptr=bytePtr;
  int   ret=0;
  memset(bytePtr,0x00,bytes);

  while(fgets(line,MaxLine,f) != NULL ) {
    int len=strlen(line);
    dprintf(  "%s",line);
    strncpy(ptr,line,len);
    ptr +=len;
    ret=1;
  }    
  if(comment) strncpy(data()->comment,comment,EEMCDbMaxComment-1);
  return ret;
}

template <class T>
int 
EEmcDbXML<T>::write(FILE *f) 
{
  char line[MaxLine];
  char *ptr=bytePtr;
  int len=0;
  do {
    strncpy(line,ptr,MaxLine);
    len=strlen(line);
    if(len>0) { 
      fprintf(f,"%s",line);
      dprintf(  "%s",line);
    }
    ptr+=len;
  } while(len>0);
  setComment(data()->comment);
  return 1;
}


// ================================================================
//
// ================================================================

// ==============  eemcDbADCconf ==================================
template<> 
int 
EEmcDbCCIO<eemcDbADCconf>::scan (const char *line, char *name, int i)
{ 
  int r=sscanf(line,"%s %d %d  ",name,data()->crate+i ,data()->channel+i);
  if(r>=3)dprintf(  "%s %d %d\n",name,data()->crate[i],data()->channel[i]);
  return (r>=3);
}

template<>
int 
EEmcDbCCIO<eemcDbADCconf>::print(      char *line, char *name, int i)
{
  return (sprintf(line,"%s %d %d\n",name,data()->crate[i],data()->channel[i])>=3);
}


// ==============  eemcDbPMTconf ==================================
template<>
int
EEmcDbCCIO<eemcDbPMTconf>::scan(const char *line, char *name, int i)
{
  int r=sscanf(line,"%s %d %d %d %d %d",name,
		 data()->barcode+i, data()->sn+i         , data()->baseBarcode+i,
		 data()->baseSN+i , data()->baseAddress+i);

  if(r>=6)dprintf("%s %d %d %d %d %d\n",name,
		  data()->barcode[i], data()->sn[i]      , data()->baseBarcode[i],
		  data()->baseSN[i] , data()->baseAddress[i]);

  return (r>=6);
}

template<>
int
EEmcDbCCIO<eemcDbPMTconf>::print(char *line, char *name, int i)
{
  return (sprintf(line,"%s %d %d %d %d %d \n",name,
		    data()->barcode[i], data()->sn[i]         ,data()->baseBarcode[i],
		    data()->baseSN[i] , data()->baseAddress[i])>=6);
}


// ==============  eemcDbBoxconf ==================================
template<>
int
EEmcDbCCIO<eemcDbBoxconf>::scan (const char *line, char *name, int i)
{
  int r=sscanf(line,"%s %d %d",name,data()->barcode+i, data()->hvBranch+i);
  if(r>=3)dprintf("%s %d %d\n",name,data()->barcode[i],data()->hvBranch[i]);
  return(r>=3);
}

template<>
 int
EEmcDbCCIO<eemcDbBoxconf>::print(char *line, char *name, int i)
{
  return (sprintf(line,"%s %d %d\n",name,data()->barcode[i],data()->hvBranch[i])>=3);
}

// ==============  eemcDbPMTcal ==================================
template<>
int
EEmcDbCCIO<eemcDbPMTcal>::scan (const char *line , char *name, int i)
{
  int r=sscanf(line,"%s %f %f %f"  ,    name,data()->gain+i ,data()->egain+i ,data()->hv+i);
  if(r>=3)dprintf("%s %.4f %.4f %.3f\n",name,data()->gain[i],data()->egain[i],data()->hv[i]);
  return(r>=3);
}


template<>
int
EEmcDbCCIO<eemcDbPMTcal>::print(char *line, char *name, int i)
{
  return (sprintf(line,"%s %.4f %.4f %.3f\n",name,data()->gain[i],data()->egain[i],data()->hv[i])>=3);
}

// ==============  eemcDbPMTname ==================================
template<>
int
EEmcDbCCIO<eemcDbPMTname>::scan (const char *line , char *name, int i)
{
  if(i==0) {// clear data before first use
    memset(data()->tubeName,' ',sizeof(data()->tubeName));
    data()->tubeName[sizeof(data()->tubeName)-1]=0;
  }
  char txt  [EEMCDbMaxName];
  int r=sscanf(line,"%s %s",    name,txt);
  packString(data()->tubeName,i,EEMCDbMaxName,txt);
  if(r>=2)dprintf("%s %s\n",name,txt);
  return(r>=2);
}


template<>
int
EEmcDbCCIO<eemcDbPMTname>::print(char *line, char *name, int i)
{
  char txt  [EEMCDbMaxName];
  unpackString(data()->tubeName  ,i,EEMCDbMaxName   ,txt);
  return (sprintf(line,"%s %s\n",name,txt)>=2);
}

// ==============  eemcDbPMTped ==================================
template<>
int
EEmcDbCCIO<eemcDbPMTped>::scan (const char *line, char *name, int i)
{
  int r=sscanf(line,"%s %f %f"  ,name,data()->ped+i,data()->sig+i);
  if(r>=3)dprintf(  "%s %f %f\n",name,data()->ped[i],data()->sig[i]);
  return(r>=3);
}

template<>
int
EEmcDbCCIO<eemcDbPMTped>::print(char *line, char *name, int i)
{
  return (sprintf(line,"%s %.4f %.4f\n",name,data()->ped[i],data()->sig[i])>=3);
}

// ==============  eemcDbPMTstat =================================
template<>
int
EEmcDbCCIO<eemcDbPMTstat>::scan (const char *line, char *name, int i)
{
  int r=sscanf(line,"%s %hx %hx"      ,name,data()->stat+i ,data()->fail+i);
  if(r>=3)dprintf(  "%s 0x%hx 0x%hx\n",name,data()->stat[i],data()->fail[i]);
  return(r>=3);
}

template<>
int
EEmcDbCCIO<eemcDbPMTstat>::print(char *line, char *name, int i)
{
  return (sprintf(line,"%s 0x%04hx 0x%04hx\n",name,data()->stat[i],data()->fail[i])>=3);
}

// ==============  eemcDbPIXcal ==================================
template<>
int
EEmcDbCCIO<eemcDbPIXcal>::scan (const char *line , char *name, int i)
{
  int r=sscanf(line,"%s %f %f "  ,    name,data()->gain+i ,data()->egain+i );
  if(r>=3)dprintf("%s %.4f %.4f\n",name,data()->gain[i],data()->egain[i]);
  return(r>=3);
}


template<>
int
EEmcDbCCIO<eemcDbPIXcal>::print(char *line, char *name, int i)
{
  return (sprintf(line,"%s %.4f %.4f\n",name,data()->gain[i],data()->egain[i])>=3);
}

// ==============  eemcDbPIXname ==================================
template<>
int
EEmcDbCCIO<eemcDbPIXname>::scan (const char *line , char *name, int i)
{
  if(i==0) {// clear data before first use
    memset(data()->tubeName,' ',sizeof(data()->tubeName));
    data()->tubeName[sizeof(data()->tubeName)-1]=0;
  }
  char txt  [EEMCDbMaxName];
  int r=sscanf(line,"%s %s",    name,txt);
  packString(data()->tubeName,i,EEMCDbMaxName,txt);
  if(r>=2)dprintf("%s %s\n",name,txt);
  return(r>=2);
}


template<>
int
EEmcDbCCIO<eemcDbPIXname>::print(char *line, char *name, int i)
{
  char txt  [EEMCDbMaxName];
  unpackString(data()->tubeName  ,i,EEMCDbMaxName   ,txt);
  return (sprintf(line,"%s %s\n",name,txt)>=2);
}
// ================================================================
//
// ================================================================

// ==============  eemcDbPMTchar =================================
template<>
int
EEmcDbQAIO<eemcDbPMTchar>::scan(const char *line, int i)
{
  eemcDbPMTchar *s = data(i);
  int r=sscanf(line,"%d %f %f %f",&(s->sn),&(s->speRes),&(s->nomHV),&(s->darkC));
  if(  data(i)->sn<=  0  ) return 0;   
  data(i)->comment[0]=0x00; //
  if(r>=4) dprintf("%8d %6.3f %8.3f %8.3f\n",s->sn,s->speRes,s->nomHV,s->darkC);
  return(r>=4);
}

template<>
int
EEmcDbQAIO<eemcDbPMTchar>::print(char *line, int i)
{
  eemcDbPMTchar *s = data(i);
  if(  data(i)->sn<=  0  ) return 0;   
  return (sprintf(line,"%d %.4f %.3f %.3f\n",s->sn,s->speRes,s->nomHV,s->darkC));
}

// ==============  eemcDbCWchar =================================
template<>
int
EEmcDbQAIO<eemcDbCWchar>::scan(const char *line, int i)
{
  eemcDbCWchar *s = data(i);
  int r=sscanf(line,"%d %d %f",&(s->sn),&(s->address),&(s->maxHV)); 
  if(  data(i)->sn<=  0  ) return 0;   
  s->dumm      = 0.0;
  data(i)->comment[0]=0x00; //
  if(r>=3) dprintf("%8d %8d %8.3f\n",s->sn,s->address,s->maxHV);
  return(r>=3);
}

template<>
int
EEmcDbQAIO<eemcDbCWchar>::print(char *line, int i)
{
  eemcDbCWchar *s = data(i);
  if(  data(i)->sn<=  0  ) return 0;   
  return (sprintf(line,"%d %d %.3f\n",s->sn,s->address,s->maxHV));
}


// =============================================================
//
// =============================================================

// ==============  eemcDbHVsys =================================
template<>
int
EEmcDbHVIO<eemcDbHVsys>::scan(const char *line, int i)
{
  eemcDbHVsys *s = new eemcDbHVsys;
  int r=sscanf(line,"%s %d %d %f %d %f %s",
	       s->name,&(s->hvBranch),&(s->address),&(s->maxHV),&(s->dac),&(s->hv),s->comment);
  if( (indexArr[i]=indexFromString(s->name,getSize())) < 0 ) { 
    fprintf(stderr,"EEmcDbHVIO<eemcDbHVsys>::scan():  name %s is invalid\n",s->name);
    return -1;
  }
  memcpy(data(i),s,sizeof(eemcDbHVsys));
  if(r>=6) dprintf("%8s %4d %4d %8.1f %4d %8.1f \"%s\" \n",
		   s->name,s->hvBranch,s->address,s->maxHV,s->dac,s->hv,s->comment);
  return(r>=6);
}

template<>
int
EEmcDbHVIO<eemcDbHVsys>::print(char *line, int i)
{
  eemcDbHVsys *s = data(i);
  if( s->name[0] == 0x00 ) return 0;
  int r = sprintf(line,"%s %d %d %7.3f %d %7.3f %s\n",
		  s->name,s->hvBranch,s->address,s->maxHV,s->dac,s->hv, s->comment);
  return r;

}

// ==============  eemcDbHVtemp =================================
template<>
int
EEmcDbHVIO<eemcDbHVtemp>::scan(const char *line, int i)
{
  eemcDbHVtemp *s = new eemcDbHVtemp;
  int r=sscanf(line,"%s %f %s",
	       s->name,&(s->tempC),s->comment);
  if( (indexArr[i]=indexFromString(s->name,getSize())) < 0 ) { 
    fprintf(stderr,"EEmcDbHVIO<eemcDbHVtemp>::scan():  name %s is invalid\n",s->name);
    return -1;
  }
  memcpy(data(i),s,sizeof(eemcDbHVtemp));
  if(r>=2) dprintf("%8s %8.1f \"%s\" \n",
		   s->name,s->tempC,s->comment);
  return(r>=2);
}

template<>
int
EEmcDbHVIO<eemcDbHVtemp>::print(char *line, int i)
{
  eemcDbHVtemp *s = data(i);
  if( s->name[0] == 0x00 ) return 0;
  int r = sprintf(line,"%s %7.3f %s\n",
		  s->name,s->tempC, s->comment);
  return r;

}


// ================================================================
//
// ================================================================
#if __GNUG__
template class EEmcDbCCIO<eemcDbADCconf>;
template class EEmcDbCCIO<eemcDbPMTconf>;
template class EEmcDbCCIO<eemcDbBoxconf>;
template class EEmcDbCCIO<eemcDbPMTcal> ;
template class EEmcDbCCIO<eemcDbPMTname>;
template class EEmcDbCCIO<eemcDbPMTped> ;
template class EEmcDbCCIO<eemcDbPMTstat>;
template class EEmcDbCCIO<eemcDbPIXcal> ;
template class EEmcDbCCIO<eemcDbPIXname>;

template class EEmcDbQAIO<eemcDbPMTchar>;
template class EEmcDbQAIO<eemcDbCWchar> ;

template class EEmcDbHVIO<eemcDbHVsys>  ;
template class EEmcDbHVIO<eemcDbHVtemp> ;

template class EEmcDbXML<eemcDbXMLdata> ;
#endif

// $Log: EEmcDbIO.C,v $
// Revision 1.1  2013/01/25 16:46:48  stevens4
// Scripts used to upload EEMC tables to the DB
//
// Revision 1.11  2004/01/13 16:43:20  zolnie
// allowed for inline comments
// lines starting with # will be ignored (except the first one
// which contains the signature)
// for EEmcDbCCIO, EEmcDbQAIO, EEmcDbHVIO
// but no for EEmcDbXML, KretDbBlobSIO
//
// Revision 1.10  2003/11/30 04:16:17  zolnie
// PIX dbases in eemcdb
//
// Revision 1.9  2003/10/28 21:18:48  zolnie
// updates for Run2004
//
// Revision 1.8  2003/08/20 17:10:15  zolnie
// fixed scan in eemcPMTstat
//
// Revision 1.7  2003/08/19 18:56:31  zolnie
// added PMTstat table
//
// Revision 1.6  2003/08/07 16:33:22  zolnie
// replaced confusing --noWrite/-w option with a clearer one: --dataonly/-d
//
// Revision 1.5  2003/04/11 18:04:46  balewski
// add I/O for PMTname
//
// Revision 1.4  2003/04/10 21:44:24  zolnie
// *** empty log message ***
//
// Revision 1.3  2003/04/10 15:12:48  zolnie
// new dbase changes
//
// Revision 1.2  2003/02/04 18:10:08  zolnie
// added eemcHVtemp online database
//
// Revision 1.1  2003/01/28 23:22:18  balewski
// start
//
// Revision 1.13  2003/01/28 22:34:59  zolnie
// make sure quiet is quiet (once more)
//
// Revision 1.12  2003/01/27 14:54:50  zolnie
// back at IU
//
// Revision 1.11  2003/01/25 20:09:10  balewski
// add BlobS, remove old kret*
//
// Revision 1.10  2003/01/24 20:54:32  zolnie
// merger with Jan + updates for "HVindex" stuff
//
// Revision 1.9  2003/01/24 17:11:34  balewski
// cleanup
//
// Revision 1.8  2003/01/24 16:44:48  balewski
// added WCM+someRing online info
//
// Revision 1.7  2003/01/22 02:31:57  balewski
// small fix
//
// Revision 1.6  2003/01/22 02:03:22  zolnie
// grand challenge for Jan
//
// Revision 1.5  2003/01/21 23:47:40  balewski
// HVsys table added
//
// Revision 1.4  2003/01/10 18:48:33  zolnie
// submision version
//
// Revision 1.3  2003/01/10 04:52:02  zolnie
// updates to Tcl/Tk interface (czyli Zadana Pana Jana)
//
// Revision 1.2  2003/01/03 21:14:49  zolnie
// fixed string packing in EEmcDbCCIO<T>::read(FILE *f)
// added resetString
// first version of tkEEmcDb
//
