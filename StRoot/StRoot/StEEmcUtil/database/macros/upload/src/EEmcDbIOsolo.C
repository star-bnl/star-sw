#include <stdio.h>
#include <string.h>

#include "EEmcDbIO.h"
#include "kretDbBlobS.hh"

// ================================================================
//
// ================================================================

KretDbBlobSIO::KretDbBlobSIO(int n) : EEmcDbIOBase(n,sizeof(kretDbBlobS)) {};


inline kretDbBlobS*
KretDbBlobSIO::data  (int i) { return ( (kretDbBlobS *)bytePtr + i ); };

//-----------------------------------------------------
//-----------------------------------------------------
//-----------------------------------------------------
int 
 KretDbBlobSIO::read(FILE *f)
{
  // fprintf(stdout,"Jestem w  kretBlobS file->DB\n");
  memset(bytePtr,0x00,bytes);
  kretDbBlobS *s = data();
  int i=0;
  do  {
    int c=fgetc(f); 
    if(c==EOF) break;
    s->dataS[i]=c;
    i++;
    if(i>= KRETmxBlobSlen) {
      fprintf(stderr,"%d data too  long for BlobS , ABORT\n",i);
      return 0;
    }
  } while(1);
  s->dataS[i]=0; // terminate string at EOF
  //printf("data=%s=\n",s->data);
  if(comment) strncpy(s->comment,comment,KRETDbMaxComment-1);
  return 1;
}

//-----------------------------------------------------
//-----------------------------------------------------
//-----------------------------------------------------
int 
KretDbBlobSIO::write(FILE *f) 
{
  // fprintf(stdout,"Jestem w kretBlobS  DB-->FILE\n");
  kretDbBlobS *s = data(0);
  
  int i=0;
  int k=0;
  do  { 
    int c=s->dataS[i++]; 
    if(c==0) break; // end of data
    k+=fputc(c,f); 
    if(i>= KRETmxBlobSlen) {
      fprintf(stderr,"Unexpected, too long data  %d from BlobS , STOP\n",i);
      return 0;
    }
  }  while(1);
  setComment(s->comment);
  return 1;
}

