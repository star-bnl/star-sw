// $Id: SpinDbIO.C,v 1.1 2005/09/30 23:47:48 balewski Exp $:

#include <stdlib.h>
#include <stdio.h>

#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <ctype.h>

#include "SpinDbIO.h"
#include "spinDbAPI.h"

#include "spinDbV124.hh"
#include "spinDbStar.hh"
#include "spinDbBXmask.hh"

// ================================================================
// 
// ================================================================
SpinDbIOBase::SpinDbIOBase (int nelem, int size)
{
  bytes    = nelem*size;
  nElem    = nelem;
  bytePtr  = new char[bytes];
  indexArr = new int[nElem];
  comment  = NULL;
  for(int i=0;i<nElem;i++) indexArr[i]=i; // default index
}

SpinDbIOBase::~SpinDbIOBase() { 
  if(bytePtr ) delete [] bytePtr ; 
  if(indexArr) delete [] indexArr; 
};
 // =======================================================
// ========================================================

SpinDbV124IO::SpinDbV124IO(int n) : SpinDbIOBase(n,sizeof(spinDbV124)) {};

inline spinDbV124*
SpinDbV124IO::data  (int i) { return ( (spinDbV124 *)bytePtr + i ); };

//-----------------------------------------------------
//-----------------------------------------------------
int 
SpinDbV124IO::read(FILE *f) {
  // fprintf(stdout,"Jestem w SpinDbV124IO  file->DB\n");
  memset(bytePtr,0x00,bytes);
  spinDbV124 *t = data();

  int i=0;
  int nV=0;
  for(i=0;i<SPINDbMaxRing;i++) nV+=fscanf(f,"%d",&t->bucketOffset[i]);
  for(i=0;i<SPINDbMaxRing;i++) nV+=fscanf(f,"%d",&t->rotatorState[i]);
  if(nV!=4) return 0; //crash it
  //  fprintf(stdout,"Read in %d values\n",nV);

  for(i=0;i<SPINDbMaxBuckets;i++) {
    char cx[10];
    int ix;
    nV=fscanf(f,"%8s %d ",cx,&ix);
    // fprintf(stdout,"%8s %d %d\n",cx,ix,nV);
    if(nV!=2) return 0; //crash it
    if(ix!=i+1) return 0; //crash it
    int j;
    unsigned char val=0;
    for(j=0;j<8;j++) {
      if(cx[j]=='1') val+=(1<<(7-j));
      else if(cx[j]!='0')return 0; //crash it
      // fprintf(stdout,"j=%d, c=%c val=0x%x\n",j,cx[j],val);
    }
    t->v124bits[i]=val;
    //fprintf(stdout,"tb=%d, val=0x%x\n",ix,val);
    //if(ix>=3) break; // tmp
  }

  if(comment) strncpy(t->comment,comment,SPINDbMaxComment-1);

  //write(stdout);   fprintf(f,"\ncomm=%s=\n",t->comment);// tmp
  return 1;

}

//-----------------------------------------------------
//-----------------------------------------------------
int 
SpinDbV124IO::write(FILE *f) {
  // fprintf(stdout,"Jestem w  SpinDbV124IO DB-->FILE\n");
  spinDbV124 *t = data(0);
  int i=0;

  for(i=0;i<SPINDbMaxRing;i++)   fprintf(f,"%d ",t->bucketOffset[i]);
  fprintf(f,"\n");
  for(i=0;i<SPINDbMaxRing;i++)   fprintf(f,"%d ",t->rotatorState[i]);
  fprintf(f,"\n");
  for(i=0;i<SPINDbMaxBuckets;i++) {
    unsigned char val= t->v124bits[i];
    int j;
    for(j=0;j<8;j++) {
      int bb=0;
      if(val& (1<<(7-j))) bb=1;
      fprintf(stdout,"%d",bb);
    }
    fprintf(stdout," %3d\n",i+1);
  }
  setComment(t->comment);
  return 1;
}


// =========================================================
// =========================================================

SpinDbStarIO::SpinDbStarIO(int n) : SpinDbIOBase(n,sizeof(spinDbStar)) {};


inline spinDbStar*
SpinDbStarIO::data  (int i) { return ( (spinDbStar *)bytePtr + i ); };

//-----------------------------------------------------
//-----------------------------------------------------
int 
SpinDbStarIO::read(FILE *f) {
  // fprintf(stdout,"Jestem w SpinDbStarIO  file->DB\n");
  memset(bytePtr,0x00,bytes);
  spinDbStar *t = data();
  memset(t,0,sizeof(spinDbStar));

  int nV=fscanf(f,"%d %d",&t->bXoff7, &t->bXoff48);
  if(nV!=2) return 0; //crash it
  fprintf(stdout,"Read in %d values\n",nV);

  if(comment) strncpy(t->comment,comment,SPINDbMaxComment-1);

  return 1;

}

//-----------------------------------------------------
//-----------------------------------------------------
int 
SpinDbStarIO::write(FILE *f) {
  // fprintf(stdout,"Jestem w  SpinDbStarIO DB-->FILE\n");
  spinDbStar *t = data(0);
 
  fprintf(f,"%d %d\n",t->bXoff7, t->bXoff48);
  setComment(t->comment);
  return 1;
}




// =======================================================
// =======================================================

SpinDbBXmaskIO::SpinDbBXmaskIO(int n) : SpinDbIOBase(n,sizeof(spinDbBXmask)) {};


inline spinDbBXmask*
SpinDbBXmaskIO::data  (int i) { return ( (spinDbBXmask *)bytePtr + i ); };

//-----------------------------------------------------
//-----------------------------------------------------
int 
SpinDbBXmaskIO::read(FILE *f) {
  fprintf(stdout,"Jestem w SpinDbBXmaskIO  file->DB\n");
  memset(bytePtr,0x00,bytes);
  spinDbBXmask *t = data();
  memset(t,0,sizeof(spinDbBXmask));
  int nV=0;
  do  {
    int val;
    int ret=fscanf(f,"%d ",&val);
    if(ret==EOF) break;
    if(ret!=1) return 0; //crash it
    if(val<0 || val>=SPINDbMaxBXings) return 0; //crash it
    t->bXmask[val]=1;
    nV++;
  } while(1);

  fprintf(stdout,"Read in %d values\n",nV);

  if(comment) strncpy(t->comment,comment,SPINDbMaxComment-1);

  write(stdout);   fprintf(f,"\ncomm=%s=\n",t->comment);// tmp
  return 1;

}

//-----------------------------------------------------
//-----------------------------------------------------
int 
SpinDbBXmaskIO::write(FILE *f) {
  fprintf(stdout,"Jestem w  SpinDbBXmaskIO DB-->FILE\n");
  spinDbBXmask *t = data(0);
  int i,k=0;
  for(i=0;i<SPINDbMaxBXings;i++){
    if(t->bXmask[i]==0) continue;
    k++;
    fprintf(f,"%d ",i);
    if(k%16==0)fprintf(f,"\n");
  }
  fprintf(f,"\n");
  setComment(t->comment);
  return 1;
}




// $Log: SpinDbIO.C,v $
// Revision 1.1  2005/09/30 23:47:48  balewski
// start
//
