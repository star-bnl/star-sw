/* STAR browser per VAT conference.  May 16 1995, Herb Ward. */
/***********************************************************  INCLUDES  **/
#include <stdio.h>
#include <math.h>
#if !defined(sun) && !defined(WIN32)
#include <strings.h>
#else
#include <string.h>
#endif /*sun*/
#include <stdlib.h>
#include "dstype.h"
#include "dsxdr.h"
/* #include "ds.h" */
#define EXTERN extern
#include "brow.h"
#include "dscuts.h"
/***********************************************************  FUNCTIONS  **/
void GetRidOfWindows(void);
#define MODULUS 10
void CreateDsPointer(int eventNumber,char *fn,FILE **ff,XDR *xdr,
      DS_DATASET_T **dsPtr) {
  int ii,los=0;
  if ((*ff = fopen(fn, "rb")) == NULL) {
    PP"I cannot read \"%s\".  This should be a xdf file.\n",fn); exit(2);
  }
  xdrstdio_create(xdr, *ff, XDR_DECODE);
  if(eventNumber>=MODULUS) PP"Scanning file, on event number:\n");
  for(ii=0;ii<=eventNumber;ii++) {
    if(ii%MODULUS==0&&ii>0) { PP"%4d ",ii); fflush(stdout); los++; }
    if(los>=15) { los=0; PP"\n"); }
    if(!xdr_dataset(xdr,dsPtr)) {
      PP"\nERROR:  \007Did not find event number %d.\n",eventNumber);
      if(ii>0) PP"The largest event number in the file is %d.\n",ii-1);
      PP"Ie, the file contains %d events.\n",ii);
      exit(2);
    }
  }
  if(los>0) PP"\n");
}
void tbrNewDSView(DS_DATASET_T**,long);
void main(argc,argv) int argc; char **argv; {
  FILE *stream; XDR xdr; DS_DATASET_T *dsPtr=NULL;
  if(argc!=3||!strcmp(argv[1],"help")) {
    PP"----------------------------------------------------------------\n");
    PP"Usage %s myfile.xdf eventnumber\n",argv[0]);
    PP"The eventnumber starts at 0, but the first data event is number 1:\n");
    PP"        header \"event\" 0\n");
    PP"     first data event  1\n");
    PP"    second data event  2\n");
    exit(2);
  }
  CreateDsPointer(atoi(argv[2]),argv[1],&stream,&xdr,&dsPtr);


                        tbrNewDSView(&dsPtr,1);


  /* 6-28-95 this brings up the series of three window
  types.  Later the first two will be merged, and the result of this merge
  will even show columns, though the columns will be for display (not click-
  ing). */
  PP"End of table browser (standalone mode).\n");
  exit(0);
}
