/* STAR browser per VAT conference.  May 16 1995, Herb Ward. */
/***********************************************************  INCLUDES  **/
#include <stdio.h>
#include <math.h>
#include <strings.h>
#include <stdlib.h>
#include "dstype.h"
#include "dsxdr.h"
/* #include "ds.h" */
#define EXTERN extern
#include "brow.h"
#include "cuts.h"
/***********************************************************  FUNCTIONS  **/
void GetRidOfWindows(void);
void CreateDsPointer(char *fn,FILE **ff,XDR *xdr,DS_DATASET_T **dsPtr) {
  if ((*ff = fopen(fn, "rb")) == NULL) {
    PP"I cannot read \"%s\".  This should be a xdf file.\n",fn); exit(2);
  }
  xdrstdio_create(xdr, *ff, XDR_DECODE);
  if(!xdr_dataset(xdr,dsPtr)) Err(  3);
}
void tbrNewDSView(DS_DATASET_T**,long);
void main(argc,argv) int argc; char **argv; {
  FILE *stream; XDR xdr; DS_DATASET_T *dsPtr=NULL;
  if(argc!=2||!strcmp(argv[1],"help")) {
    PP"Usage %s myfile.xdf\n",argv[0]); exit(2);
  }
  CreateDsPointer(argv[1],&stream,&xdr,&dsPtr);


                        tbrNewDSView(&dsPtr,1);


  /* 6-28-95 this brings up the series of three window
  types.  Later the first two will be merged, and the result of this merge
  will even show columns, though the columns will be for display (not click-
  ing). */
  PP"End of table browser (standalone mode).\n");
  exit(0);
}
