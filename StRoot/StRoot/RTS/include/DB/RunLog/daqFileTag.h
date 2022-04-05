/*    daqFileTag.h
*
*  Table: daqFileTag
*
*       description: //  Daq input to FileList
*/ 

#ifndef __daqFileTag__H
#define __daqFileTag__H

struct daqFileTag { 

  unsigned int  run;             /* unique run number  */
  unsigned int  beginEvent;      /* 1st event number in file  */
  unsigned int  endEvent;        /* last event number in file  */
  unsigned int  numberOfEvents;  /* total number of events  */
  unsigned int  fileSequence;    /* sequence part of filename  */
  char  file[256];               /* unique file name without (hpss) path  */
  unsigned int  hpss;            /* is the data at hpss? */  
  
  unsigned int  fileStream;      /* new for version 10.0, stream part of filename */

  // New 2/18/09
  int   isCombo;                 // is this file combined in another 
  char  comboFile[256];          // filename for comboFile

};  
#endif 
