/*    daqRunTag.h
*
*  Table: daqRunTag
*
*       description: //  Daq input to Run-TagDb
*/ 

#ifndef __daqRunTag__H
#define __daqRunTag__H

struct daqRunTag { 

   unsigned int  run;   
   unsigned int  whichtag;              /*   0 start, 1 stop  */
   unsigned int  startTime;             /*   NULL if stoprun  */
   unsigned int  stopTime;               /*   NULL if startrun  */
   unsigned int  runType;            
   unsigned int  totalNumberOfEvents;  
  unsigned int  whichEvb;
 };  
#endif 
