/***************************************************************************
 *
 * $Id: StDbTime.h,v 1.5 2002/01/30 15:40:48 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Time class for "unix & date-time" timestamp access to DB
 *
 ***************************************************************************
 *
 * $Log: StDbTime.h,v $
 * Revision 1.5  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.4  2000/02/15 20:27:45  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.3  2000/01/27 05:54:35  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.2  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.1  1999/09/30 02:06:11  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBTIME_H
#define STDBTIME_H

#include <string.h>


class StDbTime {

public:

unsigned int munixTime;
char* mdateTime;

  StDbTime(): munixTime(0), mdateTime(0) {};
  StDbTime(unsigned int utime): mdateTime(0) { munixTime = utime;};

  StDbTime(const char* dtime): munixTime(0) { if(dtime){
                                         mdateTime=new char[strlen(dtime)+1]; 
                                         strcpy(mdateTime,dtime);
                                         } else {mdateTime=0;}};

  StDbTime(StDbTime& time)  { munixTime=0; mdateTime=0;
  if(time.mdateTime){
    mdateTime=new char[strlen(time.mdateTime)+1];
    strcpy(mdateTime,time.mdateTime);
  }
  munixTime=time.munixTime;
  }
  ~StDbTime() { if(mdateTime) delete [] mdateTime; }

  void setUnixTime(unsigned int utime) { munixTime = utime;}
  void setDateTime(const char* dtime) { if(!dtime) return;
                                  if(mdateTime) delete [] mdateTime;
                                  mdateTime=new char[strlen(dtime)+1]; 
                                  strcpy(mdateTime,dtime); };

  void setTime(unsigned int utime, const char* dtime){ 
                                             setUnixTime(utime);
                                             setDateTime(dtime);}
  unsigned int getUnixTime() { return munixTime; }
  char* getDateTime(){ return mdateTime; }

};


#endif







