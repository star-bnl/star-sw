/** 
 * @file  StiTimer.h
 */
#ifndef StiTimer_H
#define StiTimer_H 1
#include "TStopwatch.h"
class TList;


/** 
 * @class StiTimer
 * @brief Definition of toolkit
 */
class StiTimer 
{
public:
static void Init (const char *name,TStopwatch *&sw,int &tally);
static void Print(const char *option="");
static void Clear(const char *option="");

private:
static TList *fgList;

public:
static TStopwatch *fgFindTimer;
static int         fgFindTally;
};




#endif

