#ifndef _RTSMOTHER_H_
#define _RTSMOTHER_H_

#include <sys/types.h>
#include <time.h>

struct SegDef
{
  int segment;
  int size;
};

struct ProcessDef {
  pid_t pid;
  time_t start_time;
  char *path;
  char *args[10];
  int restart_on_reboot;
};

int rtsMother(char *myLogName, 
	      SegDef segments[],
	      ProcessDef processes[],
	      char *out=NULL,
	      char *err=NULL);

#endif
