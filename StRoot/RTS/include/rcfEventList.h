#ifndef RCFEVENTLIST_H
#define RCFEVENTLIST_H

#define RCF_MAX_EVENTS 1000

struct rcfEvent 
{
  int size;
  char *ptr;     /* This is calculated from shm + offset */
};

struct rcfEventList
{
  struct rcfEvent event[RCF_MAX_EVENTS];
  int size;
  int nEvts;
  char filename[255];
  int append;
};

#endif

