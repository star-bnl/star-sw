// Utility functions for run configuration info

#include <RC_Config.h>
#include <rtsSystems.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <rtsLog.h>
#include "cfgutil.h"

// seems not thread safe,
// but this static stuff is fine
// because the same initialization is OK for all readers...
static int init=0;
static char evpgroups[32][40];

int getRccnf(const char *fn, rccnf *desc)
{
  char buff[80];

  //printf("Hello\n");

  memset(desc, 0, sizeof(rccnf));

  FILE *f = fopen(fn, "ro");
  if(!f) return -1;
  
  if(fscanf(f,"run %d\n",&desc->run) != 1) {
    fclose(f);
    return -1;
  }

  if(fscanf(f,"detmask 0x%x\n",&desc->detMask) != 1) {
    printf("Blah\n");
    fclose(f);
    return -1;
  }

  desc->detMask |= (1<<TRG_ID);
  desc->detMask |= (1<<SC_ID);
  desc->detMask &= ~(1<<DAQ_ID);

  // skip a bunch
  int found=0;
  while(fgets(buff,80,f)) {
    if(strstr(buff, "evpgroups:") != buff) continue;
    found = 1;
    break;
  }
  
  if(!found) {
    fclose(f);
    return -1;
  }
  
  int in1,in2;

  while(fscanf(f,"%s %d %d\n",buff,&in1,&in2) == 3) {
    desc->grpMask |= (1<<in1);
  }

  fclose(f);
  return 0;
}

void initEvpGroups()
{
  char str[80],str2[80];
  memset(evpgroups, 0, sizeof(evpgroups));
  int id;

  FILE *f = fopen("/RTS/conf/handler/evpGroups.txt", "ro");
  if(!f) return;

  while(fgets(str, 80, f) == str) {
    if(str[0] == '#') continue;

    sscanf(str, "%d %s\n",&id, str2);

    strcpy(evpgroups[id],str2);    
  }

  printf("Bye\n");

  fclose(f);
  init = 1;
}


// single det
int s2did(char *str)
{
  for(int i=0;i<32;i++) {
    const char *t = rts2name(i);
    if(t) {
      if(strcasecmp(t,str) == 0) return i;
    }
  }

  return -1;
}

// comma delimited
UINT32 str2detmask(const char *str)
{
    char *_strtok_static;

    char s[256];
    UINT32 mask = 0;
    int id;

    strcpy(s,str);
    char *t = strtok_r(s,",",&_strtok_static);

    while(t) {
	id = s2did(t);
	if(id >= 0) {
	    mask |= (1<<id);
	}
	else {
	    LOG(DBG, "Invalid detector: %s",t,0,0,0,0);
	}
   
	t = strtok_r(NULL,",",&_strtok_static);
    }
  
    return mask;
}

// single group
int s2gid(const char *str)
{
  for(int i=0;i<32;i++) {
    if(strcasecmp(str,evpgroups[i]) == 0) return i;
  }

  return -1;
}

// comma delimited
UINT32 str2evpgroupmask(const char *str)
{
    char *_strtok_static;
    char s[256];
    UINT32 mask = 0;
    int id;

    if(init == 0) initEvpGroups();

    strcpy(s,str);
    char *t = strtok_r(s,",",&_strtok_static);

    while(t) {
	id = s2gid(t);

	if(id >= 0) {
	    mask |= (1<<id);
	}
	else {
	    LOG(ERR, "Invalid evpgroup: %s",t,0,0,0,0);
	}
   
	t = strtok_r(NULL,",",&_strtok_static);
    }
  
    return mask;
}

#ifdef DEBUG_UTIL

int main(int argc, char *argv[])
{
  rtsLogLevel(DBG);
  rtsLogOutput(RTS_LOG_STDERR);

  rccnf x;

  getRccnf("0.txt",&x);

  printf(":::%d : 0x%x : 0x%x\n",x.run,x.detMask,x.grpMask);

  LOG(ERR, "Hello",0,0,0,0,0);
  char *str = "tpc,daq,trg,svt,horse";
  char *str2 = "pedestal,b1,b3,b4";
  printf("dets:    %s  0x%x\n",str,str2detmask(str));
  printf("evpgrps: %s  0x%x\n",str2,str2evpgroupmask(str2));
}

#endif
