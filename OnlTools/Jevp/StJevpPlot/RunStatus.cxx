#include "RunStatus.h"
#include <rtsLog.h>

ClassImp(RunStatus) ;

RunStatus::RunStatus()
{
  run = 0;
  status = NULL; 
  setStatus("unknown");
}

RunStatus::~RunStatus()
{
  if(status) delete status;
}

void RunStatus::setStatus(const char *s)
{
  if(status) delete status;
  status = new char[strlen(s)+1];
  strcpy(status, s);
}

void RunStatus::dump()
{
  LOG("JEFF","Run Status----------------");
  LOG("JEFF", "run=%d",run);
  LOG("JEFF", "status=%s",status);
  LOG("JEFF", "--------------------------");
}

int RunStatus::getNumericStatus(char *str)
{
  static char *statname[5] = { "unknown", "starting", "running", "stopping", "stopped" };

  for(int i=0;i<5;i++) {
    if(strcmp(str, statname[i]) == 0) return i;
  }
  return 0;
}

