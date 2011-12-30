#include "BuilderStatus.h"
#include <rtsLog.h>

ClassImp(BuilderStatus) ;

BuilderStatus::BuilderStatus()
{
  name = NULL;
  status = NULL;
  setStatus("stopped");
  setName("none");
  run = 0;
  lastEventTime = 0;
  events = 0;
  //detectorsNeeded = 0;
  //official = 0;
  //sockid = 0;
}

BuilderStatus::~BuilderStatus()
{
  if(name) delete name;
  if(status) delete status;
}

void BuilderStatus::setName(const char *s)
{
  if(name) delete name;
  int l = strlen(s);
  name = new char[l+1];
  strcpy(name, s);
}

void BuilderStatus::setStatus(const char *s)
{ 
  if(status) delete status;
  int l = strlen(s);
  status = new char[l+1];
  strcpy(status, s);
}
