#include "Name.h"

#include <cstring>
#include <cstdio>

// definitions

void
setName(char* buf,const char* base,
	int bin,int weight,const char* pm)
{

  if(pm){
    sprintf(buf,"bin%d.weight%d.%s.%s",bin,weight,pm,base);
  }
  else{
    sprintf(buf,"bin%d.weight%d.%s",bin,weight,base);
  }
}

void 
setNameWeight(char* buf, const char* base,int bin, const char* pm)
{
  if(pm){
    sprintf(buf,"bin%d.weight.%s.%s",bin,pm,base);
  }
  else{
    sprintf(buf,"bin%d.weight.%s",bin,base);
  }

}

void
setName(char* buf,const char* base,
	int bin,const char* pm)
{

  if(pm){
    sprintf(buf,"bin%d.%s.%s",bin,pm,base);
  }
  else{
    sprintf(buf,"bin%d.%s",bin,base);
  }
}


void 
setName(char* buf, const char* base,const char* ew,const char* pm)
{
  sprintf(buf,"%s.%s.%s",ew,pm,base);
}

void
setName(char* buf, const char* base,const char* pm)
{
  sprintf(buf,"%s.%s",pm,base);
}
