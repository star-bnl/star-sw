
#include "StFarmSpy.h"


int StFarmSpy::fgError  = -1998;
TSocket *StFarmSpy::fgSocket = NULL;

//	StFarmSpy IMPLEMENTATION

StFarmSpy::StFarmSpy(const char *host, int port)
{
   if (fgSocket) {//Double init
     printf("StFarmSpy:: Warning: Double Init, Ignored\n");
     return;}
   
   fgSocket = new TSocket(host,port);
   fgError = (! fgSocket->IsValid());
   if (!fgError) {
     printf("Socket opened \n"); 
   } else {
     //     printf("*** Socket FAILED***\n");
     printf("*** Warning: Root spy system is not running ***\n");
     fgError = 1998;
   };
}
void StFarmSpy::StartJob(const char *JobName         
   	       ,const char *InputData,const char *OuputData
               ,int Ierr,const char *Comm)
{ 
  char buf[1024]; char *cc = buf;
  if (fgError) return;
  int JobId = gSystem->GetPid();
  
  const char *user = gSystem->HomeDirectory();
  const char *pwd  = gSystem->pwd();
  
  cc += sprintf(cc,"StartJob");
  cc += sprintf(cc,"(\"%s\"",JobName);
  cc += sprintf(cc,",%d",JobId);
  cc += sprintf(cc,",\"%s\"",user);
  cc += sprintf(cc,",\"%s\"",pwd );
  cc += sprintf(cc,",\"%s\"",InputData );
  cc += sprintf(cc,",\"%s\"",OuputData );
  cc += sprintf(cc,",%d",Ierr);
  cc += sprintf(cc,",\"%s)\"",Comm );
  cc += sprintf(cc,");");
  fgError = ( fgSocket->Send(buf) <0) ;
}  
void StFarmSpy::EndJob(Int_t Ierr, const char *Com)
{   
  char buf[1024]; 
  if (fgError) return;
  sprintf(buf,"EndJob(%d,\"%s\");",Ierr,Com);
  fgError = ( fgSocket->Send(buf) <0) ;
}  

void StFarmSpy::NewEvent(Int_t Run,Int_t Event)
{
  char buf[80]; 
  if (fgError) return;
  double cpu = (double)clock()/CLOCKS_PER_SEC*1000;
  sprintf(buf,"NewEvent(%d,%d,%20.3e);",Run,Event,cpu);
  fgError = ( fgSocket->Send(buf) <0) ;

}

void StFarmSpy::EndEvent(Int_t Run,Int_t Event, int Ierr,const char *Comm)
{
  char buf[1024]; 
  if (fgError) return;
  double cpu = (double)clock()/CLOCKS_PER_SEC*1000;
  sprintf(buf,"EndEvent(%d,%d,%20.3e,%d,\"%s\");",Run,Event,cpu,Ierr,Comm);
  fgError = ( fgSocket->Send(buf) <0) ;
}
  
void StFarmSpy::Comment(const char *Comm)
{
  char buf[1024]; 
  if (fgError) return;
  sprintf(buf,"Comment(\"%s\");",Comm);
  fgError = ( fgSocket->Send(buf) <0) ;
}
void StFarmSpy::Remark(const char *Comm)
{
  char buf[1024]; 
  if (fgError) return;
  sprintf(buf,"Remark(\"%s\");",Comm);
  fgError = ( fgSocket->Send(buf) <0) ;
}
  
  
