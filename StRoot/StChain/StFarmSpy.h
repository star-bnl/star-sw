#ifndef _StFarmSpy_hh_
#define _StFarmSpy_hh_ 1998
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "TROOT.h"
#include "TSystem.h"
#include "TNamed.h"
#include "TSocket.h"
#include "TDatime.h"

//#ifdef __CINT__

//#pragma link off all globals;
//#pragma link off all classes;
//#pragma link off all functions;

//#pragma link C++ class StFarmSpy;
//#endif

class StFarmSpy {

public:
  StFarmSpy(const char *host="sol.star.bnl.gov",int port=9090);
  ~StFarmSpy(){delete fgSocket;};
  static void Close(){delete fgSocket;fgError=-1;};
  static void StartJob(const char *JobName         
   	              ,const char *InputData="",const char *OuputData=""
                      ,int Ierr=0,const char *Comm ="");
  static void EndJob(Int_t Ierr=0,const char *Comm="");
  static void NewEvent(Int_t Run,Int_t Event);
  static void EndEvent(Int_t Run,Int_t Event, int Ierr,const char *Comm="");
  static void Comment(const char *Com);
  static void Remark (const char *Rem);

//Data members

  static TSocket *fgSocket;	// Spy socket
  static int fgError;		// Spy error, if non zero all calls ignored

};
#endif /*_StFarmSpy_hh_*/
