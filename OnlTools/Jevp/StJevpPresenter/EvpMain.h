#ifndef _EVPMAIN_H_
#define _EVPMAIN_H_

#include "Jevp/StJevpServer/EvpConstants.h"
#include "TObject.h"

extern const char *presenterCurrFile;
extern int presenterCurrLine;

#define CP presenterCurrFile=__FILE__;presenterCurrLine=__LINE__

class EvpMain : public TObject 
{
 private:
  EvpMain() {
    server = (char *)"evp.starp.bnl.gov";
    serverport = JEVP_PORT;
    display = (char *)"shift";
    displayFile = 0;
  }  

 public:

  char *currFile;
  int currLine;

  // Argument values
  char *server;
  int serverport;
  char *display;    // file if no server, otherwise label
  int displayFile;

  int parseArgs(int argc, char *argv[]);

  virtual ~EvpMain(){}
  static int _main(int argc, char **argv );
  static int main(char *args);
  ClassDef(EvpMain,0);
};

extern EvpMain *evpMain;

#endif
