#ifndef STAR_evpServerMain
#define STAR_evpServerMain

#include "TObject.h"

 class EvpServer;

class evpServerMain : public TObject {
       static EvpServer  *mEvpServer;
  public:
       evpServerMain(){;}
       virtual ~evpServerMain(){;}
       static int main(int argc, const char* argv[]);

    ClassDef(evpServerMain,0);
};

#endif
