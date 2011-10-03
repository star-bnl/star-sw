#ifndef __aroot__
#define __aroot__

class aroot 
{ public:
    aroot();
    virtual ~aroot(){}; 
    static void kuexec (const char* command);
    void   operator () (const char *command = "help") {kuexec (command);}
  ClassDef(aroot,0)
};
extern aroot &kuexec;  // export <kuexec> object global pointer to CINT
#endif


