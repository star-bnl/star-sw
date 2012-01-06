#ifndef _EVPMESSAGE_H_
#define _EVPMESSAGE_H_

#include <TROOT.h>

class EvpMessage : public TObject {
 public:
  char *source;
  char *cmd;
  char *args;

  void setSource(const char *source);
  void setCmd(const char *cmd);
  void setArgs(const char *args);

  const char *getSource() { return source ? source : ""; }
  const char *getCmd() { return cmd ? cmd : ""; }
  const char *getArgs() { return args ? args : ""; }

  EvpMessage();
  ~EvpMessage();
  
  ClassDef(EvpMessage, 1);
};


#endif
