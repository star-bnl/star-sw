#include "EvpMessage.h"

ClassImp(EvpMessage);

void EvpMessage::setCmd(const char *cmd) {
  if(this->cmd) delete [] this->cmd;
  if(cmd == NULL) cmd = "";
  int len = strlen(cmd);
  this->cmd = new char[len+1];
  strcpy(this->cmd, cmd);
}

void EvpMessage::setArgs(const char *args) {
  if(this->args) delete [] this->args;
  if(args == NULL) args = "";
  int len = strlen(args);
  this->args = new char[len+1];
  strcpy(this->args, args);
} 

void EvpMessage::setSource(const char *source) {  
  if(this->source) delete [] this->source;
  if(source == NULL) source = "";
  int len = strlen(source);
  this->source = new char[len+1];
  strcpy(this->source, source);
}

EvpMessage::EvpMessage() {
  cmd = NULL;
  args = NULL;
  source = NULL;
}

EvpMessage::~EvpMessage() {
  if(cmd != NULL) delete [] cmd;
  if(args != NULL) delete [] args;
  if(source != NULL) delete [] source;
}
  
