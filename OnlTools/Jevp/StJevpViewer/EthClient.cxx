#include "EthClient.h"

EthClient::EthClient() {
    socket = NULL;
    displayFile = NULL;
    tabs_valid = 0;
    serverTags = NULL;
}

// Only change tabs_valid if we actually succeed in connecting
bool EthClient::connectToServer(const char *server, int port) {
    // First disconnect from current server, if sensible.
    if(socket != NULL) {
	socket->Close();
	socket = NULL;
    }
  
    socket = new TSocket(server, port);
    if(socket->IsValid()) {
      tabs_valid = 0;
      return true;
    }
    
    //LOG(CRIT, "Error connecting to: %s:%d", server, port);
    delete socket;
    socket = NULL;
    
    return false;
}

bool EthClient::connected() {
  if(socket) return true;
  return false;
}

void EthClient::send(const char *cmd, const char *args) {
  //if(!socket) return;
  EvpMessage msg;
  msg.setCmd(cmd);
  msg.setArgs(args);
  msg.setSource((char *)"presenter");
  send(&msg);
}

void EthClient::send(TObject *msg) {
  if(!socket) return;
  TMessage mess(kMESS_OBJECT);
  mess.WriteObject(msg);
  socket->Send(mess);
}

TObject *EthClient::receive() {
  TMessage *mess;

  if(!socket) return NULL;

  int ret = socket->Recv(mess);
  if(ret == 0) {
    LOG("JEFF", "Error receiving over socket");
    delete socket;

    tabs_valid = 0;
    socket = NULL;
    return NULL;
  }
    
  TObject *result = (TObject *)mess->ReadObject(mess->GetClass());

  delete mess;    
  return result;
}


void EthClient::readDisplayFromServer(const char *displayName) {
  strcpy(myDisplayName, displayName);

  //  LOG("JEFF", "Reading display %s from server!", displayName);
  send("display_desc", displayName);
  EvpMessage *tabdata = dynamic_cast<EvpMessage *>(receive());
    
  if(!tabdata) {
    displayFile = new DisplayFile();
    return;
  }

  displayFile = new DisplayFile();
  displayFile->ReadBuff(tabdata->args, strlen(tabdata->args));   
  displayFile->setDisplay(displayFile->getDisplayNodeFromName(displayName));
  displayFile->updateDisplayRoot();
    
  //LOG("JEFF", "read it..");
  delete tabdata;
}

RunStatus *EthClient::getRunStatus() {
  send("GetStatus", "");
  RunStatus *rs = dynamic_cast<RunStatus *>(receive());
  if(!rs) {
    rs = new RunStatus();

    rs->timeOfLastChange = lastRunStatus.timeOfLastChange;
    if(lastRunStatus.run != -1) {
      rs->timeOfLastChange = time(NULL);
    }
    rs->run = -1;
    rs->setStatus("unconnected");
  }

  memcpy(&lastRunStatus, rs, sizeof(RunStatus));
  return rs;
}

// NULL if not a page
DisplayNode *EthClient::getTabDisplayBranchOrLeaf(u_int combo_idx) {
  if(!displayFile) return NULL;
  return displayFile->getTab(combo_idx);
}

DisplayNode *EthClient::getTabDisplayBranch(u_int combo_idx) {
  if(!displayFile) return NULL;
  DisplayNode *node = displayFile->getTab(combo_idx);
  if(!node) return NULL;
  if(node->leaf) return NULL;
  return node;
}

DisplayNode *EthClient::getTabDisplayLeaf(u_int combo_idx) {
  if(!connected()) {
    if(combo_idx != 1) return NULL;
    
    DisplayNode *node = new DisplayNode();
    node->setName("noserver");
    return node;
  }
  
  if(!displayFile) return NULL;

  DisplayNode *node = displayFile->getTab(combo_idx);
  if(!node) return NULL;
  if(!node->leaf) return NULL;
  return node;
}

char *EthClient::getTabName(u_int combo_idx) {
  if(!displayFile) return NULL;
  DisplayNode *node = displayFile->getTab(combo_idx);
  if(!node) {
    LOG("JEFF", "Null in getTabName");
    return NULL;
  }
  return node->name;
}

int EthClient::updateServerTags() {  // returns zero if no change...

  if(!connected()) {    
    return !tabs_valid;
  }


  int ret = 0;
  send("getServerTags","");
  EvpMessage *msg = dynamic_cast<EvpMessage *>(receive());
  if(!msg) {
    LOG(ERR, "Can't get server tags for %s\n", myDisplayName);
    return 1;
  }
  const char *args = msg->getArgs();
  if(!serverTags || (strcmp(serverTags, args) != 0)) {
    
    //LOG("JEFF", "Changing serverTags from %p to %p", serverTags, args);
    //    
    //LOG("JEFF", "a");
    if(serverTags) {
      free(serverTags);
    }
    
    //LOG("JEFF", "a");
    serverTags = (char *)malloc(strlen(args) + 1);
    //LOG("JEFF", "a");
    strcpy(serverTags, args);
    // LOG("JEFF", "a");
    display()->setServerTags(serverTags);
    //LOG("JEFF", "a");
    display()->updateDisplayRoot();
    
    ret = 1;
  }
  
  delete msg;
  return ret;
}

