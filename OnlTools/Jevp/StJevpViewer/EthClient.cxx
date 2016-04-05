#include "EthClient.h"

EthClient::EthClient() {
    socket = NULL;
    displayFile = NULL;
}

void EthClient::connectToServer(const char *server, int port) {
    // First disconnect from current server, if sensible.
    if(socket != NULL) {
	socket->Close();
	socket = NULL;
    }
  
    socket = new TSocket(server, port);
    if(socket->IsValid()) {
	return;
    }

    LOG(CRIT, "Error connecting to: %s:%d", server, port);
    delete socket;
    socket = NULL;
    exit(0);
}

void EthClient::send(const char *cmd, const char *args) {
    EvpMessage msg;
    msg.setCmd(cmd);
    msg.setArgs(args);
    msg.setSource((char *)"presenter");
    send(&msg);
}

void EthClient::send(TObject *msg) {
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(msg);
    socket->Send(mess);
}

TObject *EthClient::receive() {
    TMessage *mess;

    int ret = socket->Recv(mess);
    if(ret == 0) {
	LOG(CRIT, "Error reading");
	exit(0);
    }
    
    TObject *result = (TObject *)mess->ReadObject(mess->GetClass());

    delete mess;    
    return result;
}


void EthClient::readDisplayFromServer(const char *displayName) {
    LOG(DBG, "Reading display %s from server!", displayName);

    send("display_desc", displayName);
    EvpMessage *tabdata = dynamic_cast<EvpMessage *>(receive());
    
    if(!tabdata) {
	LOG(CRIT, "Didn't get a DisplayFile class");
	exit(0);
    }

    displayFile = new DisplayFile();
    displayFile->ReadBuff(tabdata->args, strlen(tabdata->args));   
    displayFile->setDisplay((char *)displayName);
    
    delete tabdata;
}

RunStatus *EthClient::getRunStatus() {
    send("GetStatus", "");
    RunStatus *rs = dynamic_cast<RunStatus *>(receive());

    if(!rs) {
	LOG(CRIT, "Didn't get run status");
	exit(0);
    }

    return rs;
}

// NULL if not a page
DisplayNode *EthClient::getTabDisplayNode(u_int combo_idx) {
    DisplayNode *node = displayFile->getTab(combo_idx);
    if(!node) return NULL;
    if(!node->leaf) return NULL;
    return node;
}

char *EthClient::getTabName(u_int combo_idx) {
    DisplayNode *node = displayFile->getTab(combo_idx);
    if(!node) return NULL;
    return node->name;
}



