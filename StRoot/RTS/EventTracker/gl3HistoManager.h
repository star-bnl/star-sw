#ifndef GL3HISTOMANAGER_H
#define GL3HISTOMANAGER_H

#include "gl3Histo.h"
#include <list>

class gl3HistoContainer {
 public:
    int nBytes;
    int nHistos;
    int runNumber ;
    int buffer;
};

class gl3HistoManager
{
 public:
    static gl3HistoManager * instance();

    int listenAtPort(int port=3333);
    int checkRequest();

    void add(gl3Histo *histo);
    void remove(gl3Histo *histo);

    int  resetHistos();
    int  saveHistos(char * filename);

 protected:
    gl3HistoManager();

    int  writeHistos (char *buffer) ;

 private:
    // instance for singleton mechanism
    static gl3HistoManager *pinstance;

    // list of histograms to be managed
    std::list<gl3Histo *> histoList;

    // general controls
    enum {crit, oper, err, warn, note, dbg } verbosity;
    //CRIT, OPER, ERR, WARN, NOTE, DBG}  verbosity;

    // network request related information
    bool is_listening; 
    int socketFd;
    int remoteSocket ;

    // max space for historgram data to be written/sent 
    int maxBytes;
};

#endif

