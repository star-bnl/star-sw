#ifndef _JEVPPLOTSET_H_
#define _JEVPPLOTSET_H_

// This is the user class for creating plot's
// 

#include <Rtypes.h>
#include <TROOT.h>
#include <TSocket.h>
#include "JevpPlot.h"
#include "DAQ_READER/daqReader.h"
#include "Jevp/StJevpPlot/BuilderStatus.h"
#include "Jevp/StJevpPlot/JLatex.h"
#include "Jevp/StJevpPlot/JLine.h"

#define DEFAULT_CLIENTDATADIR "/a/jevp/client"

class JevpPlotSet : public TObject {
  
 public:
  BuilderStatus builderStatus;
  JevpPlotSet();

  char *getPlotSetName();

  // Plot management
  //
  // Generally, one adds plots only at initialization
  // and accesses the plots using getPlot()
  //
  // Importantly, the memory still owned by the calling class
  int addPlot(JevpPlot *plot);
  JevpPlot *getPlot(char *name);

  void removePlot(char *name);
  int getNumberOfPlots();
  JevpPlot *getPlotByIndex(int i);
  void dump();

  // initialize() is called once at the begininning of the program
  // the arguments are the command line arguments of the program
  void _initialize(int argc, char *argv[]);
  virtual void initialize(int argc, char *argv[]);

  // startrun is called once at the begining of the run
  void _startrun(daqReader *rdr);
  virtual void startrun(daqReader *rdr);
  
  // stoprun is called once at the end of the run
  virtual void stoprun(daqReader *rdr);
  void _stoprun(daqReader *rdr);

  // event is called once for each event
  void _event(daqReader *rdr);
  virtual void event(daqReader *rdr);

  // this is the selection criteria for events
  virtual int selectEvent(daqReader *rdr);

  // this is the selection criteria for runs
  virtual int selectRun(daqReader *rdr);

  void Main(int argc, char *argv[]);

  void resetAllPlots();

  int send(TObject *msg); // used only for special cases...such as base server run info

  char *clientdatadir;
  char *confdatadir;
  char *plotsetname;

  char *hello_cmds;   // do I want to be base_client?  "steal if so"

  void addServerTags(char *tags);

 private:
  TSocket *socket;

  int run;
  int die_at_endofrun;

  TList plots;    // The plots built
  
  unsigned int current_run;

 
  int base_client;    // am I base client?

  daqReader *reader;
  char *diska;      // event pool path
  char *daqfile;    // data file / null for live
  char *server;     // server
  int serverport;   // server port
  char *socketName;
  char *pdf;        // direct pdf file output
  char *loglevel;
  char *buildxml;
  void buildTheXml();

  int update_time;

  int pause;
  int parseArgs(int argc, char *argv[]);   
  int connect(char *host, int port);
  int updatePlots();
  void writePdfFile();

  JevpPlot *plotEvtsByTrigger;
  JevpPlot *plotTimeByTrigger;
  JevpPlot *plotTime;

  double n_pertrg[64];
  double avg_time_pertrg[64];

 public:
  ClassDef(JevpPlotSet, 1);
};

#endif
