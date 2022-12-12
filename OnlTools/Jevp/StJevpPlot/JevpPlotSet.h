#ifndef _JEVPPLOTSET_H_
#define _JEVPPLOTSET_H_

// This is the user class for creating plot's
// 

#include <Rtypes.h>
#include <TROOT.h>
#include <TSocket.h>
#include <THashList.h>

#include "JevpPlot.h"
#include "Jevp/StJevpPlot/BuilderStatus.h"
#include "Jevp/StJevpPlot/JLatex.h"
#include "Jevp/StJevpPlot/JLine.h"
#include "Jevp/StJevpServer/JevpServer.h"

//#include "StDaqLib/TRG/trgStructures2009.h"
//#include "StEvent/StTriggerData2009.h"
//#include "StEvent/StTriggerData.h"

#include <unistd.h>
//#include <RTS/include/SUNRT/clockClass.h>

class JevpServer;

#define DEFAULT_CLIENTDATADIR "."


#define PCPC(x) dbgCallSourceLine=__LINE__;dbgCallParam=x
#define PCP dbgBuilderLine=__LINE__

class RtsTimer_root;

class JevpPlotSet : public TObject {
  
 public:
  BuilderStatus builderStatus;
  JevpPlotSet(JevpServer *server = NULL);
  unsigned long long int getMemUse();
  int dbgBuilderLine;
  int dbgCallSourceLine;
  int dbgCallParam;

  char *getPlotSetName();
  char *getDebugInfo() {
    static char str[256];
    sprintf(str, "%d:call %d: param %d", dbgBuilderLine, dbgCallSourceLine, dbgCallParam);
    return str;
  }
  

  time_t startTime;
  int timelog_fd;

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

  void setDisabled() {
    disabled = 1;
  }
  
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

  char *clientdatadir;
  char *confdatadir;
  char *plotsetname;
  char *xml;
  char *hello_cmds;   // do I want to be base_client?  "steal if so"

  int minEvts;
  int maxEvts;

  void addServerTags(const char *tags);
  //char *getServerTags();

  RtsTimer_root *processingTimer;
  double processingTime;
  int numberOfEventsRun;

  double getAverageProcessingTime() {
    double n = (numberOfEventsRun>0) ? numberOfEventsRun : 1;

    return (processingTime / n);
  }

  THashList plots;    // The plots built

  //StTriggerData *getStTriggerData(daqReader *rdr);

  void logDbVariable(const char *variable_name, double value);
  static void staticLogDbVariable(const char *name, double value, int my_run, int my_time, const char *my_plotsetname, const char *my_clientdatadir);

  JevpServer *parent;

 private:
  u_int lastevttm;

  int disabled;
  int run;

  char servertags[512];
  
  unsigned int current_run;

  int base_client;    // am I base client?

  char *diska;      // event pool path
  char *daqfile;    // data file / null for live

  char *daqfiles[20]; //multiple input files
  int  ifile;         //current working file
  int  nfile;         //number of files

  char *pdf;        // direct pdf file output
  char *loglevel;
  char *buildxml;
  void buildTheXml();

  int update_time;

  int pause;
  int parseArgs(int argc, char *argv[]);   
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
