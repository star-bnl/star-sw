#include <TROOT.h>
#include <TServerSocket.h>
#include <TSocket.h>
#include <TMessage.h>
#include <TMonitor.h>
#include <TClass.h>

#include "Jevp/StJevpPlot/EvpMessage.h"
#include "Jevp/StJevpPlot/JevpPlot.h"
#include "DisplayDefs.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "EvpConstants.h"
#include "Jevp/StJevpPlot/BuilderStatus.h"
#define MAX_DISPLAY_DEFS 20

class JevpServer {
 public:
  TServerSocket *ssocket; 
  TMonitor *mon;

  char *pdfdir;        // pdf dir
  char *refplotdir;    // ref plot dir
  char *basedir;       // base for config files...
  char base_client[40];
  
  char *displays_fn;
  DisplayFile *displays;
  char *serverTags;

  int run;  // will be run info...
  int nodb;
  int die;
  int killbuilders;
  int launchbuilders;
  char *daqfilename;
  char *socketName;

  TList plots;
  RunStatus runStatus;

  TList builders;  

  int myport;

  JevpServer() {
    myport = JEVP_PORT;
    ssocket = NULL;
    mon = NULL;
    refplotdir = (char *)DEFAULT_REF_PLOT_DIR;
 
 
    displays = NULL;
    displays_fn = NULL;
 

    base_client[0] = '\0';
    basedir = (char *)DEFAULT_BASEDIR;
    refplotdir = (char *)DEFAULT_REF_PLOT_DIR;
    pdfdir = (char *)DEFAULT_PDFDIR;
    nodb = 0;
    socketName = NULL;
    launchbuilders = 0;
    killbuilders = 0;
    die = 0;
    daqfilename = NULL;
    serverTags = NULL;
  };

  void archive_display_file();
  void addToPallete(JevpPlot *plot);

  int launchNewServer(char *run);
  static void main(int argc, char *argv[]);
  void parseArgs(int argc, char *argv[]);
  int init(int port);
  void getMessage();
  void shiftRefPlotsUp(char *name, int idx);
  void shiftRefPlotsDown(char *name, int idx);
  int getMaxRef(char *name);
  void deleteReferencePlot(char *name, int idx);
  void handleEvpMessage(TSocket *s, EvpMessage *msg);
  void handleEvpPlot(TSocket *s, JevpPlot *plot);
  void saveReferencePlot(JevpPlot *plot);
  JevpPlot *getPlot(char *name);
  void handleGetPlot(TSocket *s, char *argstring);
  void handleSwapRefs(char *args);
  void performStopRun();
  void performStartRun();
  void clearForNewRun();
  void dump();
  int updateDisplayDefs();
  void DrawCrossOfDeath(char *str);

  char *getParamFromString(char *dest, char *source, char *param=NULL);
  
  int writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page);
  int writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page, int nosibs);
  void writePdf(int display, int run);
  void getMonitorString(char *s, EvpMessage *m);

  BuilderStatus *getBuilderStatusBySocket(unsigned long long int sock);
  BuilderStatus *getBuilderStatusByName(char *name);
  char *checkRunStatus(BuilderStatus *builderStat, RunStatus *stat);
  void launchBuilders();

  int execScript(const char *name,  char *args[], int waitforreturn=1);

  void addServerTag(char *tag);
  void addServerTags(char *tags);
};
