#ifndef _JEVPSERVER_H_
#define _JEVPSERVER_H_

#include <TROOT.h>
#include <TServerSocket.h>
#include <TSocket.h>
#include <TMessage.h>
#include <TMonitor.h>
#include "JTMonitor.h"
#include <TClass.h>

#include "DAQ_READER/daqReader.h"
#include <PDFUtil/PdfIndex.hh>
#include "Jevp/StJevpPlot/EvpMessage.h"
#include "Jevp/StJevpPlot/JevpPlot.h"
#include "DisplayDefs.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "EvpConstants.h"
#include "Jevp/StJevpPlot/BuilderStatus.h"

class PdfFileBuilder;

#define MAX_DISPLAY_DEFS 20

class JevpServer {
 public:
  int log_output;
  char *log_dest;
  int log_port;
  char *log_level;

  PdfFileBuilder *pdfFileBuilder;

  TServerSocket *ssocket;
  JTMonitor *mon;
  
  int throttleAlgos;
  double throttle_time;

  int isL4;

  char *clientdatadir;
  char *pdfdir;        // pdf dir
  char *rootfiledir;
  char *refplotdir;    // ref plot dir
  char *basedir;       // base for config files...
  char *diska;
  int myport;
  int logevent;
  char *launchArgs;    // used to pass the lanuch arguments back to reader thread...
  int makepallete;     // **JUST** make pallete
  daqReader *rdr;

  char *displays_fn;      // Display Information...
  DisplayFile *displays;
  char *serverTags;
  int justUpdateDisplayPallete;
  void justUpdatePallete();

  int eventsThisRun;
  // int run;                // run number
  int nodb;               // send to db?
  int die;                // die when the run is over? or wait for the next run...
  int maxevts;
  int evtsInRun;
  char *daqfilename;      // NULL if running real data
  
  int cdaqfilename;
  int ndaqfilenames;
  char *daqfilenames[10];

  RunStatus runStatus;    // are we in a run or not?
  TList builders;         
  
  JevpPlot *jevpSummaryPlot;

  void debugBuilders(int line);

  unsigned long long int getMemUse();

  JevpServer() {
    myport = JEVP_PORT;
    logevent = 0;
    maxevts = 0;
    evtsInRun = 0;
    ssocket = NULL;
    mon = NULL;
    refplotdir = (char *)DEFAULT_REF_PLOT_DIR;
    diska = (char *)"/";
 
    displays = NULL;
    displays_fn = NULL;
    
    basedir = (char *)DEFAULT_BASEDIR;
    refplotdir = (char *)DEFAULT_REF_PLOT_DIR;
    pdfdir = (char *)DEFAULT_PDFDIR;
    rootfiledir = (char *)DEFAULT_ROOTFILEDIR;
    nodb = 0;
    die = 0;
    daqfilename = NULL;
    serverTags = NULL;
    launchArgs = NULL;

    jevpSummaryPlot = NULL;
  };
  
  void writeRootFiles();
  static void main(int argc, char *argv[]);
  void parseArgs(int argc, char *argv[]);
  int init(int port, int argc, char *argv[]);

  void performStopRun();                                 // Handle run status
  void performStartRun();
  void clearForNewRun();
  int calculateAndUpdateRunStatus(BuilderStatus *changedBuilder);
  void addServerTag(char *tag);
  void addServerTags(char *tags);


  void archive_display_file();                           // Archive utilities

  void freePallete();
  void addToPallete(JevpPlot *plot);

  JevpPlot *getJevpSummaryPlot();                        // Build The Summary plot...

  int handleEvent();
  void handleClient(int delay);

  void handleNewEvent(EvpMessage *msg);
  void handleEvpMessage(TSocket *s, EvpMessage *msg);
  void handleEvpPlot(TSocket *s, JevpPlot *plot);
  void handleGetPlot(TSocket *s, char *argstring);
  void handleSwapRefs(char *args);

  double liney(double x);


  int getMaxRef(char *name);                             // Reference plots...
  void shiftRefPlotsUp(char *name, int idx);
  void shiftRefPlotsDown(char *name, int idx);
  void deleteReferencePlot(char *name, int idx);
  void saveReferencePlot(JevpPlot *plot);

  // Write the histograms out....
  //int writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page);
  //int writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page, int nosibs);
  void writeRunPdf(int display, int run);
  //void writePdf(char *fn, int combo_index);



  JevpPlot *getPlot(char *name);

  void dump();
  void writePalleteFile();
  int updateDisplayDefs();
  void DrawCrossOfDeath(char *str);

  char *getParamFromString(char *dest, char *source, char *param=NULL);
  void getMonitorString(char *s, EvpMessage *m);



  void readSocket();
  
  /*   void *readerThread(void *); */
  /*   void readerThreadSend(TSocket *socket, char *cmd); */
  /*   void readerThreadWait(TSocket *socket); */

  int execScript(const char *name,  char *args[], int waitforreturn=1);
};

void *JEVPSERVERreaderThread(void *);

#endif
