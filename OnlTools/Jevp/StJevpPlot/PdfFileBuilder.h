#ifndef _PDFFILEBUILDER_H_
#define _PDFFILEBUILDER_H_

#include <TROOT.h>
#include <TClass.h>
#include <PDFUtil/PdfIndex.hh>
#include "Jevp/StJevpServer/JevpServer.h"
#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DisplayDefs.h"

class JevpServer;
class JevpPlotSet;

class PdfFileBuilder {

 private:
    DisplayFile *displays;
    JevpServer *server;
    JevpPlotSet *plotset;

    char *serverTags;
  
 public:

    // One and only one of "server"/"plotset" should be set.
    PdfFileBuilder(DisplayFile *displays, JevpServer *server, JevpPlotSet *plotset) {
	this->server = server;
	this->plotset = plotset;
	this->displays = displays;

	serverTags = (char *)"";
    }

    // Very strange looking function!   But server and plotset are not
    // derived from same class, even though they do have this function in
    // common...
    JevpPlot *getPlotByName(char *name);  
    void write(char *filename, int displayNumber, int ignoreServerTags = 1, char *serverTags = (char *)"");
    void writePdf(char *filename, int combo_index);

 private:
    int writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page);
    int writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page);  

    void DrawCrossOfDeath(char *str);
};


#endif
