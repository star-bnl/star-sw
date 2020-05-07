#ifndef _CANVASIMAGEBUILDER_H_
#define _CANVASIMAGEBUILDER_H_

// This is the program called by the server which produces canvases and 
// places them in a queue for the ImageWriter thread
//
#include <TROOT.h>
#include <TClass.h>
#include "Jevp/StJevpServer/JevpServer.h"
#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DisplayDefs.h"

class JevpServer;
class JevpPlotSet;
class RunStatus;
class CanvasImageBuilder {

 private:
    DisplayFile *displays;
    JevpServer *server;
    JevpPlotSet *plotset;
    ImageWriter *imageWriter;

    char *serverTags;
  
 public:

    // One and only one of "server"/"plotset" should be set.
    CanvasImageBuilder(DisplayFile *displays, JevpServer *server, JevpPlotSet *plotset, ImageWriter *imageWriter) {
	LOG("JEFF", "Created image writer");
	this->server = server;
	this->plotset = plotset;
	this->displays = displays;
	this->imageWriter = imageWriter;

	serverTags = (char *)"";
    }

    // Very strange looking function!   But server and plotset are not
    // derived from same class, even though they do have this function in
    // common...
    JevpPlot *getPlotByName(char *name);  

    //   writePDF(filename, 1) is the method to write all files...
    //

    // Invoke as writeIndex(dir, fn);
    int sendToImageWriter(char *directory, RunStatus *rs, int numberOfEvents, const char *serverTags, bool force=false);

 private:
    void writeIndex(char *directory, char *fn, int combo_index = 1);
    int writeImages(char *directory);
    int writeRunStatus(char *directory, RunStatus *rs, int numberOfEvents, const char *serverTags);

 private:
    void writeIndexFiles(FILE *f, DisplayNode *node, int page, int tabs);
    // Does above recursively...
    int writeIndexFromNode(FILE *f, DisplayNode *node, char *dir, int page, int tabs);

    int writeImageFile(char *dir, DisplayNode *node, double ymax=-999);
    // Does above recursively...
    int writeImageFiles(char *dir, DisplayNode *node, int page);

    /*
    void writeAllFiles(char *directory);

    void write(char *filename, int displayNumber, int ignoreServerTags = 1, char *serverTags = (char *)"");

 
  
    void writePdf(char *filename, int combo_index);

 private:
    int writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page);
    int writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page);  

    void DrawCrossOfDeath(char *str);
    */
};


#endif
