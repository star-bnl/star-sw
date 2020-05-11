#ifndef _CANVASIMAGEBUILDER_H_
#define _CANVASIMAGEBUILDER_H_

// This is the program called by the server which produces canvases and 
// places them in a queue for the ImageWriter thread
//

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>

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
    char basedir[256];
    DisplayFile *displays;
    JevpServer *server;
    JevpPlotSet *plotset;
    ImageWriter *imageWriter;

    char *serverTags;
    int writeIdx;

 public:

    // One and only one of "server"/"plotset" should be set.
    CanvasImageBuilder(char *basedir, DisplayFile *displays, JevpServer *server, JevpPlotSet *plotset, ImageWriter *imageWriter);

    void setDisplays(DisplayFile *displays) { this->displays = displays; }

    // Very strange looking function!   But server and plotset are not
    // derived from same class, even though they do have this function in
    // common...
    JevpPlot *getPlotByName(char *name);  
    void sendDieToImageWriter();

    // Invoke as writeIndex(dir, fn);
    int sendToImageWriter(RunStatus *rs, int numberOfEvents, const char *serverTags, bool force=false);

 private:
    void writeIndex(int combo_index = 1);
    int writeImages();
    int writeRunStatus(RunStatus *rs, int numberOfEvents, const char *serverTags);

 private:
    void writeIndexFiles(FILE *f, DisplayNode *node, int page, int tabs);
    // Does above recursively...
    int writeIndexFromNode(FILE *f, DisplayNode *node, char *dir, int page, int tabs);

    int writeImageFile(char *dir, DisplayNode *node, double ymax=-999);
    // Does above recursively...
    int writeImageFiles(DisplayNode *node, int page);

 
};


#endif
