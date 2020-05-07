#ifndef _IMAGEWRITER_H_
#define _IMAGEWRITER_H_

#include <TROOT.h>
#include <TClass.h>
#include <TCanvas.h>
#include <RTS/include/UNIX/ThreadsMsgQueue.hh>
#include <pthread.h>

#define MAX_IMAGEWRITER_CANVAS 3000
void *ImageWriterThread(void *);

class JevpPlot;

struct CanvasSlot {
    JevpPlot *plot;
    char name[256];
};

class ImageWriter {
    thrMsgQueue<CanvasSlot> *slotQ;
    int nHisto;
    int file_idx;
 public:
    // mutex for any drawing functions...
    pthread_mutex_t mux;

    ImageWriter(char *basedir);
    void writeImage(char *fn, JevpPlot *plot, double ymax);
    void writeToImageWriter(CanvasSlot *slot);
    void loop();
};


#endif
