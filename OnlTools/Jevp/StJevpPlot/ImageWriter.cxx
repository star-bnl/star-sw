#include <stdlib.h>
#include <sys/stat.h>

#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <RTS/include/SUNRT/clockClass.h>
#include <rtsLog.h>
#include "ImageWriter.h"
#include "JevpPlot.h"

#define TITLEBGCOLOR 14
#define BGCOLOR 17
#define HISTOCOLOR 10

RtsTimer_root imageClock;

void *ImageWriterThread(void *iw) {
    ImageWriter *imageWriter = (ImageWriter *)iw;

    imageWriter->loop();

    return NULL;
}

void ImageWriter::writeImage(char *fn, JevpPlot *plot, double ymax) {
    static int canvasNumber=1;
    
    gStyle->SetCanvasColor(BGCOLOR);
    
    if(ymax != -999) {
	if(plot->logy) {
	    plot->setMaxY(ymax * 2);
	}
	else {
	    plot->setMaxY(ymax * 1.1);
	}
    }

    char canvasName[64];
    sprintf(canvasName, "imageCanvas%d", canvasNumber++);
    
    TCanvas *canvas = new TCanvas(canvasName,canvasName,1000,800);
    canvas->cd();
  
    gPad->SetFillColor(BGCOLOR);
 
    gPad->SetFrameFillColor(HISTOCOLOR);
    gPad->SetTopMargin(.10);
    //gStyle->SetOptTitle(0);
    
    plot->draw();
   
    //LOG("JEFF", "Wrote images: %s", node->name);

    canvas->SaveAs(fn);
    delete canvas;
    delete plot;
}

ImageWriter::ImageWriter() {
    slotQ = new thrMsgQueue<CanvasSlot>(MAX_IMAGEWRITER_CANVAS);
    pthread_mutex_init(&mux, NULL);
}

void ImageWriter::loop() {
    int nHisto=0;

    LOG("JEFF", "ImageWriter loop starting");
    for(;;) {
	CanvasSlot slot;
	
	slotQ->receive(&slot);	
	
	if(slot.plot == NULL) {
	    if(nHisto > 0) {
		LOG("JEFF", "Saved %d plots in %lf seconds", nHisto, imageClock.record_time());
	    }
	    nHisto = 0;

	    rename("/tmp/jevp", slot.name);
	}
	else {
	    if(nHisto == 0) {
		LOG("JEFF", "Got a canvas %s after %lf seconds", slot.name, imageClock.record_time());
	    }
	    
	    nHisto++;

	    RtsTimer_root ttt;
	    pthread_mutex_lock(&mux);
	    double tttt =  ttt.record_time();
	    if(tttt > .1) {
		LOG("JEFF", "image mux took %lf seconds", tttt);
	    }
	    
	    writeImage(slot.name, slot.plot, -999); 
	    pthread_mutex_unlock(&mux);
	}
    }
}

void ImageWriter::writeToImageWriter(CanvasSlot *slot)
{
    slotQ->send(slot);
}
