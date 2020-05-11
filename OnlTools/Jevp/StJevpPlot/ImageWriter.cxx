#include <stdlib.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>


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

#define XX(x) ImageWriterDrawingPlot = 10000*x + __LINE__
int ImageWriterDrawingPlot = 0;

RtsTimer_root imageClock;
RtsTimer_root ic2;

static void makedir(char *directory) {
    struct stat64 info;
    if(stat64(directory, &info) == 0) return;

    char dir[100];
    strcpy(dir, directory);
    char *tok = strtok(dir, "/");
    char path[100];
    strcpy(path, "/");

    do {
	strcat(path, tok);
	if(stat64(path, &info ) != 0) {
	    //LOG("JEFF","making %s", path);
 	    mkdir(path,0777);
	}
	else {
	    //LOG("JEFF","%s exists", path);
	}
	strcat(path, "/");
    } while((tok = strtok(NULL, "/")));
}

static char *getPath(char *fn) {
    static char path[256];
    strcpy(path, fn);
    int n = strlen(path)-1;
    for(int i=n;i>0;i--) {
	if(path[i] == '/') {
	    path[i] = '\0';
	    return path;
	}
    }
    return NULL;
}

void *ImageWriterThread(void *iw) {
    int imageWriterTid = syscall(SYS_gettid);
    
    LOG("JEFF", "imageWriter TID = %d", imageWriterTid);


    ImageWriter *imageWriter = (ImageWriter *)iw;

    imageWriter->loop();

    return NULL;
}

void ImageWriter::writeImage(char *fn, JevpPlot *plot, double ymax) {
    static int canvasNumber=1;
    XX(nHisto);
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
    
    double t1,t2,t3,t4,t5;
    ic2.record_time();

    //TCanvas *canvas = new TCanvas(canvasName,canvasName,1000,800);
    TCanvas *canvas = new TCanvas(canvasName,canvasName,1500,1200);
    canvas->cd();
  
    gPad->SetFillColor(BGCOLOR);
 
    gPad->SetFrameFillColor(HISTOCOLOR);
    gPad->SetTopMargin(.10);
    //gStyle->SetOptTitle(0);
    
    t1 = ic2.record_time()*1000;
    
    XX(nHisto);
    plot->draw();
    XX(nHisto);
   
    t2 = ic2.record_time()*1000;

    //LOG("JEFF", "Wrote images: %s", node->name);

    //canvas->SaveAs(fn);

    t3 = ic2.record_time()*1000;

    //if(canvasNumber == 9 || canvasNumber == 10) {
    //	LOG("JEFF", "(%d) -- %s", canvasNumber, plot->GetPlotName());
    //}

    XX(nHisto);
    //canvas->Print(fn);
    canvas->SaveAs(fn);
    XX(nHisto);

    t4 = ic2.record_time()*1000;

    //LOG("JEFF", "Write %s", fn); 
    XX(nHisto);
    delete canvas;
    XX(nHisto);
    delete plot;
    XX(nHisto);
}

ImageWriter::ImageWriter(char *basename) {  
    strcpy(this->basename, basename);
    slotQ = new thrMsgQueue<CanvasSlot>(MAX_IMAGEWRITER_CANVAS);
    pthread_mutex_init(&mux, NULL);
}

void ImageWriter::loop() {
    nHisto=0;

    //LOG("JEFF", "ImageWriter loop starting");
    for(;;) {
	CanvasSlot slot;
	
	//LOG("JEFF", "receive from slotQ %p", slotQ);
	slotQ->receive(&slot);	
	
	//LOG("JEFF", "Got slot: %p %d", slot.plot, slot.writeIdx);

	if(slot.plot == NULL) {
	    if(slot.writeIdx == -1) {
		LOG("JEFF", "Got die command, exiting.");
		return;
	    }

	    if(nHisto > 0) {
		nHisto = 0;
		char o[256];
		char d[256];
		strcpy(o, slot.name);
		sprintf(o, "/tmp/%s_build_%08d", slot.name, slot.writeIdx);
		sprintf(d, "/tmp/%s_done_%08d", slot.name, slot.writeIdx);

		LOG("JEFF", "Saved %d plots in %lf seconds: s", nHisto, imageClock.record_time(), d);

		rename(o, d);
	    }
	}
	else {
	    if(nHisto == 0) {
		LOG("JEFF", "Got a canvas %s after %lf seconds", slot.name, imageClock.record_time());
	    }
	    
	    nHisto++;

	    char *path = getPath(slot.name);
	    if(!path) {
		LOG("JEFF", "Error finding path for %s", slot.name);
	    }
	    else {
		//LOG("JEFF", "makedir %s", path);
		makedir(path);
	    }
	    
	    RtsTimer_root ttt;
	    pthread_mutex_lock(&mux);
	    double tttt =  ttt.record_time();
	    if(tttt > .1) {
		//LOG("JEFF", "image mux took %lf seconds", tttt);
	    }
	 
	    writeImage(slot.name, slot.plot, -999); 
	    pthread_mutex_unlock(&mux);
	}
    }
}

void ImageWriter::writeToImageWriter(CanvasSlot *slot)
{
    if(slot->plot == NULL) {
	//LOG("JEFF", "slotQ->n = %d", slotQ->entries());
    }

    //LOG("JEFF", "send to slotQ: %p", slotQ);
    slotQ->send(slot);
}
