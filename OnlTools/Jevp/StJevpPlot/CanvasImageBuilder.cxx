// This is the program called by the server which produces canvases and 
// places them in a queue for the ImageWriter thread
//


#include "CanvasImageBuilder.h"
#include "ImageWriter.h"
#include "RunStatus.h"

#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TPaveText.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>

#define TITLEBGCOLOR 14
#define BGCOLOR 17
#define HISTOCOLOR 10
#define BLACKCOLOR 1

#define XX(x) canvasBuilderLine = x*10000 + __LINE__
int canvasBuilderLine=0;

char *removeSpaces(char *s) {
    static char _str[512];
    
    char *str = _str;

    while(*s != '\0') {
	if(*s == ' ') {
	    *str = '_';
	}
	else {
	    *str = *s;
	}
	str++; s++;
    }
    *str = '\0';
    return _str;
}

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

CanvasImageBuilder::CanvasImageBuilder(char *basedir, DisplayFile *displays, JevpServer *server, JevpPlotSet *plotset, ImageWriter *imageWriter) {
    LOG("JEFF", "Created CanvasImageBuilder");
    this->server = server;
    this->plotset = plotset;
    this->displays = displays;
    this->imageWriter = imageWriter;
    strcpy(this->basedir, basedir);

    serverTags = (char *)"";

    // Find initial writeIdx
    //
    // The writeIdx indicates the number of calls to the sendToImageWriter()
    // function.   The reason it is needed is that the control files are
    // written from the builder, while the images are written by the
    // imageWriter.  The files must be consitent between the two, and free
    // race conditions, so we write into:
    //    /tmp/{basedir}_build_{writeIdx} directory
    // Then after complete, imageBuilder copies to
    //    /tmp/{basedir}_done_{writeIdx} directory.

    writeIdx = 0;
    char testdir[256];
    sprintf(testdir, "%s_done_", basedir);
    
    DIR *dp = opendir("/tmp");
    struct dirent *entry;
    if(dp == NULL) {
	LOG("ERR", "No /tmp directory");
    }
    else {
	while((entry = readdir(dp)) != NULL) {
	    //LOG("JEFF", "testdir=%s entry->d_name=%s", testdir, entry->d_name);

	    if(memcmp(testdir,entry->d_name,strlen(testdir)) == 0) {
		
		int x = atoi(&entry->d_name[strlen(testdir)]);
		//LOG("JEFF", "got one: %s %d %s %d", entry->d_name, strlen(testdir), &entry->d_name[strlen(testdir)+1], x);
		if(x > writeIdx) writeIdx = x;
	    }
	}
	closedir(dp);
    }
    writeIdx++;

    LOG("JEFF", "Created CanvasImageBuilder: writeIdx=%d", writeIdx);
}

JevpPlot *CanvasImageBuilder::getPlotByName(char *name) {
  if(server) {
      JevpPlot *ptr = server->getPlot(name);
      //CP;
      if(ptr == NULL) {
	  LOG("JEFF", "ptr=null %s",name);
      }
      return ptr;
  }
  if(plotset) {
    return plotset->getPlot(name);
  }
  return NULL;
}

void CanvasImageBuilder::writeIndexFiles(FILE *f, DisplayNode *node, int page, int tabs) {
    char fullName[1000];
    node->getTabName(fullName);
   
    DisplayNode *cnode = node;
    while(cnode) {
	for(int q=0;q<tabs;q++) { fprintf(f, "   "); };
	fprintf(f, "[ %s:%d ]\n", removeSpaces(cnode->name), page);
	cnode = cnode->next;
    }
}

int CanvasImageBuilder::writeIndexFromNode(FILE *f, DisplayNode *node, char *currdir, int page, int tabs) {
    int npages = 0;
    if(node->leaf) {
	fprintf(f,"%s : %d\n", currdir, page); 
	writeIndexFiles(f, node, page, 1);
	return 1;
    }
    else {
	//for(int q=0;q<tabs;q++) { fprintf(f, "   "); };
	//fprintf(f, "%s", removeSpaces(node->name));
	
	if(node->child) {
	    //fprintf(f, " {\n");
	    char ndir[512];
	    strcpy(ndir, currdir);
	    strcat(ndir, "/");
	    strcat(ndir, removeSpaces(node->name));
	    npages += writeIndexFromNode(f, node->child, ndir, page, tabs+1);
	    //for(int q=0;q<tabs;q++) { fprintf(f, "   "); };
	    //fprintf(f, "}");
	}
	
	if(node->next) {
	    //fprintf(f, ",\n");
	    npages += writeIndexFromNode(f, node->next, currdir, page + npages, tabs);
	}
	//else fprintf(f,"\n");

	return npages;
    }
}

// Invoke as writeIndex();
// 
void CanvasImageBuilder::writeIndex(int combo_index) {
    //LOG("JEFF", "Write index: starting combo_index = %d %p", combo_index, displays);
    char fullbasedir[256];
    sprintf(fullbasedir, "/tmp/%s_build_%08d", basedir, writeIdx);

    makedir(fullbasedir);
    char fullfile[256];
    sprintf(fullfile, "%s/idx.txt", fullbasedir);
    
    FILE *f = fopen(fullfile, "w");
    if(!f) {
	LOG("JEFF", "Error opening %s (%s)", fullfile, strerror(errno));
	return;
    }
    DisplayNode *root = displays->getTab(combo_index);
    if(root == NULL) {
	LOG("JEFF", "no displays yet");
    }

    if(combo_index == 0) {
	LOG("JEFF", "disproot = 0x%x root = 0x%x", displays->displayRoot, root);
	root = displays->displayRoot;
    }

    // Write page 1 indo Pdf Index...
    int pages = writeIndexFromNode(f, root, "", 1, 0);
    
    fclose(f);

    //LOG("JEFF", "Done writeIndex %d pages", pages);
}


// Clone the plot and send to the image writer...
int CanvasImageBuilder::writeImageFile(char *dir, DisplayNode *node, double maxy) {
    char fn[256];
    //sprintf(fn, "%s/%s.jpg", dir, removeSpaces(node->name));
    sprintf(fn, "%s/%s.svg", dir, removeSpaces(node->name));
 
    JevpPlot *plot = getPlotByName(node->name);
    if(!plot) {
	LOG("JEFF", "Can't find plot %s\n", node->name);
	return 0;
    }
    
    CanvasSlot canvasSlot;
    canvasSlot.plot = (JevpPlot *)((TObject *)plot)->Clone();
    strcpy(canvasSlot.name, fn);
    canvasSlot.writeIdx = writeIdx;
    
    imageWriter->writeToImageWriter(&canvasSlot);
    return 1;
}


int CanvasImageBuilder::writeImageFiles(DisplayNode *node, int page) {
    int npages = 0;

    //LOG("JEFF", "node: %s page %d", node->name, page);
    if(node->leaf) {
	char ndir[256];
	sprintf(ndir, "/tmp/%s_build_%08d/%03d", basedir, writeIdx, page);
	//makedir(ndir);

	
	// Do the page...
	DisplayNode *cnode = node;
	while(cnode) {
	    writeImageFile(ndir, cnode);
	    cnode = cnode->next;
	}
	
	return 1;
    }
    else {
	if(node->child) {
	    npages += writeImageFiles(node->child, page + npages);
	}
	
	if(node->next) {
	    npages += writeImageFiles(node->next, page + npages);
	}
    }
    return npages;
}

int CanvasImageBuilder::writeRunStatus(RunStatus *rs, int evtCnt, const char *serverTags)
{
    char fn[256];
    sprintf(fn, "/tmp/%s_build_%08d/runStatus.json", basedir, writeIdx);

    // LOG("JEFF", "Writing run status to file %s", fn);
    FILE *f = fopen(fn, "w");
    if(!f) {
	LOG("JEFF", "Error opening file for run status (%s)", strerror(errno));
	return -1;
    }
    
    if(serverTags == NULL) {
	serverTags = "none";
    }
    
    fprintf(f, "{ ");
    fprintf(f,"\"%s\":%d,", "run", rs->run);
    fprintf(f,"\"%s\":\"%s\",", "status", rs->status);
    fprintf(f,"\"%s\":%d,", "firstEvtTime", rs->firstEvtTime);   
    fprintf(f,"\"%s\":%d,", "firstEvtNumber", rs->firstEvtNumber);   
    fprintf(f,"\"%s\":%d,", "lastEvtTime", rs->lastEvtTime);   
    fprintf(f,"\"%s\":%d,", "lastEvtNumber", rs->lastEvtNumber);   
    fprintf(f,"\"%s\":%d,", "nEvts", evtCnt);   
    fprintf(f,"\"%s\":%ld,", "writeTime", time(NULL));
    fprintf(f,"\"%s\":%d,", "lastStatusChangeTime", rs->timeOfLastChange);
    fprintf(f,"\"%s\":\"%s\"", "serverTags", serverTags); 
    fprintf(f, "}");
    fclose(f);
    return 0;
}

void CanvasImageBuilder::sendDieToImageWriter() {
    CanvasSlot end;
    end.plot = NULL;
    end.writeIdx = -1;
    sprintf(end.name, "");   // image writer appends the "_done"
    imageWriter->writeToImageWriter(&end);
}
    
int CanvasImageBuilder::sendToImageWriter(RunStatus *rs, int numberOfEvents, const char *serverTags, bool force) {
    XX(0);
    //LOG("JEFF", "sendToImageWriter 0x%x", imageWriter);
    int nwriting = imageWriter->getNWriting();
    if((nwriting > 0) && !force) {
	//	LOG("JEFF", "skip... nwriting = %d", nwriting);
	return 0;
    }

    //LOG("JEFF", "Write to image writer: nWriting=%d writeIdx=%d", nwriting, writeIdx);
    XX(2);      // writeIdx is guarenteed to be updated before the mux returns!
    pthread_mutex_lock(&imageWriter->mux);
    XX(1);
    writeIndex();
    XX(1);
    writeRunStatus(rs, numberOfEvents, serverTags);
    XX(1);
    int cnt = writeImages();
    XX(1);
    writeIdx++;
    pthread_mutex_unlock(&imageWriter->mux);
    XX(999);
    return cnt;
}

// Assumes that the index has been writen, so the directory exists...
int CanvasImageBuilder::writeImages() {
    DisplayNode *root = displays->getTab(1);
  
    int pages = writeImageFiles(root, 1);

    CanvasSlot end;
    end.plot = NULL;
    end.writeIdx = writeIdx;
    sprintf(end.name, basedir);   // image writer appends the "_done"
    imageWriter->writeToImageWriter(&end);
    //LOG("JEFF", "Wrote end: %s to imagewriter", end.name);
    return pages;
}


