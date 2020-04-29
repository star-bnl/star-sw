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


JevpPlot *CanvasImageBuilder::getPlotByName(char *name) {
  if(server) {
    return server->getPlot(name);
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

// Invoke as writeIndex(dir, fn);
// 
void CanvasImageBuilder::writeIndex(char *basedir, char *fn, int combo_index) {
    //LOG("JEFF", "Write index: starting combo_index = %d %p", combo_index, displays);

    makedir(basedir);
    char fullfile[256];
    strcpy(fullfile, basedir);
    strcat(fullfile, "/");
    strcat(fullfile, fn);
    
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

    LOG("JEFF", "Done writeIndex %d pages", pages);
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
    //LOG("JEFF", "plot = %p", canvasSlot.plot);
    strcpy(canvasSlot.name, fn);
    
    imageWriter->writeToImageWriter(&canvasSlot);
    return 1;
}


int CanvasImageBuilder::writeImageFiles(char *dir, DisplayNode *node, int page) {
    int npages = 0;

    //LOG("JEFF", "node: %s page %d", node->name, page);
    if(node->leaf) {
	char ndir[256];
	sprintf(ndir, "%s/%03d", dir, page);
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
	    //char ndir[512];
	    //strcpy(ndir, dir);
	    //strcat(ndir, "/");
	    //strcat(ndir, removeSpaces(node->name));
	    npages += writeImageFiles(dir, node->child, page + npages);
	}
	
	if(node->next) {
	    npages += writeImageFiles(dir, node->next, page + npages);
	}
    }
    return npages;
}

int CanvasImageBuilder::writeRunStatus(char *basedir, RunStatus *rs, int evtCnt, const char *serverTags)
{
    char fn[256];
    sprintf(fn, "%s/runStatus.json", basedir);

    LOG("JEFF", "Writing run status to file %s", fn);
    FILE *f = fopen(fn, "w");
    if(!f) {
	LOG("JEFF", "Error opening file for run status");
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
    fprintf(f,"\"%s\":%d,", "writeTime", time(NULL));
    fprintf(f,"\"%s\":%d,", "lastStatusChangeTime", rs->timeOfLastChange);
    fprintf(f,"\"%s\":\"%s\"", "serverTags", serverTags); 
    fprintf(f, "}");
    fclose(f);
    return 0;
}

// Assumes that the index has been writen, so the directory exists...
int CanvasImageBuilder::writeImages(char *basedir) {
    DisplayNode *root = displays->getTab(1);
  
    int pages = writeImageFiles(basedir, root, 1);

    CanvasSlot end;
    end.plot = NULL;
    strcpy(end.name, basedir);   // image writer appends the "_done"
    imageWriter->writeToImageWriter(&end);

    return pages;
}


