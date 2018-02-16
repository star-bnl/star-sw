//******************************************************
//*** Merges events from .daq files 
//*** 
//*** usage:   daqFileMerger [-omit n] -o output.daq fn1.daq fn2.daq ...
//***
//***          merges daq files. 
//***          The events in the file will be ordered such that 
//***              a)  The events from each file will be in the same order as in the original files
//***              b)  The events' source files will be randomly distributed throughout the run.
//***              c)  The optional omit parameter will omit n% of the events randomly
//***          
//*****************************************************

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

void displayHelp()
{
    printf("Usage:  daqFileMerger [-omit n] -o outputfile.daq source1.daq source2.daq ...\n");
    printf("        The files will be merged such that:\n");
    printf("                a)  The events from each file will be in the same order as in the original files\n");
    printf("                b)  The events' source files will be randomly distributed throughout the run.\n");
    printf("                c)  The optional omit parameter will omit n%% of the events randomly\n");
}

int omit;
int firstSourceFile;
char *outputfile;

void parseArgs(int argc, char *argv[])
{
    int arg = 1;
    
    for(arg=1;arg<argc;arg++) {
	if(strcmp(argv[arg], "-omit") == 0) {
	    arg++;
	    omit = atoi(argv[arg]);
	}
	else if(strcmp(argv[arg], "-o") == 0) {
	    arg++;
	    outputfile = argv[arg];
	    firstSourceFile = arg+1;

	    printf("output file: %s, omit = %d%%\n", outputfile, omit);
	    return;
	}
    }

    displayHelp();
    exit(0);
}
	
int getGoodEvent(daqReader *rdr) {
    for(;;) {
	char *ret = rdr->get(0, EVP_TYPE_ANY);

	if(ret) {
	    if(rdr->status) continue;    // Not a good event?
	    return 0;
	}
	else {    // what happened?
	    if(rdr->status == EVP_STAT_OK) continue;
	    if(rdr->status == EVP_STAT_EOR) return 1;

	    // An error!
	    LOG(ERR, "Error reading!");
	    return -1;
	}
    }
}
	
struct Spec {
    int file;
    int random;
};

int main(int argc, char *argv[])
{
    srand((unsigned int)time(NULL));

    rtsLogOutput(RTS_LOG_STDERR) ;
    rtsLogLevel((char *)WARN) ;

    parseArgs(argc, argv);

    int fd = open(outputfile, O_CREAT | O_WRONLY, 0666);
    if(fd < 0) {
	printf("Error opening output file: %s", outputfile);
	exit(0);
    }
    int n_sourcefiles = argc - firstSourceFile;
  
    int *n_evts = (int *)malloc(sizeof(int) * n_sourcefiles);
    daqReader **rdrs = (daqReader **)malloc(sizeof(daqReader *) * n_sourcefiles);

    int tot_evts = 0;

    for(int i=0;i<n_sourcefiles;i++) {
	n_evts[i] = 0;


	rdrs[i] = new daqReader(argv[firstSourceFile + i]);
	int ret;
	while((ret = getGoodEvent(rdrs[i])) == 0) {
	    n_evts[i]++;
	}

	tot_evts += n_evts[i];
	printf("File #%02d: %s    (%d events)\n",i, argv[firstSourceFile + i], n_evts[i]);
	
	delete rdrs[i];
	rdrs[i] = new daqReader(argv[firstSourceFile + i]);
    }


    // Randomize....  First get the events with a random tag
    Spec *specs = (Spec *)malloc(sizeof(Spec) * tot_evts);
    int idx = 0;
    for(int i=0;i<n_sourcefiles;i++) {
	for(int j=0;j<n_evts[i];j++) {
	    specs[idx].file = i;
	    specs[idx].random = rand();
	    idx++;
	}
    }

    // Now sort by random
    for(int i=0;i<tot_evts;i++) {
	for(int j=i+1;j<tot_evts;j++) {
	    if(specs[i].random > specs[j].random) {
		int t = specs[i].file;
		specs[i].file = specs[j].file;
		specs[j].file = t;
		t = specs[i].random;
		specs[i].random = specs[j].random;
		specs[j].random = t;
	    }
	}
    }

    // Now print out an ordering!
    int k=0;
    for(int i=0;i<tot_evts;i++) {
	
	int file = specs[i].file;
	
	int ret = getGoodEvent(rdrs[file]);
	if(ret != 0) {
	    printf("Error reading from %d: ret = %d", file, ret);
	    continue;
	}

	
	// Omit randomly, then print out!
	double zz = rand();
	zz /= (double)RAND_MAX;
	zz *= 100;
	if(zz >= omit) {
	    printf("[%07d] File: #%02d Evt: #%07d  Seq: #%07d\n", k, file, rdrs[file]->event_number, rdrs[file]->seq);
	    
	    ret = write(fd, rdrs[file]->memmap->mem, rdrs[file]->event_size);
	    if(ret < 0) {
		printf("Error writing to file %s\n", outputfile);
		break;
	    }
	    
	    k++;
	}
    }
  

    close(fd);

    for(int i=0;i<n_sourcefiles; i++) {
	int ret = getGoodEvent(rdrs[i]);
	if(ret != 1) {
	    
	    printf("Error reading EOF from %d: ret = %d\n", file, ret);
	    continue;
	}

	delete rdrs[i];
    }
    
    // LOG(INFO, "Keep event #%d (ptr=0x%x sz=%d)",rdr->seq,rdr->memmap->mem,rdr->event_size);
    // write(STDOUT_FILENO, rdr->memmap->mem, rdr->event_size);

    return 0;
}

