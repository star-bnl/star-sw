#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>

#include "daqFormats.h"
#include "FtfSl3.h"

#define BSIZE	(1024*4)

unsigned int swap32(unsigned int in);

int SB, RB, MZ ;



char *finder(char *buf, char *str, int len) ;

static char *buff ;


int main(int argc, char *argv[])
{
	int rest;
	char *start ;
	off_t pos ;
	int fd ;
	struct TPCSECLP *seclp ;

	int itrack;
	int events = 1 ;

	char *mm ;

	int debugLevel = 5 ;
	//
	//   Setup tracker
	//
	FtfSl3       tracker ;
	tracker.debugLevel = debugLevel ;
	FtfPara* para = &(tracker.para);
	tracker.setup();
	tracker.para.fillTracks = 1 ;

	char* bufferC; 
	char* fileName = "/afs/rhic/star/users/yepes/try/cos_B1.dta";



	fd = open (fileName, S_IREAD, 0777) ;
	if (fd < 0) {
		perror(fileName) ;
		return -1 ;
	}


	// get the file size ;
	pos = lseek (fd, 0, SEEK_END) ;
	fprintf (stderr, "File size %d\n", (int) pos) ;

	mm = (char *) mmap (0, pos, PROT_READ, MAP_PRIVATE, fd, 0) ;

	if(mm == NULL) {
		perror("mmap") ;
		return -1 ;
	}

	buff = mm ;


//	for(;;) {
	for (events=0; events<2;) {

	        fprintf (stderr, "****look for next event****\n");
		// check reamining buffer size
		if (((int)(buff-mm) + BSIZE) > pos) {
		        rest = (int) pos - (int)(buff-mm);
		}
		else {
		        rest = BSIZE;
		} 
		//fprintf(stderr, "rest= %i\n", rest);
		if ((start = finder(buff,"TPCSECLP",rest))) {
			;
		}
		else {
			buff += BSIZE ;
			//fprintf(stderr,"Skipping buff-mm %d \n",buff-mm) ;

			if(((int)(buff-mm) + BSIZE) > pos) return 0 ;
			continue ;
		}

		//fprintf(stderr,"Starting at buff-mm %d, start 0x%08X \n",buff-mm,(char *)start-mm) ;

		seclp = (struct TPCSECLP *) start ;


		fprintf(stderr,"***** EVENT %d, token %d *****\n",
			events++, swap32(seclp->bh.token)) ;

                printf ( "before read Sector\n");
		tracker.readSector(seclp) ;	

		tracker.processSector();

		printf("\n");
		for (itrack=0; itrack<tracker.nTracks; itrack++) {
		        printf("Track # %d \n", itrack);
			printf("pt: \t %f \n", tracker.track[itrack].pt);
			printf("nHits: \t %d \n", tracker.track[itrack].nHits);
			printf("z0 \t %f \n", tracker.track[itrack].z0);
			printf("r0 \t %f \n", tracker.track[itrack].r0);
			printf("tanl \t %f \n", tracker.track[itrack].tanl);
			printf("psi \t %f \n", tracker.track[itrack].psi);
			printf("phi0 \t %f \n", tracker.track[itrack].phi0);
			printf("chi1 \t %f \n", tracker.track[itrack].chi2[0]);
			printf("chi2 \t %f \n", tracker.track[itrack].chi2[1]);
			printf("\n");
		}
		

		buff += 4*BSIZE;
	}

	return 0 ;

}



char *finder(char *buf, char *str, int len)
{
	int i ;
	char *ss, *fbuf ;
	int first, cou ;

	ss = str ;
	cou =  strlen(str) ;

	first = 1 ;

	for(i=0;i<len;i++) {
	        //fprintf(stderr,"i: %u\n", i);
		if(*buf == *ss) {
			ss++ ;
			cou-- ;
			if(first) fbuf = buf ;
			first = 0 ;
			if(cou == 0) {
				//fprintf(stderr,"Found it %s!\n",str) ;
				return fbuf ;
			}
		}
		else {
			ss = str ;
			cou = strlen(str) ;
			first = 1 ;
		}
		buf++ ;
	}
	return NULL ;
}







