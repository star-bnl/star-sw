#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// This source relies on the RTS "LOG" machinery to print log messages
// of misc. severities...
// If you don't like it, uncomment the following line
//#define NO_RTS_LOG

// you must do this!

#ifdef NO_RTS_LOG
#define LOG(a1,a2,b1,b2,b3,b4,b5)
#else
#include <rtsLog.h>
#endif



#include <evpReader.hh>

#include <rtsSystems.h>	// for detector ids...


int main(int argc, char *argv[])
{
	char *mem ;
	int ret ;
	extern char *optarg ;
	extern int optind ;
	int c ;
	char *datap ;
	class evpReader *evp = NULL ;
	int good = 0, bad = 0 ;
	static char loglevel[10] ;
	static char mountpoint[32] ;
	int logwhere ;
	int fcount ;
	char *fnames[1000] ;	// up to 1000 files, hope that's enough...
	int i ;
	int dump_det ;

	// parse command line arguments
	// but set the defaults before...

	int evtype = EVP_TYPE_ANY ;	// any
	strcpy(loglevel,ERR) ;
	strcpy(mountpoint,"") ;
	logwhere = 0 ;
	dump_det = 0 ;	// default is no dump

	while((c = getopt(argc,argv,"t:d:m:w:D")) != EOF)
	switch(c) {
	case 't' :
		evtype = atoi(optarg) ;	// request type
		break ;
	case 'd' :
		strcpy(loglevel,optarg) ;
		break ;
	case 'm' :
		strncpy(mountpoint,optarg,sizeof(mountpoint)-1) ;
		break ;
	case 'w' :
		logwhere = atoi(optarg) ;
		break ;
	case 'D' :
		dump_det = 1 ;	// dump data for this detector
		break ;
	case '?' :
		fprintf(stderr,"Usage: %s [-t type ] [-d LOG ] [-m mountpoint] [-w wherelog ] [-D det_id] [file]\n",argv[0]) ;
		return -1 ;
		break ;
	}



#ifndef NO_RTS_LOG
	rtsLogLevel(loglevel) ;

	if(logwhere) {
		rtsLogOutput(logwhere) ;
	}
#endif


	// repack pointers to filenames
	fcount = 0 ;
	if(optind < argc) {
		while(optind < argc) {
			fnames[fcount] = argv[optind] ;
			optind++ ;
			fcount++ ;
		}
	}
	else {
		fnames[fcount] = NULL ;
		fcount = 1 ;
	}


	good = bad = 0 ;


	for(i=0;i<fcount;i++) {
		// One must first call the constructor of the data
		// stream.
		// This is very lightweight and doesn't take too much
		// time or space.
		// One can have multiple streams but keep in mind
		// that each detector reader has one buffer!
		// The argument is either a string in which case
		// the call will determine if it's a directory
		// or a file and proceed accordingly.
		// If the argument is NULL it will connect to the
		// current LIVE! run
	

		LOG(DBG,"Calling constructor [%d/%d], with filename %s",(i+1),(fcount),(int)fnames[i],0,0) ;
		evp = new evpReader(fnames[i]) ;

		// MUST CHECK THE STATUS!!!
		// as well as the variable itself!
		if(evp == NULL) {
			fprintf(stderr,"Constructor for the evpReader failed for file %s!?\n",fnames[i]) ;
			return -1 ;
		}


		if(evp->status) {	// error in constructor
			fprintf(stderr,"Error initializing reader for file %s!\n",fnames[i]);
			return -1 ;
		}


		// set the evp.star mountpoint i.e. if evp.star is mounted in your local
		// /net/evp.star then mountpoint="/net/evp.star"
		evp->setEvpDisk(mountpoint) ;


		// The EVENT LOOP!!!!

		for(;;) {
		int end_of_file ;

		mem = evp->get(0,evtype) ;	// get the next event!

			
		end_of_file = 0 ;
		if(mem == NULL) {	// event not valid
			switch(evp->status) {
			case EVP_STAT_OK :	// should retry as fast as possible...
				continue ;
			case EVP_STAT_EOR :	// EOR or EOR
				if(evp->isevp) {	// keep going until the next run...
					sleep(5) ;	// ...but let's be friendly...
					continue ;
				}
				else {
					end_of_file = 1 ;
					break ;
				}
			case EVP_STAT_EVT :
				bad++ ;
				LOG(WARN,"Problem getting event - skipping [good %d, bad %d]",good,bad,0,0,0) ;
				sleep(5) ;	// wait a sec...
				continue ;
			case EVP_STAT_CRIT :
				LOG(CRIT,"Critical error - halting...",0,0,0,0,0) ;
				return -1 ;
			}
				
		}

		if(end_of_file) break ;	// go to next file


		// once evp->get returns a valid pointer the event is available
		good++ ;
		if((good%1000)==0) {
			LOG(WARN,"Events processed %d...",good,0,0,0,0) ;
		}

		LOG(NOTE,"**** Event %d: bytes %d, token %d, trg_cmd %d, FILE %s",evp->event_number,evp->bytes,
		    evp->token,evp->trgcmd,(int)evp->file_name) ;

		// make humanly readable time from the UNIX time...
		char *stime ;
		stime = ctime((long int *)&evp->evt_time) ;
		*(stime+strlen(stime)-1) = 0 ;

		LOG(NOTE,"     Trigger Word 0x%02X, time %u (%s), daqbits 0x%04X",evp->trgword,evp->evt_time,(int)stime,evp->daqbits,0) ;


		// the return value of the "get" method points to datap so one may
		// use it of one knows what one is doing (unlikely)

		datap = mem ;

		// everything beyond this point is up to the user
		// and all the calls are optional



		ret = pp2ppReader(datap) ;

		if(ret < 0) {	// error
			LOG(ERR,"pp2pp: error in data (%d) - continuing...",ret,0,0,0,0) ;
		}
		else if(ret == 0) {	// no data
			LOG(NOTE,"pp2pp: not present...",0,0,0,0,0) ;
		}
		else {
			int bxing, yxing ;
			
			yxing = pp2pp.sec[0].xing/2 -1 ;
			bxing = pp2pp.sec[1].xing/2 -1 ;
			if(bxing == -1) bxing = 59 ;
			if(yxing == -1) yxing = 59 ;


			LOG(NOTE,"pp2pp: evt %d: trg type 0x%08X, trg xing %d, sec1 %d, sec2 %d",
			    good,pp2pp.cam.type,pp2pp.cam.xing,pp2pp.sec[0].xing,pp2pp.sec[1].xing) ;

			if(pp2pp.sec[0].len == 0) yxing = pp2pp.cam.xing ;
			if(pp2pp.sec[1].len == 0) bxing = pp2pp.cam.xing ;

			if((bxing != yxing) ||
			   (bxing != (int)pp2pp.cam.xing) ||
			   (yxing != (int)pp2pp.cam.xing)) {

				if(pp2pp.cam.type != 0x40000000) {
					LOG(WARN,"Xing mismatch: evt %d: trg type 0x%08X, trg xing %d, sec1 %d, sec2 %d",
					    good,pp2pp.cam.type,pp2pp.cam.xing,pp2pp.sec[0].xing,pp2pp.sec[1].xing) ;
				}
			}

			int i, j ;
			// check errors
			for(i=0;i<MAXSEQ;i++) {
				for(j=0;j<MAXCHAIN;j++) {
					if(pp2pp.sec[0].len && pp2pp.sec[0].err[i][j]) {
						LOG(WARN,"Event %d: sector %d, seq %d, chain %d in error",good,0+1,i+1,j+1,0) ;
					}
					if(pp2pp.sec[1].len && pp2pp.sec[1].err[i][j]) {
						LOG(WARN,"Event %d: sector %d, seq %d, chain %d in error",good,1+1,i+1,j+1,0) ;
					}
				}
			}



			for(i=0;i<3;i++) {
				if(i==2) {
					if(pp2pp.cam.len == 0) {
						LOG(NOTE,"CAMAC: not present",i,0,0,0,0) ;
						continue ;
					}
					else {
						LOG(NOTE,"CAMAC: tkn %d, seq %u, xing %u, type 0x%04X",
						    pp2pp.cam.token,pp2pp.cam.seq,pp2pp.cam.xing,pp2pp.cam.type,0) ;
					}
				}
				else {
					if(pp2pp.sec[i].len == 0) {
						LOG(NOTE,"Silicon %d: not present",i,0,0,0,0) ;
						continue ;
					}
					else {
						LOG(NOTE,"Silicon %d: tkn %d, seq %u, xing %u, type 0x%04X",i,
						    pp2pp.sec[i].token,pp2pp.sec[i].seq,pp2pp.sec[i].xing,pp2pp.sec[i].type) ;
					}
				}


				if(dump_det) {
					u_int n, a ;

					switch(i) {
					case 2 :	// camac
						printf("%d %d %d %u %u\n",good,i,-1,pp2pp.cam.xing,pp2pp.cam.type) ;
						for(n=0;n<21;n++) {
							for(a=0;a<12;a++) {
								if(pp2pp.cam.d[n][a] != 0xFFFFFFFF) {
									printf("%d %d %d %d %u\n",good,i,n+1,a+1,pp2pp.cam.d[n][a]) ;

								}
							}
						}
						break ;
					default :	// silicon
						
						printf("%d %d %d %u %u %d %d\n",good,i,-1,pp2pp.sec[i].xing,pp2pp.sec[i].type,0,0) ;
						int seq, chain,svx,ch ;

						for(seq=0;seq<4;seq++) {
						for(chain=0;chain<4;chain++) {
						for(svx=0;svx<6;svx++) {
						for(ch=0;ch<128;ch++) {
							printf("%d %d %d %d %d %d %u\n",good,i,seq,chain,svx,ch,pp2pp.sec[i].d[seq][chain][svx][ch]) ;
						}}}}
						break ;

					}
				}
			}
		}

		}	// end of EVENT LOOP

		delete evp ;
	}// end of input file loop

	return 0 ;
}
