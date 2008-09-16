#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
typedef unsigned int UINT32;
//#include <SVT/key_map.h>

// This source relies on the RTS "LOG" machinery to print log messages
// of misc. severities...
// If you don't like it, uncomment the following line
//#define NO_RTS_LOG
// This changes the default swwitches when running in the STAR environment
//#define STAR_OFFLINE_ENVIRONMENT

#include <rtsLog.h>


#include <TPC/rowlen.h>
#include <TPC/trans_table.hh>

// main include file - resides in daqman.star.bnl.gov:/RTS/include
// which MUST be in your include path....

#include <evpReader.hh>

#include <rtsSystems.h>	// for detector ids...

extern int sanityCheck(char *datap) ;

int repack = 0;
int repack_trigger = 0;
char repack_filename[80];

int main(int argc, char *argv[])
{
	char *mem ;
	int ret ;
	extern char *optarg ;
	extern int optind ;
	int c ;
	char *datap ;
	class evpReader *evp = NULL ;
	u_int good = 0, bad = 0 ;
	static char loglevel[10] ;
	static char mountpoint[32] ;
	int logwhere ;
	int fcount ;
	char *fnames[1000] ;	// up to 1000 files, hope that's enough...
	int i ;
	int dump_det ;
	int check_only ;
	// parse command line arguments
	// but set the defaults before...
	u_int max_events ;
	u_int first_event;
//	int rd = 0;
	int evtype = EVP_TYPE_ANY ;	// any
	strcpy(loglevel,"WARN") ;
	strcpy(mountpoint,"") ;

	repack_filename[0] = '\0';

#ifdef STAR_OFFLINE_ENVIRONMENT
	logwhere = RTS_LOG_STDERR ;
#else
	logwhere = RTS_LOG_STDERR|RTS_LOG_NET ;
#endif

	check_only = 0 ;	// don't _just_ check the headers...

	dump_det = -1 ;	// default is no dump

	first_event = 1;
	max_events = 0xFFFFFFFF ;	// a lot...

	while((c = getopt(argc,argv,"Ct:d:m:w:D:n:f:r:T:")) != EOF)
	switch(c) {
	case 't' :
		evtype = atoi(optarg) ;		// request type
		break ;
	case 'd' :				// logevel i.e. DBG, WARN etc.
		strcpy(loglevel,optarg) ;
		break ;
	case 'm' :				// mountpoint for data on evp i.e. /evp
		strncpy(mountpoint,optarg,sizeof(mountpoint)-1) ;
		break ;
	case 'w' :				// w==2 is STDERR, w==1 is UDP, w==3 is both
		logwhere = atoi(optarg) ;
		break ;
	case 'D' :				// dump data for this detector
		dump_det = atoi(optarg) ;	
		break ;
	case 'C' :				// only run sanity checks
		check_only = 1 ;
		break ;
	case 'n' :
		max_events = atoi(optarg) ;
		break ;
	case 'f' :
	        first_event = atoi(optarg) ;
		break;
        case 'r' :
	  repack = 1;
	  strcpy(repack_filename, optarg);
	  break;

	case 'T' :
	  repack_trigger = atoi(optarg);
	  break;
		
	case '?' :
		fprintf(stderr,"Usage: %s [-t type ] [-d LOG ] [-C] [-m mountpoint] [-w wherelog ] [-D det_id] [-f first] [-n num] [-r repackfilename] [-T triggertorepack] <files...>\n",argv[0]) ;
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
	

		if(fnames[i]) LOG(DBG,"Calling constructor [%d/%d], with filename %s",(i+1),(fcount+1),(int)fnames[i],0,0) ;
		else LOG(DBG,"Calling constructor [%d/%d], with filename NULL",(i+1),(fcount+1),0,0,0) ;

		evp = new evpReader(fnames[i]) ;

		// MUST CHECK THE STATUS!!!
		// as well as the variable itself!
		if(evp == NULL) {
			char *fl = "NULL" ;
			if(fnames[i]) fl = fnames[i] ;
			
			fprintf(stderr,"Constructor for the evpReader failed for file %s!?\n",fl) ;
			return -1 ;
		}


		if(evp->status) {	// error in constructor
			char *fl = "NULL" ;
			if(fnames[i]) fl = fnames[i] ;

			fprintf(stderr,"Error initializing reader for file %s!\n",fl);
			return -1 ;
		}


		// set the evp.star mountpoint i.e. if evp.star is mounted in your local
		// /net/evp.star then mountpoint="/net/evp.star"
		evp->setEvpDisk(mountpoint) ;


		// The EVENT LOOP!!!!

		for(int eeee=1;;eeee++) {
		int end_of_file ;

		// JML test...

//#define GETEVENTNUMTEST
#ifndef GETEVENTNUMTEST
		mem = evp->get(0,evtype) ;	// get the next event!
#else
		mem = evp->get(eeee,evtype);
#endif
		if(mem == NULL) eeee--;

		end_of_file = 0 ;

		if(mem == NULL) {	// event not valid

		  LOG(DBG, "mem == NULL %d",evp->status);

			switch(evp->status) {
			case EVP_STAT_OK :	// should retry as fast as possible...
				continue ;
			case EVP_STAT_EOR :	// EOR or EOR - might contain token 0!
			  LOG(DBG, "End of run");
			  evp->readall_reset();
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


     
#ifdef GETEVENTNUMTEST
		LOG("JEFF", "event_number=%d seq=%d readall_lastevt=%d",
		    evp->event_number, evp->seq, evp->readall_lastevt);

		if(evp->readall_rundone) {
		  rd++;
		  LOG(DBG,"run is done but continuing...");
		  if(rd > 5) { 
		    LOG(DBG,"resetting to new run...");
		    evp->readall_reset();   // this clears the run
		    rd = 0;
		    eeee=0;
		    continue;
		  }
		}
#endif

		if(evp->event_number < first_event) {
		  LOG(DBG, "Skipping event %d",evp->event_number);
		  continue;
		}
		

		if(sanityCheck(mem)) {
			LOG(CAUTION,"Bad event - skipping...") ;
			continue ;
		}

		// once evp->get returns a valid pointer the event is available
		good++ ;
		if(good>max_events) break ;	   


		if((good%1000)==0) {
			LOG(INFO,"Events processed %d...",good,0,0,0,0) ;
		}


		LOG(INFO,"**** Event %d (file: %d): bytes %d, token %d, FILE %s",evp->seq, evp->event_number,evp->bytes,
		    evp->token,(int)evp->file_name) ;

		LOG(NOTE,"**** Event %d: bytes %d, token %d, trg_cmd %d, FILE %s",evp->event_number,evp->bytes,
		    evp->token,evp->trgcmd,(int)evp->file_name) ;

		

		// make humanly readable time from the UNIX time...
		char *stime ;
		stime = ctime((long int *)&evp->evt_time) ;
		*(stime+strlen(stime)-1) = 0 ;

		LOG(NOTE,"     Trigger Word 0x%02X, time %u (%s), daqbits 0x%04X, evpgroups 0x%04X",evp->trgword,evp->evt_time,(int)stime,evp->daqbits,evp->evpgroups) ;
	

		if(repack) {
		  LOG(DBG, "Repack? 0x%x 0x%x",repack_trigger, evp->daqbits);
		  if(((repack_trigger) & evp->daqbits)==repack_trigger) {
		    if(strlen(repack_filename) > 0) {
		      LOG(NOTE, "Repacking event");
		      evp->writeCurrentEventToDisk(repack_filename);
		    }
		    else {
		      LOG(ERR, "Not repacking because have no filename");
		    }
		  }
		}


		LOG(NOTE,"     Dets: 0x%x/0x%x    evpgroups: 0x%x/0x%x",
		    evp->detectors,evp->detsinrun,evp->evpgroups,evp->evpgroupsinrun,0);

		if(check_only) continue ;

//if(evp->token != 0) continue ;

		// the return value of the "get" method points to datap so one may
		// use it of one knows what one is doing (unlikely)

		datap = mem ;

		// everything beyond this point is up to the user
		// and all the calls are optional
		// i.e. the FTPC guys would just call the ftpReader(datap)

		// Here's an SFS example
		if(evp->sfs) {
		  fs_filelist list;
		  fs_dir *dir = evp->sfs->opendir("/");

		  fflush(stdout);
		  if(dir) {
		    evp->sfs->mem_ls(&list,1,dir);
		    
		    for(int i=0;i<list.n;i++) {
		      LOG(NOTE,"DIR: %s",list.filename[i]);
		    }
		  }
		}


#ifdef RTS_PROJECT_PP
		ret = pp2ppReader(datap) ;

		if(ret < 0) {	// error
			LOG(ERR,"pp2pp: error in data (%d) - continuing...",ret,0,0,0,0) ;
		}
		else if(ret == 0) {	// no data
			LOG(NOTE,"pp2pp: not present...",0,0,0,0,0) ;
		}
		else {
			LOG(NOTE,"pp2pp: returned %d",ret,0,0,0,0) ;
			for(i=0;i<3;i++) {
				if(pp2pp.sec[i].len == 0) {
					LOG(NOTE,"Sector %d: not present",i,0,0,0,0) ;
				}
				else {
					LOG(NOTE,"Sector %d: tkn %d, seq %u, xing %u, type 0x%04X",i,
					    pp2pp.sec[i].token,pp2pp.sec[i].seq,pp2pp.sec[i].xing,pp2pp.sec[i].type) ;
				}
			}
		}
#else

		ret = scReader(datap);
		if(ret <= 0) {
		  if(ret < 0) {
		    LOG(ERR, "SC RAW: problems in data (%d) - continuing...",ret,0,0,0,0);
		  }
		  else {
		    LOG(NOTE, "SC RAW: not present...",0,0,0,0,0);
		  }
		}
		else {    // Do something with SC data...
		  if (dump_det == SC_ID) {   // SC_ID = 10
		    printf("SC bank event=%d seq=%d valid=%d, timelag=%d, mag_field=%f (Tesla)\n",evp->event_number, evp->seq,sc.valid,sc.timelag,sc.mag_field);
		    for(i=0;i<16;i++) {
		      printf("  Rich Scalers[%d] = %d\n",i,sc.rich_scalers[i]);
		    }
		    fflush(stdout);
		  }
		}


		ret = 0 ;
		ret = trgReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"TRG RAW: problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"TRG RAW: not present...",0,0,0,0,0) ;
			}
		}
		else {	// do something with it...
			int i ;
			u_int ctbch, mwcch, zdcch ;

			LOG(NOTE,"TRG RAW: %d bytes",ret,0,0,0,0) ;

			
			ctbch = mwcch = zdcch = 0 ;

			for(i=0;i<240;i++) {
				ctbch += trg.CTB[i] ;
			}

			for(i=0;i<96;i++) {
				mwcch += trg.MWC[i] ;
			}

			for(i=0;i<8;i++) {
				zdcch += trg.ZDC[i] ;
			}

		
			if(dump_det == TRG_ID) {
			  for(i=0;i<16;i++) {
			    printf("ZDC[%d] = %d\n",i,trg.ZDC[i]);
			  }
			  for(i=0;i<8;i++) {
			    printf("ZDC_L1[%d] = %d\n",i,trg.ZDC_l1[i]);
			  }
			  for(i=0;i<32;i++) {
			    printf("ZDCSMD[%d] = %d\n",i,trg.ZDCSMD[i]);
			  }

			  for(i=0;i<96;i++) {
			    printf("BBC[%d] = %d\n",i,trg.BBC[i]);
			  }
			}

			LOG(NOTE,"TRG RAW: CTB charge %d, MWC charge %d, ZDC charge %d",ctbch,mwcch,zdcch,0,0) ;
		}


		// search for a specific Trigger ID
//#define CUT_ON_TRG 
#ifdef CUT_ON_TRG 
		int t ;
		for(t=0;t<32;t++) {
			LOG(NOTE,"Trigger %d : %u",t,trg.offline_id[t]) ;
			if(trg.offline_id[t] == 15007) break ;	// i.e. 15105 is central trigger, 15007 is MB
		}
		if(t >= 32) continue ;
		static int dumped ;
		dumped++ ;
		if(dumped>=17) return 0 ;
#endif



		// TPC!
		int sec ;
		for(sec=0;sec<24;sec++) {
			ret = tpcReader(datap,sec) ;
			//LOG("JEFF", " sec %d:  ret=%d",sec+1,ret);

			switch(ret) {
			case EVP_NO_DET :
				LOG(NOTE,"TPC: Sector %d not present...",sec+1,0,0,0,0) ;
				break ;
			case EVP_NO_DATA :
				LOG(NOTE,"TPC: Sector %d not present...",sec+1,0,0,0,0) ;
				break ;
			case EVP_DATA_ERR :
				LOG(ERR,"TPC: Sector %d: problems with data [%d], token %d, EVP num %d, EVB seq %d - continuing...",sec+1,ret,evp->token,evp->event_number,evp->seq) ;
				break ;
			default :
				LOG(NOTE,"TPC: Sector %d: %d bytes",sec+1,ret,0,0,0) ;
				break ;
			}

			//if(ret == EVP_NO_DET) break ;


			if(ret <= 0) continue ;	// try another sector

			LOG(DBG,"Got TPC data...") ;

			// example usage: calculate total charge and 
			// print occupancy
			int r,p,t ;
			u_int adc ;
			u_char val ;



			adc = 0 ;
			if(tpc.mode==0) {	// normal event
				u_int tot_pix = 0 ;
				u_int cl_count = 0 ;
				int i ;

				for(r=0;r<45;r++) {	// padrow
//				for(r=0;r<12;r++) {	// padrow
					for(p=0;p<182;p++) {	// pad
					for(t=0;t<tpc.counts[r][p];t++) {	
						val = tpc.adc[r][p][t] ;										
						int vali = log8to10_table[val];
						adc += val ;
						if(val) tot_pix++ ;

						if(dump_det == TPC_ID) {
							int timebin = tpc.timebin[r][p][t] ;
							//if((sec == 0) && (evp->seq==8) && timebin < 401)
							printf("%d %d %d %d %d %d\n",evp->seq,sec+1,r+1,p+1,timebin,vali) ;
						}
					}
					}

					if(tpc.has_clusters) {
						cl_count += tpc.cl_counts[r] ;
					}

					if(tpc.has_clusters && (dump_det == TPC_ID)) {
						for(i=0;i<tpc.cl_counts[r];i++) {
						  tpc_cl *c = &tpc.cl[r][i] ;
						  
						  printf("%d %d %f %f %d %d %d %d %d %d\n",
						     sec+1,r+1,c->p,c->t,c->charge,c->flags,c->p1,c->p2,c->t1,c->t2) ;
						}
					}
				}



				LOG(NOTE,"TPC: Sector %d: occupancy %3d %%, charge %d, pixels %u, clusters %d",sec+1,
				    (int)(100.0 *((double)tpc.channels_sector/(double)tpc.max_channels_sector)),adc,tot_pix,cl_count) ;


// Example for rebuilding clusters,  this overwrites the clusters already found, if any exist.
#define REBUILD_CLUSTERS
#ifdef REBUILD_CLUSTERS

				// This builds clusters from the raw data
				// overwriting the cluster data in tpc structure

				int ncl_recount = fcfReader(sec+1);
				if((dump_det == TPC_ID)) {
				//if(1) {
				  for(r=0;r<45;r++) {	// padrow
				 
				      for(i=0;i<tpc.cl_counts[r];i++) {
					tpc_cl *c = &tpc.cl[r][i] ;
					
					
					printf("%d %d %f %f %d %d %d %d %d %d\n",
					       sec+1,r+1,c->p,c->t,c->charge,c->flags,c->p1,c->p2,c->t1,c->t2) ;
				      }
				    }
				  
				}
				LOG(NOTE, "TPC: Sector %d: %d recalc clusters", sec+1,ncl_recount);
#endif


			}
			else {	// special mode - currently just for pedestals
				// special mode has different packing i.e. it
				// overrides the meaning of the structure

				// tpc.adc contains the pedestals
				// i.e. the pedestal of sector, row 12, pad 3, timebin 0 is
				// ped = tpc.adc[11][2][0]
				// note that the index goes from 0 not 1 !

				// tpc.timebins contains the RMS's shifted 4 bits
				// i.e. for the RMS of the above
				// rms = (double)tpc.timebins[11][2][0] / 16.0

				LOG(NOTE,"TPC: Sector %d is mode 1 (non-zero suppressed)",sec+1,0,0,0,0) ;
				if(dump_det==TPC_ID) {
					for(r=0;r<45;r++) {
						for(p=0;p<tpc_rowlen[r+1];p++) {
							for(t=0;t<=400;t++) {
								printf("%d %d %d %d %d %f\n",
								       sec+1, r+1, p+1, t, tpc.adc[r][p][t],(double)tpc.timebin[r][p][t]/16.0);
							}
						}
					}
				}
			}

		}



		ret = svtReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"SVT: problems with data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"SVT: not present...",0,0,0,0,0) ;
			}
		}
		else {
			// example usage: calculate total charge and 
			// print occupancy
			int s,r,p,a,t ;
			u_int adc ;
			u_char val, tb ;

			LOG(NOTE,"SVT: %d bytes",ret,0,0,0,0) ;

			if(svt.mode==0) {
				adc = 0 ;
				for(s=0;s<24;s++) {	// receivers
				for(r=0;r<3;r++) {	// mezzanines
				for(p=0;p<6;p++) {	// asic/hybrid
				for(a=0;a<240;a++) {	// anode
				for(t=0;t<svt.counts[s][r][p][a];t++) {
					val = svt.adc[s][r][p][a][t] ;
					tb = svt.timebin[s][r][p][a][t] ;

 					if(dump_det == SVT_ID) {
 							printf("daq: %d %d %d %d %d %d\n",s,r,p,a,tb,val) ;
 					}

// 					if(dump_det == SVT_ID) {
// 					  printf("svt:  %d %d %d %d %d %d %d\n",
// 						 svt.B[s][r][p],
// 						 svt.L[s][r][p],
// 						 svt.W[s][r][p],
// 						 svt.H[s][r][p],
// 						 a+1,tb,val);
// 					}

					adc += val ;
				}}}}}

				LOG(NOTE,"SVT: occupancy %3d %% (channels %d), charge %d",(int)(100.0 *((double)svt.channels/(double)(svt.max_channels))),svt.channels,adc,0,0) ;
			}
			else {
				u_int ped, rms ;
				// look under TPC
				if(dump_det == SVT_ID) {

				for(s=0;s<24;s++) {	// receivers
				for(r=0;r<3;r++) {	// mezzanines
				for(p=0;p<6;p++) {	// asic/hybrid
				for(a=0;a<240;a++) {	// anode
				for(t=0;t<128;t++) {
					ped = svt.adc[s][r][p][a][t] ;
					rms = svt.timebin[s][r][p][a][t] ;

					printf("%d %d %d %d %d %d %d\n",s,r,p,a,t,ped,rms) ;
				}}}}}

				}

			}
		}

		ret = ftpReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"FTP: problems with data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"FTP: not present...",0,0,0,0,0) ;
			}
		}
		else {
			// example usage: calculate total charge and 
			// print occupancy
			int s,r,p,t ;
			u_int adc ;
			u_char val ;

			adc = 0 ;

			LOG(NOTE,"FTP: %d bytes",ret,0,0,0,0) ;
			if(ftp.mode == 0) {
				int cou[2] ;
				for(s=0;s<2;s++) {	// FTPC: east, west
				cou[s] = 0 ;
				for(r=0;r<10;r++) {	// 10 rows each
				for(p=0;p<960;p++) {	// 960 pads each row
				for(t=0;t<ftp.counts[s][r][p];t++) {
					val = ftp.adc[s][r][p][t] ;
					adc += val ;
					cou[s]++ ;
				}}

				LOG(DBG,"FTP %d: RB %d: %d pixels",s+1,r+1,cou) ;
				}}
				//fprintf(stderr,"FTP %d %d %d %d\n",evp->seq,cou[0],cou[1],cou[1]-cou[0]) ;
				LOG(NOTE,"FTP: occupancy %3d %%, charge %d",(int)(100.0 *((double)ftp.channels/(double)ftp.max_channels)),adc,0,0,0) ;
			}
			else {
				// look under TPC
			}
		}

		ret = pmdReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"PMD: problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"PMD: not present...",0,0,0,0,0) ;
			}
		}
		else {
			if(pmd.mode == 0) {	// normal event
				LOG(NOTE,"PMD: occupancy %3d %%, status 0 %d, status 1 %d",(int)(100.0 *((double)pmd.channels/(double)pmd.max_channels)),pmd.status[0],pmd.status[1],0,0) ;

				if(dump_det == PMD_ID) {
					printf("%d %2d %d %4d %u\n",
					       good,-1,-1,pmd.status[0],pmd.status[1]) ;


					int cr, m, ch, sec ;
					for(sec=0;sec<2;sec++) {
					for(cr=0;cr<PMD_CRAMS_MAX;cr++) {
						for(m=0;m<2;m++) {
							for(ch=0;ch<PMD_CRAMS_CH_MAX;ch++) {
								if(pmd.adc[sec][cr][m][ch] == 0) continue ;

								printf("%d %d %2d %d %4d %u\n",
								       good,sec,cr,m,ch,pmd.adc[sec][cr][m][ch]) ;
							}
						}
					}
					}
				}
			}
			else {	// pedestal
				LOG(NOTE,"PMD: PEDESTAL event",(int)(100.0 *((double)pmd.channels/(double)pmd.max_channels)),0,0,0,0) ;

				if(dump_det == PMD_ID) {
					int cr, m, ch, sec ;
					for(sec=0;sec<2;sec++) {
					for(cr=0;cr<PMD_CRAMS_MAX;cr++) {
					for(m=0;m<2;m++) {
					for(ch=0;ch<PMD_CRAMS_CH_MAX;ch++) {
						printf("%d %2d %d %4d %f %f %f\n",
						       sec,cr,m,ch,(double)pmd.ped[sec][cr][m][ch]/16.0,
						       (double)pmd.rms[sec][cr][m][ch]/16.0,
						       (double)pmd.thr[sec][cr][m][ch]/16.0) ;
					}
					}
					}
					}
				}
			}
				
		}

		
#if defined(TPXREADER) && defined(_TPX_READER_H_)
		ret = tpxReader(datap, 18-1, STYLE_TPX) ;
		if(ret <= 0) {
		  if(ret < 0) {
		    LOG(ERR, "TPX: problems in data (%d) continuing",ret,0,0,0,0);
		  }
		  else {
		    LOG(NOTE, "TPX: not present...",0,0,0,0,0);
		  }
		}
		else {
		  LOG(NOTE, "TPX present %d bytes",ret,0,0,0,0);

		  int i,j,k;

		  if(dump_det == TPX_ID) {
		    for(i=0;i<46;i++) {
		      for(j=0;j<182;j++) {
			for(k=0;k<tpx.counts[i][j];k++) {

			// event, sector, row, pad, tb, adc
			  printf("%d %d %d %d %d %d\n",
				 evp->seq,
				 18,
				 i+1,j+1,
				 tpx.timebin[i][j][k],
				 tpx.adc[i][j][k]);
			}
		      }
		    }
		  }
		}
#endif
		
		ret = ssdReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"SSD: problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"SSD: not present...",0,0,0,0,0) ;
			}
		}
		else {
			// RB, mezzanine, asic, channel, strip
			int r,c,s ;
			
			LOG(NOTE,"SSD: occupancy %3d %%  [%d bytes]",(int)(100.0 *((double)ssd.channels/(double)ssd.max_channels)),ret,0,0,0) ;

			if(dump_det == SSD_ID) {
			// OK, what is the mode
			if(ssd.mode == 0) {	// normal data
				for(r=0;r<40;r++) {
				for(c=0;c<64;c++) {
					for(s=0;s<ssd.counts[r][c];s++) {
						// Row, pad, strip, adc
						printf("%d %2d %3d %3d\n",r,c,
						       ssd.strip[r][c][s],
						       ssd.adc[r][c][s]) ;
					}
				}
				}

			}
			else {	// PED, RMS - special handling
				for(r=0;r<40;r++) {
				for(c=0;c<64;c++) {
					for(s=0;s<192;s++) {
						// Row,pad,strip,pedestal,rms
						printf("%d %d %d %3d %f\n",r,c,s,ssd.adc[r][c][s],
						       (double)ssd.strip[r][c][s]/16.0) ;
					}
				}
				}

			}
			}
		}


		ret = emcReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"EMC (any): problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"EMC (any): not present...",0,0,0,0,0) ;
			}
		}
		else {
			LOG(NOTE,"EMC (all): %d bytes",ret,0,0,0,0) ;

			if(emc.btow_in) {
				int i, j ;

				if(emc.btow_ch) {
					LOG(NOTE,"BTOW: occupancy: %3d %%",
					    (int)(100.0 *((double)emc.btow_ch/(double)emc.btow_max_ch)),0,0,0,0) ;
				}
				else {
					LOG(WARN,"BTOW: all data  0???",0,0,0,0,0) ;
				}

				if(dump_det == BTOW_ID) {


					for(i=0;i<BTOW_MAXFEE;i++) {
						for(j=0;j<BTOW_PRESIZE;j++) {
							printf("Event %d: BTOW preamble %d:%d : 0x%04X (%u dec)\n",good,i,j,emc.btow_pre[i][j],emc.btow_pre[i][j]) ;
						}
					}
					for(i=0;i<BTOW_MAXFEE;i++) {
						for(j=0;j<BTOW_DATSIZE;j++) {
							printf("Event %d: BTOW data %d:%d 0x%04X (%u dec)\n",good,i,j,emc.btow_new[i][j],emc.btow_new[i][j]) ;
						}
					}
				}


			}
			else {
				LOG(NOTE,"BTOW: not present...",0,0,0,0,0) ;
			}

			if(emc.bsmd_in) {

				if(emc.bsmd_ch) {
					LOG(NOTE,"BSMD: occupancy: %3d %%",
					    (int)(100.0 *((double)emc.bsmd_ch/(double)emc.bsmd_max_ch)),0,0,0,0) ;
				}
				else {
					LOG(WARN,"BSMD: all data  0???",0,0,0,0,0) ;
				}

				if(dump_det == BSMD_ID) {
					int i,j ;

					for(i=0;i<12;i++) {
					for(j=0;j<4800;j++) {
						//data = l2h16(*(emc.btow_raw+i)) ;
						printf("BSMD fiber %d: ch %4d: 0x%04X [%5u]\n",i+1,j,emc.bsmd[i][j],emc.bsmd[i][j]) ;
					}
					}

				}

			}
			else {
				LOG(NOTE,"BSMD: not present...",0,0,0,0,0) ;
			}

#if I_KNOW_ABOUT_BARREL_PRESHOWER
			if(emc.bpre_in) {

				if(emc.bpre_ch) {
					LOG(NOTE,"BPRE: occupancy: %3d %%",
					    (int)(100.0 *((double)emc.bpre_ch/(double)emc.bpre_max_ch)),0,0,0,0) ;
				}
				else {
					LOG(WARN,"BPRE: all data below 0???",0,0,0,0,0) ;
				}
			}
			else {
				LOG(NOTE,"BPRE: not present...",0,0,0,0,0) ;
			}
#endif
			if(emc.etow_in) {
				int i, j;

				if(emc.etow_ch) {
					LOG(NOTE,"ETOW: occupancy: %3d %%",
					    (int)(100.0 *((double)emc.etow_ch/(double)emc.etow_max_ch)),0,0,0,0) ;
				}
				else {
					LOG(WARN,"ETOW: all data below 0???",0,0,0,0,0) ;
				}


				if(dump_det == ETOW_ID) {


					for(i=0;i<ETOW_MAXFEE;i++) {
						for(j=0;j<ETOW_PRESIZE;j++) {
							printf("ETOW preamble %d:%d : 0x%04X (%u dec)\n",i,j,emc.etow_pre[i][j],emc.etow_pre[i][j]) ;
						}
					}
					for(i=0;i<ETOW_MAXFEE;i++) {
						for(j=0;j<ETOW_DATSIZE;j++) {
							printf("ETOW data %d:%d 0x%04X (%u dec)\n",i,j,emc.etow[i][j],emc.etow[i][j]) ;
						}
					}
				}
			}
			else {
				LOG(NOTE,"ETOW: not present...",0,0,0,0,0) ;
			}

			if(emc.esmd_in) {
				int i, j ;

				if(emc.esmd_ch) {
					LOG(NOTE,"ESMD: occupancy: %3d %%",
					    (int)(100.0 *((double)emc.esmd_ch/(double)emc.esmd_max_ch)),0,0,0,0) ;
				}
				else {
					LOG(WARN,"ESMD: all data below 0???",0,0,0,0,0) ;
				}


				if(dump_det == ESMD_ID) {
					for(i=0;i<ESMD_MAXFEE;i++) {
						for(j=0;j<ESMD_PRESIZE;j++) {
							printf("ESMD preamble %d:%d 0x%04X (%u dec)\n",i,j,emc.esmd_pre[i][j],emc.esmd_pre[i][j]) ;
						}
					}

					for(i=0;i<ESMD_MAXFEE;i++) {
						for(j=0;j<ESMD_DATSIZE;j++) {
							printf("ESMD data %d:%d 0x%04X (%u dec)\n",i,j,emc.esmd[i][j],emc.esmd[i][j]) ;
						}
					}
				}


			}
			else {
				LOG(NOTE,"ESMD: not present...",0,0,0,0,0) ;
			}


		}

		ret = tofReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"TOF: problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"TOF: not present...",0,0,0,0,0) ;
			}
		}
		else {
			LOG(NOTE,"TOF: %d bytes",ret,0,0,0,0) ;
			if(dump_det == TOF_ID) {
				u_int ddl, j ;
				for(ddl=0;ddl<4;ddl++) {
					if(tof.ddl_words[ddl]) {
						for(j=0;j<tof.ddl_words[ddl];j++) {
							printf("TOF DDL %d, word %d\t0x%08X\n",ddl,j,tof.ddl[ddl][j]) ;
						}
					}
				}
			}
		}

		ret = l3Reader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"L3: problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"L3: not present...",0,0,0,0,0) ;
			}
		}
		else {
			LOG(NOTE,"L3: %d bytes",ret,0,0,0,0) ;
		}

		ret = fpdReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"FPD: problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"FPD: not present...",0,0,0,0,0) ;
			}
		}
		else {
			LOG(NOTE,"FPD: %d bytes",ret,0,0,0,0) ;
		}

		ret = ricReader(datap) ;
		if(ret <= 0) {
			if(ret < 0) {
				LOG(ERR,"RIC: problems in data (%d) - continuing...",ret,0,0,0,0) ;
			}
			else {
				LOG(NOTE,"RIC: not present...",0,0,0,0,0) ;
			}
		}
		else {
			LOG(NOTE,"RIC: %d bytes",ret,0,0,0,0) ;
			LOG(NOTE,"RICH: occupancy: %3d %%",
			    (int)(100.0 *((double)ric.channels/(double)ric.max_channels)),0,0,0,0) ;

		}

#endif	// end of RTS_PROJECT_PP


		}	// end of EVENT LOOP


		LOG(INFO,"Processed %s: %d good, %d bad events total...",(int)fnames[i],good,bad,0,0) ;

		delete evp ;
	}	// end of input file loop

	return 0 ;
}
