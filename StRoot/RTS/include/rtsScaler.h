#ifndef _RTS_SCA_H_
#define _RTS_SCA_H_

/* LINUX issues */
typedef unsigned int uint ;
#ifndef SHM_SHARE_MMU
#define SHM_SHARE_MMU  0
#endif



/* 
	Default monitoring host.
	Default monitoring UDP port.
*/

#define RTS_SCA_PORT		8101
#define RTS_SCA_HOST		"boothost"
#define RTS_SCA_FILE		"/RTS/log/scalerServer.dta"

#define TRG_HISTO_PORT		8102

/*
	All the items are unsigned ints because I don't want to deal
	with allignment problems on var. compilers/platforms.
	All MUST be BigEndian!
*/



#define RTS_SCA_VERSION	0x000100000	/* in yyyy.xxxx BCD format */



/* total number of entities (i.e. node-task pairs we support) */
#define RTS_SCA_MAX_NODES	(16)
#define RTS_SCA_USER_WORDS	50	/* NX4bytes of storage for user defined entries */ 
#define RTS_SCA_TRG_WORDS	20 

struct rtsScaStruct {
/* these MUST be present! */
	unsigned int size ;		/* size in bytes of this message */
	unsigned int node ;		/* RTS Node Id of this sender */
	unsigned int task ;		/* RTS TaskId of the sender */
	unsigned int version ;		/* version of this struct - see RTS_SCA_VERSION */
	unsigned int tim ;		/* current time in UNIX seconds */
	unsigned int state ;		/* RC States */
	unsigned int user[RTS_SCA_USER_WORDS] ;
	struct trg_sca {
		unsigned int trgword ;
		unsigned int evts_ok ;	// num of events to taper
		unsigned int evts_bad ;	// num of events which never completed
		unsigned int mb_sec ;	// date rate of events to taper
		unsigned int evts_built ;
		unsigned short evts_sec ; // rate of events to taper
		unsigned char dead ;	// percent deadtime
		unsigned char flags ;	// flags - undefined for now...
		unsigned int res[3] ;	// reserved
	} trg_sca[RTS_SCA_TRG_WORDS] ;
} ;


// total size in shared memory
#define RTS_SCA_DATA_SIZE	(sizeof(struct rtsScaStruct)*RTS_SCA_MAX_NODES)

#define SCA_TPC	16
#define SCA_L25 26
#define SCA_L3  27
#define SCA_ALL 28

#define SCA_LAST 29



struct trgHisto {
	unsigned int trgw ;
	unsigned int bits ;
	unsigned int mask ;
	unsigned int h[128] ;
} ;

struct allHisto {
	unsigned int tot_size ;
	unsigned int clock ;
	unsigned int count ;
	struct trgHisto trgHisto[100] ;
} ;



#endif
