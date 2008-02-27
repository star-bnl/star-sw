#ifndef _TPC_READER_H_
#define _TPC_READER_H_

#include <sys/types.h>

#define TPC_READER_MAX_CLUSTERS	5000


// cluster structures
struct tpc_cl  {
	float p ;
	float t ;
	u_short charge ;
	u_short flags ;
	u_short t1, t2, p1, p2 ;
//#ifdef FCF_SIM_ON
	u_short adc_max ;
//#endif
} ;

struct tpc_t {

	int mode ;	// 0 normal, 1 pedestals/RMSs
	int max_channels_all ;
	int max_channels_sector ;
	int channels_sector ;
	int has_clusters ;	// are there any clusters in the data?

	// how many valid timebins in this pad
	u_short counts[45][182] ;

	// up to 512 valid timebins (count is in counts)
	// timebin is overloaded with RMS data if mode==1
	u_short timebin[45][182][512] ;

	// up to 512 valid adcs (same count as above...)
	// overloaded with PED data if mode==1
	u_char adc[45][182][512] ;

	// how many valid clusters in this row
	u_short cl_counts[45] ;

	// cluster structures
	struct tpc_cl cl[45][TPC_READER_MAX_CLUSTERS] ;

	u_int *cl_p[45][3] ;	// points MZ row data

	u_char rdo_present[6] ;	// boolean stating the presence of an RDO
} ;

extern struct tpc_t tpc ;

extern int tpcReader(char *mem, int sector) ;
extern int fcfReader(int sector, int *t0 = NULL, u_int *gain = NULL);
#endif
