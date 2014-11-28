#ifndef _SSD_READER_H_
#define _SSD_READER_H_

#include <sys/types.h>

/*
	SSD has 10 ladders, 16 modules, 2 sides, 768 strips each 8bits wide

	However! The DAQ maps the data as:
		40 pseudo-rows
			64 pseudo-pads
				192 pseudo-timebins (strips)
	
	The pseudo-rows are constructed as
		ReceiverBoard[0..3] * 10 + Mezzanine[0..1] * 5 + ASIC[0..5]


*/
namespace  OLDEVP {
struct ssd_t {
        ssd_t();
 void   reset();
 int    check();
 
        int fenceA;
	int channels ;
	int mode ;	// 0 normal, 1 pedestals/RMSs
	int max_channels ;

	// how many valid strips in this hybrid
	u_char counts[40][64] ;

	// Up to 192 valid strips (count is in counts)
	// strip is overloaded with RMS data if mode==1
	u_char strip[40][64][192];

	// Up to 192 valid adcs (same count as above...)
	// overloaded with PED data if mode==1
	u_char adc[40][64][192];

	// To facilitate decoding in raw events
	// this points to raw data of ONE mezzanine
	// this raw data is 6*64*512 bytes long
	// There are only 2 mezzanines/RB
	u_char *raw[4][2] ;
        int fenceZ;

};

extern struct ssd_t ssd ;

extern int ssdReader(char *mem) ;
}
#endif
