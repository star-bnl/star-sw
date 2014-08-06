#ifndef _DAQ_SSD_H_
#define _DAQ_SSD_H_

#include <stdio.h>
#include <DAQ_READER/daq_det.h>

/*
	SSD has 10 ladders, 16 modules, 2 sides, 768 strips each 8bits wide

	However! The DAQ maps the data as:
		40 pseudo-rows
			64 pseudo-pads
				192 pseudo-timebins (strips)
	
	The pseudo-rows are constructed as
		ReceiverBoard[0..3] * 10 + Mezzanine[0..1] * 5 + ASIC[0..5]


*/

struct ssd_t {
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

} ;



extern int ssd_reader(char *mem, struct ssd_t *ssd, u_int driver) ;


class daq_ssd : public daq_det {
private:
	class daq_dta *handle_legacy() ;

	class daq_dta *legacy ;	// "legacy" bank

	static const char *help_string ;
protected:


public:
	daq_ssd(daqReader *rts_caller=0) ;
	~daq_ssd() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_SSD_H_
