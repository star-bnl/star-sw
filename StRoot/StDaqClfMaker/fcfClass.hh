/*
	Version 3.0	C++ Class-based version
	Version 3.11	Overhaul of internal structures
	
*/

#ifndef _FCF_CLASS_HH_

#define _FCF_CLASS_HH_

// flag definitions - NEVER CHANGE
#define FCF_ONEPAD		1
#define FCF_DOUBLE_PAD		2
#define FCF_DOUBLE_T		4
#define FCF_FALLING		8
#define FCF_TWOPAD		16

// FCF paramters defaults - DON'T MUCK AROUND!
#define FCF_PARAM1		2
#define FCF_MIN_ADC_T		3
#define FCF_MIN_ADC_PAD		12


typedef unsigned int u_int;
typedef unsigned short u_short;
typedef unsigned char u_char;

class fcfClass {
public:

	fcfClass(int det, u_short *table = NULL) ;	// constructor

	// the following are set by the constructor depending on the detector parameter
	// They may be changed explicitly for tests & debug thus they are "public"

	int detector ;	// i.e. TPC_ID, SVT_ID
	u_int maxTimebin ;	// 511 TPC, 127 SVT
	u_int maxCPP ;	// 8 SVT, 31 TPC

	// the following MUST BE SET once!
	u_int svtPedestal ;	// when using SVT pedestal offet...
	u_int timebinLo ;
	u_int timebinHi ;
	u_int chargeMin ;

	u_int deconTime  ;
	u_int deconPad  ;
	u_int doCuts  ;

	int param1  ;
	int minAdcT  ;
	int minAdcPad  ;

	// the following changes from call to call (row to row...)
	int row ;
	int padStart ;
	int padStop ;	// absolute start/stop pads

	u_int *adcOff ;	// offsets for the current row only!
	u_int *cppOff ;	
	u_int maxClusters ;	// maximum allowed # clusters in this row

	// used perhaps...
	int sb ;	// i.e. sector number, 1..24
	int rb ;	// receiver board,	1..12
	int mz ;	// mezzanine, 1..3


	// THA function!
	int finder(u_char *adcin, u_short *cppin, u_int *outres) ;

	void set8to10(u_short *extTable) ;

private:
	
	u_short *a8to10 ;	// internal copy
	int noADCconversion ;

	// used in UNIX versions only...
	u_short adc8to10_storage[256] ;

} ;

/* Intended use:

	// ONCE!
	class fcfClass tpc_cf(TPC_ID) ;	// once!

	tpc_cf.set8to10(adc8to10_table) ;	// once!

	tpc_cf.svtPedestal = 0 ;	// 0 for the TPC...
	tpc_cf.timebinLo = 0 ;		// timebin starts from 0!
	tpc_cf.timebinHi= 511 ;		// ends at 511!
	tpc_cf.chageMin = 40 ;		// minimum charge allowed per cluster


	u_int *outres ;
	u_int data_for_sector[1000000] ;

	
	for(sec=1;sec<24;sec++) {

		u_int got_rows = 0 ;		// zero row count

		outres = data_for_sector ;	// point to big-buffer

		u_int *num_of_rows = outres ;	// remember the first pointer for later storage of
						// the total number of rows...
		outres++ ;			// and advance it...


		for(row=1;row<45;row++) {
			tpc_cf.row = row ;
			tpc_cf.padStart = yada_start ;	// depends...
			tpc_cf.padEnd = whatever ;	// depends...
			// make adcOff, cppOff point to correct offsets in the data
			tpc_cf.adcOff = &adcOff[row][0] ;	// pad starts from 1
			tpc_cf.cppOff = &cppOff[row][0] ;	// pad starts from 1


			// make adc_data, cpp_data point to data...
			adc_data = (u_char *) asd ;
			cpp_data = (u_short *) asdads ;
			
			// ...and run!
			ret = tpc_cf.finder(adc_data,cpp_data,outres) ;

			if(ret > 0) {	// number of 4byte words used ...
				got_rows++ ;
				outres += ret ;
			}
		}

		if(got_rows) {		// got at least one row!

			*num_of_rows = got_rows ;

			// use the data somehow...

		}
	}


*/


#endif
