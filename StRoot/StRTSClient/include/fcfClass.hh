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
#define FCF_ROW_EDGE		16	// touched end of row
#define FCF_BROKEN_EDGE		32	// touches one of the mezzanine edges
#define FCF_DEAD_EDGE		64	// touches a dead pad
#define FCF_IGNORE		128

// FCF paramters defaults - DON'T MUCK AROUND!
#define FCF_PARAM1		2
#define FCF_MIN_ADC_T		3
#define FCF_MIN_ADC_PAD		12


#define FCF_GAIN_SHIFT		6			// shift left/righ
#define FCF_GAIN_FACTOR		(1<<FCF_GAIN_SHIFT)	// 64!!!


#ifdef FCF_EXTENSION
#define FCF_WORDS_PER_CLUSTER	3
#else
#define FCF_WORDS_PER_CLUSTER	2
#endif

#if defined(unix) || defined(__ROOT__)
	typedef unsigned int u_int;
	typedef unsigned short u_short;
	typedef unsigned char u_char;  
#endif


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
	int padMax ;	// maximum pad for this row

	u_int *adcOff ;	// offsets for the current row only!
	u_short *cppOff ;	
	u_int maxClusters ;	// maximum allowed # clusters in this row

	// offset to the pad-to-pad t0 correction (multipled by gain and 64!), V4.10
	int *t0Corr ;

	// offset to the pad gain table (multipled by 64!), 
	u_int *gainCorr ;

	u_short *startFlags ;

	// used perhaps...
	int sb ;	// i.e. sector number, 1..24
	int rb ;	// receiver board,	1..12
	int mz ;	// mezzanine, 1..3


	// THA function!
	int finder(u_char *adcin, u_short *cppin, u_int *outres) ;

	// support
	inline int saveRes(struct fcfResx *res_p[], int cou, u_int *output) ;
	void set8to10(u_short *extTable) ;

private:
	struct fcfResx *resx[2][512] ;
	
	u_short *a8to10 ;	// internal copy
	int noADCconversion ;

	// used in UNIX versions only...
	u_short adc8to10_storage[256] ;

	u_int chargeMinCorrect ;
} ;


#endif
