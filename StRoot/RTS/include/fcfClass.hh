/*
	Version 3.0	C++ Class-based version
	Version 3.11	Overhaul of internal structures
	
*/

#ifndef _FCF_CLASS_HH_
#define _FCF_CLASS_HH_

#include <sys/types.h>


// steering - watch it!
//#define FCF_ANNOTATE_CLUSTERS



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
#define FCF_PARAM1		2	// was 2
#define FCF_MIN_ADC_T		3	// was 3
#define FCF_MIN_ADC_PAD		12	// was 12


#define FCF_GAIN_SHIFT		6			// shift left/righ
#define FCF_GAIN_FACTOR		(1<<FCF_GAIN_SHIFT)	// 64!!!


#define FCF_WORDS_PER_CLUSTER	2


#if defined(__unix) || defined(__ROOT__) || defined(__CINT__)

typedef unsigned int u_int;
typedef unsigned short u_short;
typedef unsigned char u_char;  

#define FCF_SIM_ON

#else

#ifdef FCF_ANNOTATE_CLUSTERS
#undef FCF_ANNOTATE_CLUSTERS
#endif

#ifdef FCF_DEBUG_OUTPUT
#undef FCF_DEBUG_OUTPUT
#endif

#ifdef FCF_SIM_ON
#undef FCF_SIM_ON
#endif

#endif


// used if the FCF_ANNOTATE_CLUSTERS is set...
struct fcfPixAnnotate {
	u_short adc ;	// the 10 bit ADC value of the charge
	u_short cl_id ;	// the FCF cluster ID (local to the row)
	u_short id_simtrk ;	// the Simulated track ID (local to the event)
	u_short res ;		// reserved...
} ;


// used for simulated tracks...
struct FcfSimOutput {   // this is just the payload definition.
  short id_simtrk;
  short id_quality;
  short cl_id ;		// local cluster id
  short reserved;	// reserved
};


#ifdef FCF_ANNOTATE_CLUSTERS
#ifndef FCF_SIM_ON
#define FCF_SIM_ON
#endif

#ifndef __ROOT__

extern struct fcfPixAnnotate fcfPixA[45][182][512] ;	// a biiiig one!

#else
extern struct fcfPixAnnotate pixStruct[183][512] ;

#endif

#endif


class fcfClass {
public:

	fcfClass(int det, u_short *table = NULL) ;	// constructor

	// the following are set by the constructor depending on the detector parameter
	// They may be changed explicitly for tests & debug thus they are "public"

	// Tonko added this for TPCMZCLD tracking
	u_int cvs_revision ;

	int detector ;		// i.e. TPC_ID, SVT_ID; set in constructor
	u_int maxTimebin ;	// 511 TPC, 127 SVT
	u_int maxCPP ;		// 8 SVT, 31 TPC

	// the following MUST BE SET once!
	u_int svtPedestal ;	// when using SVT pedestal offet...
	u_int timebinLo ;
	u_int timebinHi ;
	u_int chargeMin ;


	u_int deconTime  ;	// unused...
	u_int deconPad  ;	// unused...

	u_int doCuts  ;

	int param1  ;
	int minAdcT  ;
	int minAdcPad  ;

	// the following changes from call to call (row to row...)
	int row ;	// from 1..45
	int padStart ;	// Absolute starting pad i.e. 1..182
	int padStop ;	// Absolute last pad 
	int padMax ;	// maximum pad for this row

	u_int *adcOff ;	// offsets for the current row only!
	u_short *cppOff ;	
	u_int maxClusters ;	// maximum allowed # clusters in this row

        short *simIn;           // simulation track id
        u_int *simOut;

	// offset to the pad-to-pad t0 correction (multipled by gain and 64!), V4.10
	int *t0Corr ;

	// offset to the pad gain table (multipled by 64!), 
	u_int *gainCorr ;

	u_short *startFlags ;

	// used perhaps...
	int sb ;	// i.e. sector number, 1..24
	int rb ;	// receiver board,	1..12 or 1..6 for Offline
	int mz ;	// mezzanine, 1..3 (0 for Offline)


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


// Tonko: merged fcfAfterburner.hh into here

struct fcfHit {
	u_short pad ;
	u_short tm ;
	u_short f ;
	u_short c ;
	u_short p1,p2,t1,t2 ;
        short id_simtrk;
        short id_quality;
	short cl_id ;
} ;


class fcfAfterburner {
   enum {kMax_fcfHit = 200};
public:
        fcfAfterburner();
	~fcfAfterburner() { ; } ;

        int burn(u_int *ptr_res[3], u_int *ptr_simu_res[3] = NULL) ;
	int next(fcfHit *out) ;
	int compare(u_int *p1[3], u_int *p2[3]) ;

	void decode(u_int *data, fcfHit *h, u_int *sim = 0) ;	// utility: from FCF to local
	void print_hit(char *, fcfHit *hit) ;	// utility

	u_int do_swap ;		// 0=no, 1=yes => set by "burn"
	u_int do_merge ;	// merge broken rows
	u_int do_cuts ;		// apply additional cuts
	u_int row ;		// picked up from the data...
	
        void setVerbose(bool v) { verbose = v; };
private :
        bool verbose;

	u_int last_n, last_i, last_count, last_stage ;
	u_int **ptr ;	// storage for the burn arg...
	u_int **ptr_simu ;	// storage for the simulation ptr. 2nd burn argument

	u_int edge[4] ;
	struct fcfHit broken[4][kMax_fcfHit] ;
	u_int cou_broken[4] ;

	int output(fcfHit *l) ;	// the cut function; return TRUE if accepted
	int check_merge(fcfHit *l, fcfHit *r) ;	// merges r into l and returns TRUE 
} ;



#endif
