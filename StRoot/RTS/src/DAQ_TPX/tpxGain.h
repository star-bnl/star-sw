#ifndef _TPX_GAIN_HH_
#define _TPX_GAIN_HH_

// #include <stdio.h>

// #include "tpxCore.h"



class tpxGain
{
public:
	tpxGain() ;
	~tpxGain() ;

	
	struct gains_t {
		float g ;
		float t0 ;
	} ;

	// used in running
	struct gains_t *gains[24] ;	// pointers to sector contribs, [46][183]

	void set_gains(int s, int r, int p, float g, float t0) {
		struct gains_t *gs = get_gains(s,r,p) ;

		gs->g = g ;
		gs->t0 = t0 ;
	}

	struct gains_t *get_gains(int s, int r, int p) {
		return (gains[s-1] + r*182 + (p-1)) ;
	}

	u_int bad_fee[25][7][37] ;	// [x][y][36] contains the count!

	// below used while calculating only
	struct aux_t {
		short low_pulse ;	// count of low gain
		short noise ;		// count of noisy
		short high_pulse ;	// count of pulse to high
		short wrong_peak ;	// hm?
		short cou ;		// count of good events
		short need ;		// count of expected events!
	} *aux	;	// [24][46][182]

	struct aux_t *get_aux(int s, int r, int p) {
		return (aux + (s-1)*46*182 + r*182 + (p-1)) ;
	} ;

	struct means_t {
		double g ;
		double t0 ;
		double g_rms ;
		double t0_rms ;

	} *means ;	// [24][46]

	struct means_t *get_means(int s, int r) {
		return (means + (s-1)*46 + r) ;
	} ;
	
	
	int from_file(char *fname, int sector = 0) ;

	// below used only during calculation
	int to_file(char *fname) ;

	void init(int sec=0) ;			// zap's structs; assume all TPX
	void accum(char *evbuff, int bytes) ;	// parses one RDO's worth of pulser data
	void ev_done() ;			// marks an event done...

	void calc() ;				// calculates gains and bad pads

	void compare(char *fname) ;			// compares in memory image with a given file
	int summarize(char *fname, FILE *log_file=0) ;			// prints a summary of bad channels to stdout

	void free_store() ;

private:
	void do_default(int sector) ;

	time_t	load_time ;


	int sector ;
	int events ;
	int tb_start, tb_stop ;	// timebin window
	int charge_peak ;	// peak of the charge
} ;

#endif
