#ifndef _DAQ100_DECISION_H_
#define _DAQ100_DECISION_H_

#include <daqModes.h>

/* 
	The function returns a bitfield:

	Bit2	Do HLT
	Bit1	Do Cluster Finding
	Bit0	Do raw ADC
*/

extern inline int daq100Decision(int t, u_int daq_cmd, u_int run_type, u_int cl_run, u_int zero_wr)
{
	int fmt, proc, hlt ;

	proc = 0 ;
	fmt = 0 ;
	hlt = 0 ;

	if(run_type == RUN_TYPE_DAQCHECK) return 1 ;	// JUST fmt
	if((t <= 0) || (t>=4096)) return 1 ;		// JUST fmt for token 0 or other odd tokens...

	// cluster finder only in non-pedestal runs
	if(run_type != RUN_TYPE_PED) {	// only for non-pedestal runs!
		if(cl_run == 0) proc = 0 ;
		else if((t % cl_run)==0) proc = 1 ;

		// HLT
//		if(daq_cmd & DAQCMD_HLT_RUN) hlt = 1 ;	// Tonko: removed on Dec 13, 2009
	}

	// raw formatting
#define ITPC_TOKEN_HACK
#ifdef ITPC_TOKEN_HACK
	if(daq_cmd & 0x1) {
#else
	if(daq_cmd & DAQCMD_FMT_ONLY) {	// always i.e. zerobias
#endif
		fmt = 1 ;
	}
	else {
		if(zero_wr == 0) fmt = 0 ;
		else if((t % zero_wr)==0) fmt = 1 ;
	}


	// special run types override
	switch(run_type) {
	case RUN_TYPE_PULSER :
	case RUN_TYPE_PED_B :	// misc tests...
//	case RUN_TYPE_LASER :	// Tonko: removed laser on Apr 14, 2009
		fmt = 1 ;
		break ;
	}

	// assume HLT for physics runs whenever we also want clusterfinding.
	// should not really be used but just for completness
	// Tonko: Dec 13, 2009
	if(run_type == RUN_TYPE_PHYS) {
		if(proc) hlt = 1 ;
	}

	return (hlt << 2) | (proc << 1) | fmt ;

}

#endif
