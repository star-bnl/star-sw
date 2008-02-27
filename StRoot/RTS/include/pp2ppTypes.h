#ifndef _PP2PP_TYPES_H_
#define _PP2PP_TYPES_H_

// Tonko's flags
// event type bits
#define EVT_TYPE_EL	(1<<0)
#define EVT_TYPE_SD	(1<<1)
#define EVT_TYPE_PULS	(1<<2)

// wish list for dead-time readout...
#define INP_SI_E_DEAD	(1<<3)
#define INP_SI_W_DEAD	(1<<4)
#define INP_CAM_DEAD	(1<<5)
#define INP_MASTER_DEAD	(1<<6)


// spin states
#define INP_B_PLUS	(1<<8)
#define INP_B_MIN	(1<<9)
#define INP_B_NULL	(1<<10)

#define INP_Y_PLUS	(1<<12)
#define INP_Y_MIN	(1<<13)
#define INP_Y_NUL	(1<<14)

// these are not really inputs
#define EVT_TYPE_DEAD	(1<<30)	// deadtime event
#define EVT_TYPE_DUMMY	(1<<31) // dummy (test) event - do not try to unpack CAMAC!

// output register bits
#define PULS_COMP_READY (1<<0)
#define PULS_DEAD_READY  (1<<1)
#define ENA_EL		(1<<2)
#define ENA_SD		(1<<3)

#define ENA_DD		(1<<4)
#define ENA_TYPE_4	(1<<5)
#define ENA_PULS	(1<<6)
#define PULS_BUNCH_READY	(1<<7)

#define ENA_CAMAC	(1<<8)
#define ENA_SILICON_1	(1<<9)
#define ENA_SILICON_2   (1<<10)
#define INH_DEAD_TIME	(1<<11)

// end of Tonko's flags

#endif
