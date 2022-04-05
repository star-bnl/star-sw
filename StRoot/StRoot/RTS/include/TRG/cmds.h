#ifndef _TRG_CMDS_H_
#define _TRG_CMDS_H_


/*
	The following 16 Trigger Commands correspond to the document
	"Trigger/Clock Distribution Tree" V1.1a, Feb 29, 1996

	Physics, Pulser and Config are the _only_ commands that may reach DAQ!
	All the others represent an error and should be flagged and ignored by DAQ!

	Tonko

*/


/* impossible */
#define TRG_CMD_NONE	0

/* various cleanup */
#define TRG_CMD_CLEAR	1
#define TRG_CMD_RESET	2

/* reserved */
#define TRG_CMD_SPARE	3

/* Physics */
#define TRG_CMD_TRIG_0	4
#define TRG_CMD_TRIG_1	5
#define TRG_CMD_TRIG_2	6
#define TRG_CMD_TRIG_3	7

/* Pulsers, laser etc. */
#define TRG_CMD_PULS_0	8			// charge injection
#define TRG_CMD_PULS_1	9			// laser(TPC);FEE test(SVT)
#define TRG_CMD_PULS_2	10			// ground plane(TPC)
#define TRG_CMD_PULS_3	11			// buffer readout

/* Special config geography */
#define TRG_CMD_CONFIG	12

/* aborts, accepts etc. */
#define TRG_CMD_ABORT		13
#define TRG_CMD_ACCEPT_L1	14
#define TRG_CMD_ACCEPT_L2	15



#endif
