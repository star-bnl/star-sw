#ifndef _DAQ_MODES_H_
#define _DAQ_MODES_H_
/*
	Tonko, 11/4/1999
*/


#define PED_MODE_OFF	0
#define PED_MODE_ON	1
#define PED_MODE_AUTO	4	// ALTRO: automatic pedestal

#define GAIN_MODE_LINEAR	0	// pedestal; everything above 255 stays at 255
#define GAIN_MODE_SEESAW	1	// most significant 2 bits (8 & 9) are stripped
#define GAIN_MODE_LOG		2	// pulser; canonical logaritmic response
#define GAIN_MODE_CORRECTED	3	// tail cancellation; canonical log. resp. X correct gain
#define GAIN_MODE_SVT		4	// SVT
#define GAIN_MODE_SVT_CORRECTED	5	// SVT X correct gain

#define RUN_TYPE_TEST		0	// do nothing with the event
#define RUN_TYPE_PED		1	// calculate pedestals
#define RUN_TYPE_GAIN		2	// calculate gains
#define RUN_TYPE_PHYS		3	// run cluster finder
#define RUN_TYPE_LASER		4
#define RUN_TYPE_PULSER		5
#define RUN_TYPE_CONFIG		6	// configuration/geometry run
#define RUN_TYPE_COSMICS	7
#define RUN_TYPE_DEBUG		10	// used only During Board Debugging!
#define RUN_TYPE_DAQCHECK	11	// used to check the DAQ hardware
#define RUN_TYPE_FCF		12	// used to time FCF
#define RUN_TYPE_TPX_TEST	21	// test: sets TPX defaults for zero-suppression
#define RUN_TYPE_TPX_DBG	22	// test: same as 21 except sets very small timebins
#define RUN_TYPE_TRG		30	// TPX: trigger only data
#define RUN_TYPE_TPX_EMUL	40	// TPX: local debugging
#define RUN_TYPE_EVB_TEST	50	// EVB testing...

// DO NOT EVER change this!
#define RUN_TYPE_FLASH		100	// download flash to TPX RDOs
#define RUN_TYPE_FEE_PROM	101	// download flash to TPX RDOs
#define RUN_TYPE_BOB_PROM	102	// download flash to TPX RDOs

// test runs of misc types, Tonko
#define RUN_TYPE_PED_A		200
#define RUN_TYPE_PED_B		201
#define RUN_TYPE_PULSER_A	202

/* special DAQ commands sent out via Trigger */
#define DAQCMD_DEFAULT		0	/* Run cluster finder _and_ wait for FORMAT_DATA */
#define DAQCMD_FMT_WAIT		1	/* Will keep the buffer and wait for a FORMAT_DATA */

/* Removed: was never really used #define DAQCMD_CL_RUN		2 */	/* Run cluster finder */
#define DAQCMD_HLT_RUN		2	/* Run HLT -- new from FY09 */

#define DAQCMD_FMT_ONLY		4	/* _Just_ format data - no clusters! */

#define DAQCMD_SPEC_PED		8	/* used in pedestals runs for special hardware handling i.e. PMD*/
#define DAQCMD_IGNORE		15	/* completelly ignores the event - acts as an ABORT - not used but could for cleanup events*/



#endif	/* DAQ_MODES */

