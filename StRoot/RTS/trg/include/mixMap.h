/*
 *
 *  Header Name:    mixMap.h
 *  Header Number:  
 *  Package Name:   
 *  Created:        Chris Perkins (cwperkins@lbl.gov) 17Jan07
 *  Description:    Header file containing cable map arrays
 *  History:        
 *                  cp     17Jan07   created, debugged
 *                  jmn    16Dec07   updated for MIX crate 
 *                         MTD (2 DSM), VPD(4),  P2P(2),  TOF(1) but only 1to1 LUT
 *
 */

#ifndef mixMap_h
#define mixMap_h

#ifndef RAW_MIX_LEN
#define RAW_MIX_LEN 128
#endif

#define RAW_MTD_LEN 32
#define RAW_VPD_LEN 64
#define RAW_PP2PP_LEN 32

/* positions in array also correspond to positions in RAW data array */

/* position in array  gives MIX slot number */

int mixSlot[RAW_MIX_LEN] =  {  6,  6,  6,  6,  6,  6,  6,  6, 
			       6,  6,  6,  6,  6,  6,  6,  6,
			       7,  7,  7,  7,  7,  7,  7,  7, 
			       7,  7,  7,  7,  7,  7,  7,  7, 
			       8,  8,  8,  8,  8,  8,  8,  8, 
			       8,  8,  8,  8,  8,  8,  8,  8,
			       9,  9,  9,  9,  9,  9,  9,  9, 
			       9,  9,  9,  9,  9,  9,  9,  9, 
			       10, 10, 10, 10, 10, 10, 10, 10,
			       10, 10, 10, 10, 10, 10, 10, 10,
			       11, 11, 11, 11, 11, 11, 11, 11,
			       11, 11, 11, 11, 11, 11, 11, 11,
			       12, 12, 12, 12, 12, 12, 12, 12,
			       12, 12, 12, 12, 12, 12, 12, 12,
                               13, 13, 13, 13, 13, 13, 13, 13,
                               13, 13, 13, 13, 13, 13, 13, 13
};

/* position in array gives MIX channel number */

int mixChannel[RAW_MIX_LEN] = {  7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8,
				 7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8,
				 7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8,
				 7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8,
				 7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8,
				 7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8,
				 7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8,
				 7,  6,  5,  4,  3,  2,  1,  0,
				 15, 14, 13, 12, 11, 10,  9,  8
};

#endif

