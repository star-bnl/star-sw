/*
 *
 *  Header Name:    bbcMap.h
 *  Header Number:  
 *  Package Name:   
 *  Created:        Z Milosevich ( zoran.milosevich@cmu.edu ) 27Dec02
 *  Description:    Header file containing cable map arrays
 *  History:        
 *                  zm     27Dec02   created, debugged
 *                  akio   28Oct04   chaged for adding large tile TAC
 *
 */

#ifndef bbcMap_h
#define bbcMap_h

#ifndef RAW_BBC_LEN
#define RAW_BBC_LEN 96
#endif

/* positions in array also correspond to positions in RAW data array */

/* position in array  gives BDB slot number */

int bdbSlot[RAW_BBC_LEN] =  {  6,  6,  6,  6,  6,  6,  6,  6, 
                               6,  6,  6,  6,  6,  6,  6,  6,
                               8,  8,  8,  8,  8,  8,  8,  8, 
                               8,  8,  8,  8,  8,  8,  8,  8, 
                              10, 10, 10, 10, 10, 10, 10, 10,
                              10, 10, 10, 10, 10, 10, 10, 10,
                              12, 12, 12, 12, 12, 12, 12, 12,
                              12, 12, 12, 12, 12, 12, 12, 12, 
                              14, 14, 14, 14, 14, 14, 14, 14, 
			      14, 14, 14, 14, 14, 14, 14, 14,
                              15, 15, 15, 15, 15, 15, 15, 15, 
                              15, 15, 15, 15, 15, 15, 15, 15 
                            };
    

/* position in array gives BDB channel number */

int bdbChannel[RAW_BBC_LEN] = {  7,  6,  5,  4,  3,  2,  1,  0,
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


/* position in array gives pmt number*/

int bbcPmt[RAW_BBC_LEN] =  {  11,  10,   9,   3,   2,   8,   7,   1,
                              11,  10,   9,   3,   2,   8,   7,   1,
                              11,  10,   9,   3,   2,   8,   7,   1,
                              11,  10,   9,   3,   2,   8,   7,   1,
			      16,  15,  14,   6,   5,  13,  12,   4, 
			      16,  15,  14,   6,   5,  13,  12,   4, 
			      16,  15,  14,   6,   5,  13,  12,   4, 
			      16,  15,  14,   6,   5,  13,  12,   4, 
                              24,  23,  22,  21,  20,  19,  18,  17,
                              24,  23,  22,  21,  20,  19,  18,  17, 
                              24,  23,  22,  21,  20,  19,  18,  17,
                              24,  23,  22,  21,  20,  19,  18,  17 
                           };

/* position in array gives either ADC - A or TAC - T  */

char bbcAdcTac[RAW_BBC_LEN] = { 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 
                                'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T',
                                'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 
                                'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 
                                'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 
                                'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 
                                'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 
                                'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 
                                'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 
                                'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 
                                'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 
                                'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T' 
                              };

/* position in array gives either East - E or West - W  */

char bbcEastWest[RAW_BBC_LEN] = { 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E',
                                  'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 
                                  'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W',  
                                  'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W',  
                                  'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 
                                  'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 
                                  'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W',  
                                  'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W',  
                                  'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 
                                  'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 
                                  'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W',
                                  'W', 'W', 'W', 'W', 'W', 'W', 'W', 'W'
                                };  

#endif
