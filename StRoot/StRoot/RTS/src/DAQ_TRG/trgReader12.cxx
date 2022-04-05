#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>

// one needs to diffuse the crummy trigger stuff first
#define TRG_VERSION 0x12

#include <daqFormats.h>
#include <rtsSystems.h>


#include "daq_trg.h"

// this maps from the raw data to the following representation
// sector1,slat1,ch1 ... ch5, sector1,slat2,ch1.... ch5, sector2,slat1,ch1...

static unsigned char ctbMap[240] = {
7, 6, 5, 4, 3, 23, 22, 21, 20, 19, 
2, 1, 0, 15, 14, 18, 17, 16, 31, 30, 
13, 12, 11, 10, 9, 29, 28, 27, 26, 25, 
39, 38, 37, 36, 35, 55, 54, 53, 52, 51, 
34, 33, 32, 47, 46, 50, 49, 48, 63, 62, 
45, 44, 43, 42, 41, 61, 60, 59, 58, 57, 
71, 70, 69, 68, 67, 87, 86, 85, 84, 83, 
66, 65, 64, 79, 78, 82, 81, 80, 95, 94, 
77, 76, 75, 74, 73, 93, 92, 91, 90, 89, 
103, 102, 101, 100, 99, 119, 118, 117, 116, 115, 
98, 97, 96, 111, 110, 114, 113, 112, 127, 126, 
109, 108, 107, 106, 105, 125, 124, 123, 122, 121, 
135, 134, 133, 132, 131, 151, 150, 149, 148, 147, 
130, 129, 128, 143, 142, 146, 145, 144, 159, 158, 
141, 140, 139, 138, 137, 157, 156, 155, 154, 153, 
167, 166, 165, 164, 163, 183, 182, 181, 180, 179, 
162, 161, 160, 175, 174, 178, 177, 176, 191, 190, 
173, 172, 171, 170, 169, 189, 188, 187, 186, 185, 
199, 198, 197, 196, 195, 215, 214, 213, 212, 211, 
194, 193, 192, 207, 206, 210, 209, 208, 223, 222, 
205, 204, 203, 202, 201, 221, 220, 219, 218, 217, 
231, 230, 229, 228, 227, 247, 246, 245, 244, 243, 
226, 225, 224, 239, 238, 242, 241, 240, 255, 254, 
237, 236, 235, 234, 233, 253, 252, 251, 250, 249, 
} ;


static unsigned char mwcMap[96] = {
71, 70, 69, 68, 67, 66, 65, 64, 79, 78, 77, 76, 
95, 94, 93, 92, 87, 86, 85, 84, 83, 82, 81, 80, 
99, 98, 97, 96, 111, 110, 109, 108, 103, 102, 101, 100, 
119, 118, 117, 116, 115, 114, 113, 112, 127, 126, 125, 124, 
7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 
31, 30, 29, 28, 23, 22, 21, 20, 19, 18, 17, 16, 
35, 34, 33, 32, 47, 46, 45, 44, 39, 38, 37, 36, 
55, 54, 53, 52, 51, 50, 49, 48, 63, 62, 61, 60, 
} ;




// read the Trigger RAW data
int trgReader12(char *arg, trg_t *trg)
{
	int i ;

	struct TRGD *trgd = (struct TRGD *)arg ;

	LOG(DBG,"TrgDataFmtVer 0x%X, %c %c %c %c", trgd->desc.TrgDataFmtVer,
	    trgd->sum.TrgSumHeader[0],
	    trgd->sum.TrgSumHeader[1],
	    trgd->raw[0].CTBdataHeader[0], 
	    trgd->raw[0].CTBdataHeader[1]) ;
	LOG(DBG,"TrgSumBytes %d, %c %c %c %c", trgd->sum.TrgSumBytes,
	    trgd->sum.L0RegHeader[0],
	    trgd->sum.L0RegHeader[1],
	    trgd->raw[0].RawDetHeader[0], 
	    trgd->raw[0].RawDetHeader[1]) ;

	for(i=0;i<240;i++) {
		trg->CTB[i] = trgd->raw[0].CTB[ctbMap[i]] ;
	}

	for(i=0;i<96;i++) {
		trg->MWC[i] = trgd->raw[0].MWC[mwcMap[i]] ;
	}

	for(i=0;i<16;i++) {
		trg->ZDC[i] = trgd->sum.DSM.ZDC[i] ;
	}


	trg->trg_sum = NULL ;

	return 0 ;
}

