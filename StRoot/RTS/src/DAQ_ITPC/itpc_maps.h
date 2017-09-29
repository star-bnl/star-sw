#ifndef _ITPC_MAPS_H_
#define _ITPC_MAPS_H_

/* Map goes from SAMPA:ch to connector pin on the iFEE Schematics*/

#ifdef ITPC_FY17

/* FY17 "bad" FEE map due to Tim screwing up*/
unsigned char itpc_sampa_to_pin[2][32] = {
{ 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34},
{45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,70,72,73,74,75,76}
} ;

#else	/* Fixed for the final FEE */

unsigned char itpc_sampa_to_pin[2][32] = {
{ 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45 },
{ 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3 }
} ;

#endif

/* Map goes from the pin of the old FEE (called "jx") to the pin of the new padplane connector.
	even=0,odd=1
*/	
unsigned char itpc_adapter_jx_to_pin[2][45] = {
//Even
{
0,0,0,0,0,0,0,
 4, 6, 8,10,12,14,16,18,20,22,24,26,28,30,32,34,
46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,
0,0,0,0,0,0
},


//Odd
{
0,0,0,0,0,0,0,
 3, 5, 7, 9,11,13,15,17,19,21,23,25,27,29,31,33,
45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,
0,0,0,0,0,0
}

} ;


#endif
