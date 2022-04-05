#ifndef _FTP_PADFINDER_H_
#define _FTP_PADFINDER_H_

/*
  This file specifies the Receiver to Mezzanine to daqrow relation.
  It is done in the following way.
  Each FTPC has 30 sectors which consichts out of 2 daqrows and 320 dapads.
  The counting of the daqrow within the physical row is linear i.e. 
  
 row 1 == daqrows  1-6 
 row 2 == daqrows 7-12
 ....
 
 Within a daqrow the daqpads are arraged linear from 1-160.
 The pads within a row a counted counter clockwise for both FTPC by 
 looking in direction of the positive z-axis (west) of the STAR coordinate system. 
 
 IT'S NOT SYMETRIC IN RESPECT TO THE COLLISION POINT !!!!!!!!

 This means that pad 1 of each row of the west FTPC starts on sectors 1 7 13 19 25 
 and for the east FTPC at sectors 6 12 18 24 30.

 Revision 1.0  10/05/2000  A.S.
 
*/

#ifndef FTP_RB
#define FTP_RB 20
#define FTP_MZ 3
#endif

#define FTP_NDAQROWS 2

static unsigned char ftpc_daqrow[FTP_RB][FTP_MZ][FTP_NDAQROWS] =
{
    //ftpc west 1
    {
	{1,7},
	{13,19},
	{25,31}
    },

    {
	{37,43},
	{49,55},
	{50,56}
    },
    
    {
	{2,8},
	{14,20},
	{26,32}
    },
    
    {
	{38,44},
	{39,45},
	{51,57}
    },
    
    {
	{3,9},
	{15,21},
	{27,33}
    },
    
    {
	{4,10},
	{16,22},
	{28,34}
    },
    
    {
	{40,46},
	{52,58},
	{53,59}
    },
    
    {
	{5,11},
	{17,23},
	{29,35}
    },
    
    {
	{41,47},
	{42,48},
	{54,60}
    },
    
    {
	{6,12},
	{18,24},
	{30,36}
    },
    
    //ftpc east 2
    
    {
	{6,12},
	{18,24},
	{30,36}
    },
  
    {
	{42,48},
	{54,60},
	{53,59}
    },
    
    {
	{5,11},
	{17,23},
	{29,35}
    },
 
    {
	{41,47},
	{40,46},
	{52,58}
    },
    
    {
	{4,10},
	{16,22},
	{28,34}
    },
    
    {
	{3,9},
	{15,21},
	{27,33}
    },
    
    {
	{39,45},
	{51,57},
	{50,56}
    },
    
    {
	{2,8},
	{14,20},
	{26,32}
    },

    {
	{38,44},
	{37,43},
	{49,55}
    },
 
    {
	{1,7},
	{13,19},
	{25,31}
    }
    
};


#endif
