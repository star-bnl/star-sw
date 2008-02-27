#ifndef _L3_READER_H_
#define _L3_READER_H_

#include <sys/types.h>
#include <evpSupport.h>
#include <daqFormats.h>


#define L3_MAX_NR_TRACKS 10000

inline float fswap(float swapped)
{
        unsigned int* uintptr = (unsigned int*) &swapped;
        unsigned int uintvar = l2h32(*uintptr);
        float* floatvar = (float*)&uintvar;
        return *floatvar;
}


#ifdef UNIX_LITTLE_ENDIAN
#define l2hfloat(x) (x)
#define b2hfloat(x) (fswap(x))
#else
#define l2hfloat(x) (fswap(x))
#define b2hfloat(x) (x)
#endif


struct l3 {
    int mode;
    int channels;
    int max_channels;
    
    u_int tracks_num;
    u_int cluster_num;
    float xVertex;
    float yVertex;
    float zVertex;

    global_track track[L3_MAX_NR_TRACKS];
};

extern struct l3 l3;

extern int l3Reader(char *mem);
extern int l3Reader(L3_P *l3p);
#endif
