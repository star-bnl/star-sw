#ifndef common_inc
#define common_inc

// common include file
// declares some functions and constants
#include <math.h>
// #include <afx.h>

// Standard constants
#undef FALSE
#undef TRUE
#undef NULL

#define FALSE   0
#define TRUE    1
#define NULL    0

typedef int BOOL;
typedef unsigned short WORD;

typedef unsigned int size_t;

#define Pi 3.14159265359
#define To_deg    57.29577951

// Start and stop padrow
#define HIGH_PADROW 45
#define LOW_PADROW 1

// square
inline double square (double value)
{
	return value * value;
}

inline double seta(double r, double z)
{
	return (3.0 * (z) / (fabs(z)+2.0*(r)));
}


#endif
