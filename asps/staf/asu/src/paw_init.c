#include "fortranc.h"

#define paw_init_count_ F77_NAME(paw_init_count,PAW_INIT_COUNT)

static paw_init_count=0;
long type_of_call paw_init_count_() { return paw_init_count++; }

