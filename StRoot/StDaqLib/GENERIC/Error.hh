#ifndef ERROR_H
#define ERROR_H
#include <stdio.h>

#define ERROR(x) {errnum = x;sprintf(errstr0,"%s::%d",__FILE__,__LINE__);return;} 

#define pERROR(x) {errnum = x;sprintf(errstr0,"%s::%d",__FILE__,__LINE__);}
#define spERROR(x) {detector->errnum=x;sprintf(detector->errstr0,"%s::%d",__FILE__,__LINE__);}

#define ERR_FILE 1;
#define ERR_CRC 2;
#define ERR_SWAP 3;
#define ERR_BANK 4;
#define ERR_MEM 5;
#define ERR_NOT_DATA_BANK 6;
#define ERR_BAD_ARG 7;
#define ERR_ENDR_ENCOUNTERED 8;
#endif
