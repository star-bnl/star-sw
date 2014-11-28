#ifndef _EVP_SUPPORT_H
#define _EVP_SUPPORT_H

#include <sys/types.h>
#include "rts.h"

#define EVP_NO_DET	0
#define EVP_NO_DATA	(-1)
#define EVP_DATA_ERR	(-2) 
namespace  OLDEVP {
extern int checkBank(char *m, char *what) ;

// UGLY UGLY hack
extern u_int evp_daqbits ;
}

#endif
