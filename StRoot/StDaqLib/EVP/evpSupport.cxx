#include <sys/types.h>
#include <string.h>
#include <stdio.h>

#include "rtsLog.h"

#include "evpSupport.h"

int checkBank(char *m, char *what) ;


int checkBank(char *m, char *what)
{
        char bank[10];


        // LOG(DBG,"Checking bank for [%s]...",(uint)what,0,0,0,0) ;

        printf("bbb above memcpy\n");
        memcpy(bank,m,8) ;
        printf("bbb below memcpy\n");
        bank[9] = 0 ;

        if(memcmp(m,what,strlen(what)) != 0) {
                // LOG(ERR,"Wrong bank type: expecting [%s], is [%s]",(uint)what,(uint)bank,0,0,0) ;
                return -1 ;
        }
        else {
                // LOG(DBG,"Bank Compare for [%s] OK...",(uint)what,0,0,0,0) ;
        }

        return 0 ;
}
