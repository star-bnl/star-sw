#ifndef CC_P
        #ifdef __cplusplus
                #define CC_P "C"
        #else
                #define CC_P
        #endif
#endif /*CC_P*/

#include <stdlib.h>

extern CC_P void asuAllocStats(void);
extern CC_P void *asuAlloc(size_t size);
extern CC_P void asuFree(void *ptr);
extern CC_P void *asuRealloc(char *old, size_t size);
