#ifndef CC_P
        #ifdef __cplusplus
                #define CC_P "C"
        #else
                #define CC_P
        #endif
#endif /*CC_P*/

#include <stdlib.h>

extern CC_P int asuAllocDebugLevel(int level);
extern CC_P void asuAllocStats(void);
extern CC_P void *asuAlloc(size_t size);
extern CC_P void *asuCalloc(size_t nobj, size_t size);
extern CC_P void asuFree(void *ptr);
extern CC_P void *asuRealloc(char *old, size_t size);

#define ASUALLOC(SSS) asuAlloc(SSS);if(2 <= asuAllocDebugLevel(-1))printf("%s.%d-ALLOC(%d)\n",__FILE__,__LINE__,SSS);fflush(0)
#define ASUCALLOC(NNN,SSS) asuCalloc(NNN,SSS);if(2 <= asuAllocDebugLevel(-1))printf("%s.%d-CALLOC(%d,%d)\n",__FILE__,__LINE__,NNN,SSS);fflush(0)
#define ASUFREE(PPP) asuFree(PPP);if(2 <= asuAllocDebugLevel(-1))printf("%s.%d-FREE(%p)\n",__FILE__,__LINE__,PPP);fflush(0)
#define ASUREALLOC(OOO,SSS) asuRealloc(OOO,SSS);if(2 <= asuAllocDebugLevel(-1))printf("%s.%d-REALLOC(%p,%d)\n",__FILE__,__LINE__,OOO,SSS);fflush(0)

