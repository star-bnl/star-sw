
#include "emlLib.h"

#ifdef __cplusplus
extern "C" {
#endif

char *dsuTableChform(DS_DATASET_T *);
char *dsuColumnChform(DS_DATASET_T *, size_t);
char *dsuCWNdsSpec2chform(char *);
char *dsuCWNchform2dsSpec(char *);

STAFCV_T dsuLongwordifyRow(DS_DATASET_T *, size_t, char **);
STAFCV_T dsuUnLongwordifyRow(DS_DATASET_T *, size_t, char **);

size_t dsuColumnOffset(DS_DATASET_T *, size_t);

#ifdef __cplusplus
}
#endif

