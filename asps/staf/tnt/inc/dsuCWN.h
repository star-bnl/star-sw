
#include "emlLib.h"

#ifdef __cplusplus
extern "C" {
#endif

char * dsuCWNdsSpec2chform(char * spec);
char * dsuCWNchform2dsSpec(char * chform);

STAFCV_T dsuLongwordifyRow(DS_DATASET_T *table, size_t irow
		, char **ppData);
STAFCV_T dsuUnLongwordifyRow(DS_DATASET_T *table, size_t irow
		, char **ppData);

size_t dsuColumnOffset(DS_DATASET_T *table, size_t icolumn);

#ifdef __cplusplus
}
#endif

