#include <string.h>
#define DS_PRIVATE
#include "dsxdr.h"

/**********************************************************************
*
* xdr_dataset_type - encode or decode abstract types for dataset
*
* RETURNS: TRUE if success else FALSE
*/
bool_t xdr_dataset_type(XDR *xdrs, DS_DATASET_T **ppDataset, size_t dim)
{
        char buf[DS_MAX_SPEC_LEN+1], *str;
        size_t *tList = NULL;
        DS_DATASET_T *pDataset = *ppDataset;

        if (!dsTypeListCreate(&tList, DS_XDR_HASH_LEN + 1)) {
                return FALSE;
        }
        if (xdrs->x_op == XDR_ENCODE) {
                if (!xdr_types(xdrs, pDataset, tList)) {
                        goto fail;
                }
                str = buf;
                if (!dsDatasetSpecifier(str, sizeof(buf), pDataset)) {
                        goto fail;
                }
                if (!xdr_string(xdrs, &str, sizeof(buf))) {
                        DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
                        goto fail;
                }
                goto success;
        }
        if (xdrs->x_op != XDR_DECODE) {
                DS_LOG_ERROR(DS_E_INVALID_XDR_OP);
                goto fail;
        }
        for(;;) {
                str = buf;
                if (!xdr_string(xdrs, &str, sizeof(buf) - 1)) {
printf("(((%s)))\n",str);
                        DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
                        goto fail;
                }
                if (strncmp(buf, "type", 4) == 0) {
                        if (!dsTypeListEnter(tList, buf + 4, &str)) {
                                goto fail;
                        }
                }
                else if (strncmp(buf, "data", 4) == 0) {
                        if (!dsCreateDataset(&pDataset,
                                dim, tList, buf+4, &str)) {
                                goto fail;
                        }
                        *ppDataset = pDataset;
                        goto success;
                }
                else {
                        DS_LOG_ERROR(DS_E_INVALID_DATASET);
                        goto fail;
                }
        }
        fail:
                dsTypeListFree(tList);
                return FALSE;
        success:
                dsTypeListFree(tList);
                return TRUE;
}
