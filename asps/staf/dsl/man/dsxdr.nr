.TH "DSXDR" 3 "16 Dec 1993" "DRAFT" "DATA STRUCTURE LIB"
.SH SYNOPSIS
.nf
.ft B
#include "dstype.h"
#include "dsxdr.h"
.ft
.fi
.LP
.ft B
.nf
bool_t xdr_dataset(xdrs, ppDataset)
XDR *xdrs;
DS_DATASET_T **ppDataset;
.fi
.ft
.LP
.ft B
.nf
bool_t xdr_dataset_type(xdrs, ppDataset, dim)
XDR *xdrs;
DS_DATASET_T **ppDataset;
unsigned dim;
.fi
.ft
.LP
.ft B
.nf
bool_t xdr_dataset_data(xdrs, pDataset)
XDR *xdrs;
DS_DATASET_T *pDataset;
.fi
.ft
.LP
.SH DESCRIPTION
.B xdr_dataset(\|)
encodes or decodes the dataset specified by
.I ppDataset
to or from the XDR stream
.IR xdrs .
.LP
.B xdr_dataset_type(\|)
encodes or decodes type information for the dataset specified by
.IR ppDataset .
.LP
.B xdr_dataset_data(\|)
encodes or decodes data information for the dataset specified by
.IR pDataset .

.SH RETURN VALUES
<to be supplied>
.SH SEE ALSO
<to be supplied>
