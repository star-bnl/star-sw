/* logging.i */
%module logging
%{
 /* Lazzy example, add header in the .i and not much work needed from now on */
#include "TxEventLog.h"
#include "TxEventLogFactory.h"
%}

%include "TxEventLog.h"
%include "TxEventLogFactory.h"
