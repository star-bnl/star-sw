/* logging.i */
%module logging
%{
 /* Lazzy example, add header in the .i and not much work needed from now on */
#include "TxEventLog.h"
#include "TxEventLogFactory.h"
%}

%include "TxEventLog.h"
%include "TxEventLogFactory.h"
      
%pragma(java) jniclasscode=%{
  static {
    try {
        System.loadLibrary("logging");
    } catch (UnsatisfiedLinkError e) {
      System.err.println("Native code library failed to load. \n" + e);
      System.exit(1);
    }
  }
%}

