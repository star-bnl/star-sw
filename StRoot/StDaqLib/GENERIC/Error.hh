/***************************************************************************
 * $Id: Error.hh,v 1.5 1999/12/06 22:53:19 levine Exp $
 * Author: Jeff Landgraf
 *
 *  change log
 * 20-Jul-99 MJL Added new error code ERR_BAD_HEADER
 *
 ***************************************************************************
 * Description: error codes
 *
 ***************************************************************************
 * $Log: Error.hh,v $
 * Revision 1.5  1999/12/06 22:53:19  levine
 * Cleaned up information generated on failure to initialize EventReader
 *
 * Revision 1.4  1999/12/03 21:38:33  levine
 * Error message added "Info:  End of File encountered"
 *
 * Revision 1.3  1999/07/21 21:33:08  levine
 * changes to include error logging to file.
 *
 * There are now 2 constructors for EventReader:
 *
 *  EventReader();
 *  EventReader(const char *logfilename);
 *
 * Constructed with no argument, there is no error logging. Supplying a file name
 * sends all diagnostic output to the named file (N.B. opens in append mode)
 *
 * See example in client.cxx for constructing a log file name based on the
 * datafile name.
 *
 * It is strongly advised to use the log file capability. You can grep it for
 * instances of "ERROR:" to trap anything noteworthy (i.e., corrupted data files).
 *
 * Revision 1.2  1999/07/02 04:37:41  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/
#ifndef ERROR_H
#define ERROR_H
#include <stdio.h>

#define ERROR(x) {errnum = x;sprintf(errstr0,"%s::%d",__FILE__,__LINE__);return;} 

#define pERROR(x) {errnum = x;sprintf(errstr0,"%s::%d",__FILE__,__LINE__);}

#define spERROR(x) {detector->errnum=x;sprintf(detector->errstr0,"%s::%d",__FILE__,__LINE__);}

#define ERR_FILE 1
#define ERR_CRC 2
#define ERR_SWAP 3
#define ERR_BANK 4
#define ERR_MEM 5
#define ERR_NOT_DATA_BANK 6
#define ERR_BAD_ARG 7
#define ERR_ENDR_ENCOUNTERED 8
#define ERR_BAD_HEADER 9
#define INFO_MISSING_BANK 10
#define INFO_END_FILE 11


#define MX_MESSAGE 11

#endif

