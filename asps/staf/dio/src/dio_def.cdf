**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        dio_def.cdf
**:DESCRIPTION: Command Definition File for DIO
**:HISTORY:	30apr96-v100a-cet- Beta Release Version
**:HISTORY:     12dec95-v001a-cet- Update
**:HISTORY:     23jun95-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME DIO_DEF

************************************************************************
************************************************************************
** DIO
>MENU DIO
>Guidance
Data Input/Output commands.
.
VERSION v1.00a (30apr96).
.
Commands for the Dataset Input/Output ASP.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** DIO/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show count of known DIO objects.
.
>ACTION KAM_DIO_COUNT

** ---------------------------------------------------------------------
** DIO/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List DIO Streams.
.
>ACTION KAM_DIO_LIST

** ---------------------------------------------------------------------
** DIO/NEWFILESTREAM ALIAS FILE [ MODE ]
>COMMAND NEWFILESTREAM
>PARAMETERS
ALIAS 'dioFileStream alias.' C
FILE 'Name of data file.' C D='-b'
+
MODE 'Read/write mode of stream.' C R='R,W,U' D='R'
>GUIDANCE
Create a new dioFileStream.
.
FILE == "-b" will invoke the tcl/tk browser.
.
MODE == "R" = Read Only
        "W" = Write Only
	"U" = Update (Read/Write)
.
>ACTION KAM_DIO_NEWFILESTREAM

** ---------------------------------------------------------------------
** DIO/NEWSOCKSTREAM ALIAS HOST PORT [ MODE ]
>COMMAND NEWSOCKSTREAM
>PARAMETERS
ALIAS 'dioSockStream alias.' C
HOST 'Name of remote host.' C
PORT 'Socket port number.' I R='1024:9999'
+
MODE 'Read/write mode of stream.' C R='R,W,U' D='R'
>GUIDANCE
Create a new dioSockStream.
.
HOST is the TCP/IP address of the host to which to connect.
.
PORT is the service port number to which to connect.
.
MODE == "R" = Read Only
        "W" = Write Only
	"U" = Update (Read/Write)
.
>ACTION KAM_DIO_NEWSOCKSTREAM


************************************************************************
************************************************************************
** DIO/STREAM
>MENU STREAM
>Guidance
Data Input/Output stream commands.
.

** ---------------------------------------------------------------------
** DIO/STREAM/OPEN ALIAS MODE
>COMMAND OPEN
>PARAMETERS
ALIAS 'dioFileStream alias.' C
MODE 'Read/write mode of stream.' C R='R,W,U' D='R'
>GUIDANCE
Open a file for the stream.
.
MODE == "R" = Read Only
        "W" = Write Only
	"U" = Update (Read/Write)
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_DIOSTREAM_OPEN

** ---------------------------------------------------------------------
** DIO/STREAM/CLOSE ALIAS
>COMMAND CLOSE
>PARAMETERS
ALIAS 'dioFileStream alias.' C
>GUIDANCE
Close the stream's file.
.
>ACTION KAM_DIOSTREAM_CLOSE

** ---------------------------------------------------------------------
** DIO/STREAM/GETEVENT ALIAS [ DATASET ]
>COMMAND GETEVENT
>PARAMETERS
ALIAS 'dioFileStream alias.' C
+
DATASET 'Dataset name.' C D='.'
>GUIDANCE
Read from a DIO file stream ONTO a memory-resident dataset.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_DIOSTREAM_GETEVENT

** ---------------------------------------------------------------------
** DIO/STREAM/PUTEVENT ALIAS [ DATASET ]
>COMMAND PUTEVENT
>PARAMETERS
ALIAS 'dioFileStream alias.' C
+
DATASET 'Dataset name.' C D='.'
>GUIDANCE
Write a memory-resident dataset to a DIO file stream.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_DIOSTREAM_PUTEVENT

** ---------------------------------------------------------------------
** DIO/STREAM/MODE ALIAS
>COMMAND MODE
>PARAMETERS
ALIAS 'dioFileStream alias.' C
>GUIDANCE
Show the I/O mode of DIO stream.
.
>ACTION KAM_DIOSTREAM_MODE

** ---------------------------------------------------------------------
** DIO/STREAM/STATE ALIAS
>COMMAND STATE
>PARAMETERS
ALIAS 'dioFileStream alias.' C
>GUIDANCE
Show the state of DIO stream.
.
>ACTION KAM_DIOSTREAM_STATE

************************************************************************
************************************************************************
** DIO/FILESTREAM
>MENU \FILESTREAM
>Guidance
Data Input/Output fileStream commands.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** DIO/FILESTREAM/FILENAME ALIAS
>COMMAND FILENAME
>PARAMETERS
ALIAS 'dioFileStream alias.' C
>GUIDANCE
Show the fileName of DIO stream.
.
>ACTION KAM_DIOFILESTREAM_FILENAME

************************************************************************
************************************************************************
** DIO/SOCKSTREAM
>MENU \SOCKSTREAM
>Guidance
Data Input/Output sockStream commands.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** DIO/SOCKSTREAM/MAXHANDSHAKES [ COUNT ]
>COMMAND MAXHANDSHAKES
>PARAMETERS
ALIAS 'dioSockStream alias.' C
+
COUNT 'Maximum handshakes to try.' I R='-1:100000' D='-1'
>GUIDANCE
Set or get the maximnum number of handshakes to try.
.
 N.B.- The server and client handshake protocols must match.
.
>ACTION KAM_DIOSOCKSTREAM_MAXHANDSHAKES

** ---------------------------------------------------------------------
** DIO/SOCKSTREAM/HOST ALIAS
>COMMAND HOST
>PARAMETERS
ALIAS 'dioSockStream alias.' C
>GUIDANCE
Show the host name of DIO sockStream.
.
>ACTION KAM_DIOSOCKSTREAM_HOST

** ---------------------------------------------------------------------
** DIO/SOCKSTREAM/PORT ALIAS
>COMMAND PORT
>PARAMETERS
ALIAS 'dioSockStream alias.' C
>GUIDANCE
Show the port number of DIO sockStream.
.
>ACTION KAM_DIOSOCKSTREAM_PORT

