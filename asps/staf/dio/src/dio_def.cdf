**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        dio_def.cdf
**:DESCRIPTION: Command Definition File for DIO package.
**:<--------------------------------------------------------------------
**
>NAME DIO_DEF
**
************************************************************************
** DIO
>MENU DIO
>GUIDANCE
Dataset_Input_and_Output commands.
.
 #(@)$Id: dio_def.cdf,v 1.6 1998/03/16 01:29:04 fisyak Exp $  Edited by Bill Love 25 Feb 98
.
DIO is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. dioFactory)
and zero or more worker object interfaces.
.
DIO worker objects include:
   dioStream - See DIO/STREAM
      - A generic data stream object. Abstract base class for all dio
        stream objects.
   dioFileStream - See DIO/FILESTREAM
      - A data stream object associated with a disk file.
   dioSockStream - See DIO/SOCKSTREAM
      - A data stream object associated with a TCP/IP socket.
   dioTapeStream - See DIO/TAPESTREAM
      - A data stream object associated with a magnetic tape device.
      - NOTICE -- dioTapeStream is not yet implemented.
.
The DIO worker objects handle input to and output from stream-like
sources of data containing XDF (XDR-based Dataset Format) data.
.
Each XDF data stream contains a sequence of unencapsulated DSL datasets
containing zero or more tables and/or other datasets. For historical
reasons, an unencapsulated dataset is refered to as an "event".
However, this is not an accurate term.
.
In this ASP, the term event simply refers to a dataset in an XDF stream
which is not contained within another dataset (ie. is unencapsulated by
another dataset).
.
These "events" can actually refer to real physics events, calibration
sets, header datasets, etc. The important distinction to be made is
that each READ or WRITE operation operates on a single dataset and its
sub-hierarchy (ie. all datasets and tables below the single dataset
within the overall hierarchy).
.
As an example: Consider the following hypothetical hierarchy of
datasets (lowercase names) and tables (UPPERCASE NAME).
.
	      .--EMC_GAIN
	      |--STAR_BFIELD
       .--calib/
       |      |--SVT_GAIN
       |      `--TPC_STC
       |         
   event/
       |  
       |                  .----JETS
       |             .--emc/
       |             |    `----TOWERS
       |             |         
       |             |    .----HITS
       |             |--svt/
       |             |    `----TRACKS
       |             |         
       |     .---dst_0/
       |     |       |         
       |     |       |    .----CLUSTERS
       |     |       `--tpc/
       |     |            |----HITS
       |     |            `----TRACKS
       |     |                 
       |     |              .--TRACKS
       |     |       .--global/
       |     |---dst_1/
       `--data/
	     |---dst_2/
	     |       |         
	     |       |    .----KAONS
	     |       |    |----MUONS
	     |       `--pid/
	     |            |----PIONS
	     |            `----PROTONS
	     |                 
	     |     .----EMC_HIT
	     |     |----SVT_ADC
	     `---raw/
		   |----SVT_MAP
		   |----TPC_ADC
		   `----TPC_MAP
.
If the "event/" dataset is written to or read from an XDF stream, a
total of 12 datasets and 21 tables will be output or input. If,
however, the "event/data/raw/" dataset is written to or read from an
XDF stream, only 1 dataset and 5 tables will be output or input.
.
For details of the XDF data format, please see Web pages for the DSL
package.
.
** ---------------------------------------------------------------------
** DIO/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show the current count of DIO worker objects.
.
DESCRIPTION: 
.
COUNT is a readonly long attribute which reflects the number of DIO
worker objects currently registered with the DIO object factory.
Constructing a new DIO worker object increments COUNT by 1,
destroying an existing DIO worker object decrements COUNT by 1.
.
DIO worker objects include:
   dioStream - See DIO/STREAM
      - A generic data stream object. Abstract base class for all dio
        stream objects.
   dioFileStream - See DIO/FILESTREAM
      - A data stream object associated with a disk file.
   dioSockStream - See DIO/SOCKSTREAM
      - A data stream object associated with a TCP/IP socket.
   dioTapeStream - See DIO/TAPESTREAM
      - A data stream object associated with a magnetic tape device.
      - NOTICE -- dioTapeStream is not yet implemented.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   The current value of COUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current count of DIO worker objects.
.
   StAF> DIO/COUNT
   DIO:    Object count = 18
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_dio_count_%C
**
** ---------------------------------------------------------------------
** DIO/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all currently registered DIO worker objects.
.
DESCRIPTION: 
.
Show a one-line description for each DIO worker object currently
registered with the DIO object factory in a table for quick,
simple perusal.
.
The one-line description for each object is the result of an invokation
of that object's listing method. The typical content of this listing is:
	0> OID
	   The object's OID attribute (see SOC) presented as "%5d".
	1> Lock State
           The object's LOCK attribute (see SOC) presented as the
	   divider character between the OID column and the NAME:OBJECT
	   column. An object whose LOCK attribute is TRUE (cannot be
	   deleted) uses the "-" character, whereas an object whose
	   LOCK attribute is FALSE (can be deleted) uses "|" character.
	2> NAME:OBJECT
	   The object's NAME attribute (see SOC) presented as "%-15s".
	   Object names longer than 15 characters are abreviated with a
	   "~" character at midpoint.
	   An object name is synonymous with an object instance.
	3> TYPE:CLASS
	   The object's TYPE attribute (see SOC) presented as "%-15s".
	   Object types longer than 15 characters are abreviated with a
	   "~" character at midpoint. 
	   An object type is synonymous with an object class.
	4> DESCRIPTION
	   A class-specific description of the object.
	   For Filestreams this is the name of the file with indicators 
           whether it is read/write and whether it is open/closed.
.
ARGUMENTS: 
.
   None.
.
RETURN: 
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioFactory::list()
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. List all currently registered DIO worker objects.
.
 staf++ > dio/list
 
 +------------------------------------------------------------------------
 |********************* DIO - Dataset Input/Output listing ***************
 +-------+-----------------+-----------------+----------------------------
 | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION   		  
 +-------+-----------------+-----------------+----------------------------
 |    41 | GeoTables       | dioFileStream   | (R,C) /afs/rhic/star/starli 
 |    69 | raw_data        | dioFileStream   | (R,O) /star/mds/data/SD97/c  
 +-------+-----------------+-----------------+----------------------------
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_dio_list_%C
**
** ---------------------------------------------------------------------
** DIO/NEWFILESTREAM NAME FILE [ MODE ]
>COMMAND NEWFILESTREAM
>PARAMETERS
NAME 'Name for new dioFilestream object' C
FILE    'File name of XDF data file' C
+
MODE    'Read/write mode' C D='R' R='R,W'
>GUIDANCE
Create a new dioFilestream object.
.
DESCRIPTION: 
.
Each dioFilestream created by the dioFactory shows up as an object
managed by the dioFactory (see DIO/COUNT and DIO/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new dioFilestream object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     dioFilestream object in subsequent commands.
.
   FILE - File name of XDF data file.
   - Unix file name.
.
   MODE - Read/write mode
   - MODE = R - Read Only
	    W - Write Only
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioFactory::newFilestream
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Open an output file
.
 STAF> DIO/NEWFILESTREAM DST /star/sol/users/love/data/dst1.xdf W
.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explanation of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   DIO/FILESTREAM
.
>ACTION kam_dio_newfilestream_%C
**
** ---------------------------------------------------------------------
** DIO/NEWSOCKSTREAM NAME HOST PORT [ MODE ]
>COMMAND NEWSOCKSTREAM
>PARAMETERS
NAME 'Name for new dioSockstream object' C
HOST    'Host name of remote host' C
PORT    'Socket port number' I R='1024:9999'
+
MODE    'Read/write mode' C D='R' R='R,W'
>GUIDANCE
Create a new dioSockstream object.
.
DESCRIPTION: 
.
Each dioSockstream created by the dioFactory shows up as an object
managed by the dioFactory (see DIO/COUNT and DIO/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new dioSockstream object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     dioSockstream object in subsequent commands.
   - More guidance needed here.
.
   HOST - Host name of remote host.
   - or TCP/IP address of the host to which to connect.
.
   PORT - Socket port number.
   - Service port number to which to connect.
   - PORT is ignored for MODE == W.
.
   MODE - Read/write mode
   - MODE = R - Read Only
	    W - Write Only
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioFactory::newSockstream
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explanation of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   DIO/SOCKSTREAM
.
>ACTION kam_dio_newsockstream_%C
**
************************************************************************
** DIO/FILESTREAM
>MENU FILESTREAM
>GUIDANCE
dioFilestream object commands.
.
Commands found under the DIO/FILESTREAM menu can be applied to objects
which implement the dioFilestream interface.
.
dioFileStream - A data stream object associated with a disk file.
.
**
** ---------------------------------------------------------------------
** DIO/FILESTREAM/FILENAME SOREF
>COMMAND FILENAME
>PARAMETERS
SOREF 'dioFilestream object SORef' C
>GUIDANCE
Get the FILENAME attribute of the dioFilestream SOREF.
.
DESCRIPTION: 
.
FILENAME is the XDF file on disk from which dioFilestream object SOREF
reads or to which dioFilestream object SOREF writes.
Readonly attributes cannot be changed from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioFilestream interface.
.
RETURN:
.
   None.
.
EXAMPLES: 
.
EG1.
.
 staf++ > dio/filestream/filename DST
 DIO:    File name = (/star/sol/users/love/data/dst1.xdf) 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioFilestream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_diofilestream_filename_%C
**
************************************************************************
** DIO/SOCKSTREAM
>MENU \SOCKSTREAM
>GUIDANCE
dioSockstream object commands.
.
Commands found under the DIO/SOCKSTREAM menu can be applied to objects
which implement the dioSockstream interface.
.
dioSockStream - A data stream object associated with a TCP/IP socket.
.
**
** ---------------------------------------------------------------------
** DIO/SOCKSTREAM/MAXHANDSHAKES SOREF [ NEW_VALUE ]
>COMMAND MAXHANDSHAKES
>PARAMETERS
SOREF 'dioSockstream object SORef' C
+
NEW_VALUE 'New value of MAXHANDSHAKES attribute' I D=-1 R='-1:10000'
>GUIDANCE
Get or set the MAXHANDSHAKES attribute of the dioSockstream SOREF.
.
DESCRIPTION: 
.
MAXHANDSHAKES is a read-writable attribute which determines how many
times to attempt to establish a connection between dioSockstream object
SOREF and the remote socket before failing.
.
To get the current value of MAXHANDSHAKES, leaving MAXHANDSHAKES 
unchanged, do not specify a new value in the optional argument 
NEW_VALUE.
.
To set a new value of MAXHANDSHAKES, specify the new value as the 
optional argument NEW_VALUE.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioSockstream interface.
.
   NEW_VALUE - New value for the MAXHANDSHAKES attribute.
   - DEFAULT: Show the current value of MAXHANDSHAKES, do not change it.
.
RETURN:
.
   The current value of MAXHANDSHAKES is pushed onto the STAF_RESULT
   stack (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the MAXHANDSHAKES attribute of
dioSockstream object "bob".
.
   StAF> DIO/SOCKSTREAM/MAXHANDSHAKES bob
   More guidance needed here.
.
EG2. Set the MAXHANDSHAKES attribute of dioSockstream object "bob" to
123.
.
   StAF> DIO/SOCKSTREAM/MAXHANDSHAKES bob 123
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioSockstream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
   N.B.- The server and client handshake protocols must match.
.
SEE ALSO: 
.
>ACTION kam_diosockstream_maxhandshakes_%C
**
** ---------------------------------------------------------------------
** DIO/SOCKSTREAM/HOST SOREF
>COMMAND HOST
>PARAMETERS
SOREF 'dioSockstream object SORef' C
>GUIDANCE
Get the HOST attribute of the dioSockstream SOREF.
.
DESCRIPTION: 
.
HOST is a readonly attribute which defines the remote host to which to
connect. 
Readonly attributes cannot be changed from the user interface.
.
The HOST attribute has no meaning for a source dioSockStream object
(ie. MODE = W).
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioSockstream interface.
.
RETURN:
.
   None.
.
EXAMPLES: 
.
EG1. Show the current value of the HOST attribute of
    dioSockstream object "bob".
.
   StAF> DIO/SOCKSTREAM/HOST bob
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioSockstream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
   The HOST attribute has no meaning if MODE = W.
.
SEE ALSO: 
.
>ACTION kam_diosockstream_host_%C
**
** ---------------------------------------------------------------------
** DIO/SOCKSTREAM/PORT SOREF
>COMMAND PORT
>PARAMETERS
SOREF 'dioSockstream object SORef' C
>GUIDANCE
Get the PORT attribute of the dioSockstream SOREF.
.
DESCRIPTION: 
.
PORT is a readonly attribute which defines the service port on the
remote node to which to connect.
Readonly attributes cannot be changed from the user interface.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioSockstream interface.
.
RETURN:
.
   The current value of PORT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the PORT attribute of
    dioSockstream "bob".
.
   StAF> DIO/SOCKSTREAM/PORT bob
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioSockstream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
   N.B. PORT must be the number of a free port. See man socket.
.
SEE ALSO: 
.
>ACTION kam_diosockstream_port_%C
**
************************************************************************
** DIO/STREAM
>MENU \STREAM
>GUIDANCE
dioStream object commands.
.
Commands found under the DIO/STREAM menu can be applied to objects
which implement the dioStream interface.
.
dioStream - A generic data stream object. Abstract base class for all
dio stream objects.
.
**
** ---------------------------------------------------------------------
** DIO/STREAM/MODE SOREF
>COMMAND MODE
>PARAMETERS
SOREF 'dioStream object SORef' C
>GUIDANCE
Get the I/O MODE of the dioStream SOREF.
.
DESCRIPTION: 
.
MODE is a readonly attribute which determines whether dioStream object
SOREF reads from, or writes to its associated data stream.
Readonly attributes cannot be changed from the user interface.
.
The valid values of MODE are:
.
   READONLY - Read Only
   WRITEONLY - Write Only
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioStream interface.
.
RETURN:
.
   The current value of MODE is pushed onto the STAF_RESULT stack
   (see SOC) and a message is printed to stdout.
.
EXAMPLES: 
.
EG1. Show the current value of the MODE attribute of
    dioStream "DST".
.
 staf++ > dio/stream/mode DST        
 DIO:    Stream mode = (WRITEONLY) 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioStream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_diostream_mode_%C
**
** ---------------------------------------------------------------------
** DIO/STREAM/STATE SOREF
>COMMAND STATE
>PARAMETERS
SOREF 'dioStream object SORef' C
>GUIDANCE
Get the current STATE of the dioStream SOREF.
.
DESCRIPTION: 
.
STATE is a readonly attribute which reflects the current state of the 
dioStream object SOREF.
Readonly attributes cannot be changed from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
Valid values of STATE are:
   OPENED 
   CLOSED
   READING
   WRITING
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioStream interface.
.
RETURN:
.
   The current value of STATE is pushed onto the STAF_RESULT stack
   (see SOC) and a message is printed to stdout.
.
EXAMPLES: 
.
EG1. Show the current value of the STATE attribute of
    dioStream "DST".
.
 staf++ > dio/stream/state DST
 DIO:    Stream state = (OPENED) 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioStream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_diostream_state_%C
**
** ---------------------------------------------------------------------
** DIO/STREAM/CLOSE SOREF
>COMMAND CLOSE
>PARAMETERS
SOREF 'dioStream object SORef' C
>GUIDANCE
Terminate communication with associated data stream.  The state attribute
of the stream becomes "CLOSED".
.
DESCRIPTION: 
.
CLOSE is a member function of objects which implement the dioStream
interface, including:
.
   dioFileStream - See DIO/FILESTREAM
      - A data stream object associated with a disk file.
   dioSockStream - See DIO/SOCKSTREAM
      - A data stream object associated with a TCP/IP socket.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the dioStream interface.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioStream::CLOSE
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Close the "DST" Filestream.
 staf++ > dio/stream/close DST
 staf++ > dio/stream/state DST
 DIO:    Stream state = (CLOSED) 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioStream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_diostream_close_%C
**
** ---------------------------------------------------------------------
** DIO/STREAM/GETEVENT SOREF [ DATASET ]
>COMMAND GETEVENT
>PARAMETERS
SOREF 'dioStream object SORef' C
+
DATASET 'In memory dataset name' C D='.'
>GUIDANCE
Read a dataset from an XDF data stream into memory.
.
DESCRIPTION: 
.
GETEVENT is a member function of objects which implement the dioStream
interface, including:
.
   dioFileStream - See DIO/FILESTREAM
      - A data stream object associated with a disk file.
   dioSockStream - See DIO/SOCKSTREAM
      - A data stream object associated with a TCP/IP socket.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the dioStream interface.
.
   DATASET - Optional in-memory dataset name.  If not given, the
   dataset name found on the file will be used.  If the dataset exists,
   it will be replaced with the dataset from the stream, if not it will
   be created.  The stream must be open and in READ mode.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioStream::GETEVENT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Read in an event from the stream "raw_data"
.
 staf++ > dio/stream/getevent raw_data
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioStream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
   BAD_MODE_OR_STATE - Attempt to read from a writeonly stream or a closed
stream.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_diostream_getevent_%C
**
** ---------------------------------------------------------------------
** DIO/STREAM/OPEN SOREF [ MODE ]
>COMMAND OPEN
>PARAMETERS
SOREF 'dioStream object SORef' C
+
MODE    'Read/write mode' C D='R' R='R,W'
>GUIDANCE
Initiate communication with a data stream.  I.e. set the state to 
OPENED.
.
DESCRIPTION: 
.
OPEN is a member function of objects which implement the dioStream
interface, including:
.
   dioFileStream - See DIO/FILESTREAM
      - A data stream object associated with a disk file.
   dioSockStream - See DIO/SOCKSTREAM
      - A data stream object associated with a TCP/IP socket.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the dioStream interface.
.
   MODE - Read/write mode
   - Options are READING and WRITING
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioStream::OPEN
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Change the state of the "DST" datastream to OPENED.  Set
the mode to Write-only.
.
 staf++ > dio/stream/open DST W
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
implements the dioStream interface.
(See SOC/BIND to dynamically bind the proper resources, or
rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_diostream_open_%C
**
** ---------------------------------------------------------------------
** DIO/STREAM/PUTEVENT SOREF [ DATASET ]
>COMMAND PUTEVENT
>PARAMETERS
SOREF 'dioStream object SORef' C
+
DATASET 'In memory dataset name' C D='.'
>GUIDANCE
Write a dataset from memory to an XDF data stream.
.
DESCRIPTION: 
.
PUTEVENT is a member function of objects which implement the dioStream
interface, including:
.
   dioFileStream - See DIO/FILESTREAM
      - A data stream object associated with a disk file.
   dioSockStream - See DIO/SOCKSTREAM
      - A data stream object associated with a TCP/IP socket. 
      The stream must be open and in WRITE mode.
   Dataset contents ( typically tables and other datasets ) are written
 from memory to an output stream.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the dioStream interface.
.
   DATASET - Path to an in-memory dataset name.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioStream::PUTEVENT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Write the dataset Tracks contained in the dataset ProducedData
to the filestream DST.
.
 staf++ > dio/stream/putevent DST ProducedData/Tracks
.
EG2. Write the entire ProducedData dataset to the filestream DST.
.
 staf++ > dio/stream/putevent DST ProducedData
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the dioStream interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
   BAD_MODE_OR_STATE - Attempt to write to a readonly stream or a closed
      stream.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_diostream_putevent_%C
**




