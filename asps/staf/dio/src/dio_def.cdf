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
 #(@)$Id: dio_def.cdf,v 1.3 1997/12/22 17:41:12 tull Exp $
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
   dioObject - See DIO/OBJECT
	       - More guidance needed here.
.
More guidance needed here.
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
destructing an existing DIO worker object decrements COUNT by 1.
.
DIO worker objects include:
   More guidance needed here.
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
>ACTION KAM_DIO_COUNT
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
	   More guidance needed here.
.
DIO worker objects include:
   More guidance needed here.
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
   StAF> DIO/LIST
   +-------------------------------------------------------------------
   |*********************** DIO - Not a valid DIO listing *************
   +-------+-----------------+-----------------+-----------------------
   | OID   | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION
   +-------+-----------------+-----------------+-----------------------
   +-------+-----------------+-----------------+-----------------------
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DIO_LIST
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
   - More guidance needed here.
.
   FILE - File name of XDF data file.
   - More guidance needed here.
.
   MODE - Read/write mode
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioFactory::newFilestream
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new dioFilestream with NAME "bob"
.
   StAF> DIO/NEWFILESTREAM bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explaination of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   DIO/FILESTREAM
.
>ACTION KAM_DIO_NEWFILESTREAM
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
   - More guidance needed here.
.
   PORT - Socket port number.
   - More guidance needed here.
.
   MODE - Read/write mode
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioFactory::newSockstream
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new dioSockstream with NAME "bob"
.
   StAF> DIO/NEWSOCKSTREAM bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explaination of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   DIO/SOCKSTREAM
.
>ACTION KAM_DIO_NEWSOCKSTREAM
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
More guidance needed here.
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
FILENAME is a readonly attribute which reflects the value of the FILENAME
attribute of the dioFilestream SOREF. Readonly attributes cannot be changed
from the user interface.
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
   The current value of FILENAME is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the FILENAME attribute of
    dioFilestream "bob".
.
   StAF> DIO/FILESTREAM/FILENAME bob
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
>ACTION KAM_DIOFILESTREAM_FILENAME
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
More guidance needed here.
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
MAXHANDSHAKES is a read-writable attribute which determines the value of
the MAXHANDSHAKES attribute.
.
To get the current value of MAXHANDSHAKES, leaving MAXHANDSHAKES unchanged, do not
specify a new value in the optional argument NEW_VALUE.
.
To set a new value of MAXHANDSHAKES, specify the new value as the optional
argument NEW_VALUE.
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
   The current value of MAXHANDSHAKES is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the MAXHANDSHAKES attribute of dioSockstream 
    object "bob".
.
   StAF> DIO/SOCKSTREAM/MAXHANDSHAKES bob
.
EG2. Set the MAXHANDSHAKES attribute of dioSockstream object "bob" to 123.
.
   StAF> DIO/SOCKSTREAM/MAXHANDSHAKES bob 123
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
SEE ALSO: 
.
>ACTION KAM_DIOSOCKSTREAM_MAXHANDSHAKES
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
HOST is a readonly attribute which reflects the value of the HOST
attribute of the dioSockstream SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioSockstream interface.
.
RETURN:
.
   The current value of HOST is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the HOST attribute of
    dioSockstream "bob".
.
   StAF> DIO/SOCKSTREAM/HOST bob
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
SEE ALSO: 
.
>ACTION KAM_DIOSOCKSTREAM_HOST
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
PORT is a readonly attribute which reflects the value of the PORT
attribute of the dioSockstream SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
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
SEE ALSO: 
.
>ACTION KAM_DIOSOCKSTREAM_PORT
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
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** DIO/STREAM/MODE SOREF
>COMMAND MODE
>PARAMETERS
SOREF 'dioStream object SORef' C
>GUIDANCE
Get the MODE attribute of the dioStream SOREF.
.
DESCRIPTION: 
.
MODE is a readonly attribute which reflects the value of the MODE
attribute of the dioStream SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioStream interface.
.
RETURN:
.
   The current value of MODE is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the MODE attribute of
    dioStream "bob".
.
   StAF> DIO/STREAM/MODE bob
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
>ACTION KAM_DIOSTREAM_MODE
**
** ---------------------------------------------------------------------
** DIO/STREAM/STATE SOREF
>COMMAND STATE
>PARAMETERS
SOREF 'dioStream object SORef' C
>GUIDANCE
Get the STATE attribute of the dioStream SOREF.
.
DESCRIPTION: 
.
STATE is a readonly attribute which reflects the value of the STATE
attribute of the dioStream SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the dioStream interface.
.
RETURN:
.
   The current value of STATE is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the STATE attribute of
    dioStream "bob".
.
   StAF> DIO/STREAM/STATE bob
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
>ACTION KAM_DIOSTREAM_STATE
**
** ---------------------------------------------------------------------
** DIO/STREAM/CLOSE SOREF
>COMMAND CLOSE
>PARAMETERS
SOREF 'dioStream object SORef' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
CLOSE is a member function of objects which implement the dioStream
interface.
.
More guidance needed here.
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
EG1. Invoke the CLOSE method function of dioStream "bob"
     More guidance needed here.
.
   StAF> DIO/STREAM/CLOSE bob 
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
>ACTION KAM_DIOSTREAM_CLOSE
**
** ---------------------------------------------------------------------
** DIO/STREAM/GETEVENT SOREF [ DATASET ]
>COMMAND GETEVENT
>PARAMETERS
SOREF 'dioStream object SORef' C
+
DATASET 'In memory dataset name' C D='.'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
GETEVENT is a member function of objects which implement the dioStream
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the dioStream interface.
.
   DATASET - In memory dataset name
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioStream::GETEVENT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the GETEVENT method function of dioStream "bob"
     More guidance needed here.
.
   StAF> DIO/STREAM/GETEVENT bob 
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
>ACTION KAM_DIOSTREAM_GETEVENT
**
** ---------------------------------------------------------------------
** DIO/STREAM/OPEN SOREF [ MODE ]
>COMMAND OPEN
>PARAMETERS
SOREF 'dioStream object SORef' C
+
MODE    'Read/write mode' C D='R' R='R,W'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
OPEN is a member function of objects which implement the dioStream
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the dioStream interface.
.
   MODE - Read/write mode
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioStream::OPEN
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the OPEN method function of dioStream "bob"
     More guidance needed here.
.
   StAF> DIO/STREAM/OPEN bob 
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
>ACTION KAM_DIOSTREAM_OPEN
**
** ---------------------------------------------------------------------
** DIO/STREAM/PUTEVENT SOREF [ DATASET ]
>COMMAND PUTEVENT
>PARAMETERS
SOREF 'dioStream object SORef' C
+
DATASET 'In memory dataset name' C D='.'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
PUTEVENT is a member function of objects which implement the dioStream
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the dioStream interface.
.
   DATASET - In memory dataset name
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   dioStream::PUTEVENT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the PUTEVENT method function of dioStream "bob"
     More guidance needed here.
.
   StAF> DIO/STREAM/PUTEVENT bob 
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
>ACTION KAM_DIOSTREAM_PUTEVENT
**
