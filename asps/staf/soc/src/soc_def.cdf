**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        soc_def.cdf
**:DESCRIPTION: Command Definition File for SOC package.
**:<--------------------------------------------------------------------
**
>NAME SOC_DEF
**
************************************************************************
** SOC
>MENU SOC
>GUIDANCE
Service_and_Object_Catalog commands.
.
 #(@)$Id: soc_def.cdf,v 1.7 1998/03/16 01:51:29 fisyak Exp $
.
SOC is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus architecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. socFactory)
and zero or more worker object interfaces.
.
SOC worker objects include:
   socObject - See SOC/OBJECT
      - A generic object for test purposes only.
      - N.B. - All objects in the system implement the socObject
        interface.
.
The SOC package provides the central registry for each object controled
within StAF.
.
The repository is based on a typed and named system in which each object
has a unique NAME:TYPE specification. In addition, each object known
to the socCatalog has a unique Object IDentifier (OID) which specifies
that particular object.
.
OID - Object IDentifier
.
The Object IDentifier is currently a simple sequential number and is
unique only within a single StAF process.
.
SOREF - Stringafied Object REFerence
.
The Stringafied Object REFerence to an object in StAF is a character
string used to identify and/or locate an object registered with the
socCatalog. A SOREF has the form:
.
(N.B. - The current implementation of SOREF only properly handles
the  NAME format for all cases. The full functionality of this list
will be available in a future release of StAF.)
.
   NAME
      - Where NAME is the name of the object referenced.
      - The type of the referenced object must be implied by context.
   NAME:TYPE
      - Where TYPE is the type of the object referenced.
   NAME:TYPE:HOST
      - Where HOST is the computer host name where the referenced
        object is found.
   NAME::HOST
      - The type of the referenced object must be implied by context.
   :TYPE
      - Any object (the first object) of type TYPE found.
   :TYPE:HOST
      - Any object (the first object) of type TYPE found on host HOST.
   #OID
      - A character string representation of the Object ID.
.
STAF_STATUS and STAF_RESULTS vectors
.
In StAF every function of return type STAFCV_T (StAF Condition Value
Type) pushes a success or failure code onto the STAF_STATUS stack. In
KUIP, this stack is mirrored onto the STAF_STATUS(256) vector
(see /VECTOR). The most recent STAF_STATUS value is STAF_STATUS(1).
.
Valid values for the STAF_STATUS vector are:
.
   0 == STAFCV_BAD - A failure of some type occured.
   1 == STAFCV_OK - The function executed properly.
.
Also in StAF every component software class member function which
returns a numerical value (octet, short, unsigned short, long, unsigned
long, float, double) pushes its return value onto the STAF_RESULTS
stack. In KUIP, this stack is mirrored onto the STAF_RESULTS(256) vector
(see /VECTOR). The most recent STAF_RESULT value is STAF_RESULT(1).
.
At this time, member functions which return characters or character
strings, do not affect the STAF_RESULT vector.
.
** ---------------------------------------------------------------------
** SOC/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show the current count of all registered objects.
.
DESCRIPTION: 
.
COUNT is a readonly long attribute which reflects the number of all
objects currently registered with the SOC object catalog.
Constructing a new object increments COUNT by 1.  
Destroying an existing SOC worker object decrements COUNT by 1.
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
EG1. Show the current count of all registered objects.
.
   StAF> SOC/COUNT
   SOC:    Object count = 18
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_soc_count_%C
**
** ---------------------------------------------------------------------
** SOC/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all currently registered SOC worker objects.
.
DESCRIPTION: 
.
Show a one-line description for each SOC worker object currently
registered with the SOC object factory in a table.
.
The one-line description for each object is the result of an invocation
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
	   Object names longer than 15 characters are abbreviated with a
	   "~" character at midpoint.
	   An object name is synonymous with an object instance.
	3> TYPE:CLASS
	   The object's TYPE attribute (see SOC) presented as "%-15s".
	   Object types longer than 15 characters are abbreviated with a
	   "~" character at midpoint. 
	   An object type is synonymous with an object class.
	4> DESCRIPTION
	   A class-specific description of the object.  For example, for
           a table, the number of rows allocated/used and the size of a
           row.  For a dataset the number of entries.  For a filestream,
           the mode, state and associated filename, etc.  
          
.
Unlike the LIST command of other object factories, the SOC/LIST command
lists all registered objects whether directly instantiated by the
socCatalog object or not.
.
Also, unlike the LIST command of other object factories, the SOC/LIST
command lists the socCatalog (ie. the object factory) itself. Not, just
the worker objects.
.
ARGUMENTS:
.
   None.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the
   socCatalog::list()
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES:
.
EG1. List all registered objects.
.
   StAF> SOC/LIST
   +-------------------------------------------------------------------
   |******************* SOC - Service & Object Catalog listing ********
   +-------+-----------------+-----------------+-----------------------
   | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION
   +-------+-----------------+-----------------+-----------------------
   |     0 - soc             | socCatalog      | 17/2048 obj.s
   |     1 - spx             | spxFactory      | 1/2048 obj.s
   |     3 - dui             | duiFactory      | 1/2048 obj.s
   |     4 | /dui            | tdmDataset      | 0 ent.s
   |     5 - dio             | dioFactory      | 0/2048 obj.s
   |     6 - ami             | amiBroker       | 5/2048 obj.s
   |     7 | pamc            | amiInvoker      | 2 arg.s
   |     8 | pamcc           | amiInvoker      | 2 arg.s
   |     9 | pamf            | amiInvoker      | 2 arg.s
   |    10 | tbr             | tbrFactory      | 0/2048 obj.s
   |    11 | tbr_MotifViewer | tbrMotifViewer  |
   |    12 - tnt             | tntFactory      | 0/2048 obj.s
   |    13 - top             | topFactory      | 0/2048 obj.s
   |    14 | bob             | socObject       |
   |    15 | chess           | spxGrid         | Size = (16, 16)
   |    16 | tfs_filt        | amiInvoker      | 1 arg.s
   |    17 | tfs_g2t         | amiInvoker      | 8 arg.s
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
>ACTION kam_soc_list_%C
**
** ---------------------------------------------------------------------
** SOC/NEWOBJECT NAME
>COMMAND NEWOBJECT
>PARAMETERS
NAME 'Name for new socObject object' C
>GUIDANCE
Create a new socObject object.
.
DESCRIPTION: 
.
Each socObject created by the socCatalog shows up as an object
managed by the socCatalog (see SOC/COUNT and SOC/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new socObject object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     socObject object in subsequent commands.
   - More guidance needed here, most definitely, as to just what an 
     undifferentiated socObject is good for.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socCatalog::newObject
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new socObject with NAME "bob"
.
   StAF> SOC/NEWOBJECT bob
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
   SOC/OBJECT
.
>ACTION kam_soc_newobject_%C
**
** ---------------------------------------------------------------------
** SOC/BIND PKG [ SOLIB ]
>COMMAND BIND
>PARAMETERS
PKG     'Dynamically loadable package (ASP/PAM) name' C
+
SOLIB   'Sharable library name' C D='-'
>GUIDANCE
Dynamically bind all resources for a ASP or PAM package.
.
DESCRIPTION: 
.
BIND is a member function of the socCatalog interface.
.
On machine architectures supporting dynamic loading, the BIND function
dynamically binds a shareable image for a StAF software component
package.
.
Both ASPs and PAMs can be dynamically bound.
.
If PKG is an ASP, SOC/BIND will initialize the ASP (by calling
ASP_init()), and then start the ASP (by calling ASP_start()). At
completion of the BIND command, the ASP object factory and user
interface commands are available as though the ASP had been statically
linked with the STAF executable.
.
If PKG is a PAM, SOC/BIND will initialize the PAM (by calling
PAM_init()), and then start the PAM (by calling PAM_start()). At
completion of the BIND command, all PAM calculation objects and
table types are available as though the PAM had been statically linked
with the STAF executable.
.
ARGUMENTS: 
.
   PKG - Dynamically loadable package (ASP/PAM) name
   - A Three Letter Acronym (TLA) denoting an Analysis Service Package
   (ASP) or Physics Analysis Module (PAM) which is not currently extant
   in the StAF process (see SOC/LIST).
.
   SOLIB - Sharable library name
   - A reference to the shared object file on disk which contains
   the sharable resources for the PKG.
   - DEFAULT: The default behavior is to search for a shared library
   with the name 'libPKG.so' in the user's LD_LIBRARY_PATH.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socCatalog::BIND
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Bind to the PAM tfs in the default location.
.
   StAF> AMI/LIST
.
   +-------------------------------------------------------------------
   |****************** AMI - Analysis Module Interface listing ********
   +-------+-----------------+-----------------+-----------------------
   | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION
   +-------+-----------------+-----------------+-----------------------
   |     7 | pamc            | amiInvoker      | 2 arg.s
   |     8 | pamcc           | amiInvoker      | 2 arg.s
   |     9 | pamf            | amiInvoker      | 2 arg.s
   +-------+-----------------+-----------------+-----------------------
.
   STAF> SOC/BIND tfs
   tfs_filt module loaded
   tfs_g2t module loaded
   STAF> AMI/LIST
.
   +-------------------------------------------------------------------
   |****************** AMI - Analysis Module Interface listing ********
   +-------+-----------------+-----------------+-----------------------
   | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION
   +-------+-----------------+-----------------+-----------------------
   |     7 | pamc            | amiInvoker      | 2 arg.s
   |     8 | pamcc           | amiInvoker      | 2 arg.s
   |     9 | pamf            | amiInvoker      | 2 arg.s
   |    16 | tfs_filt        | amiInvoker      | 1 arg.s
   |    17 | tfs_g2t         | amiInvoker      | 8 arg.s
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
>ACTION kam_soc_bind_%C
**
** ---------------------------------------------------------------------
** SOC/DELETEOID OID
>COMMAND DELETEOID
>PARAMETERS
OID     'Object ID' I
>GUIDANCE
Delete a registered object by OID.
.
DESCRIPTION: 
.
DELETEOID is a member function of the socCatalog interface.
.
DELETEOBJECT will locate a registered object with the specified OID and
invoke that object's Destructor method.
.
ARGUMENTS: 
.
   OID - Object ID
  Each object registered with SOC has a unique integer ID which is
listed by the soc/list command or returned by the SOC/IDOBJECT command. 
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socCatalog::DELETEOID
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Delete object with OID == 123.
.
   StAF> SOC/DELETEOID 123
.
EXCEPTIONS: 
.
BUGS: 
.
   Objects must have a LOCK attribute == FALSE to be deleted.
.
SEE ALSO: 
   SOC/OBJECT/LOCK
.
>ACTION kam_soc_deleteoid_%C
**
** ---------------------------------------------------------------------
** SOC/DELETEID OID
>COMMAND DELETEID
>PARAMETERS
OID     'Object ID' I
>GUIDANCE
Obsolete command.
.
DESCRIPTION: 
This command is obsolete.  Please use SOC/DELETEOID instead.
.
SEE ALSO: 
   SOC/DELETEOID
.
>ACTION kam_soc_deleteid_%C
**
** ---------------------------------------------------------------------
** SOC/DELETEOBJECT NAME [ TYPE ]
>COMMAND DELETEOBJECT
>PARAMETERS
NAME    'Registered object name' C
+
TYPE    'Registered interface name' C D='-'
>GUIDANCE
Obsolete Command.
.
DESCRIPTION: 
This command is obsolete.  Please use SOC/OBJECT/DELETE instead.
..
DELETEOBJECT is a member function of the socCatalog interface.
.
DELETEOBJECT will locate a registered object with the specified NAME
and TYPE and invoke that object's Destructor method.

.
ARGUMENTS: 
.
   NAME - Registered object name.
   - More guidance needed here.
.
   TYPE - Registered interface name.
   - DEFAULT: The default behavior is to delete any (the first) object
     located with the specifiec NAME.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socCatalog::DELETEOBJECT
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Delete object of TYPE == grid and NAME == chess.
.
   StAF> SOC/DELETEOBJECT chess grid
.
EXCEPTIONS: 
.
BUGS: 
.
   Objects must have a LOCK attribute == FALSE to be deleted.
.
SEE ALSO: 
   SOC/OBJECT/LOCK
.
>ACTION kam_soc_deleteobject_%C
**
** ---------------------------------------------------------------------
** SOC/IDOBJECT NAME [ TYPE ]
>COMMAND IDOBJECT
>PARAMETERS
NAME    'Registered object name' C
+
TYPE    'Registered interface name' C D='-'
>GUIDANCE
Identify a registered object.
.
DESCRIPTION: 
.
IDOBJECT is a member function of the socCatalog interface.
.
IDOBJECT will return the OID of a registered object specified by SOREF
== NAME:TYPE.
.
ARGUMENTS: 
.
   NAME - Registered object name.
.
   TYPE - Registered interface name.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socCatalog::IDOBJECT
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Identify an object of TYPE dioFilestream and NAME DST.
.
 staf++ > soc/idobject DST dioFileStream
 SOC:    Object idRef =  101 
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
 SOC/OBJECT/OID
.
>ACTION kam_soc_idobject_%C
**
** ---------------------------------------------------------------------
** SOC/RELEASE PKG
>COMMAND RELEASE
>PARAMETERS
PKG     'Dynamically loaded package (ASP/PAM) name' C
>GUIDANCE
Release a dynamically bound ASP or PAM package.
.
DESCRIPTION: 
.
RELEASE is a member function of the socCatalog interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   PKG - Dynamically loaded package (ASP/PAM) name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socCatalog::RELEASE
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Release the tfs PAM.
.
   StAF> SOC/RELEASE tfs
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   SOC/BIND
.
>ACTION kam_soc_release_%C
**
************************************************************************
** SOC/OBJECT
>MENU OBJECT
>GUIDANCE
socObject object commands.
.
Commands found under the SOC/OBJECT menu can be applied to objects
which implement the socObject interface.
.
The socObject class is the base class for all component objects within
StAF. Hence, all objects can be treated as socObjects (I.E. SOC/OBJECT
commands can be applied to any component object within StAF.).
.
**
** ---------------------------------------------------------------------
** SOC/OBJECT/LOCK OID [ NEW_VALUE ]
>COMMAND LOCK
>PARAMETERS
OID 'socObject object ID' I
+
NEW_VALUE 'New value of LOCK attribute' C D='-' R='-,T,F'
>GUIDANCE
Get or set the LOCK attribute of the socObject SOREF.
.
DESCRIPTION: 
.
LOCK is a read-writable attribute which determines whether an object
can be deleted from the system. Under normal behavior, attempting to
delete a locked object (LOCK == TRUE) will fail.
.
To get the current value of LOCK, leaving LOCK unchanged, do not
specify a new value in the optional argument NEW_VALUE.
.
To set a new value of LOCK, specify the new value as the optional
argument NEW_VALUE.
.
ARGUMENTS: 
.
   OID - Object ID (see SOC).
   -  denoting an object implementing the socObject interface.
.
   NEW_VALUE - New value for the LOCK attribute.
      - LOCK == T(rue) denotes the object cannot be deleted.
      - LOCK == F(alse) denotes the object can be deleted.
   - DEFAULT: Show the current value of LOCK, do not change it.
.
RETURN:
.
   The current value of LOCK is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the LOCK attribute of socObject
    object with OID == 14.
.
   StAF> SOC/OBJECT/LOCK 14
   SOC:    Object lock = FALSE
.
EG2. Lock socObject object with OID == 14.
.
   StAF> SOC/OBJECT/LOCK 14 T
.
EG3. Unlock socObject object with OID == 14.
.
   StAF> SOC/OBJECT/LOCK 14 F
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by #OID can be found which
      implements the socObject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   SOC/OBJECT/LOCK ought to use SOREF instead of OID.  Doesn't actually
return OBJECT_NOT_FOUND but KAM_INVALID_IDREF.
.
SEE ALSO: 
.
>ACTION kam_socobject_lock_%C
**
** ---------------------------------------------------------------------
** SOC/OBJECT/NAME OID
>COMMAND NAME
>PARAMETERS
OID 'socObject object ID' I
>GUIDANCE
Get the NAME attribute of the socObject #OID.
.
DESCRIPTION: 
.
NAME is a readonly attribute which reflects the value of the NAME
attribute of the socObject #OID. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   OID - Object ID (see SOC).
   -  denoting an object implementing the socObject interface.
.
RETURN:
.
   The current value of NAME is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the NAME attribute of
    socObject 99.
.
 staf++ > soc/object/name 99 
 SOC:    Object name = /dui/BEGIN_RUN/GN6 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by #OID can be found which
      implements the socObject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   SOC/OBJECT/NAME ought to use SOREF instead of OID.
   - The value of OID can be determined by using SOC/OBJECT/OID.
   Doesn't actually return OBJECT_NOT_FOUND but KAM_INVALID_IDREF.
.
.
SEE ALSO: 
.
>ACTION kam_socobject_name_%C
**
** ---------------------------------------------------------------------
** SOC/OBJECT/OID NAME [ TYPE ]
>COMMAND OID
>PARAMETERS
NAME 'Registered Object Name.' C
+
TYPE 'Known Object Type.' C D='-'
>GUIDANCE
Get the OID attribute of the socObject NAME:TYPE.
.
DESCRIPTION: 
.
OID is a readonly attribute which reflects the value of the OID
attribute of the socObject NAME:TYPE. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   NAME - Registered Object Name.
.
   TYPE - Known Object Type.
.
RETURN:
.
   The current value of OID is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the OID attribute of object
   "chess:spxGrid".
.
   StAF> SOC/OBJECT/OID chess spxGrid
   SOC:    Object OID = 14
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by NAME:TYPE can be found which
      implements the socObject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   SOC/OBJECT/OID ought to use SOREF instead of NAME & TYPE.
.
SEE ALSO: 
.
>ACTION kam_socobject_oid_%C
**
** ---------------------------------------------------------------------
** SOC/OBJECT/TYPE OID
>COMMAND TYPE
>PARAMETERS
OID 'socObject object ID' I
>GUIDANCE
Get the TYPE attribute of the socObject #OID.
.
DESCRIPTION: 
.
TYPE is a readonly attribute which reflects the value of the TYPE
attribute of the socObject #OID. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   OID - Object ID (see SOC).
   -  denoting an object implementing the socObject interface.
.
RETURN:
.
   The current value of TYPE is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the TYPE attribute of the object with OID 99;
.
 staf++ > soc/object/type 99 
 SOC:    Object type = tdmTable 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by #OID can be found which
      implements the socObject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   SOC/OBJECT/TYPE ought to use SOREF instead of OID.
   - The value of OID can be determined by using SOC/OBJECT/OID.
.
SEE ALSO: 
.
>ACTION kam_socobject_type_%C
**
** ---------------------------------------------------------------------
** SOC/OBJECT/VERSION OID
>COMMAND VERSION
>PARAMETERS
OID 'socObject object ID' I
>GUIDANCE
Get the VERSION attribute of the socObject #OID.
.
DESCRIPTION: 
.
VERSION is a readonly attribute which reflects the value of the VERSION
attribute of the socObject #OID. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   OID - Object ID (see SOC).
   -  denoting an object implementing the socObject interface.
.
RETURN:
.
   The current value of VERSION is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the VERSION attribute of
    socObject OID == 14.
.
   StAF> SOC/OBJECT/VERSION 14
   SOC:    Object version = dev
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by #OID can be found which
      implements the socObject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   SOC/OBJECT/VERSION ought to use SOREF instead of OID.
   - The value of OID can be determined by using SOC/OBJECT/OID.
.
SEE ALSO: 
.
>ACTION kam_socobject_version_%C
**
** ---------------------------------------------------------------------
** SOC/OBJECT/DELETE NAME TYPE
>COMMAND DELETE
>PARAMETERS
NAME 'Registered Object Name.' C
TYPE 'Known Object Type.' C
>GUIDANCE
Directly invoke the destructor method of object NAME:TYPE.
.
DESCRIPTION: 
.
DELETE is a member function of objects which implement the socObject
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   NAME - Registered Object Name.
.
   TYPE - Known Object Type.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socObject::DELETE
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Delete registered object "chess:spxGrid".
.
   StAF> SOC/OBJECT/DELETE chess spxGrid
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by NAME:TYPE can be found which
      implements the socObject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
  Fails to delete stream objects?
 staf++ > soc/object/delete bob dioFileStream
 
 *** Break *** Segmentation violation
 Interrupt trace routine not available 
 
 *** Break *** Simulated break


   SOC/OBJECT/DELETE ought to use SOREF instead of NAME & TYPE.
   Objects must have a LOCK attribute == FALSE to be deleted.
.
SEE ALSO: 
   SOC/DELETEOBJECT
   SOC/DELETEOID
.
>ACTION kam_socobject_delete_%C
**
** ---------------------------------------------------------------------
** SOC/OBJECT/IMPLEMENTS SOREF INTERFACE
>COMMAND IMPLEMENTS
>PARAMETERS
OID 'socObject object ID' I
INTERFACE       'Known interface name' C
>GUIDANCE
Inquire whether object #OID implements interface IFACENAME.
.
DESCRIPTION: 
.
IMPLEMENTS is a member function of all registered component objects in
StAF.
.
If object #OID's interface is a subclass of another interface, then
object #OID implements that base class interface as well as it's own.
This means that object #OID can be treated as an object of the base
class when appropriate.
.
E.G. A dioFileStream object is a subclass of a dioStream object. Hence,
any file stream object can be treated as a generic stream object. I.E.
An object created with the DIO/NEWFILESTREAM command can be manipulated
with the DIO/STREAM/* commands as well as the DIO/FILESTREAM/* commands.
.
ARGUMENTS: 
.
   OID - Object ID (see SOC).
   - denoting an object implementing the socObject interface.
.
   INTERFACE - Known interface name.
   - The name of an interface (eg. name of TYPE) known to StAF.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   socObject::IMPLEMENTS
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Inquire whether object #14 implements the socObject interface.
.
   StAF> SOC/OBJECT/IMPLEMENTS 14 socObject
   SOC:    Object (bob) DOES implement (socObject)
.
EG2. Inquire whether object #14 implements the socCatalog interface.
.
   StAF> SOC/OBJECT/IMPLEMENTS 14 socCatalog
   SOC:    Object (bob) DOES NOT implement (socCatalog)
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by #OID can be found which
      implements the socObject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   SOC/OBJECT/IMPLEMENTS ought to use SOREF instead of OID.
   - The value of OID can be determined by using SOC/OBJECT/OID
.
SEE ALSO: 
.
>ACTION kam_socobject_implements_%C
**
