**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        soc_def.cdf
**:DESCRIPTION: Service & Object Catalog Command Definition File.
**:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
**:HISTORY:	30apr96-v100a-cet- Beta Release Version
**:HISTORY:     12feb96-v002a-cet- Remove BIND & RELEASE
**:HISTORY:     13nov95-v001a-cet- Update
**:HISTORY:     26jul95-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME SOC_DEF

************************************************************************
************************************************************************
** SOC
>MENU SOC
>GUIDANCE
Service & Object Catalog commands.
.
VERSION v1.00a (30apr96).
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** SOC/BIND ASP [ SOLIB ]
>COMMAND BIND
>PARAMETERS
ASP 'ASP name.' C
+
SOLIB 'Sharable library name.' C D='-'
>GUIDANCE
Bind to an Analysis Service Package.
.
 ************************
 * Not Yet Implemented  *
 ************************
>ACTION KAM_SOC_BIND

** ---------------------------------------------------------------------
** SOC/RELEASE ASP [ SOLIB ]
>COMMAND RELEASE
>PARAMETERS
ASP 'ASP name.' C
+
SOLIB 'Sharable library name.' C D='-'
>GUIDANCE
Release a bound Analysis Service Package.
.
 ************************
 * Not Yet Implemented  *
 ************************
>ACTION KAM_SOC_RELEASE

** ---------------------------------------------------------------------
** SOC/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show count of registered Services and Objects.
.
>ACTION KAM_SOC_COUNT

** ---------------------------------------------------------------------
** SOC/DELETEID IDREF
>COMMAND DELETEID
>PARAMETERS
IDREF 'Registered Object ID' I 
>GUIDANCE
Delete object #ID.
.
>ACTION KAM_SOC_DELETEID

** ---------------------------------------------------------------------
** SOC/DELETEOBJECT NAME [ TYPE ]
>COMMAND DELETEOBJECT
>PARAMETERS
NAME 'Registered Object Name' C
+
TYPE 'Registered Object Type' C D='-'
>GUIDANCE
Delete a registered object.
.
>ACTION KAM_SOC_DELETEOBJECT

** ---------------------------------------------------------------------
** SOC/IDOBJECT NAME [ TYPE ]
>COMMAND IDOBJECT
>PARAMETERS
NAME 'Registered Object Name' C
+
TYPE 'Registered Object Type' C D='-'
>GUIDANCE
Identify a registered object.
.
>ACTION KAM_SOC_IDOBJECT

** ---------------------------------------------------------------------
** SOC/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all registered Services and Objects.
.
>ACTION KAM_SOC_LIST

** ---------------------------------------------------------------------
** SOC/NEWOBJECT NAME
>COMMAND NEWOBJECT
>PARAMETERS
NAME 'Object Name' C
>GUIDANCE
Create and register a new socObject object.
.
>ACTION KAM_SOC_NEWOBJECT

************************************************************************
************************************************************************
** SOC/OBJECT
>MENU OBJECT
>GUIDANCE
SocObject commands.
.
** ---------------------------------------------------------------------
** SOC/OBJECT/NAME IDREF
>COMMAND NAME
>PARAMETERS
IDREF 'Identification Reference.' I D=-1
>GUIDANCE
Show name attribute of a registered object.
.
>ACTION KAM_SOCOBJECT_NAME

** ---------------------------------------------------------------------
** SOC/OBJECT/TYPE IDREF
>COMMAND TYPE
>PARAMETERS
IDREF 'Identification Reference.' I D=-1
>GUIDANCE
Show type attribute of a registered object.
.
>ACTION KAM_SOCOBJECT_TYPE

** ---------------------------------------------------------------------
** SOC/OBJECT/VERSION IDREF
>COMMAND VERSION
>PARAMETERS
IDREF 'Identification Reference.' I D=-1
>GUIDANCE
Show version attribute of a registered object.
.
>ACTION KAM_SOCOBJECT_VERSION

** ---------------------------------------------------------------------
** SOC/OBJECT/LOCK IDREF [ LOCK ] 
>COMMAND LOCK
>PARAMETERS
IDREF 'Identification Reference.' I D=-1
+
LOCK 'New lock value' C R='-,T,F' D='-'
>GUIDANCE
Show or set the lock attribute of a registered object.
.
>ACTION KAM_SOCOBJECT_LOCK

** ---------------------------------------------------------------------
** SOC/OBJECT/IMPLEMENTS IDREF INTERFACE
>COMMAND IMPLEMENTS
>PARAMETERS
IDREF 'Identification Reference.' I D=-1
INTERFACE 'Interface name' C
>GUIDANCE
Show whether a registered object implements an interface.
.
>ACTION KAM_SOCOBJECT_IMPLEMENTS

