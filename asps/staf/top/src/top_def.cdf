**:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        top_def.cdf
**:DESCRIPTION: Command Definition File for TOP.
**:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
**:HISTORY:     23oct96-v001a-cet- Make it work
**:HISTORY:     13jun96-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME TOP_DEF

************************************************************************
************************************************************************
** TOP
>MENU TOP
>GUIDANCE
Table OPerators commands.
.
VERSION v0.01a (23oct96).
.
Commands for the Table OPerators ASP.
.
    +--------------------------------------+
    | Selection Specifcation               |
    +--------------------------------------+
    A selection specification is used to select columns for a
    table output from a project (or join and project).
.
    The syntax of a selection specification has the format:
	'{' select_column_specifier ',' .... '}'
    where a select_column_specifier is of the form:
	column_specifier
    or:
	column_specifier <WHITE_SPACE> new_column_name
    where a column_specifier is of the form:
	column_name
    or:
	table_name '.' column_name
.
    If a selection specification is applied for a project (or a
    join and project), the output table will contain only those
    columns specified by the selection specification.
.
    If no selection specification is applied for a project (or a 
    join and project), the output table will contain all columns
    from the input table(s).
.
    Examples:
	'{x, y, z}'
	'{x q1, y q2, z q3}'
	'{intab.x outab.q1, intab.y outab.q2, intab.z outab.q3}'
	'{g2t_vertex.id vid, g2t_track.ge_pid, g2t_track.n_tpc_hit}'
.
    +--------------------------------------+
    | Where Clause                         |
    +--------------------------------------+
    A where clause is used to define the condition for joining
    rows of two tables in a join (and project).
.
    The syntax of a where clause has the format:
	'{' join_pair_specifier ',' ... '}'
    where a join_pair_specifier is of the form:
	column_specifier
    or:
	column_specifier <WHITE_SPACE> column_specifier
    where the two column_specifier's MUST come from different tables
    and where a column_specifier is of the form:
	column_name
    or:
	table_name '.' column_name
.
    If a where clause is applied for a join (and project), the
    output table will contain rows composed of data from both 
    tables where both tables have equal (INTEGER) values in the
    specified columns (eg. equal primary-key/foreign-key pairs).
.
    If no where clause is applied for a join (and project) the join
    will occur on all (INTEGER) columns with the same name in both
    tables.
.
    Examples:
	'{id}'
	'{pkey fkey}'
	'{tab1.pkey tab2.fkey}'
	'{g2t_vertex.id g2t_track.start_vertex_p}'
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TOP/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show count of registered TOP Agents.
.
>ACTION KAM_TOP_COUNT

** ---------------------------------------------------------------------
** TOP/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List TOP Agents.
.
>ACTION KAM_TOP_LIST

** ---------------------------------------------------------------------
** TOP/NEWPROJECT AGENT [ SELECT ]
>COMMAND NEWPROJECT
>PARAMETERS
AGENT 'Project Agent Name' C 
+
SELECT 'Original selection specification.' C D='-'
>GUIDANCE
Create and register a new topProject agent.
.
See HELP TOP for syntax of SELECT.
.
>ACTION KAM_TOP_NEWPROJECT

** ---------------------------------------------------------------------
** TOP/NEWJOIN AGENT [ SELECT WHERE ]
>COMMAND NEWJOIN
>PARAMETERS
AGENT 'Join Agent Name' C 
+
SELECT 'Original selection specification.' C D='-'
WHERE 'Original where clause.' C D='-'
>GUIDANCE
Create and register a new topJoin agent.
.
See HELP TOP for syntax of SELECT.
See HELP TOP for syntax of WHERE.
.
>ACTION KAM_TOP_NEWJOIN

** ---------------------------------------------------------------------
** TOP/NEWMASK AGENT [ CUTFUNC ]
>COMMAND NEWMASK
>PARAMETERS
AGENT 'Mask Agent Name' C 
+
CUTFUNC 'Original cut function.' C D='-'
>GUIDANCE
Create and register a new topMask agent.
.
>ACTION KAM_TOP_NEWFILTER

** ---------------------------------------------------------------------
** TOP/NEWSORT AGENT [ SORTFUNC ]
>COMMAND NEWSORT
>PARAMETERS
AGENT 'Sort Agent Name' C 
+
SORTFUNC 'Original sort function.' C D='-'
>GUIDANCE
Create and register a new topSort agent.
.
>ACTION KAM_TOP_NEWSORT

************************************************************************
************************************************************************
** TOP/PROJECT_AGENT
>MENU PROJECT_AGENT
>GUIDANCE
topProject commands.
.

** ---------------------------------------------------------------------
** TOP/PROJECT_AGENT/SELECTSPEC AGENT [ SELECT ]
>COMMAND SELECTSPEC
>PARAMETERS
AGENT 'Name of Project agent.' C
+
SELECT 'New selection specification.' C D='-'
>GUIDANCE
Show or change selection specification.
.
See HELP TOP for syntax of SELECT.
.
>ACTION KAM_TOPPROJECT_SELECTSPEC

** ---------------------------------------------------------------------
** TOP/PROJECT_AGENT/PROJECT AGENT TABLE1 TABLE2 [ SELECT ]
>COMMAND PROJECT
>PARAMETERS
AGENT 'Name of Project agent.' C
TABLE1 'Name of table from which to project.' C
TABLE2 'Name of table to which to project.' C
+
SELECT 'New selection specification.' C D='-'
>GUIDANCE
Project one table onto another.
.
See HELP TOP for syntax of SELECT.
.
>ACTION KAM_TOPPROJECT_PROJECT

** ---------------------------------------------------------------------
** TOP/PROJECT_AGENT/RESET AGENT
>COMMAND RESET
>PARAMETERS
AGENT 'Name of Project agent.' C
>GUIDANCE
Reset a topProject agent.
.
>ACTION KAM_TOPPROJECT_RESET

************************************************************************
************************************************************************
** TOP/JOIN_AGENT
>MENU \JOIN_AGENT
>GUIDANCE
topJoin agent commands.
.

** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/SELECTSPEC AGENT [ SELECT ]
>COMMAND SELECTSPEC
>PARAMETERS
AGENT 'Name of Join agent.' C
+
SELECT 'New selection specification.' C D='-'
>GUIDANCE
Show or change selection specification.
.
See HELP TOP for syntax of SELECT.
.
>ACTION KAM_TOPJOIN_SELECTSPEC

** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/WHERECLAUSE AGENT [ WHERE ]
>COMMAND WHERECLAUSE
>PARAMETERS
AGENT 'Name of Join agent.' C
+
WHERE 'New where clause.' C D='-'
>GUIDANCE
Show or change where clause.
.
See HELP TOP for syntax of WHERE.
.
>ACTION KAM_TOPJOIN_WHERECLAUSE

** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/JOIN AGENT TABLE1 TABLE2 TABLE3 [ SELECT WHERE ]
>COMMAND JOIN
>PARAMETERS
AGENT 'Name of Join agent.' C
TABLE1 'Name of first input table.' C
TABLE2 'Name of second input table.' C
TABLE3 'Name of output table.' C
+
SELECT 'Original selection specification.' C D='-'
WHERE 'Original where clause.' C D='-'
>GUIDANCE
Join two tables to form a third.
.
See HELP TOP for syntax of SELECT.
See HELP TOP for syntax of WHERE.
.
>ACTION KAM_TOPJOIN_JOIN

** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/RESET AGENT
>COMMAND RESET
>PARAMETERS
AGENT 'Name of Join agent.' C
>GUIDANCE
Reset a topJoin agent.
.
>ACTION KAM_TOPJOIN_RESET

