**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        spx_def.cdf
**:DESCRIPTION: Service Package eXample Command Definition File.
**:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
**:HISTORY:	30apr96-v100a-cet- Beta Release Version
**:HISTORY:     21nov95-v000a-cet- ReCreation
**:<--------------------------------------------------------------------

>NAME SPX_DEF

************************************************************************
************************************************************************
** SPX
>MENU SPX
>GUIDANCE
Service Package eXample commands.
.
VERSION v1.00a (30apr96).
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** SPX/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show count of known SPX objects.
.
>ACTION kam_spx_count_%C

** ---------------------------------------------------------------------
** SPX/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all known SPX objects.
.
>ACTION kam_spx_list_%C

** ---------------------------------------------------------------------
** SPX/NEWDUMMY NAME
>COMMAND NEWDUMMY
>PARAMETERS
NAME 'spxDummy name.' C
>GUIDANCE
Create a new spxDummy object.
.
>ACTION kam_spx_newdummy_%C

** ---------------------------------------------------------------------
** SPX/NEWGRID NAME HEIGHT WIDTH
>COMMAND NEWGRID
>PARAMETERS
NAME 'spxGrid name.' C
HEIGHT 'Grid height.' I
WIDTH 'Grid width.' I
>GUIDANCE
Create a new spxGrid object.
.
>ACTION kam_spx_newgrid_%C

************************************************************************
************************************************************************
** SPX/DUMMY
>MENU DUMMY
>GUIDANCE
spxDummy object commands.
.

** ---------------------------------------------------------------------
** SPX/DUMMY/NCALLS DNAME
>COMMAND NCALLS
>PARAMETERS
DNAME 'spxDummy object name.' C
>GUIDANCE
Shows number of calls to spxDummy functions.
.
>ACTION kam_spxdummy_ncalls_%C

** ---------------------------------------------------------------------
** SPX/DUMMY/NULL DNAME
>COMMAND NULL
>PARAMETERS
DNAME 'spxDummy object name.' C
>GUIDANCE
Does nothing.
.
>ACTION kam_spxdummy_null_%C

************************************************************************
************************************************************************
** SPX/GRID
>MENU \GRID
>GUIDANCE
spxGrid object commands.
.

** ---------------------------------------------------------------------
** SPX/GRID/HEIGHT DNAME
>COMMAND HEIGHT
>PARAMETERS
DNAME 'spxGrid object name.' C
>GUIDANCE
Show height attribute of spxGrid object.
.
>ACTION kam_spxgrid_height_%C

** ---------------------------------------------------------------------
** SPX/GRID/WIDTH DNAME
>COMMAND WIDTH
>PARAMETERS
DNAME 'spxGrid object name.' C
>GUIDANCE
Show width attribute of spxGrid object.
.
>ACTION kam_spxgrid_width_%C

** ---------------------------------------------------------------------
** SPX/GRID/GET DNAME M N
>COMMAND GET
>PARAMETERS
DNAME 'spxGrid object name.' C
M 'First index of cell.' I
N 'Second index of cell.' I
>GUIDANCE
Get cell value of spxGrid object.
.
>ACTION kam_spxgrid_get_%C

** ---------------------------------------------------------------------
** SPX/GRID/SET DNAME M N VALUE
>COMMAND SET
>PARAMETERS
DNAME 'spxGrid object name.' C
M 'First index of cell.' I
N 'Second index of cell.' I
VALUE 'New value of cell.' I
>GUIDANCE
Set cell value of spxGrid object.
.
>ACTION kam_spxgrid_set_%C

