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
>ACTION KAM_SPX_COUNT

** ---------------------------------------------------------------------
** SPX/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all known SPX objects.
.
>ACTION KAM_SPX_LIST

** ---------------------------------------------------------------------
** SPX/NEWDUMMY NAME
>COMMAND NEWDUMMY
>PARAMETERS
NAME 'spxDummy name.' C
>GUIDANCE
Create a new spxDummy object.
.
>ACTION KAM_SPX_NEWDUMMY

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
>ACTION KAM_SPX_NEWGRID

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
>ACTION KAM_SPXDUMMY_NCALLS

** ---------------------------------------------------------------------
** SPX/DUMMY/NULL DNAME
>COMMAND NULL
>PARAMETERS
DNAME 'spxDummy object name.' C
>GUIDANCE
Does nothing.
.
>ACTION KAM_SPXDUMMY_NULL

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
>ACTION KAM_SPXGRID_HEIGHT

** ---------------------------------------------------------------------
** SPX/GRID/WIDTH DNAME
>COMMAND WIDTH
>PARAMETERS
DNAME 'spxGrid object name.' C
>GUIDANCE
Show width attribute of spxGrid object.
.
>ACTION KAM_SPXGRID_WIDTH

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
>ACTION KAM_SPXGRID_GET

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
>ACTION KAM_SPXGRID_SET

