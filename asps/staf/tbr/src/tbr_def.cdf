**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tbr_def.cdf
**:DESCRIPTION: Command Definition File for TBR
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:HISTORY:     18jun96-v002a-cet- Remove COUNT & LIST
**:HISTORY:     11mar96-v000a-cet,hjw- Creation
**:<--------------------------------------------------------------------

>NAME TBR_DEF

************************************************************************
************************************************************************
** TBR
>MENU TBR
>Guidance
Table BRowser commands.
.
VERSION v1.00a (30apr96).
.
Commands for the Table BRowser ASP.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TBR/COUNT
**>COMMAND COUNT
**>PARAMETERS
**>GUIDANCE
**Show count of known TBR objects.
**.
**>ACTION KAM_TBR_COUNT

** ---------------------------------------------------------------------
** TBR/LIST
**>COMMAND LIST
**>PARAMETERS
**>GUIDANCE
**List known TBR objects.
**.
**>ACTION KAM_TBR_LIST

************************************************************************
** TBR/MOTIF
>MENU \MOTIF
>GUIDANCE
Motif Table BRowser commands.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TBR/MOTIF/VIEW
>COMMAND VIEWDATASET
>PARAMETERS
>GUIDANCE
Launch the Motif Table BRowser and view dataset.
.
The Motif Table BRowser always launches at the root directory.
.
>ACTION KAM_TBR_MOTIF_VIEWDATASET

