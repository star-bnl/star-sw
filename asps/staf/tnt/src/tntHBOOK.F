c:Copyright 1996, Lawrence Berkeley National Laboratory
c:>--------------------------------------------------------------------
C:FILE:         tntHBOOK.F
C:DESCRIPTION:  HBOOK-like routines for TNT
c:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
C:BUGS:         -- STILL IN DEVELOPMENT --
C:HISTORY:      14jun96-v000a-cet- creation
c:>--------------------------------------------------------------------

c:<--------------------------------------------------------------------
c:<--------------------------------------------------------------------
        INTEGER*4 FUNCTION initPAWC()
        implicit none

#include "tntPAWC.inc"

        call HLIMIT(nwpaw)
        initPAWC = 0

        end

c:<---------------------------------------------------------------------
C:ROUTINE:      CHARACTER*80 TNT_TITLE
C:DESCRIPTION:  return NTUPLE title
C:ARGUMENTS:    HID - INTEGER*4 - HBOOK ID
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      CHARACTER*80 FUNCTION TNT_NT_TITLE(HID)
      IMPLICIT NONE
      INTEGER*4 HID
      character*80 chtitl
      integer*4 nvar
      character*8 chtag(100)
      real*4 rlow
      real*4 rhigh

      nvar = 100
      call HGIVEN(hid,chtitl,nvar,chtag,rlow,rhigh)
      TNT_NT_TITLE = chtitl

      RETURN
      END

c:<---------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 TNT_NT_COLUMN_COUNT
C:DESCRIPTION:  return number of columns in NTUPLE 
C:ARGUMENTS:    HID - INTEGER*4 - HBOOK ID
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      INTEGER*4 FUNCTION TNT_NT_COLUMN_COUNT(HID)
      IMPLICIT NONE
      INTEGER*4 HID
      character*80 chtitl
      integer*4 nvar
      character*8 chtag(100)
      real*4 rlow
      real*4 rhigh

      nvar = 100
      call HGIVEN(hid,chtitl,nvar,chtag,rlow,rhigh)
      TNT_NT_COLUMN_COUNT = nvar

      RETURN
      END

c:<---------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 TNT_NT_ENTRY_COUNT
C:DESCRIPTION:  return number of entrys in NTUPLE
C:ARGUMENTS:    HID - INTEGER*4 - HBOOK ID
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      INTEGER*4 FUNCTION TNT_NT_ENTRY_COUNT(HID)
      IMPLICIT NONE
      INTEGER*4 HID
      integer*4 nvar
      integer*4 nentrys

      nvar = 100
      call HNOENT(hid,nentrys)
      TNT_NT_ENTRY_COUNT = nentrys

      RETURN
      END

c:<---------------------------------------------------------------------
C:ROUTINE:      CHARACTER*8 TNT_NT_TAG
C:DESCRIPTION:  return Nth column tag of NTUPLE 
C:ARGUMENTS:    HID - INTEGER*4 - HBOOK ID
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      CHARACTER*8 FUNCTION TNT_NT_TAG(HID,NCOL)
      IMPLICIT NONE
      INTEGER*4 HID
      INTEGER*4 NCOL
      character*80 chtitl
      integer*4 nvar
      character*8 chtag(100)
      real*4 rlow
      real*4 rhigh

      nvar = 100
      call HGIVEN(hid,chtitl,nvar,chtag,rlow,rhigh)
      TNT_NT_TAG = chtag(NCOL)

      RETURN
      END

c:<---------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 TNT_START_PAW
C:DESCRIPTION:  Initialize and start PAW
C:ARGUMENTS:    *** NONE ***
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      INTEGER*4 FUNCTION TNT_START_PAW()
      IMPLICIT NONE

#include "tntPAWC.inc"
      LOGICAL*1 firstime/.TRUE./
      integer*4 iwtyp/1/
*
      if(firstime)then
!17jun96-cet!	 CALL PAW(NWPAW,IWTYP)
         CALL IGINIT(0)
	 CALL IGWKTY(iwtyp)
	 CALL IGSSE(6,iwtyp)
	 WRITE(*,*)'HISTOGRAM'
	 CALL HISDEF()
	 WRITE(*,*)'FUNCTION'
	 CALL FUNDEF()
	 WRITE(*,*)'NTUPLE'
	 CALL NTUDEF()
	 WRITE(*,*)'GRAPHICS'
	 CALL GRADEF()
	 WRITE(*,*)'PICTURE'
	 CALL PICDEF()
	 WRITE(*,*)'ZEBRA'
	 CALL ZEBDEF()
	 WRITE(*,*)'FORTRAN'
	 CALL FORDEF()

	 firstime = .FALSE.
      else
	 WRITE(*,*)'PAW has already been started.'
      endif
*
      TNT_START_PAW = 0
      RETURN
      END

c:<---------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 TNT_START_SHARE
C:DESCRIPTION:  Initialize and start shared memory
C:ARGUMENTS:    *** NONE ***
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      INTEGER*4 FUNCTION TNT_START_SHARE()
      IMPLICIT NONE

#include "tntPAWC.inc"
      LOGICAL*1 firstime/.TRUE./
*
      if(firstime)then
	 call hlimap(nwpaw,'STAF') ! HACK 'STAF' should be variable
	 firstime = .FALSE.
      else
	 WRITE(*,*)'Only one shared memory space is supported.'
      endif
*
      TNT_START_SHARE = 0
      RETURN
      END

