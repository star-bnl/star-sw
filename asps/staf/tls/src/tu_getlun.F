      INTEGER*4 FUNCTION TU_GETLUN( unit )
c
c  This function performs the sam action at the VMS routine LIB$GET_LUN.
c  It returns an available logical unit number in 'unit' and returns
c  a success status value if it is successful.  Otherwise it returns
c an error status.
c
      IMPLICIT NONE
      INTEGER*4 unit
      INTEGER*4 max_units, min_unit
      PARAMETER (max_units = 99)
      PARAMETER (min_unit = 1)
      LOGICAL isopen
      INTEGER*4 i
c
      i = max_units+1
      unit = 0
      isopen = .true.
      DO WHILE(isopen .and. i.gt.min_unit)
         i = i - 1
         INQUIRE( i, opened=isopen )
      END DO
      IF (isopen) THEN
         TU_GETLUN = 0
      ELSE
         TU_GETLUN = 1
         unit = i
      END IF
      RETURN
      END
