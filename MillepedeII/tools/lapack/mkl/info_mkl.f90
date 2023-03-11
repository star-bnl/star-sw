! CHK 220621
!
! Get MKL information
!
PROGRAM info_mkl

    CHARACTER*198  buf
    INTEGER major, minor, patch
    
    PRINT *, 'MKL version string'
    CALL mkl_get_version_string(buf)
    WRITE(*,'(a)') buf
    
    CALL ilaver( major,minor, patch )
    WRITE(*,100) major,minor, patch
100 FORMAT(' LAPACK version ',i0,'.',i0,'.',i0)

END PROGRAM info_mkl
