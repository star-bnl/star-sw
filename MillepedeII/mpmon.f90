!> \file
!! Progress monitoring.
!!
!! \author Claus Kleinwort, DESY, 2020 (Claus.Kleinwort@desy.de)
!!
!! \copyright
!! Copyright (c) 2020 Deutsches Elektronen-Synchroton,
!! Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
!! This library is free software; you can redistribute it and/or modify
!! it under the terms of the GNU Library General Public License as
!! published by the Free Software Foundation; either version 2 of the
!! License, or (at your option) any later version. \n\n
!! This library is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU Library General Public License for more details. \n\n
!! You should have received a copy of the GNU Library General Public
!! License along with this program (see the file COPYING.LIB for more
!! details); if not, write to the Free Software Foundation, Inc.,
!! 675 Mass Ave, Cambridge, MA 02139, USA.
!!
!! Monitor progress in routines taking significant amount of cpu time.
!!

!> Monitoring data.
MODULE mpmonpgs
    USE mpdef
    IMPLICIT NONE

    INTEGER(mpi) :: lun     !< output unit
    INTEGER(mpi) :: nrep    !< repetition rate
    INTEGER(mpi) :: nrepmi  !< repetition rate max increase

END MODULE mpmonpgs

!> Initialize monitoring.
!!
!! \param [in] l   output unit
!! \param [in] n1  repetition rate start value
!! \param [in] n2  repetition rate max increase
!!
SUBROUTINE monini(l,n1,n2)
    USE mpmonpgs

    IMPLICIT NONE

    INTEGER(mpi), INTENT(IN)          :: l
    INTEGER(mpi), INTENT(IN)          :: n1
    INTEGER(mpi), INTENT(IN)          :: n2

    CHARACTER (LEN=24) :: chdate

    lun=l
    nrep=n1
    nrepmi=n2
    CALL fdate(chdate)
    WRITE(lun,*) ' Starting - ', chdate

END SUBROUTINE monini

!> Progress monitoring.
!!
!! \param [in] i   index
!!
!! If index >= nrep print index and update nrep -> nrep + min(nrep,nrepmi)
!!
SUBROUTINE monpgs(i)
    USE mpmonpgs

    IMPLICIT NONE

    INTEGER(mpi), INTENT(IN)          :: i

    IF (i >= nrep) THEN
        WRITE(lun,*) ' Index: ', i
        nrep=nrep+min(nrep,nrepmi)
    END IF

END SUBROUTINE monpgs

!> End monitoring.
SUBROUTINE monend()
    USE mpmonpgs

    IMPLICIT NONE

    CHARACTER (LEN=24) :: chdate

    CALL fdate(chdate)
    WRITE(lun,*) ' Ending - ', chdate

END SUBROUTINE monend
