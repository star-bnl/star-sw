C:---------------------------------------------------------------------
C:FILE:         
C:DESCRIPTION:  init PAWC and KUIP, execute $STAF_LOGON
c:AUTHOR:       mc - MCronqvist@lbl.gov
C:HISTORY:      18nov96-v001d-mc - Creation
        subroutine STAF_KUIP_INIT()
        implicit none 
        
	integer*4 paw_init_count
        integer   NPAWMEM
        parameter (NPAWMEM = 1000000)
        integer   pawmem(NPAWMEM)
        character ename*10
        character evalue*100
        integer   LENOCC
        common/pawc/pawmem

	if(paw_init_count().gt.0)then
		write(*,*)'ASU: PAW is already INITed'
		return
	endif

        write (6,*) 'ASU: Initializing hlimit'
        call hlimit(npawmem)
        
        write (6,*) 'ASU: Initializing kuip'
        call kuinit(10000)
        
        write (6,*) 'ASU: Initializing vecdef'
        call vecdef()
        
        ename = 'STAF_LOGON'
        call GETENV(ename, evalue)
        if(LENOCC(evalue).gt.0) then
          write(*,'(a)') 'ASU: Executing '//evalue(1:LENOCC(evalue))
          call KUEXEL('EXEC $STAF_LOGON')
        endif

        end
