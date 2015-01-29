!> \file
!! Data structures.

!> Parameters, variables, dynamic arrays.
!!
!! For parameters which can be set from command line or
!! steering files more details are available in: \ref option_page.

MODULE mpmod
    USE mpdef
    IMPLICIT NONE
    SAVE
    ! steering parameters
    INTEGER(mpi) :: ictest=0  !< test mode '-t'
    INTEGER(mpi) :: metsol=0  !< solution method (1: inversion, 2: diagonalization, 3: \ref minresqlpmodule::minresqlp "MINRES-QLP")
    INTEGER(mpi) :: matsto=2  !< (global) matrix storage mode (1: full, 2: sparse)
    INTEGER(mpi) :: mprint=1  !< print flag (0: minimal, 1: normal, >1: more)
    INTEGER(mpi) :: mdebug=0  !< debug flag (number of records to print)
    INTEGER(mpi) :: mdebg2=10 !< number of measurements for record debug printout
    INTEGER(mpi) :: mreqen=10 !< required number of entries (for variable global parameter)
    INTEGER(mpi) :: mitera=1  !< number of iterations
    INTEGER(mpi) :: nloopn=0  !< number of data reading, fitting loops
    INTEGER(mpi) :: mbandw=0  !< band width of preconditioner matrix
    INTEGER(mpi) :: lunkno=0  !< flag for unkown keywords
    INTEGER(mpi) :: lhuber=0  !< Huber down-weighting flag
    REAL(mps)    :: chicut=0.0  !< cut in terms of 3-sigma cut, first iteration
    REAL(mps)    :: chirem=0.0  !< cut in terms of 3-sigma cut, other iterations, approaching 1.
    REAL(mps)    :: chhuge=50.0 !< cut in terms of 3-sigma for unreasonable data, all iterations
    INTEGER(mpi) :: nrecpr=0  !< record number with printout
    INTEGER(mpi) :: nrecp2=0  !< record number with printout
    INTEGER(mpi) :: nrec1 =0  !< record number with largest residual
    INTEGER(mpi) :: nrec2 =0  !< record number with largest chi^2/Ndf
    REAL(mps)    :: value1=0.0!< largest residual
    REAL(mps)    :: value2=0.0!< largest chi^2/Ndf
    REAL(mps)    :: dwcut=0.0 !< down-weight fraction cut
    INTEGER(mpi) :: isubit=0  !< subito flag '-s'
    REAL(mps)    :: wolfc1=0.0!< C_1 of strong Wolfe condition
    REAL(mps)    :: wolfc2=0.0!< C_2 of strong Wolfe condition
    REAL(mpd) :: mrestl=1.0E-06 !< tolerance criterion for MINRES-QLP
    REAL(mpd) :: mrtcnd=1.0E+07 !< transition (QR -> QLP) (matrix) condition for MINRES-QLP
    INTEGER(mpi) :: mrmode=0  !< MINRES-QLP mode (0: QR+QLP, 1: only QR, 2: only QLP factorization)
    INTEGER(mpi) :: nofeas=0  !< flag for skipping making parameters feasible
    INTEGER(mpi) :: nhistp=0  !< flag for histogram printout
    REAL(mps)    :: delfun=0.0!< expected function change
    REAL(mps)    :: actfun=0.0!< actual function change
    REAL(mps)    :: angras=0.0!< angle between gradient and search direction
    INTEGER(mpi) :: iterat=0  !< iterations in solution
    INTEGER(mpi) :: nregul=0  !< regularization flag
    REAL(mps)    :: regula=1.0!< regularization parameter, add regula * norm(global par.) to objective function
    REAL(mps)    :: regpre=0.0!< default presigma
    INTEGER(mpi) :: matrit=0  !< matrix calculation up to iteration MATRIT
    INTEGER(mpi) :: icalcm=0  !< calculation mode (for \ref xloopn "XLOOPN") , >0: calculate matrix
    INTEGER(mpi) :: numbit=1  !< number of bits for pair counters
    INTEGER(mpi) :: nbndr =0  !< number of records with bordered band matrix for local fit
    INTEGER(mpi) :: nbdrx =0  !< max border size for local fit
    INTEGER(mpi) :: nbndx =0  !< max band width for local fit
    INTEGER(mpi) :: nrecer=0  !< record with error (rank deficit or Not-a-Number) for printout
    INTEGER(mpi) :: nrec3 = huge(nrec3) !< (1.) record number with error
    INTEGER(mpi) :: mreqpe=1  !< min number of pair entries
    INTEGER(mpi) :: mhispe=0  !< upper bound for pair entry histogrammimg
    INTEGER(mpi) :: msngpe=0  !< upper bound for pair entry single precision storage
    INTEGER(mpi) :: mcmprs=0  !< compression flag for sparsity (column indices)
    INTEGER(mpi) :: mthrd =1  !< number of (OpenMP) threads
    INTEGER(mpi) :: mxrec =0  !< max number of records
    INTEGER(mpi) :: matmon=0  !< record interval for monitoring of (sparse) matrix construction
    INTEGER(mpi) :: lfitnp=huge(lfitnp) !< local fit: number of iteration to calculate pulls
    INTEGER(mpi) :: lfitbb=1  !< local fit: check for bordered band matrix (if >0)
    INTEGER(mpi) :: mnrsel=0  !< number of MINRES error labels in LBMNRS (calc err, corr with SOLGLO)
    INTEGER(mpi) :: ncache=-1 !< buffer size for caching (default 100MB per thread)
    REAL(mps), DIMENSION(3) :: fcache = (/ 0.8,  0., 0. /) !< read cache, average fill level; write cache; dynamic size
    INTEGER(mpi) :: mthrdr=1  !< number of threads for reading binary files
    INTEGER(mpi) :: mnrsit=0  !< total number of MINRES internal iterations
    INTEGER(mpi) :: iforce=0  !< switch to SUBITO for (global) rank defects if zero
    INTEGER(mpi) :: igcorr=0  !< flag for output of global correlations for inversion, =0: none
    INTEGER(mpi) :: memdbg=0  !< debug flag for memory management
    REAL(mps)    :: prange=0.0!< range (-PRANGE..PRANGE) for histograms of pulls, norm. residuals
    INTEGER(mpi) :: lsearch=2 !< iterations (solutions) with line search:
                         !! >2: all, =2: all with (next) Chi2 cut scaling factor =1., =1: last, <1: none
    ! variables
    INTEGER(mpi) :: lunlog !< unit for logfile
    INTEGER(mpi) :: lvllog !< log level
    INTEGER(mpi) :: ntgb !< total number of global parameters
    INTEGER(mpi) :: nvgb !< number of variable global parameters
    INTEGER(mpi) :: nagb !< number of fit parameters (global par. + Lagrange mult.)
    INTEGER(mpi) :: ncgb !< number of constraints
    INTEGER(mpi) :: nagbn !< max number of global paramters per record
    INTEGER(mpi) :: nalcn !< max number of local paramters per record
    INTEGER(mpi) :: naeqn !< max number of equations (measurements) per record
    INTEGER(mpi) :: nrec  !< (current) record number
    REAL(mps)    :: dflim !< convergence limit
    INTEGER(mpi), DIMENSION(0:3) :: nrejec !< rejected events
    REAL(mps), DIMENSION(0:8) :: times !< cpu time counters
    REAL(mps)    :: stepl !< step length (line search)
    CHARACTER (LEN=74) :: textl !< name of current MP 'module' (step)
    LOGICAL :: newite !< flag for new iteration
    INTEGER(mpi) :: ndfsum !< sum(ndf)
    INTEGER(mpi) :: iitera !< MINRES iterations
    INTEGER(mpi) :: istopa !< MINRES istop (convergence)
    INTEGER(mpi) :: lsinfo !< line search: returned information
    REAL         :: rstart !< cpu start time for solution iterations
    REAL(mps)    :: deltim !< cpu time difference
    INTEGER(mpi) :: npresg !< number of pre-sigmas
    INTEGER(mpi) :: nrecal !< number of records
    INTEGER(mpi) :: ndefec=0 !< rank deficit for global matrix (from inversion)
    INTEGER(mpi) :: nmiss1=0 !< rank deficit for constraints
    INTEGER(mpi) :: lcalcm !< last calclation mode
    INTEGER(mpi) :: nspc   !< number of precision for sparse global matrix (1=D, 2=D+F)
    INTEGER(mpi) :: nencdb !< encoding info (number bits for column counter)
    INTEGER(mpi), DIMENSION(100) :: lbmnrs !< MINRES error labels
    REAL(mpd) :: fvalue !< function value (chi2 sum) solution
    REAL(mpd) :: flines !< function value line search
    REAL(mpd) :: sumndf !< weighted sum(ndf)
    ! each loop
    INTEGER(mpi) :: numReadbuffer     !< number of buffers (records) in (read) block
    INTEGER(mpi) :: numBlocks         !< number of (read) blocks
    INTEGER(mpi) :: sumRecords        !< sum of records
    INTEGER(mpi) :: skippedRecords    !< number of skipped records (buffer too small)
    INTEGER(mpi) :: minRecordsInBlock !< min. records in block
    INTEGER(mpi) :: maxRecordsInBlock !< max. records in block
    ! accurate sumation
    INTEGER(mpi), PARAMETER::nexp20=1048576 ! 2**20
    REAL(mpd)::accurateDsum=0.0_mpd !< fractional part of sum
    INTEGER(mpi)::accurateNsum=0 !< sum mod 2**20
    INTEGER(mpi)::accurateNexp=0 !< sum  /  2**20
    INTEGER(mpi) :: lenGlobalVec !< length of global vector 'b' (A*x=b)
    ! dynamic arrays
    !======================================================
    ! global parameters
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: globalParameter !< global parameters (start values + sum(x_i))
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: globalParCopy !< copy of global parameters
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: globalCorrections !< correction x_i (from A*x_i=b_i in iteration i)
    REAL(mps), DIMENSION(:), ALLOCATABLE :: globalParStart     !< start value for global parameters
    REAL(mps), DIMENSION(:), ALLOCATABLE :: globalParPreSigma  !< pre-sigma for global parameters
    REAL(mps), DIMENSION(:), ALLOCATABLE :: globalParPreWeight !< weight from pre-sigma
    ! global matrix, vector
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: globalMatD !< global matrix 'A' (double, full or sparse)
    REAL(mps), DIMENSION(:), ALLOCATABLE :: globalMatF !< global matrix 'A' (float part for compressed sparse)
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: globalVector !< global vector 'x' (in A*x=b)
    ! preconditioning
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matPreCond !< preconditioner (band) matrix
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: indPreCond !< preconditioner pointer array
    ! auxiliary vectors
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: workspaceD !< (general) workspace (D)
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: workspaceLinesearch !< workspace line search
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: workspaceDiagonalization !< workspace diag.
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: workspaceEigenValues !< workspace eigen values
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: workspaceEigenVectors !< workspace eigen vectors
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: workspaceI !< (general) workspace (I)
    ! constraint matrix, residuals
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matConsProduct !< product matrix of constraints
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: vecConsResiduals !< residuals of constraints
    ! global parameter mapping
    INTEGER(mpi), DIMENSION(:,:), ALLOCATABLE :: globalParLabelIndex !< global parameters label, total -> var. index
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: globalParHashTable    !< global parameters hash table
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: globalParVarToTotal   !< global parameters variable -> total index
    INTEGER(mpi), DIMENSION(-7:0) :: globalParHeader = 0 !< global parameters (mapping) header
                                                    !!
                                                    !!  0: length of labels/indices; \n
                                                    !! -1: number of stored items; \n
                                                    !! -2: =0 during build-up; \n
                                                    !! -3: next number; \n
                                                    !! -4: (largest) prime number (< length); \n
                                                    !! -5: number of overflows; \n
                                                    !! -6: nr of variable parameters; \n
                                                    !! -7: call counter for build-up;

    ! row information for sparse matrix
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: sparseMatrixCompression !< compression info (per row)
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: sparseMatrixColumns     !< (compressed) list of columns for sparse matrix
    INTEGER(mpl), DIMENSION(:,:), ALLOCATABLE :: sparseMatrixOffsets !< row offsets for column list, sparse matrix elements
    ! read buffer
    INTEGER(mpi), DIMENSION(:,:), ALLOCATABLE :: readBufferInfo !< buffer management (per thread)
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: readBufferPointer !< pointer to used buffers
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: readBufferDataI !< integer data
    REAL(mps), DIMENSION(:), ALLOCATABLE :: readBufferDataF !< float data
    ! global parameter usage in record
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: globalIndexUsage !< indices of global par in record
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: backIndexUsage   !< list of global par in record
    ! local fit
    REAL(mpd), DIMENSION(:), ALLOCATABLE::blvec  !< local fit vector 'b' (in A*x=b), replaced by 'x'
    REAL(mpd), DIMENSION(:), ALLOCATABLE::clmat  !< local fit matrix 'A' (in A*x=b)
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE:: ibandh !< local fit 'band width histogram' (band size autodetection)
    ! scratch arrays for local fit
    REAL(mpd), DIMENSION(:), ALLOCATABLE::vbnd !< local fit band part of 'A'
    REAL(mpd), DIMENSION(:), ALLOCATABLE::vbdr !< local fit border part of 'A'
    REAL(mpd), DIMENSION(:), ALLOCATABLE::aux  !< local fit 'solutions for border rows'
    REAL(mpd), DIMENSION(:), ALLOCATABLE::vbk  !< local fit 'matrix for border solution'
    REAL(mpd), DIMENSION(:), ALLOCATABLE::vzru !< local fit 'border solution'
    REAL(mpd), DIMENSION(:), ALLOCATABLE::scdiag !< local fit workspace (D)
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE:: scflag         !< local fit workspace (I)
    REAL(mps), DIMENSION(:), ALLOCATABLE :: localCorrections !< local fit corrections (to residuals)
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: localGlobalMatrix !< matrix correlating local and global par
    ! update of global matrix
    INTEGER(mpi), DIMENSION(:,:), ALLOCATABLE :: writeBufferInfo  !< write buffer management (per thread)
    REAL(mps), DIMENSION(:,:), ALLOCATABLE :: writeBufferData     !< write buffer data (largest residual, Chi2/ndf, per thread)
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: writeBufferIndices !< write buffer for indices
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: writeBufferUpdates !< write buffer for update matrices
    INTEGER(mpi), DIMENSION(-6:6) :: writeBufferHeader = 0 !< write buffer header (-6..-1: updates, 1..6: indices)
                                                      !!
                                                      !! +/-1: buffer size (words) per thread; \n
                                                      !! +/-2: min number of free words; \n
                                                      !! +/-3: number of buffer flushes; \n
                                                      !! +/-4: number of buffer overruns; \n
                                                      !! +/-5: average fill level; \n
                                                      !! +/-6: peak fill level;
    !> list items from steering file
    INTEGER(mpi) :: lenParameters=0   !< length of list of parameters from steering file
    TYPE(listItem), DIMENSION(:), ALLOCATABLE :: listParameters   !< list of parameters from steering file
    INTEGER(mpi) :: lenPresigmas=0    !< length of list of pre-sigmas from steering file
    TYPE(listItem), DIMENSION(:), ALLOCATABLE :: listPreSigmas    !< list of pre-sgmas from steering file
    INTEGER(mpi) :: lenConstraints=0  !< length of list of constraints from steering file
    TYPE(listItem), DIMENSION(:), ALLOCATABLE :: listConstraints  !< list of constraints from steering file
    INTEGER(mpi) :: lenMeasurements=0 !< length of list of measurements from steering file
    TYPE(listItem), DIMENSION(:), ALLOCATABLE :: listMeasurements !< list of measurements from steering file
    !======================================================
    ! file information
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: mfd   !< file mode: cbinary =1, text =2, fbinary=3
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: lfd   !< length of file name
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: nfd   !< index (line) in (steering) file
    INTEGER(mpi), DIMENSION(:,:), ALLOCATABLE :: kfd !< (1,.)=  number of records in file, (2,..)= file order
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: ifd   !< file: integrated record numbers (=offset)
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: jfd   !< file: number of accepted records
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: dfd   !< file: ndf sum
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: xfd   !< file: max. record size
    REAL(mps), DIMENSION(:), ALLOCATABLE :: cfd      !< file: chi2 sum
    REAL(mps), DIMENSION(:), ALLOCATABLE :: ofd      !< file: option
    REAL(mps), DIMENSION(:), ALLOCATABLE :: wfd      !< file: weight
    CHARACTER (LEN=1024) :: filnam !< name of steering file
    INTEGER(mpi) :: nfnam  !< length of sterring file name
    CHARACTER, DIMENSION(:), ALLOCATABLE :: tfd !< file names (concatenation)
    INTEGER(mpi) :: ifile  !< current file (index)
    INTEGER(mpi) :: nfiles !< number of files
    INTEGER(mpi) :: nfilb  !< number of binary files
    INTEGER(mpi) :: nfilf  !< number of Fortran binary files
    INTEGER(mpi) :: nfilc  !< number of C binary files
    INTEGER(mpi) :: nfilw  !< number of weighted binary files
    INTEGER(mpi) :: ndimbuf=10000 !< default read buffer size (I/F words, half record length)

END MODULE mpmod
