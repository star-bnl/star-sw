      INTEGER mxpnt, mxtrack
      PARAMETER (mxpnt=50, mxtrack=20000)

      STRUCTURE /track_pointers/
	  INTEGER*4    nhit                 ! number of his per track
	  INTEGER*4    ipnt(mxpnt)          ! pointers to hits belonging to trk
          INTEGER*4    pos(mxpnt)           ! position on a track
	  REAL*4       p(9)                 ! track parameters
          REAL*4       dpar(9)              ! errors on track parameters
          INTEGER*4    nfit                 ! # of points used in fit
          INTEGER*4    flag                 ! flags from the fitting 
      END STRUCTURE
