      INTEGER mxpnt, mxtrack,mxpnt_tpc
      PARAMETER (mxpnt=120, mxtrack=20000, mxpnt_tpc=100)

      STRUCTURE /track_pointers/
	  INTEGER*4    nhit                 ! number of his per track
	  INTEGER*4    ipnt(mxpnt)          ! pointers to hits belonging to trk
          INTEGER*4    pos(mxpnt)           ! position on a track
	  INTEGER*4    det(mxpnt)           ! detector hit belongs to
	  REAL*4       p(9)                 ! track parameters
          INTEGER*4    nfit                 ! # of points used in fit
          INTEGER*4    flag                 ! flags from the fitting 
	  INTEGER*4    ntpc                 ! Number of TPC hits
      END STRUCTURE
