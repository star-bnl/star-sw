
      Subroutine SetCtq4 (Iset)
      Implicit Double Precision (A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      Parameter (Isetmax=14)
      Character Flnm(Isetmax)*12, Tablefile*40
      Data (Flnm(I), I=1,Isetmax)
     > / 'cteq4m.tbl', 'cteq4d.tbl', 'cteq4l.tbl'
     > , 'cteq4a1.tbl', 'cteq4a2.tbl', 'cteq4m.tbl', 'cteq4a4.tbl'
     > , 'cteq4a5.tbl', 'cteq4hj.tbl', 'cteq4lq.tbl'
     > , 'cteq4hq.tbl', 'cteq4hq1.tbl', 'cteq4f3.tbl', 'cteq4f4.tbl' /
      Data Tablefile / 'test.tbl' /
      Data Isetold, Isetmin, Isettest / -987, 1, 911 /

C             If data file not initialized, do so.
C    Rewrite file name to fit Pepsi distribution format
C
      If(Iset.ne.Isetold) then
        IU= NextUn()
         If (Iset .eq. Isettest) then
            Print* ,'Opening ', Tablefile
            Open(IU, File=Tablefile, Status='OLD', Err=101)
         ElseIf (Iset.lt.Isetmin .or. Iset.gt.Isetmax) Then
            Print *, 'Invalid Iset number in SetCtq4 :', Iset
            Stop
         Else
*            Tablefile=Flnm(Iset)
*            Tablefile='./pdf/' 
            Open(IPLST(2), File=CUNPOL, Status='OLD', Err=100)
            IU=IPLST(2)
         Endif
         Call ReadTbl (IU)
         Close (IU)
         Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     > 
      Stop
 101  Print*, Tablefile, ' cannot be opened '
      Print*, 'Please input the .tbl file:'
      Read (*,'(A)') Tablefile

C                             ********************
      End
