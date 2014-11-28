
      Subroutine SetCtq6 (Iset)
      Implicit Double Precision (A-H,O-Z)
#include "pepadm.inc"
      Parameter (Isetmax0=3)
      Character Flnm(Isetmax0)*20, nn*3, Tablefile*40
      Data (Flnm(I), I=1,Isetmax0)
     & /'./pdf/cteq6l.tbl','./pdf/cteq6d.tbl','./pdf/cteq6m.tbl'/
      Data Isetold, Isetmin0, Isetmin1, Isetmax1 /-987,1,101,140/

C             If data file not initialized, do so.
      If(Iset.ne.Isetold) then
         IU= NextUn6()
         If (Iset.ge.Isetmin0 .and. Iset.le.Isetmax0) Then
C            Tablefile=Flnm(Iset)//'.tbl'
            Tablefile=Flnm(Iset)
         Elseif (Iset.ge.Isetmin1 .and. Iset.le.Isetmax1) Then
            write(nn,'(I3)') Iset
            Tablefile=Flnm(1)//nn//'.tbl'
         Else
            Print *, 'Invalid Iset number in SetCtq6 :', Iset
            Stop
         Endif
         Open(IU, File=Tablefile, Status='OLD', Err=100)
 21      Call ReadTbl6 (IU)
         Close (IU)
         Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     >//'in SetCtq6!!'
      Stop
C                             ********************
      End
