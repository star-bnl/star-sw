
      Subroutine SetCtq5 (Iset)
      Implicit Double Precision (A-H,O-Z)
#include "pepadm.inc"
      Parameter (Isetmax=9)
      Character Flnm(Isetmax)*16, Tablefile*40
      Data (Flnm(I), I=1,Isetmax)
     > / './pdf/cteq5m.tbl','./pdf/cteq5d.tbl','./pdf/cteq5l.tbl', 
     >   './pdf/cteq5hj.tbl',
     >   './pdf/cteq5hq.tbl','./pdf/cteq5f3.tbl','./pdf/cteq5f4.tbl',
     >   './pdf/cteq5m1.tbl','./pdf/ctq5hq1.tbl'  /
      Data Tablefile / 'test.tbl' /
      Data Isetold, Isetmin, Isettest / -987, 1, 911 /

C             If data file not initialized, do so.
      If(Iset.ne.Isetold) then
        IU= NextUn5()
         If (Iset .eq. Isettest) then
            Print* ,'Opening ', Tablefile
 21         Open(IU, File=Tablefile, Status='OLD', Err=101)
            GoTo 22
 101        Print*, Tablefile, ' cannot be opened '
            Print*, 'Please input the .tbl file:'
            Read (*,'(A)') Tablefile
            Goto 21
 22         Continue
         ElseIf (Iset.lt.Isetmin .or. Iset.gt.Isetmax) Then
           Print *, 'Invalid Iset number in SetCtq5 :', Iset
           Stop
         Else
c            Tablefile=Flnm(Iset)
c            Open(IU, File=Tablefile, Status='OLD', Err=100)
            Print*, 'We are here yeah'
            Print*, CUNPOL
            Open(IPLST(2), File=CUNPOL, Status='OLD', Err=100)
            IU=IPLST(2)
        Endif
         Call ReadTbl5 (IU)
         Close (IU)
        Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     >//'in SetCtq5!!'
      Stop
C                             ********************
      End
