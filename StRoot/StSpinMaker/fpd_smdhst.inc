*  Definitions for SMD-based pi0 reconstruction single-event histograms

      character*6 cevt
      integer ihy1,ihy2,ihx1,ihx2
      integer ihist

      integer ismx0,ismx1,ismx2
      integer ismy0,ismy1,ismy2
      integer ismdf
      integer itow0,ipre1,ipre2,ihmx0
      parameter (ismx0=8000)
      parameter (ismx1=8100)
      parameter (ismx2=8200)
      parameter (ismy0=8300)
      parameter (ismy1=8400)
      parameter (ismy2=8500)
      parameter (ismdf=100)
      parameter (itow0=8600)
      parameter (ipre1=8700)
      parameter (ipre2=8800)
      parameter (ihmx0=19)

      integer ihevt

      common/smdhst/ihevt

      integer irun
      character*4 cpirun
      common/runinfo/irun,cpirun

