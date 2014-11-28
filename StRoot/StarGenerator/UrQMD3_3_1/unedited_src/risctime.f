c $Id: risctime.f,v 1.1 2012/11/29 21:00:08 jwebb Exp $
c	Zeit/Datum auf einer RISC6000
c	alle Angaben werden in ein array TM(9) abgespeichert:
c TM(1) = secs (0-59)
c TM(2) = mins (0-59)
c TM(3) = hours (0-23)
c TM(4) = day (1-31)
c TM(5) = month (1-12)
c TM(6) = year
c TM(7) = weekday (sunday=0,monday=1,...)
c TM(8) = day of year (0-365)
c TM(9) = nonzero if daylight savings time
c
      subroutine risctime(tm)
      INTEGER stime,TM(9),time
      stime=time()
      call gmtime(stime,tm)
      tm(5)=tm(5)+1
      tm(6)=tm(6)+1900
      return
      end
