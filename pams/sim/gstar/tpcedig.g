Module TPCEDIG is the TPC digitization
Author P.Nevski
Created on sunday
* 
   integer AgFHIT0,AgFHIT1,Iprin,ih,itra,nvl(5)
   real    hits(10)
*
begin
*
check AgFHIT0('TPCE','TPAD') == ok
*
Do While (AgFHIT1(ih,itra,nvl,hits) == ok)
   print *, ih,nvl(3),hits(7),hits(8)
enddo
*
end
