Module TPCEDIG is the TPC digitization
Author P.Nevski
Created on sunday
* 
   integer AgFHIT0,AgFHIT1,Iprin,ih,i,itra,nvl(5)
   real    hits(10)
*
begin
*
check AgFHIT0('TPCE','TPAD') == ok
*
Do While (AgFHIT1(ih,itra,nvl,hits) == ok)
   if (nvl(1)<=1&hits(3)<0) print *, ih,nvl(1),nvl(2),(hits(i),i=1,3)
enddo
*
end
