
      subroutine conf_check(nhypo_det,sharp,wt,wt_avg,shpmin,wtmin,
     1                  max_col,iconf)
      implicit none
      integer i,nhypo_det(5),max_col(6), iconf
      real sharp(5), wt(5), wt_avg, shpmin, wtmin
ccc
ccc   iconf = 1 , conflict found between detector specific pid and total
ccc   iconf = 0 , no conflict found.
ccc
      iconf = 0
      do 10 i = 1,5
      if(nhypo_det(i) .eq. 0) go to 10
      if(max_col(i) .ne. max_col(6)
     1   .and. sharp(i) .gt. shpmin .and. (wt(i)/wt_avg) .gt. wtmin)
     2   iconf = 1
10    continue
      return
      end
