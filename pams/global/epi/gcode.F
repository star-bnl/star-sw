
      subroutine gcode(col, q, id)
      implicit none
      integer col, q, id

      id = 0
      if(col .eq. 1) then
           if(q .gt. 0) id = 2
           if(q .lt. 0) id = 3
      else if(col .eq. 2) then
           if(q .gt. 0) id = 8
           if(q .lt. 0) id = 9
      else if(col .eq. 3) then
           if(q .gt. 0) id = 11
           if(q .lt. 0) id = 12
      else if(col .eq. 4) then
           if(q .gt. 0) id = 14
           if(q .lt. 0) id = 15
      else if(col .eq. 5) then
           id = 45
      else if(col .eq. 6) then
           id = 46
      else if(col .eq. 7) then
           id = 47
      end if
      return
      end
