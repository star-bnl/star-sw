      subroutine break
      save integer nCall
      data nCall/0/
      nCall = nCall + 1
      write(*,*) '@@@@@@@@ BREAK ',nCall,' BREAK BREAK @@@@@@@@@@@@@@'
      end
