c	This is a set of dummy routines to mask off some VMS runtime
c	library routines.
c						D. Olson 22-Oct-1991
c
c----------------------------------------------------------------------------
	integer function lib$spawn()
	lib$spawn = 0
	write(6,1)
1	format(' VMS_DUMMIES version of LIB$SPAWN:  does nothing!!!!')
	return
	end
c---------------------------------------------------------------------------
	integer function lib$get_lun()
	lib$get_lun = 0
	return
	end
c---------------------------------------------------------------------------
	integer function lib$free_lun()
	lib$free_lun = 0
	return
	end
c---------------------------------------------------------------------------
	integer function lib$getjpi()
	lib$getjpi = 0
	return
	end
c---------------------------------------------------------------------------
	integer function lib$init_timer()
	lib$init_timer = 0
	return
	end
c---------------------------------------------------------------------------
	integer function lib$stat_timer()
	lib$stat_timer = 0
	return
	end
c---------------------------------------------------------------------------
	integer function lib$show_timer()
	lib$show_timer = 0
	return
	end
c---------------------------------------------------------------------------
	integer function lib$sys_trnlog()
	lib$sys_trnlog = 0
	return
	end
c--------------------------------------------------------------------------
      integer function str$upcase(c1,c2)
      implicit none
      character*(*) c1,c2
      integer tu_cvtupper
      str$upcase = tu_cvtupper(c1,c2)
      return
      end
c---------------------------------------------------------------------------
	integer function sys$trnlnm()
	sys$trnlnm = 0
	return
	end
c----------------------------------------------------------------------------
	integer function sys$ascefc()
	sys$ascefc = 0
	return
	end
c---------------------------------------------------------------------------
	integer function sys$clref()
	sys$clref = 0
	return
	end
c----------------------------------------------------------------------------
	integer function sys$crembx()
	sys$crembx = 0
	return
	end
c----------------------------------------------------------------------------
	integer function sys$dacefc()
	sys$dacefc = 0
	return
	end
c----------------------------------------------------------------------------
	integer function sys$delmbx()
	sys$delmbx = 0
	return
	end
c----------------------------------------------------------------------------
	integer function sys$dlcefc()
	sys$dlcefc = 0
	return
	end
c---------------------------------------------------------------------------
	integer function sys$readef()
	sys$readef = 0
	return
	end
c---------------------------------------------------------------------------
	integer function sys$setef()
	sys$setef = 0
	return
	end
c---------------------------------------------------------------------------
	integer function sys$wflor()
	sys$wflor = 0
	return
	end
