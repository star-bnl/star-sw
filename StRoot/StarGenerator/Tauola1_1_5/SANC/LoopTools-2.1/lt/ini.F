* ini.F
* routines for initializing and setting some parameters
* this file is part of LoopTools
* last modified 28 Jun 04 th


	subroutine setmudim(mudim2_)
	implicit none
	double precision mudim2_

#include "ff.h"

	double complex Ccache(1)
	common /Cbase/ Ccache

	double complex Dcache(1)
	common /Dbase/ Dcache

	mudim2 = mudim2_

	call setcachelast(Ccache, 0)
	call setcachelast(Dcache, 0)
	end

************************************************************************

	double precision function getmudim()
	implicit none

#include "ff.h"

	getmudim = mudim2
	end

************************************************************************

	subroutine setdelta(divergence_)
	implicit none
	double precision divergence_

#include "ff.h"

	double complex Ccache(1)
	common /Cbase/ Ccache

	double complex Dcache(1)
	common /Dbase/ Dcache

	divergence = divergence_

	call setcachelast(Ccache, 0)
	call setcachelast(Dcache, 0)
	end

************************************************************************

	double precision function getdelta()
	implicit none

#include "ff.h"

	getdelta = divergence
	end

************************************************************************

	subroutine setlambda(lambda2_)
	implicit none
	double precision lambda2_

#include "ff.h"

	double complex Ccache(1)
	common /Cbase/ Ccache

	double complex Dcache(1)
	common /Dbase/ Dcache

	lambda2 = lambda2_

	call setcachelast(Ccache, 0)
	call setcachelast(Dcache, 0)
	end

************************************************************************

	double precision function getlambda()
	implicit none

#include "ff.h"

	getlambda = lambda2
	end

