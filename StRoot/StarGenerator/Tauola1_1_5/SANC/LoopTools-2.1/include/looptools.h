* looptools.h
* the header file for Fortran with all definitions for LoopTools
* this file is part of LoopTools
* last modified 1 Jul 04 th


#ifndef __LOOPTOOLS_H__
#define __LOOPTOOLS_H__

#define cc0 1
#define cc1 2
#define cc2 3
#define cc00 4
#define cc11 5
#define cc12 6
#define cc22 7
#define cc001 8
#define cc002 9
#define cc111 10
#define cc112 11
#define cc122 12
#define cc222 13

#define dd0 1
#define dd1 2
#define dd2 3
#define dd3 4
#define dd00 5
#define dd11 6
#define dd12 7
#define dd13 8
#define dd22 9
#define dd23 10
#define dd33 11
#define dd001 12
#define dd002 13
#define dd003 14
#define dd111 15
#define dd112 16
#define dd113 17
#define dd122 18
#define dd123 19
#define dd133 20
#define dd222 21
#define dd223 22
#define dd233 23
#define dd333 24
#define dd0000 25
#define dd0011 26
#define dd0012 27
#define dd0013 28
#define dd0022 29
#define dd0023 30
#define dd0033 31
#define dd1111 32
#define dd1112 33
#define dd1113 34
#define dd1122 35
#define dd1123 36
#define dd1133 37
#define dd1222 38
#define dd1223 39
#define dd1233 40
#define dd1333 41
#define dd2222 42
#define dd2223 43
#define dd2233 44
#define dd2333 45
#define dd3333 46

* for compatibility:

#define Cval(id, pos) Ccache(pos + id)
#define CCval(id, pos) CCcache(pos + id)
#define Dval(id, pos) Dcache(pos + id)
#define CDval(id, pos) CDcache(pos + id)

#define bcaini ffini
#define bcaexi ffexi

#endif

	double complex Ccache(1)
	common /Cbase/ Ccache

	double complex CCcache(1)
	common /CCbase/ CCcache

	double complex Dcache(1)
	common /Dbase/ Dcache

	double complex CDcache(1)
	common /CDbase/ CDcache

	double complex A0, CA0, B0, CB0, B1, CB1, B00, CB00, B11, CB11
	double complex DB0, CDB0, DB1, CDB1, DB00, CDB00, DB11, CDB11
	double complex C0, CC0, C0i, CC0i, D0, CD0, D0i, CD0i
	integer Cget, CCget, Dget, CDget, getcachelast
	double precision getmudim, getdelta, getlambda

	external A0, CA0, B0, CB0, B1, CB1, B00, CB00, B11, CB11
	external DB0, CDB0, DB1, CDB1, DB00, CDB00, DB11, CDB11
	external C0, CC0, C0i, CC0i, D0, CD0, D0i, CD0i
	external Cget, CCget, Dget, CDget, getcachelast
