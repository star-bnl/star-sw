STR = ${HOME}/star/str
MSG = ${HOME}/star/msg


str : libstr.a libstrc.a libstrf.a strnorm

dbg : libstr_dbg.a libstrc_dbg.a libstrf_dbg.a strnorm

all : libstr.a libstrc.a libstrf.a libstr_dbg.a libstrc_dbg.a libstrf_dbg.a strnorm


libstrf.a : strbytef.o
	rm -f libstrf.a
	ar vq libstrf.a strbytef.o
	ar ts libstrf.a

libstrc.a : strbyte_sgi.o strtermc_sgi.o
	rm -f libstrc.a
	ar vq libstrc.a strbyte_sgi.o strtermc_sgi.o
	ar ts libstrc.a

libstr.a : strlib.o strlib_sgi.o strmth.o strplot.o strterm_sgi.o str_nextarg.o
	rm -f libstr.a
	ar vq libstr.a strlib.o strlib_sgi.o strmth.o strplot.o strterm_sgi.o str_nextarg.o
	ar ts libstr.a


strlib.o : strlib.f strlib_inc
	f77 -c -O -w strlib.f

strlib_sgi.o : strlib_sgi.f
	f77 -c -O -w strlib_sgi.f

strmth.o : strmth.f
	f77 -c -O -w strmth.f

strplot.o : strplot.f
	f77 -c -O -w strplot.f

strterm_sgi.o : strterm_sgi.f
	f77 -c -O -w strterm_sgi.f

str_nextarg.o : str_nextarg.f str_ret_inc
	f77 -c -O -w str_nextarg.f


strbyte_sgi.o : strbyte_sgi.c
	cc -c -O -w strbyte_sgi.c

strtermc_sgi.o : strtermc_sgi.c
	cc -c -O -w strtermc_sgi.c

strbytef.o : strbytef.f
	f77 -c -O -w strbytef.f


libstrf_dbg.a : strbytef_dbg.o
	rm -f libstrf_dbg.a
	ar vq libstrf_dbg.a strbytef_dbg.o
	ar ts libstrf_dbg.a

libstrc_dbg.a : strbyte_sgi_dbg.o strtermc_sgi_dbg.o
	rm -f libstrc_dbg.a
	ar vq libstrc_dbg.a strbyte_sgi_dbg.o strtermc_sgi_dbg.o
	ar ts libstrc_dbg.a

libstr_dbg.a : strlib_dbg.o strlib_sgi_dbg.o strmth_dbg.o strplot_dbg.o strterm_sgi_dbg.o str_nextarg_dbg.o
	rm -f libstr_dbg.a
	ar vq libstr_dbg.a strlib_dbg.o strlib_sgi_dbg.o strmth_dbg.o strplot_dbg.o strterm_sgi_dbg.o str_nextarg_dbg.o
	ar ts libstr_dbg.a


strlib_dbg.o : strlib.f strlib_inc
	f77 -g -c -w strlib.f -o strlib_dbg.o

strlib_sgi_dbg.o : strlib_sgi.f
	f77 -g -c -w strlib_sgi.f -o strlib_sgi_dbg.o

strmth_dbg.o : strmth.f
	f77 -g -c -w strmth.f -o strmth_dbg.o

strplot_dbg.o : strplot.f
	f77 -g -c -w strplot.f -o strplot_dbg.o

strterm_sgi_dbg.o : strterm_sgi.f
	f77 -g -c -w strterm_sgi.f -o strterm_sgi_dbg.o

str_nextarg_dbg.o : str_nextarg.f str_ret_inc
	f77 -g -c -w str_nextarg.f -o str_nextarg_dbg.o


strbyte_sgi_dbg.o : strbyte_sgi.c
	cc -g -c -w strbyte_sgi.c -o strbyte_sgi_dbg.o

strtermc_sgi_dbg.o : strtermc_sgi.c
	cc -g -c -w strtermc_sgi.c -o strtermc_sgi_dbg.o


strbytef_dbg.o : strbytef.f
	f77 -g -c -w strbytef.f -o strbytef_dbg.o


strnorm : strnorm.o ${STR}/libstr.a ${STR}/libstrc.a ${MSG}/libmsg.a
	f77 -o strnorm strnorm.o -L${STR} -L${MSG} -lmsg -lstr -lstrc -lmsg -lstr -lstrc

strnorm.o : strnorm.f
	f77 -c -w strnorm.f -o strnorm.o

strlib_inc : strlib.inc
	ln -s strlib.inc strlib_inc

str_ret_inc : str_ret.inc
	ln -s str_ret.inc str_ret_inc
