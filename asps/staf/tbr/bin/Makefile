OBJECTS=../lib/irix/brow.o ../lib/irix/cuts.o ../lib/irix/draw.o
HEADERS=../inc/cuts.h
brow:	${OBJECTS}
	cc -o irix/tbr ${OBJECTS} \
	../lib/irix/libdsl.a \
	-lXm -lXt -lX11 -lm
../lib/irix/draw.o:	../src/draw.c ${HEADERS}
	cc -c -I../inc -I/star/starlib/star/dev/sys/dsl/inc \
	-o ../lib/irix/draw.o ../src/draw.c
../lib/irix/cuts.o:	../src/cuts.c ${HEADERS}
	cc -c -I../inc -I/star/starlib/star/dev/sys/dsl/inc \
	-o ../lib/irix/cuts.o ../src/cuts.c
../lib/irix/brow.o:	../src/brow.c ${HEADERS}
	cc -c -I../inc -I/star/starlib/star/dev/sys/dsl/inc \
	-o ../lib/irix/brow.o ../src/brow.c
