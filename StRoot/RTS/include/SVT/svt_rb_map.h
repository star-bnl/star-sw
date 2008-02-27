/* 
	Tonko, 7/19/00

	This board maps the DAQ's numbering scheme (sector,RB) to what
	SVT expects.
	Normally, they are the same but during the first phase of SVT
	test they may differ depending which fiber is plugged into which
	DAQ RB.
*/
#ifndef SVT_RB_MAP_H_

/*
	The index of this table is the DAQ's RB number generated as

		(svt_sector*6)+rb		where svt_sector 0..3, RB 0..5

	while the output is SVTs RB count from 0..23.
	Value 255 represents "not valid".
	Again, normally this map is 1:1 i.e. index 13 has value 13.
*/

static unsigned char svt_rb_map[24] = {
	0,12,255,255,255,255,
	255,255,255,255,255,255,
	255,255,255,255,255,255,
	255,255,255,255,255,255
} ;

#define SVT_RB_MAP_H_
#endif
