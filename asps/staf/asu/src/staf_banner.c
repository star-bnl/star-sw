#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <time.h>

void staf_banner(FILE* stream);
void staf_banner_alt(FILE* stream);
void staf_banner_alt2(FILE* stream);
void staf_banner_alt3(FILE* stream);
void wavey(int n);

   char ESC=27;
   char CR=13;

#define BXSIZ 64
#define BYSIZ 15
#define BXPOS 8
#define BYPOS 5

   char *banner=
"+--------------------------------------------------------------+"
"|+------------------------------------------------------------+|"
"||          Welcome to the STAR Analysis Framework.           ||"
"||                                                            ||"
"||    .oooooo..o ooooooooooooo       .o.       oooooooooooo   ||"
"||   d8P'    `Y8 8'   888   `8      .888.      `888'     `8   ||"
"||   Y88bo.           888          .8\"888.      888           ||"
"||    `\"Y8888o.       888         .8' `888.     888oooo8      ||"
"||        `\"Y88b      888        .88ooo8888.    888    \"      ||"
"||   oo     .d8P      888       .8'     `888.   888           ||"
"||   8\"\"88888P'      o888o     o88o     o8888o o888o          ||"
"||                                                            ||"
"||                         IS ON LINE                         ||"
"|+------------------------------------------------------------+|"
"+--------------------------------------------------------------+"
   ;

   char *moast=
"+--------------------------------------------------------------+"
"|+------------------------------------------------------------+|"
"||     Welcome to the MOdular Analysis framework for STAR.    ||"
"||                                                            ||"
"||   ooo        ooooo                                  .      ||"
"||   `88.       .888'                                .o8      ||"
"||    888b     d'888   .ooooo.   .oooo.    .oooo.o .o888oo    ||"
"||    8 Y88. .P  888  d88' `88b `P  )88b  d88(  \"8   888      ||"
"||    8  `888'   888  888   888  .oP\"888  `\"Y88b.    888      ||"
"||    8    Y     888  888   888 d8(  888  o.  )88b   888 .    ||"
"||   o8o        o888o `Y8bod8P' `Y888\"\"8o 8\"\"888P'   \"888\"    ||"
"||                                                            ||"
"||                         IS ON LINE                         ||"
"|+------------------------------------------------------------+|"
"+--------------------------------------------------------------+"
   ;

#ifdef TEST_MAIN
int main(int argc, char **argv)
{
   int i;
   staf_banner(stdout);
/*
   sleep();
   staf_banner_alt(stdout);
   sleep();
   staf_banner_alt2(stdout);
   sleep();
   staf_banner_alt3(stdout);
   for(i=0;i<atoi(argv[1]);i++)wavey(1);
*/
   return 1;
}
#undef TEST_MAIN
#include "plot.c"
#endif /*TEST_MAIN*/

/*--------------------------------*/
void staf_banner(FILE* stream)
{
   int ic,ir;

   fprintf(stream,"\n");
/*- Print banner normally. -*/
   for(ir=0;ir<BYSIZ;ir++){
      fprintf(stream,"\t");
      for(ic=0;ic<BXSIZ;ic++){
	 fprintf(stream,"%c",banner[ir*BXSIZ+ic]);
      }
      fprintf(stream,"\n");
   }
   fprintf(stream,"\n");
}

/*--------------------------------*/
void staf_banner_alt(FILE* stream)
{
   int ic,ir;

   timespec_t pause;

   pause.tv_sec = 0;
   pause.tv_nsec = 200;

   fprintf(stream,"\n");

/*- Print banner left to right. -*/
   fprintf(stream,"%c[2J",ESC);
   for(ic=0;ic<BXSIZ;ic++){
      for(ir=0;ir<BYSIZ;ir++){
	 fprintf(stream,"%c[%d;%df",ESC,ir+BYPOS,ic+BXPOS);
	 fprintf(stream,"%c",banner[ir*BXSIZ+ic]);
      }
/*    fflush(0);sleep(1); */
/*    fflush(0);nanosleep(&pause,NULL); */
   }

   fprintf(stream,"\n\n\n\n");

}


/*--------------------------------*/
void staf_banner_alt2(FILE* stream)
{
#define PUT_S(XXX,YYY,SSS) fprintf(stream,"%c[%d;%df%s",ESC,XXX,YYY,SSS)
#define PUT_C(XXX,YYY,CCC) fprintf(stream,"%c[%d;%df%c",ESC,XXX,YYY,SSS)

   int ic,ir;
   char *vl="(0x(B";		/* vertical line */
   char *hl="(0q(B";		/* horizontal line */
   char *tl="(0l(B";		/* top-left corner */
   char *tr="(0k(B";		/* top-right corner */
   char *bl="(0m(B";		/* bottom-left corner */
   char *br="(0j(B";		/* bottom-right corner */

   char *tx="(0w(B";		/* top-T (cross) */
   char *bx="(0v(B";		/* bottom-T (cross) */
   char *lx="(0t(B";		/* left-T (cross) */
   char *rx="(0u(B";		/* right-T (cross) */

   timespec_t pause;

   pause.tv_sec = 0;
   pause.tv_nsec = 200;

   fprintf(stream,"\n");

/*- Print banner boxes. -*/
   fprintf(stream,"%c[2J",ESC);
   for(ic=1;ic<BXSIZ-1;ic++){
	 PUT_S(BYPOS,ic+BXPOS,hl);
	 PUT_S(BYPOS+BYSIZ-1,ic+BXPOS,hl);
	 if((1<ic) && (ic<BXSIZ-1)){
	    PUT_S(BYPOS+1,ic+BXPOS,hl);
	    PUT_S(BYPOS+BYSIZ-2,ic+BXPOS,hl);
	 }
   }
   for(ir=1;ir<BYSIZ-1;ir++){
	 PUT_S(ir+BYPOS,BXPOS,vl);
	 PUT_S(ir+BYPOS,BXPOS+BXSIZ-1,vl);
	 if((1<ir) && (ir<BYSIZ-1)){
	    PUT_S(ir+BYPOS,BXPOS+1,vl);
	    PUT_S(ir+BYPOS,BXPOS+BXSIZ-2,vl);
	 }
   }
   PUT_S(BYPOS,BXPOS,tl);
   PUT_S(BYPOS+1,BXPOS+1,tl);
/* 	 PUT_S(BYPOS,BXPOS+1,hl);	HACK PATCH */
   PUT_S(BYPOS,BXPOS+BXSIZ-1,tr);
   PUT_S(BYPOS+1,BXPOS+BXSIZ-2,tr);
   PUT_S(BYPOS+BYSIZ-1,BXPOS,bl);
   PUT_S(BYPOS+BYSIZ-2,BXPOS+1,bl);
   PUT_S(BYPOS+BYSIZ-1,BXPOS+BXSIZ-1,br);
   PUT_S(BYPOS+BYSIZ-2,BXPOS+BXSIZ-2,br);
   fflush(0);

/*- Print banner text left to right. -*/
   for(ic=2;ic<BXSIZ-2;ic++){
      for(ir=2;ir<BYSIZ-2;ir++){
	 fprintf(stream,"%c[%d;%df",ESC,ir+BYPOS,ic+BXPOS);
	 fprintf(stream,"%c",banner[ir*BXSIZ+ic]);
      }
/*    fflush(0);nanosleep(&pause,NULL); */
   }

   fprintf(stream,"\n\n\n\n");

}

/*--------------------------------*/
void staf_banner_alt3(FILE* stream)
{
#define PUT_S(XXX,YYY,SSS) fprintf(stream,"%c[%d;%df%s",ESC,XXX,YYY,SSS)
#define PUT_C(XXX,YYY,CCC) fprintf(stream,"%c[%d;%df%c",ESC,XXX,YYY,SSS)

   int i;
   int ic,ir;
   char *vl="(0x(B";		/* vertical line */
   char *hl="(0q(B";		/* horizontal line */
   char *tl="(0l(B";		/* top-left corner */
   char *tr="(0k(B";		/* top-right corner */
   char *bl="(0m(B";		/* bottom-left corner */
   char *br="(0j(B";		/* bottom-right corner */

   char *tx="(0w(B";		/* top-T (cross) */
   char *bx="(0v(B";		/* bottom-T (cross) */
   char *lx="(0t(B";		/* left-T (cross) */
   char *rx="(0u(B";		/* right-T (cross) */

   timespec_t pause;

   pause.tv_sec = 0;
   pause.tv_nsec = 100;

   fprintf(stream,"\n");

/*- Print banner boxes. -*/
   fprintf(stream,"%c[2J",ESC);
   for(ic=1;ic<BXSIZ-1;ic++){
	 PUT_S(BYPOS,ic+BXPOS,hl);
	 PUT_S(BYPOS+BYSIZ-1,ic+BXPOS,hl);
	 if((1<ic) && (ic<BXSIZ-1)){
	    PUT_S(BYPOS+1,ic+BXPOS,hl);
	    PUT_S(BYPOS+BYSIZ-2,ic+BXPOS,hl);
	 }
   }
   for(ir=1;ir<BYSIZ-1;ir++){
	 PUT_S(ir+BYPOS,BXPOS,vl);
	 PUT_S(ir+BYPOS,BXPOS+BXSIZ-1,vl);
	 if((1<ir) && (ir<BYSIZ-1)){
	    PUT_S(ir+BYPOS,BXPOS+1,vl);
	    PUT_S(ir+BYPOS,BXPOS+BXSIZ-2,vl);
	 }
   }
   PUT_S(BYPOS,BXPOS,tl);
   PUT_S(BYPOS+1,BXPOS+1,tl);
/* 	 PUT_S(BYPOS,BXPOS+1,hl);	HACK PATCH */
   PUT_S(BYPOS,BXPOS+BXSIZ-1,tr);
   PUT_S(BYPOS+1,BXPOS+BXSIZ-2,tr);
   PUT_S(BYPOS+BYSIZ-1,BXPOS,bl);
   PUT_S(BYPOS+BYSIZ-2,BXPOS+1,bl);
   PUT_S(BYPOS+BYSIZ-1,BXPOS+BXSIZ-1,br);
   PUT_S(BYPOS+BYSIZ-2,BXPOS+BXSIZ-2,br);
   fflush(0);

for(i=0;i<6;i++){
/*- Print moast text right to left. -*/
/* for(ic=2;ic<BXSIZ-2;ic++){ */
/* for(ic=BXSIZ-3;ic>1;ic--){ */
   for(ic=BXSIZ-3;ic>i*10+1;ic--){
      for(ir=2;ir<BYSIZ-2;ir++){
	 fprintf(stream,"%c[%d;%df",ESC,ir+BYPOS,ic+BXPOS);
	 fprintf(stream,"%c",moast[ir*BXSIZ+ic]);
      }
      fflush(0);wavey(1);
   }

/*- Print banner text left to right. -*/
   for(ic=2;ic<BXSIZ-2;ic++){
      for(ir=2;ir<BYSIZ-2;ir++){
	 fprintf(stream,"%c[%d;%df",ESC,ir+BYPOS,ic+BXPOS);
	 fprintf(stream,"%c",banner[ir*BXSIZ+ic]);
      }
      fflush(0);wavey(1);
   }

   wavey(6); /*sleep(1);*/
}

/* GOTO_XY(1,23); */

}

void wavey(int n)
{
/*
   static int i0=0;
   int i,j,jj;
   float x,y,o;

   for(j=0;j<n;j++){
   for(i=0;i<64;i++){
	x = i0+i;
	y = 22. +2*sin(0.3*x);
	for(jj=1;jj<5;jj++)PLOT_C(9+i,jj,' ');
        plot_line((float)(9+(i0+i)%64),y);
   }
   i0++;
   }
*/
}
