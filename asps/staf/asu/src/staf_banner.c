#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void staf_banner(FILE* stream);
void staf_warning(FILE* stream);

#define BXSIZ 64
#define BYSIZ 15
#define BXPOS 8
#define BYPOS 5

   char *banner=
"+--------------------------------------------------------------+"
"|+------------------------------------------------------------+|"
"||         Welcome to the Standard Analysis Framework.        ||"
"||                                                            ||"
"||       .oooooo..o     .         .o.       oooooooooooo      ||"
"||      d8P'    `Y8   .o8        .888.      `888'     `8      ||"
"||      Y88bo.      .o888oo     .8\"888.      888              ||"
"||       `\"Y8888o.    888      .8' `888.     888oooo8         ||"
"||           `\"Y88b   888     .88ooo8888.    888    \"         ||"
"||      oo     .d8P   888 .  .8'     `888.   888              ||"
"||      8\"\"88888P'    \"888\" o88o     o8888o o888o             ||"
"||                                                            ||"
"||                         IS ON LINE                         ||"
"|+------------------------------------------------------------+|"
"+--------------------------------------------------------------+"
   ;
/*"||          Welcome to the STAR Analysis Framework.           ||"*/
/*
   char *banner=
"+--------------------------------------------------------------+"
"|+------------------------------------------------------------+|"
"||  Welcome to the Scientific Table-based Analysis Framework. ||"
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
*/
/*
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
*/

#ifdef TEST_MAIN
int main(int argc, char **argv)
{
   int i;
   staf_banner(stdout);
   return 1;
}
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
/* staf_warning(stream); */
}

/*--------------------------------*/
void staf_warning(FILE* stream)
{
   int i;
   fprintf(stream,"18jul96 - BUG FIX IN PROGRESS\n");
   for( i=0;i<20;i++ ){
      fprintf(stream,"WARNING -- You have linked against a bug fix version of STAF.\n");
   }
   fprintf(stream,"The normal version of the STAF libraries will soon return.\n");
   fprintf(stream,"Please wait abit and relink.\n");
}

