#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void wrline(const char *line);

main(argc, argv)       /*  LF - a program to List File     */
int argc;              /*  usage:  LF filename outfile     */
char *argv[];          /*  Programmed by A.Fedoseev, may 87*/
{
 int ln;
 enum qwe {MAXLINE=10024,MAXINT=9000000};
 char line[MAXLINE];

 FILE *fdi,*fdo;

 int ifile, Nfr, Nto, Nli, Mark=0, nlisted,ja,n;
 char *a,*aa,*cc;

 Nfr=-1996; Nto = MAXINT; Nli=MAXINT; nlisted=0;  ifile = 0; 

 for (ja=1; ja<argc; ja++) { /* loop over args */

   a = argv[ja];
   if (a[0] !='-') { /* it is file name */
     ifile++;
     if ((cc=strchr(a,':'))) {
        n = strtol(cc+1,NULL,0); 
        Nfr = n-5; if (Nfr<1) Nfr=1; 
        Nto = n+10; Mark = n; *cc='\0';}

     
     if(ifile==1) {			/* 1st arg is given */
       if (!freopen(a,"r",stdin )) { 	/* Wrong input file */
         printf("\n Input file %s not found \n",a);exit(13);
       } else                      { 	/* Good  input file*/
         printf("\n                 ****  Listing of file: %s ****\n\n",a);
     } };

     if(ifile==2) {/* 2nd arg is given */
       if (!freopen(a,"w",stdout)) { /* Wrong ouput file */
         printf("\n Output file %s open error\n",argv[2]);exit(13);}};
     continue;
   };
  
   if ( isdigit(a[1])) 	{ /* It is from/to number */
     n = strtol(a+1,NULL,0);
     if (Nfr < 0) {Nfr = n-5; Nto = n + 10; Mark=n;} else {Nto = n; Nli=999999;};
     continue;
   };

   if ( strncmp("-fr",a,2)==0)   { /* It is from number */
     if ( a[3] ) { Nfr = strtol(a+3,NULL,0);
     } else      { 
       if (aa = argv[++ja]) Nfr = strtol(aa,NULL,0);
     }; 
     continue;
   };

   if ( strncmp("-to",a,2)==0)   { /* It is to    number */
     if ( a[3] ) { Nto = strtol(a+3,NULL,0); Nli = 999999;
     } else      { 
       if (aa = argv[++ja]) Nto = strtol(aa,NULL,0); Nli =999999;
     }; 
     continue;
   };

   if ( strncmp("-li",a,2)==0)   { /* It is list number */
     if ( a[3] ) { Nli = strtol(a+3,NULL,0); Nto = 999999;
     } else      { 
       if (aa = argv[++ja]) Nli = strtol(aa,NULL,0); Nto=999999;
     }; 
     continue;
   };
 }


 
 ln=0; while(gets(line))  {
   ln++;
   if ( ln>=Nfr) {
     char mark[]=" "; if (Mark==ln) mark[0]='*';
     nlisted++; wrline(line);};
   if (nlisted>=Nli)	break;
   if (ln     >=Nto)	break;
 };

 wrline(NULL);
 fclose(stdin); fclose(stdout);
}


void wrline(const char *line)
{ 
  static char cbuf[500][512];
  static int status = 0;
  static int inbuf  = 0;
  static int warbuf = 0;
  static char whois[100];

  int jb,ifgcc,ifwar,l;
  char *who,*cc,*sl;
    
  ifgcc = !line || strstr(line,"g++")!=0 ||strstr(line,"gcc")!=0;  
  if (ifgcc || inbuf > 400) {/* gcc started*/
  
    if (warbuf) {
      printf("\n\n");
      if (whois[0]) fprintf(stdout," *** Warning in %s  ***\n",whois);
      if (whois[0]) fprintf(stderr," *** Warning in %s  ***\n",whois);
      printf("\n\n");


      for (jb=0;jb<inbuf;jb++) printf("%s\n",cbuf[jb]);
    }
    inbuf = 0;
    status = 1;
    warbuf = 0;
    whois[0]=0;
  }
  if (!status || !line) return;
  
  ifwar = strstr(line,"warning")!=0;
  warbuf |= ifwar;
  strcpy(cbuf[inbuf++],line);  
  
  who = strstr(line," -o ");
  if (!who) return;
  who = who+4 + strspn(who+4," ");
  l = strcspn(who," ");
  whois[0]=0; strncat(whois,who,l);
  sl = 0;
  cc = strrchr(whois,'/');
  if (cc) {*cc=0; sl = strrchr(whois,'/'); *cc='/';}
  if (sl) strcpy(whois,sl+1);  

}  






