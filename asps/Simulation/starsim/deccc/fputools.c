/*
   
    This file gives 3 SUBROUTINES which can be used from FORTRAN. All
    arguments are INTEGERS. Compile this program with gcc and link it
    to your FORTRAN program.

       FPUSTACK(FPUTAG,FPUSTATUS,FPUUSED,FPUOVERFLOW)
                      : Get information about the FPU stack

          FPUTAG      : FPU TAG
          FPUSTATUS   : FPU status word
          FPUUSED     : Number of stack registers used. This should 
                        normally be 0. Non zero values indicates a problem
          FPUOVERFLOW : 0 = Ok, 1 = Stack overflow occurred (FPUUSED>8).

          Warning: calling fpustack will affect the FPU control word by
                   setting the lowest 7 bits incidentally disabling all
                   interrupts.  (Why?)

       FPUGETCW(CW)   : Get the FPU control word in CW
          CW : The FPU control word

       FPUSETCW(CW)   : Set the FPU control word from CW
          CW : The FPU control word

       Reference:
         /usr/include/fpu_control.h
         info -f g77 Trouble Missing Floating
         Intel reference manuals.

 */

void inline fpustack_(int *fputag,  int *fpustatus, 
                      int *fpuused, int *fpuoverflow)
{
  static int stats[10];
  int i,stat;
  
  asm("fnstenv %a0"
      :
      :"g" (stats)
      :"%0");
  *fputag    = stats[2];
  *fpustatus = stats[1];
  *fpuoverflow = 0;
  if (stats[1]&0x41) *fpuoverflow = 1;
  *fpuused = 0;
 stat = stats[2];
  for(i=0; i<8; i++) {
    if ((stat&3)!=3) (*fpuused)++;
    stat=stat>>2;
  }
}

/* Taken from glibc 2.1 /usr/include/fpu_control.h  */
#define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*cw))
#define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*cw))

typedef unsigned int fpu_control_t __attribute__ ((__mode__ (__HI__)));

/* Fortran wrappers */

void inline fpugetcw_(int *cword) {
  fpu_control_t  cw;
  _FPU_GETCW(&cw);
  *cword = cw;
}

void inline fpusetcw_(int *cword) {
  fpu_control_t cw;
  cw = *cword;
  _FPU_SETCW(&cw);
  *cword = cw;
}

