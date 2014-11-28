#include "UrQMD.h"

//
// Declare f77 functions/subroutines
//

#define energies F77_NAME(energies,ENERGIES)
#define sys F77_NAME(sys,SYS)
#define rsys F77_NAME(rsys,RSYS)
#define cuts F77_NAME(cuts,CUTS)
#define spdata F77_NAME(spdata,SPDATA)
#define isys F77_NAME(isys,ISYS)
#define coor F77_NAME(coor,COOR)
#define frag F77_NAME(frag,FRAG)
#define aios F77_NAME(aios,AIOS)
#define pots F77_NAME(pots,POTS)
#define scoor F77_NAME(scoor,SCOOR)
#define sisys F77_NAME(sisys,SISYS)
#define ssys F77_NAME(ssys,SSYS)
#define rtdelay F77_NAME(rtdelay,RTDELAY)
#define itdelay F77_NAME(itdelay,ITDELAY)
#define svinfo F77_NAME(svinfo,SVINFO)
#define ffermi F77_NAME(ffermi,FFERMI)
#define peq F77_NAME(peq,PEQ)
#define ranf F77_NAME(ranf,RANF) /* Main random number generator */
#define iurqmd F77_NAME(iurqmd,IURQMD)
#define genevt F77_NAME(genevt,GENEVT)

extern "C" void type_of_call energies();
extern "C" void type_of_call sys();
extern "C" void type_of_call rsys();
extern "C" void type_of_call cuts();
extern "C" void type_of_call spdata();
extern "C" void type_of_call isys();
extern "C" void type_of_call coor();
extern "C" void type_of_call frag();
extern "C" void type_of_call aios();
extern "C" void type_of_call pots();
extern "C" void type_of_call scoor();
extern "C" void type_of_call sisys();
extern "C" void type_of_call ssys();
extern "C" void type_of_call rtdelay();
extern "C" void type_of_call itdelay();
extern "C" void type_of_call svinfo();
extern "C" void type_of_call ffermi();
extern "C" void type_of_call peq();
extern "C" void type_of_call ranf();
extern "C" void type_of_call iurqmd();
extern "C" void type_of_call genevt();
