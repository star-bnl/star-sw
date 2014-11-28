#include "Herwig6.h"

//
// Declare f77 functions/subroutines
//
#define hwigin F77_NAME(hwigin,HWIGIN) /* initialize Herwig common blocks*/
#define hwissp F77_NAME(hwissp,HWISSP) /* input SUSY particle (and top quark) data */
#define hwuinc F77_NAME(hwuinc,HWUINC) /* Computer parameter-dependent constansts */
#define hwusta F77_NAME(hwusta,HWUSTA) /* Call HWUSTA to make any particle stable */
//#define hwabeg F77_NAME(hwabeg,HWABEG) /* Users initial calculations */
#define hweini F77_NAME(hweini,HWEINI) /* Initialize elementary processes */
#define hwuine F77_NAME(hwuine,HWUINE) /* Initialize event */
#define hwepro F77_NAME(hwepro,HWEPRO) /* Generate hard subprocess */
#define hwbgen F77_NAME(hwbgen,HWBGEN) /* Generate parton cascades */
#define hwdhob F77_NAME(hwdhob,HWDHOB) /* Do heavy object decays */
#define hwcfor F77_NAME(hwcfor,HWCFOR) /* Do cluster formation */
#define hwcdec F77_NAME(hwcdec,HWCDED) /* Do cluster decays */
#define hwdhad F77_NAME(hwdhad,HWDHAD) /* Do unstable particle decays */
#define hwdhvy F77_NAME(hwdhvy,HWDHVY) /* Do heavy flavor hadron decays */
#define hwmevt F77_NAME(hwmevt,HWMEVT) /* Add soft underlying event if needed */
#define hwufne F77_NAME(hwufne,HWUFNE) /* Finish event */
//#define hwanal F77_NAME(hwanal,HWANAL) /* Users event analysis*/
#define hwefin F77_NAME(hwefin,HWEFIN) /* Terminate elementary process */
//#define hwaend F77_NAME(hwaend,HWAEND) /* User's terminal calculations */
#define hwrgen F77_NAME(hwrgen,HWRGEN) /* Main random number generator */
#define setbeams F77_NAME(setbeams,SETBEAMS) /* Sets Beam species */

extern "C" void type_of_call hwigin();
extern "C" void type_of_call hwissp();
extern "C" void type_of_call hwuinc();
extern "C" void type_of_call hwusta( const char *name, int nname );
extern "C" void type_of_call hweini();
extern "C" void type_of_call hwuine();
extern "C" void type_of_call hwepro();
extern "C" void type_of_call hwbgen();
extern "C" void type_of_call hwdhob();
extern "C" void type_of_call hwcfor();
extern "C" void type_of_call hwcdec();
extern "C" void type_of_call hwdhad();
extern "C" void type_of_call hwdhvy();
extern "C" void type_of_call hwmevt();
extern "C" void type_of_call hwufne();
extern "C" void type_of_call hwefin();
//extern "C" double type_of_call hwrgen( int i );
extern "C" void type_of_call setbeams( const char *part1, const char *part2, int npart1, int npart2 );

void HWUSTA( string name ){ hwusta( name.c_str(), name.size() ); }
void HWEFIN(){ hwefin(); }
void HWSetBeams( string part1, string part2 ){ setbeams( part1.c_str(), part2.c_str(), part1.size(), part2.size() ); }
void InitializeEvent( vector<string> particles )
  {
    /// Initialize internal common blocks and other stuff
    hwigin();
    /// Input SUSY particle data
    //hwissp();
    /// Compute parameter dependent constanst
    hwuinc();
    /// Set particles to be stable
    for( unsigned int i=0;i<particles.size();i++)
    {
      HWUSTA(particles[i]);
    }
    /// Initialize elementary processes
    hweini(); 
  }
void GenerateEvent()
  {
    /// Initialize event
    hwuine();
    ///GENERATE HARD SUBPROCESS 
    hwepro();
    ///GENERATE PARTON CASCADES 
    hwbgen();
    ///DO HEAVY OBJECT DECAYS 
    hwdhob();
    ///DO CLUSTER FORMATION 
    hwcfor();
    ///DO CLUSTER DECAYS 
    hwcdec();
    ///DO UNSTABLE PARTICLE DECAYS 
    hwdhad();
    ///DO HEAVY FLAVOUR HADRON DECAYS 
    hwdhvy();
    //ADD SOFT UNDERLYING EVENT IF NEEDED
    //HWMEVT();
    ///FINISH EVENT
    hwufne();
  }
