

struct tpcDedxPidAmpDb {

   float  gasCalib; /* calibration # for variations in gas gain */
   float  saturationCalib[2] ; /*parameters used to describe saturation*/

   float  eMeanPar[3]; /* parameters for Bethe_Block distribution */
   float  eAmpPar[4];  /* parameters for Maxwell_Boltz distribution */
   float  eSigPar[2];  /* parameters for the Linear */

   float  pionMeanPar[3]; /* parameters for Bethe_Block distribution */
   float  pionAmpPar[4];  /* parameters for Maxwell_Boltz distribution */
   float  pionSigPar[2];  /* parameters for the Linear */

   float  kaonMeanPar[3]; /* parameters for Bethe_Block distribution */
   float  kaonAmpPar[4];  /* parameters for Maxwell_Boltz distribution */
   float  kaonSigPar[2];  /* parameters for the Linear */

   float  protonMeanPar[3]; /* parameters for Bethe_Block distribution */
   float  protonAmpPar[4];  /* parameters for Maxwell_Boltz distribution
*/
   float  protonSigPar[2];  /* parameters for the Linear */

   float  chargedMeanPar[3]; /* parameters for Bethe_Block distribution */
   float  chargedSigPar[2];  /* parameters for the Linear */

};


