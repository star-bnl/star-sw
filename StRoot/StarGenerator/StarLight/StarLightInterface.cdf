*
* StarLight steering 
*

***************************************************************************************8
*
>Name starlightinterface
>Menu starlight
>Guidance
The STARLIGHT menu allows the user to specify the run-time parameters
of the STARLIGHT event generator.

***************************************************************************************8
*
>Command BEAM_1_Z
>Guidance 
Set the Z of beam 1
>Parameters
z1 'Z of beam 1' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command BEAM_2_Z
>Guidance 
Set the Z of beam 2
>Parameters
z2 'Z of beam 2' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command BEAM_1_A
>Guidance 
Set the A of beam 1
>Parameters
a1 'A of beam 1' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command BEAM_2_A
>Guidance 
Set the A of beam 2
>Parameters
a2 'A of beam 2' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command BEAM_GAMMA
>Guidance 
Set the lorentz gamma of the beam
>Parameters
gamma 'Lorentz gamma of the beam' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command W_MAX
>Guidance 
Set the W max
>Parameters
wmax 'W_MAX' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command W_MIN
>Guidance 
Set the W min
>Parameters
wmin 'W_MIN' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command W_N_BINS
>Guidance 
Set the number of W bins
>Parameters
nbins 'W_N_BINS' I D=10
>Action StarLightUser
***************************************************************************************8
*
>Command RAP_MAX
>Guidance 
Set the max rapidity
>Parameters
max 'RAP_MAX' R D=2.0
>Action StarLightUser
***************************************************************************************8
*
>Command RAP_N_BINS
>Guidance 
Set the number of rapidity bins
>Parameters
nbins 'RAP_N_BINS' I D=10
>Action StarLightUser
***************************************************************************************8
*
>Command CUT_PT
>Guidance 
Flag which enables the PT cut
>Parameters
flag 'PT_CUT' I D=1
>Action StarLightUser
***************************************************************************************8
*
>Command PT_MIN
>Guidance 
Set the minimum PT
>Parameters
ptmin 'PT_MIN' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command PT_MAX
>Guidance 
Set the maximum PT
>Parameters
ptmax 'PT_MAX' R D=-1.0
>Action StarLightUser
***************************************************************************************8
*
>Command CUT_ETA
>Guidance 
Flag which enables the ETA cut
>Parameters
flag 'ETA_CUT' I D=1
>Action StarLightUser
***************************************************************************************8
*
>Command ETA_MIN
>Guidance 
Set the minimum eta
>Parameters
etamin 'ETA_MIN' R D=-2.0
>Action StarLightUser
***************************************************************************************8
*
>Command ETA_MAX
>Guidance 
Set the maximum eta
>Parameters
etamax 'ETA_MAX' R D=+2.0
>Action StarLightUser
***************************************************************************************8
*
>Command PROD_MODE
>Guidance 
Set the production mode
>Parameters
prod 'PROD_MODE' I D=1
>Action StarLightUser
***************************************************************************************8
*
>Command PROD_PID
>Guidance 
Set the production PID
>Parameters
pid 'PROD_PID' I D=121
>Action StarLightUser
***************************************************************************************8
*
>Command OUTPUT_FORMAT
>Guidance
Set the output format
>Parameters
fmt 'OUTPUT_FORMAT' I D=1
>Action StarLightUser
** >Command RND_seed
** >Guidance Set the random number generator SEED **** note... refactor to use starsim RNG ****
***************************************************************************************8
*
>Command BREAKUP_MODE
>Guidance 
Set the breakup mode
>Parameters
mode 'BREAKUP_MODE' I D=1
>Action StarLightUser
***************************************************************************************8
*
>Command INTERFERENCE
>Guidance 
Set the interference
>Parameters
flag 'INTERFERENCE' I D=1
>Action StarLightUser
***************************************************************************************8
*
>Command IF_STRENGTH
>Guidance 
Set the interference strength
>Parameters
strength 'IF_STRENGTH' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command COHERENT
>Guidance 
Set the coherence
>Parameters
flag 'CONHERENT' I D=1
>Action StarLightUser
***************************************************************************************8
*
>Command INCO_FACTOR
>Guidance 
Set the incoherence factor
>Parameters
fact 'INCO_FACTOR' R D=1.0
>Action StarLightUser
***************************************************************************************8
*
>Command BFORD
>Guidance 
Set the deuteron slope parameter 
>Parameters
slope 'BFORD' R D=0.0
>Action StarLightUser
***************************************************************************************8
*
>Command INT_PT_MAX
>Guidance 
Set the maximum pt interference INT_PT_MAX
>Parameters
max 'INT_PT_MAX' R D=10.0
>Action StarLightUser
***************************************************************************************8
*
>Command INT_PT_N_BINS
>Guidance 
Set the number of bins for pt interference INT_PT_N_BINS
>Parameters
nbins 'INT_PT_N_BINS' I D=10
>Action StarLightUser
