# PHOJET

Revision: 19.1.0, Date: 2019/07/08

Authors: 
- Ralph Engel, Karlsruhe Institute of Technology, Karlsruhe, Germany

- [Anatoli Fedynitch](mailto:af.gh179@outlook.com)*, University of Alberta, Edmonton, Canada
        
- Stefan Roesler, CERN, Geneva

*(current maintainer)
   
Visit the [DPMJET GitHub repository](https://github.com/afedynitch/dpmjet) for the latest version. Use [Issues](https://github.com/afedynitch/dpmjet/issues) for reporting bugs, questions or complaints.

The code is distributed with a copy of [PYTHIA 6.4.27](https://pythiasix.hepforge.org) and relies on its fragmentation routines. Torbjorn Sjostrand is the copyright holder of PYTHIA 6.


## List of subroutines and functions
 
### Main event simulation routines
- [PHO_EVENT](../src/phojet/PHO_EVENT.f)
- [PHO_PARTON](../src/phojet/PHO_PARTON.f)
- [PHO_POSPOM](../src/phojet/PHO_POSPOM.f)
- [PHO_STDPAR](../src/phojet/PHO_STDPAR.f)
- [PHO_POMSCA](../src/phojet/PHO_POMSCA.f)
### User steering interface
- [PHO_SETMDL](../src/phojet/PHO_SETMDL.f)
- [PHO_PRESEL](../src/phojet/PHO_PRESEL.f)
### Experimental setup / photon flux calculation
- [PHO_FIXLAB](../src/phojet/PHO_FIXLAB.f)
- [PHO_FIXCOL](../src/phojet/PHO_FIXCOL.f)
- [PHO_GPHERA](../src/phojet/PHO_GPHERA.f)
- [PHO_GGEPEM](../src/phojet/PHO_GGEPEM.f)
- [PHO_WGEPEM](../src/phojet/PHO_WGEPEM.f)
- [PHO_GGBLSR](../src/phojet/PHO_GGBLSR.f)
- [PHO_GGBEAM](../src/phojet/PHO_GGBEAM.f)
- [PHO_GGHIOF](../src/phojet/PHO_GGHIOF.f)
- [PHO_GGHIOG](../src/phojet/PHO_GGHIOG.f)
- [PHO_GGFLCL](../src/phojet/PHO_GGFLCL.f)
- [PHO_GGFLCR](../src/phojet/PHO_GGFLCR.f)
- [PHO_GGFAUX](../src/phojet/PHO_GGFAUX.f)
- [PHO_GGFNUC](../src/phojet/PHO_GGFNUC.f)
- [PHO_GHHIOF](../src/phojet/PHO_GHHIOF.f)
- [PHO_GHHIAS](../src/phojet/PHO_GHHIAS.f)

### Initialization
- [PHO_INIT](../src/phojet/PHO_INIT.f)
- [PHO_DATINI](../src/phojet/PHO_DATINI.f)
- [PHO_PARDAT](../src/phojet/PHO_PARDAT.f)
- [PHO_MCINI](../src/phojet/PHO_MCINI.f)
- [PHO_EVEINI](../src/phojet/PHO_EVEINI.f)
- [PHO_HARINI](../src/phojet/PHO_HARINI.f)
- [PHO_FRAINI](../src/phojet/PHO_FRAINI.f)
- [PHO_FITPAR](../src/phojet/PHO_FITPAR.f)

### Cross section calculation
- [PHO_CSINT](../src/phojet/PHO_CSINT.f)
- [PHO_XSECT](../src/phojet/PHO_XSECT.f)
- [PHO_BORNCS](../src/phojet/PHO_BORNCS.f)
- [PHO_HARXTO](../src/phojet/PHO_HARXTO.f)
- [PHO_DSIGDT](../src/phojet/PHO_DSIGDT.f)
- [PHO_TRIREG](../src/phojet/PHO_TRIREG.f)
- [PHO_LOOREG](../src/phojet/PHO_LOOREG.f)
- [PHO_TRXPOM](../src/phojet/PHO_TRXPOM.f)
- [PHO_EIKON](../src/phojet/PHO_EIKON.f)
- [PHO_CHAN2A](../src/phojet/PHO_CHAN2A.f)
- [PHO_SCALES](../src/phojet/PHO_SCALES.f)

### Multiple interaction structure
- [PHO_IMPAMP](../src/phojet/PHO_IMPAMP.f)
- [PHO_PRBDIS](../src/phojet/PHO_PRBDIS.f)
- [PHO_SAMPRO](../src/phojet/PHO_SAMPRO.f)
- [PHO_SAMPRB](../src/phojet/PHO_SAMPRB.f)

### Hadron / photon remnant treatment, soft x selection
- [PHO_HARREM](../src/phojet/PHO_HARREM.f)
- [PHO_PARREM](../src/phojet/PHO_PARREM.f)
- [PHO_HADSP2](../src/phojet/PHO_HADSP2.f)
- [PHO_HADSP3](../src/phojet/PHO_HADSP3.f)
- [PHO_SOFTXX](../src/phojet/PHO_SOFTXX.f)
- [PHO_SELSXR](../src/phojet/PHO_SELSXR.f)
- [PHO_SELSX2](../src/phojet/PHO_SELSX2.f)
- [PHO_SELSXS](../src/phojet/PHO_SELSXS.f)
- [PHO_SELSXI](../src/phojet/PHO_SELSXI.f)
- [PHO_VALFLA](../src/phojet/PHO_VALFLA.f)
- [PHO_REGFLA](../src/phojet/PHO_REGFLA.f)
- [PHO_SEAFLA](../src/phojet/PHO_SEAFLA.f)
- [PHO_FLAUX](../src/phojet/PHO_FLAUX.f)
- [PHO_BETAF](../src/phojet/PHO_BETAF.f)
- [IPHO_DIQU](../src/phojet/IPHO_DIQU.f)

### Primordial kt and soft parton pt
- [PHO_PRIMKT](../src/phojet/PHO_PRIMKT.f)
- [PHO_PARTPT](../src/phojet/PHO_PARTPT.f)
- [PHO_SOFTPT](../src/phojet/PHO_SOFTPT.f)
- [PHO_SELPT](../src/phojet/PHO_SELPT.f)
- [PHO_CONN0](../src/phojet/PHO_CONN0.f)
- [PHO_CONN1](../src/phojet/PHO_CONN1.f)

### Simulation of hard scattering, initial state radiation
- [PHO_HARCOL](../src/phojet/PHO_HARCOL.f)
- [PHO_SELCOL](../src/phojet/PHO_SELCOL.f)
- [PHO_HARCOR](../src/phojet/PHO_HARCOR.f)
- [PHO_HARDIR](../src/phojet/PHO_HARDIR.f)
- [PHO_HARX12](../src/phojet/PHO_HARX12.f)
- [PHO_HARDX1](../src/phojet/PHO_HARDX1.f)
- [PHO_HARKIN](../src/phojet/PHO_HARKIN.f)
- [PHO_HARWGH](../src/phojet/PHO_HARWGH.f)
- [PHO_HARSCA](../src/phojet/PHO_HARSCA.f)
- [PHO_HARFAC](../src/phojet/PHO_HARFAC.f)
- [PHO_HARWGX](../src/phojet/PHO_HARWGX.f)
- [PHO_HARWGI](../src/phojet/PHO_HARWGI.f)
- [PHO_HARINT](../src/phojet/PHO_HARINT.f)
- [PHO_HARMCI](../src/phojet/PHO_HARMCI.f)
- [PHO_HARXR3](../src/phojet/PHO_HARXR3.f)
- [PHO_HARXR2](../src/phojet/PHO_HARXR2.f)
- [PHO_HARXD2](../src/phojet/PHO_HARXD2.f)
- [PHO_HARXPT](../src/phojet/PHO_HARXPT.f)
- [PHO_HARISR](../src/phojet/PHO_HARISR.f)
- [PHO_HARZSP](../src/phojet/PHO_HARZSP.f)
- [PHO_PTCUT](../src/phojet/PHO_PTCUT.f)
- [PHO_ALPHAE](../src/phojet/PHO_ALPHAE.f)
- [PHO_ALPHAS](../src/phojet/PHO_ALPHAS.f)

### Diffraction dissociation
- [PHO_DIFDIS](../src/phojet/PHO_DIFDIS.f)
- [PHO_DIFPRO](../src/phojet/PHO_DIFPRO.f)
- [PHO_DIFPAR](../src/phojet/PHO_DIFPAR.f)
- [PHO_QELAST](../src/phojet/PHO_QELAST.f)
- [PHO_CDIFF](../src/phojet/PHO_CDIFF.f)
- [PHO_DFWRAP](../src/phojet/PHO_DFWRAP.f)
- [PHO_SAMASS](../src/phojet/PHO_SAMASS.f)
- [PHO_DSIGDM](../src/phojet/PHO_DSIGDM.f)
- [PHO_DFMASS](../src/phojet/PHO_DFMASS.f)
- [PHO_SDECAY](../src/phojet/PHO_SDECAY.f)
- [PHO_SDECY2](../src/phojet/PHO_SDECY2.f)
- [PHO_SDECY3](../src/phojet/PHO_SDECY3.f)
- [PHO_DIFSLP](../src/phojet/PHO_DIFSLP.f)
- [PHO_DIFKIN](../src/phojet/PHO_DIFKIN.f)
- [PHO_VECRES](../src/phojet/PHO_VECRES.f)
- [PHO_DIFRES](../src/phojet/PHO_DIFRES.f)
- [PHO_REGPAR](../src/phojet/PHO_REGPAR.f)
- [PHO_PECMS](../src/phojet/PHO_PECMS.f)
- [PHO_SETPAR](../src/phojet/PHO_SETPAR.f)

### Fragmentation, treatment of low-mass strings
- [PHO_STRING](../src/phojet/PHO_STRING.f)
- [PHO_STRFRA](../src/phojet/PHO_STRFRA.f)
- [PHO_ID2STR](../src/phojet/PHO_ID2STR.f)
- [PHO_MCHECK](../src/phojet/PHO_MCHECK.f)
- [PHO_POMCOR](../src/phojet/PHO_POMCOR.f)
- [PHO_MASCOR](../src/phojet/PHO_MASCOR.f)
- [PHO_PARCOR](../src/phojet/PHO_PARCOR.f)
- [PHO_GLU2QU](../src/phojet/PHO_GLU2QU.f)
- [PHO_GLUSPL](../src/phojet/PHO_GLUSPL.f)
- [PHO_DQMASS](../src/phojet/PHO_DQMASS.f)
- [PHO_BAMASS](../src/phojet/PHO_BAMASS.f)
- [PHO_MEMASS](../src/phojet/PHO_MEMASS.f)

### Particle code tables, particle numbering conversion
- [PHO_PNAME](../src/phojet/PHO_PNAME.f)
- [PHO_PMASS](../src/phojet/PHO_PMASS.f)
- [IPHO_CHR3](../src/phojet/IPHO_CHR3.f)
- [IPHO_BAR3](../src/phojet/IPHO_BAR3.f)
- [IPHO_ANTI](../src/phojet/IPHO_ANTI.f)
- [IPHO_PDG2ID](../src/phojet/IPHO_PDG2ID.f)
- [IPHO_ID2PDG](../src/phojet/IPHO_ID2PDG.f)
- [IPHO_LU2PDG](../src/phojet/IPHO_LU2PDG.f)
- [IPHO_PDG2LU](../src/phojet/IPHO_PDG2LU.f)
- [IPHO_CNV1](../src/phojet/IPHO_CNV1.f)
- [PHO_HACODE](../src/phojet/PHO_HACODE.f)

### Lorentz transformations, rotations and mass adjustment
- [PHO_ALTRA](../src/phojet/PHO_ALTRA.f)
- [PHO_LTRANS](../src/phojet/PHO_LTRANS.f)
- [PHO_TRANS](../src/phojet/PHO_TRANS.f)
- [PHO_TRANI](../src/phojet/PHO_TRANI.f)
- [PHO_MKSLTR](../src/phojet/PHO_MKSLTR.f)
- [PHO_GETLTR](../src/phojet/PHO_GETLTR.f)
- [PHO_LTRHEP](../src/phojet/PHO_LTRHEP.f)
- [PHO_MSHELL](../src/phojet/PHO_MSHELL.f)
- [PHO_MASSAD](../src/phojet/PHO_MASSAD.f)

### Program debugging and internal cross-checks
- [PHO_PREVNT](../src/phojet/PHO_PREVNT.f)
- [PHO_PRSTRG](../src/phojet/PHO_PRSTRG.f)
- [PHO_CHECK](../src/phojet/PHO_CHECK.f)
- [PHO_TRACE](../src/phojet/PHO_TRACE.f)
- [PHO_REJSTA](../src/phojet/PHO_REJSTA.f)
- [PHO_ABORT](../src/phojet/PHO_ABORT.f)

### Cross section fitting
- [PHO_FITMAI](../src/phojet/PHO_FITMAI.f)
- [PHO_FITINP](../src/phojet/PHO_FITINP.f)
- [PHO_FITDAT](../src/phojet/PHO_FITDAT.f)
- [PHO_FITOUT](../src/phojet/PHO_FITOUT.f)
- [PHO_FITAMP](../src/phojet/PHO_FITAMP.f)
- [PHO_FITTST](../src/phojet/PHO_FITTST.f)
- [PHO_FITMSQ](../src/phojet/PHO_FITMSQ.f)
- [PHO_FITVD1](../src/phojet/PHO_FITVD1.f)
- [PHO_FITCN1](../src/phojet/PHO_FITCN1.f)
- [PHO_FITINI](../src/phojet/PHO_FITINI.f)

### Cross section parametrizations
- [PHO_HADCSL](../src/phojet/PHO_HADCSL.f)
- [PHO_ALLM97](../src/phojet/PHO_ALLM97.f)
- [PHO_CSDIFF](../src/phojet/PHO_CSDIFF.f)

### Random numbers
- DPMJET random number generator DT_RNDM used
- [PHO_SFECFE](../src/phojet/PHO_SFECFE.f)
- [PHO_RNDBET](../src/phojet/PHO_RNDBET.f)
- [PHO_RNDGAM](../src/phojet/PHO_RNDGAM.f)

### Auxiliary routines / numerical methods
- [PHO_GAUSET](../src/phojet/PHO_GAUSET.f)
- [PHO_GAUDAT](../src/phojet/PHO_GAUDAT.f)
- [pho_samp1d](../src/phojet/pho_samp1d.f)
- [PHO_DZEROX](../src/phojet/PHO_DZEROX.f)
- [PHO_EXPINT](../src/phojet/PHO_EXPINT.f)
- [PHO_BESSJ0](../src/phojet/PHO_BESSJ0.f)
- [PHO_BESSI0](../src/phojet/PHO_BESSI0.f)
- [pho_ExpBessI0](../src/phojet/pho_ExpBessI0.f)
- [PHO_BESSI1](../src/phojet/PHO_BESSI1.f)
- [PHO_BESSK0](../src/phojet/PHO_BESSK0.f)
- [PHO_BESSK1](../src/phojet/PHO_BESSK1.f)
- [PHO_XLAM](../src/phojet/PHO_XLAM.f)
- [PHO_SWAPD](../src/phojet/PHO_SWAPD.f)
- [PHO_SWAPI](../src/phojet/PHO_SWAPI.f)

### Parton density parametrization management / interface
- [PHO_PDF](../src/phojet/PHO_PDF.f)
- [PHO_SETPDF](../src/phojet/PHO_SETPDF.f)
- [PHO_GETPDF](../src/phojet/PHO_GETPDF.f)
- [PHO_ACTPDF](../src/phojet/PHO_ACTPDF.f)
- [PHO_QPMPDF](../src/phojet/PHO_QPMPDF.f)
- [PHO_PDFTST](../src/phojet/PHO_PDFTST.f)

### Parton density parametrizations from other authors
- [PHO_DOR98LO](../src/phojet/PHO_DOR98LO.f)
- [PHO_DOR98SC](../src/phojet/PHO_DOR98SC.f)
- [PHO_DOR94LO](../src/phojet/PHO_DOR94LO.f)
- [PHO_DOR94HO](../src/phojet/PHO_DOR94HO.f)
- [PHO_DOR94DI](../src/phojet/PHO_DOR94DI.f)
- [PHO_DOR92LO](../src/phojet/PHO_DOR92LO.f)
- [PHO_DOR92HO](../src/phojet/PHO_DOR92HO.f)
- [PHO_DORPLO](../src/phojet/PHO_DORPLO.f)
- [PHO_DORPHO](../src/phojet/PHO_DORPHO.f)
- [PHO_DORGLO](../src/phojet/PHO_DORGLO.f)
- [PHO_DORGHO](../src/phojet/PHO_DORGHO.f)
- [PHO_DORGH0](../src/phojet/PHO_DORGH0.f)
- [PHO_DOR94FV](../src/phojet/PHO_DOR94FV.f)
- [PHO_DOR94FW](../src/phojet/PHO_DOR94FW.f)
- [PHO_DOR94FS](../src/phojet/PHO_DOR94FS.f)
- [PHO_DOR92FV](../src/phojet/PHO_DOR92FV.f)
- [PHO_DOR92FW](../src/phojet/PHO_DOR92FW.f)
- [PHO_DOR92FS](../src/phojet/PHO_DOR92FS.f)
- [PHO_DORFVP](../src/phojet/PHO_DORFVP.f)
- [PHO_DORFGP](../src/phojet/PHO_DORFGP.f)
- [PHO_DORFQP](../src/phojet/PHO_DORFQP.f)
- [PHO_DORGF](../src/phojet/PHO_DORGF.f)
- [PHO_DORGFS](../src/phojet/PHO_DORGFS.f)
- [PHO_grsf1](../src/phojet/PHO_grsf1.f)
- [PHO_grsf2](../src/phojet/PHO_grsf2.f)
- [PHO_CKMTPA](../src/phojet/PHO_CKMTPA.f)
- [PHO_CKMTPD](../src/phojet/PHO_CKMTPD.f)
- [PHO_CKMTPO](../src/phojet/PHO_CKMTPO.f)
- [PHO_CKMTFV](../src/phojet/PHO_CKMTFV.f)
- [PHO_DBFINT](../src/phojet/PHO_DBFINT.f)
- [PHO_SASGAM](../src/phojet/PHO_SASGAM.f)
- [PHO_SASVMD](../src/phojet/PHO_SASVMD.f)
- [PHO_SASANO](../src/phojet/PHO_SASANO.f)
- [PHO_SASBEH](../src/phojet/PHO_SASBEH.f)
- [PHO_SASDIR](../src/phojet/PHO_SASDIR.f)
- [PHO_PHGAL](../src/phojet/PHO_PHGAL.f)
- [PHVAL](../src/phojet/PHVAL.f)