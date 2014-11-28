*
* $Id: headpss.inc,v 1.1 2000/04/28 21:54:21 longacre Exp $
*
* $Log: headpss.inc,v $
* Revision 1.1  2000/04/28 21:54:21  longacre
* add mevsim code
*
* Revision 1.4  1998/04/22 13:24:53  longacre
* correcting headpss.inc
*
* Revision 1.3  1998/04/03 18:58:46  longacre
* change order of ifirst in headpss.inc
*
* Revision 1.2  1998/04/02 21:37:51  longacre
* change ran to ranf
*
* PSSHEP(1) = B impact parameter
* PSSHEP(2) = PHI angle of impact
* PSSHEP(3) = GENERATOR NUMBER
*  1 FRITIOF : MINBIAS
*  1 FRITIOF : CENTRAL
* 10 HBT     : MINBIAS
* 11 HBT     : CENTRAL
* 20 HIJET   : RHICEVT : MINBIAS 
* 21 HIJET   : RHICEVT : CENTRAL
* 22 HIJET   : PLASMA
* 23 HIJET   : LANDAU
* 24 HIJET   : SMOKE
* 25 HIJET   : VOLCANO
* 26 HIJET   : CHIRAL
* 27 HIJET   : PTSIM
* 28 HIJET   : STRANGSIM
* 29 HIJET   : HIFLOW
* 30 HIJING  : REGULAR : MINBIAS 
* 31 HIJING  : REGULAR : CENTRAL 
* 32 HIJING  : JET     : MINBIAS  
* 33 HIJING  : JET     : CENTRAL 
* 40 PYTHIA  : MINBIAS
* 41 PYTHIA  : JET
* 42 PYTHIA  : PHOTON
* 43 PYTHIA  : W
* 50 STARLIGHT
* 60 VENUS   : MINBIAS 
* 61 VENUS   : CENTRAL  
* 62 VENUS   : CENTRAL : MEV
* 70 VNI     : NOAFTER : MINBIAS 
* 71 VNI     : AFTER   : MINBIAS 
* 72 VNI     : NOAFTER : CENTRAL 
* 73 VNI     : AFTER   : CENTRAL 
* 74 VNI     : NOAFTER : JET
* 75 VNI     : AFTER   : JET
* 76 VNI     : NOAFTER : PHOTON
* 77 VNI     : AFTER   : PHOTON
* 78 VNI     : NOAFTER : CHIRAL
* 79 VNI     : AFTER   : CHIRAL
* 80 VNI     : CENTRAL : MEV
* PSSHEP(4) = ENERGY OF N N CMS
* PSSHEP(5) = Awest.Aeast  p p  =  1.1 or Au Au = 197.197
* VSSHEP(1) = run number
* VSSHEP(2) = event number
* VSSHEP(3) = DATE
* VSSHEP(4) = TIME
*
* headpss.inc
*
      COMMON/HEADPSS/PSSHEP(5),VSSHEP(4),IFIRST
      DOUBLE PRECISION PSSHEP,VSSHEP
      INTEGER IFIRST
      SAVE /HEADPSS/
