Hi Richard, the examples you can find at 
2. [rcas6008] ~/xml $ dir ~fisyak/xml/*VF*
-rw-r--r-- 1 fisyak rhstar 3.0K Sep  2  2021 /star/u/fisyak/xml/2010VFMCE.xml
-rw-r--r-- 1 fisyak rhstar 3.0K Sep  2  2021 /star/u/fisyak/xml/2011VFMCE.xml
-rw-r--r-- 1 fisyak rhstar 2.9K Sep  2  2021 /star/u/fisyak/xml/2014VFMCE.xml
-rw-r--r-- 1 fisyak rhstar 2.5K Sep  2  2021 /star/u/fisyak/xml/2016VFMCE.xml
-rw-r--r-- 1 fisyak rhstar 2.9K Sep  2  2021 /star/u/fisyak/xml/2018VFMCE_P19ei.xml
-rw-r--r-- 1 fisyak rhstar 5.4K Feb 24 17:30 /star/u/fisyak/xml/2018VFMCE.xml
-rw-r--r-- 1 fisyak rhstar 3.5K Mar 10 09:23 /star/u/fisyak/xml/2019VFMCE.xml
-rw-r--r-- 1 fisyak rhstar 3.5K Sep  2  2021 /star/u/fisyak/xml/2020VFMCE.xml
-rw-r--r-- 1 fisyak rhstar 3.5K Sep  2  2021 /star/u/fisyak/xml/2021VFMCE.xml

These are xml files used for simulation. There are a few tricks there.
Take, for example, 2019VFMCE.xml
1.	By default it use version TFG .DEV2.  I would advise you to use the latest stable version : starver TFG22b 
2.	There is a choice of Generator type defined by submit directory name :   (set Name = `basename ${SUBMITTINGDIRECTORY}`) to get it from the 
list of possible generators:
~ 
  foreach name (K_S0Fixed  Lambda0_barFixed Lambda0Fixed XiM_barFixed XiMFixed  Omega_barFixed OmegaFixed \
                AntiLNNFixed AntiH4LFixed AntiLNFixed \
                AntiH3LFixed LNNFixed H4LFixed LNFixed H3LFixed \
                H3LdpFixedFlat  H4LtpFixedFlat  H5LFixedFlat  He4LFixedFlat  He5L3FixedFlat  He5LFixedFlat \
                H3LdpFixed  H4LtpFixed  H5LFixed  He4LFixed  He5L3Fixed  He5LFixed \
                K_S0       Lambda0_bar      Lambda0      XiM_bar      XiM       Omega_bar      Omega     \
                AntiLNN      AntiH4L      AntiLN      AntiH3L      LNN \
                H4L      LN      H3L \
                Upsilon1SmTsq Upsilon2SmTsq) 
    echo $Name | grep $name
    if (! $?) break;
#    if ($name == $Name) break;
~
3.	The Name defines a root macros from  $STAR/StarDb/Generators/*.C 
4.	I assume that the directory structure is like  ~/work/reco/2019/Efficienes/AuAu3p85GeV_fixedTarget/He5LFixed 
1/StarDb/Generators/PVxyz2018AuAu3p85GeV_fixedTarget.root
5.	There is a  PVxyz.root  file with distribution of primary vertices and their errors taken for give
if (! -r PVxyz.root ) ln -s ${SUBMITTINGDIRECTORY}/../xyz.root PVxyz.root
The xyz.root file is generated from kfpAnalysis.C root.file with macro ~fisyak/macros/PVxyz.C

Examples of these files can be found at $STAR/StarDb/Generators/PVxyz*.root
6.	The list of jobs is generated with ~fisyak/bin/RunId.pl
7.	The output will be MuDst.root files with both generated and reconstructed track. You can use these files to run kfpAnalysis.C.
I believe that for MuDst option the macro set flags for efficiency calculation. It is needed to run an example and see results.
8.	We have also a possibility to run embedding with VMC but we did not exercise it for long time (since Run 16). 
