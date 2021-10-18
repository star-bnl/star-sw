#! /usr//bin/env tcsh
  foreach d (2019 2020)
     set b = `basename ${d} .root`;
     root.exe -q -b TPoints2*UGP${d}.root  MakeTpcLengthCorrectionMD2.C >& ${b}.log
  end 
#  ln -s TpcLengthCorrectionMDF.14p5GeV.C TpcLengthCorrectionMDF.20190404.000026.C
#  ln -s TpcLengthCorrectionMDF.19GeV.C   TpcLengthCorrectionMDF.20190225.230026.C 
