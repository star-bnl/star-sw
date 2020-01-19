
# root.exe -q -b 'CheckDistortion.C("Corr4",20130215,202757)' >& Corr4FF.log
# root.exe -q -b 'CheckDistortion.C("Corr3",20130215,202757)' >& Corr3FF.log
# root.exe -q -b 'CheckDistortion.C("OBmap2D",20130215,202757)' >& OBmap2DFF.log
# root.exe -q -b 'CheckDistortion.C("OTwist",20130215,202757)' >& OTwistFF.log
# root.exe -q -b 'CheckDistortion.C("OClock",20130215,202757)' >& OClockFF.log
# root.exe -q -b 'CheckDistortion.C("Opr13",20130215,202757)' >& Opr13FF.log
# root.exe -q -b 'CheckDistortion.C("OIFC",20130215,202757)' >& OIFCFF.log
# root.exe -q -b 'CheckDistortion.C("OShortR",20130215,202757)' >& OShortRFF.log
# root.exe -q -b 'CheckDistortion.C("OBmap",20130215,202757)' >& OBmapRFF.log
# root.exe -q -b 'CheckDistortion.C("OSectorAlign",20130215,202757)' >& OSectorAlignRFF.log

# root.exe -q -b 'CheckDistortion.C("Corr4",20130329,105728)' >& Corr4RFF.log
# root.exe -q -b 'CheckDistortion.C("Corr3",20130329,105728)' >& Corr3RFF.log
# root.exe -q -b 'CheckDistortion.C("OBmap2D",20130329,105728)' >& OBmap2DRFF.log
# root.exe -q -b 'CheckDistortion.C("OTwist",20130329,105728)' >& OTwistRFF.log
# root.exe -q -b 'CheckDistortion.C("OClock",20130329,105728)' >& OClockRFF.log
# root.exe -q -b 'CheckDistortion.C("Opr13",20130329,105728)' >& Opr13RFF.log
# root.exe -q -b 'CheckDistortion.C("OIFC",20130329,105728)' >& OIFCRFF.log
# root.exe -q -b 'CheckDistortion.C("OShortR",20130329,105728)' >& OShortRRFF.log
# root.exe -q -b 'CheckDistortion.C("OBmap",20130329,105728)' >& OBmapRFF.log
# root.exe -q -b 'CheckDistortion.C("OSectorAlign",20130329,105728)' >& OSectorAlignRFF.log

#root.exe -q -b 'CheckDistortion.C("CorrY,OSpaceZ2,OGridLeak3D",20190311,0)' >& CorrY,OSpaceZ2,OGridLeak3DRFF.log 
#root.exe -q -b 'CheckDistortion.C("CorrY,OSpaceZ2,OGridLeakFull",20190311,0)' >& CorrY,OSpaceZ2,OGridLeakFullRFF.log
#2019 TFG19e "genIn,MC.2019,McTpcAna,-bbcSim,Stx,KFVertex,VFMinuitX,-hitfilt,-geantOut,evout,vmc,VMCAlignment,CorrY,OSpaceZ2,OGridLeakFull,-useXgeom,NoHistos,noTags,noRunco,sdt20190311,RunG.1"
# set tag = "noinput,MC.2019,McTpcAna,-bbcSim,Stx,KFVertex,VFMinuitX,-hitfilt,-geantOut,evout,vmc,VMCAlignment,CorrY,OSpaceZ2,OGridLeakFull,-useXgeom,NoHistos,noTags,noRunco,sdt20190311,RunG.1"
# root.exe -q -b 'CheckDistortion.C("NewTpcAlignment,'${tag}'",20191218,114628)' >& ${tag}.log &
foreach tag (CorrY,OSpaceZ2,OGridLeakFull  OBmap OPr40 OIFC OSpaceZ2 OShortR OGridLeakFull OSectorAlign)
  root.exe -q -b 'CheckDistortion.C("NewTpcAlignment,'${tag}',sdt20190311")' >& ${tag}.log &
end
# 2020 CorrY,OSpaceZ2,OGridLeakFull => NewTpcAlignment : OBmap OPr40 OIFC OSpaceZ2 OShortR OGridLeakFull OSectorAlign : 20191218.114628 
#foreach tag (CorrY,OSpaceZ2,OGridLeakFull  OBmap OPr40 OIFC OSpaceZ2 OShortR OGridLeakFull OSectorAlign)
#  root.exe -q -b 'CheckDistortion.C("NewTpcAlignment,'${tag}'",20191218,114628)' >& ${tag}.log &
#end
#set tag = "noinput,P2020a,StiCA,-evout,NoHistos,noTags,noRunco,PicoVtxVpdOrDefault"
#root.exe -q -b 'CheckDistortion.C("NewTpcAlignment,'${tag}'",20191218,114628)' >& ${tag}.log &
