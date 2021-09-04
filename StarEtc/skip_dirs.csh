#
# source skip_dirs.csh
# An example how ro skip compilation of non desirable directories

setenv SKIP_DIRS " "
#_____________________________________________________________________________
#	Normal skip dirs


#_____________________________________________________________________________
setenv SKIP_DIRS "$SKIP_DIRS StTriggerUtilities   StSvtSelfMaker  StVeloMaker "
setenv SKIP_DIRS "$SKIP_DIRS StHbtMaker St_pp2pp_Maker "
setenv SKIP_DIRS "$SKIP_DIRS OnlTools "
setenv SKIP_DIRS "$SKIP_DIRS QtRoot"
setenv SKIP_DIRS "$SKIP_DIRS GeoTestMaker  Stv "
setenv SKIP_DIRS "$SKIP_DIRS StJetMaker"
setenv SKIP_DIRS "$SKIP_DIRS StBTofPool"
setenv SKIP_DIRS "$SKIP_DIRS StEEmcPool"
setenv SKIP_DIRS "$SKIP_DIRS StEmcPool"
setenv SKIP_DIRS "$SKIP_DIRS StructPool"
setenv SKIP_DIRS "$SKIP_DIRS StFgtPool"
setenv SKIP_DIRS "$SKIP_DIRS StHighptPool"
setenv SKIP_DIRS "$SKIP_DIRS StRichPool"
setenv SKIP_DIRS "$SKIP_DIRS StSpectraPool"
setenv SKIP_DIRS "$SKIP_DIRS StSpinPool"
setenv SKIP_DIRS "$SKIP_DIRS StSvtPool"
setenv SKIP_DIRS "$SKIP_DIRS StTofPool"
setenv SKIP_DIRS "$SKIP_DIRS StTpcPool"

setenv SKIP_DIRS "$SKIP_DIRS StShadowMaker"
setenv SKIP_DIRS "$SKIP_DIRS StTrsMaker"


#_____________________________________________________________________________
#		Skip Temp
setenv SKIP_DIRS "$SKIP_DIRS sim "   #It is pams/sim
setenv SKIP_DIRS "$SKIP_DIRS StFwdTrackMaker "
setenv SKIP_DIRS "$SKIP_DIRS StiCA"   ##Not compiling in Any ROOT
setenv SKIP_DIRS "$SKIP_DIRS TPCCATracker"   ##Not compiling in Any ROOT
#_____________________________________________________________________________
#		End Temp skip
echo $SKIP_DIRS

#
