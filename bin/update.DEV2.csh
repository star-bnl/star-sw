#! /usr/local/bin/tcsh -f
source ${GROUP_DIR}/.starver .DEV2
cd $STAR
cvs update -dP | tee update.`date +%m%d%y`
#foreach d ($ROOT/5.99.99.local/root $ROOT/6.99.99/root/ $STAR/geant3 $STAR/geant4_vmc $STAR/garfieldcpp $STAR/KFParticle)
#foreach d ($ROOT/5.99.99/root $ROOT/6.99.99/root/ $STAR/geant3 $STAR/geant4_vmc $STAR/vmc  $STAR/garfieldcpp $STAR/KFParticle)
#foreach d ($ROOT/5.99.99/root $STAR/geant3 $STAR/geant4_vmc $STAR/garfieldcpp $STAR/KFParticle)
foreach d ($STAR/garfieldcpp $STAR/KFParticle)
  cd $d; pwd
  git pull
end
cd $STAR
