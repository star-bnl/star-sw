#
rm -r StarVMC/StarGeometry/*.cxx  StarVMC/StarGeometry/*.h
rm -r StarVMC/xgeometry/*.age

#Create compilable code and compile

cons +StarVMC/Geometry
cons +StarGeometry 
cons +xgeometry 
echo '******* AGML Update ENDED *******'
exit 0
