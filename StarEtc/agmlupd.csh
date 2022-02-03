#
rm -r StarVMC/StarGeometry/*.cxx  StarVMC/StarGeometry/*.h
set x = (StarVMC/xgeometry/*.age)
foreach f ($x)
  if ($f == "xgeometry.age") next
  rm -f $f
end
#Create compilable code and compile

cons +StarVMC/Geometry
cons +StarGeometry 
cons +xgeometry 
echo '******* AGML Update ENDED *******'
exit 0
