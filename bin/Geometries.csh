 egrep ' on [A-Z]' $STAR/pams/geometry/geometry/geometry.g | egrep -v '_(ON|OFF)' | grep -v '; }' | wc -l
