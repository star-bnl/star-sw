c------------------------------------------------------------------------------c
c	samples(1,n),samples(2,n) = x and z coordinate of sample	       c
c	samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c	samplesx(2,n) earliest t bucket hit for anode hit n		       c
c	samplesz(2,n) earliest anode hit for t bucket hit n		       c
c	samplesx(3,n) latest t bucket hit for anode hit n		       c
c	samplesz(3,n) latest anode hit for t bucket hit n		       c
c------------------------------------------------------------------------------c
	integer samples(2,200),nmeas
	integer nmeasx,samplesx(3,200)
	integer nmeasz,samplesz(3,200)
	real true_values(200),true_valuesx(200),true_valuesz(200)
	common /sigdat/ nmeas,nmeasx,nmeasz,samples,samplesx,
     1   samplesz,true_values,true_valuesx,true_valuesz
