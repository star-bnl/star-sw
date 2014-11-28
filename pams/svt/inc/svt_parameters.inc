c------------------------------------------------------------------------------c
c	SVT OPERATING PARAMETERS FOR STAR 2                                    c
c------------------------------------------------------------------------------c

c	anode pitch, equavalent noise charge, drift velocity, electrons per mic:
	real pitch,enc,vd,dqdx
c	maximum drift distance, wafer thickness, diffusion coefficient:
	real dmax,thickness,d 
c	coul_corr_fact=a+b*t for MIP, shaper width, time bucket:
	real a,b,shaper,deltat
c	anode response due to geometry (in microns)
	real anode_geom 
c	equavalent space bucket, electronics width:
	real bucket,electronics
c       charge/energy relation (#/GeV)
        real dqde
c	Data values:
	data pitch,enc,vd,dqdx,dmax,d /0.0250,400,5.45e5,6.667e5,4.0,30.0/
	data thickness,a /.0300,1.245e-4/
	data b,shaper,deltat /-2.85,35.0e-9,25.0e-9/
	data anode_geom /0.0/
        data dqde /2.78e8/
c	calculated values
	bucket=vd*deltat
	electronics=shaper*vd
