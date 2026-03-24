*
* Steering for agusumlif package
*
>Name userlifetime
>MENU userlife

>Guidance
Configuration of the user lifetime package, allowing users
to specify a cylindrical region for particle decays

>Command cylinder
>Guidance
Specifies the dimensions of the cylindrical region
>Parameters
rmin 'Inner radius [cm]' R D=50.0
rmax 'Outer radius [cm]' R D=200.0
dz   'Half z [cm]'       R D=200.0
>action agusl_set

>Command addpart
>Guidance 
Add a particle (G3 id) to the list of particles which will be
decayed within the specified cylinder.  Max 10.
>Parameters
ipart 'G3 id' I D=11
>action agusl_part