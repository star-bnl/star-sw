"""
============================================================================================

                         Another Geometry Modeling Language: AgML
                                                                                      r 0.90
============================================================================================

The STAR Collaboration has utilized the AgSTAR language [1] to describe its
geometry model since its inception.  AgSTAR has served us well over the
years, providing an easy to maintain interface between the Geant3 simulation
package [2].  Its key strength is that it provides a level of abstraction from
the Geant 3 geometry model, whereby the hierarchical structure of the
geoemtry tree is naturally reflected within the syntax.

AgML is an evolution of ths AgSTAR language.  It provides the capability to
create both ROOT/TGeo and AgSTAR models of the detector geometry, enabling
us to provide a single source of geometry information for both simulation
and reconstruction.

--------------------------------------------------------------------------------------------
0. Starting Points
--------------------------------------------------------------------------------------------


    For the casual user
    -------------------
 
    AgML is currently available in the eval library.
 
    $ starver eval
 
    The AgML geometries are created in starsim using the xgeometry.so library.
   
    starsim> detp geom y2012
    starsim> gexec $STAR_LIB/xgeometry.so
    starsim> gclos all
 
    When reconstructing events using the big full chain, you should add the "agml"
    option to your chain:
 
    root [0] .L bfc.C
    root [1] bfc(0,"y2012 agml ...",...);
 
 
    For the geometry developer
    --------------------------
 
    Check out the code
 
    $ cvs co StarVMC/Geometry
    $ cons                                 # compile once to create c++ and mortran files
    $ cons                                 # compile a second time to compile the libraries
                                           # this will take a while.  go have coffee.
 
    The BbcmGeo.xml file in StarVMC/Geometry/BbcmGeo/ has been annotated, and may be
    a useful module to study along with this user's guide.
 
    You can now edit the geometry file you want to work with.  When you're done making
    changes, you should
    $ cons +StarVMC/Geometry
    $ cons +StarVMC/StarGeometry
    $ cons +StarVMC/xgeometry
 
    You can visualize your changes using ROOT's OpenGL tools and event viewer.  An
    example macro is provided
 
    root [0] .L StarVMC/Geometry/macros/viewStarGeometry.C
    root [1] viewStarGeometry("tag");       // where tag is a valid geometry tag.


   Getting a list of materials
   ---------------------------
   root.exe[2] AgMaterial::List()

--------------------------------------------------------------------------------------------
1. Geometry Modules
--------------------------------------------------------------------------------------------

   Detectors in AgML are implemented in geometry modules.  Current and
   historical detector modules are stored in StarGeometry/Modules.  Each
   detector has its own subdirectry, with multiple versions of each detector
   stored in XML files called geometry modules.  Geometry modules have the
   following form:

   <!-- Module declaration and documentation -->
   <Module name="NAMEgeo" comment="DOCUMENTATION STRING">
      <Author  name="Jason C. Webb" />
      <Created date="Today"         />

      <!-- Declaration of content -->
      <Content> ... </Content>

      <!-- User defined variables and structures -->
      <Struct ... > ... </Struct>
      <varlist ...> ... </varlist>

      <!-- Filling the user structures -->
      <Fill ...> ... </Fill>

      STEERING CODE TO CREATE AND PLACE THE TOP-LEVEL VOLUME(s) OF THE MODULE
      <Create ... />
      <Placement ... />

      <!-- Declaration of Volumes -->
      <Volume ... >
      </Volume>

      <Volume ... >
      </Volume>      

      ...
   
   </Module>

   As with all things XML, the geometry descriptions are enclosed within XML tags.
   The <Module> tag begins the module, and has a corresponding end tag </Module>
   which is the last tag in the module.  Some tags, such as the Author and
   Created tags, take only a single attribute and are self closing.  In AgML, these
   are refered to as generically as operators.  Other tags, such as <Content>, <Fill>,
   <Volume>, etc... are refered to as containers.  A few tags, such as <Placement>,
   may be used as either operators or containers.

   1.1 Module Naming Conventions

       Modules should respect the following naming convention:  a four letter
       name (preferably a detector acronym) followed by the word "geo" and an
       optional revision.  Examples of legal Module names are

       tpcegeo3a
       EcalGeo6
       VpDDgeo

       etc...

       AgML is case insensitive.  That being said, it is recommended that module
       names should be CamelCased.  i.e. EcalGeo is preferred over ecalgeo.

   1.2 Struct Naming Conventions

       Structures are described below.  In order to support AgSTAR-backwards
       compatability, the names of structs should be limited to four characters.

       Members of structures should be accessed using the '.' operator.
       In other words, the structure

       <Structure name="STRX" comment="Structure X">
          <var name="x" type="float"/>
          <var name="y" type="float"/>
       </Structure>

       has two members, x and y.  They may be accessed in code blocks using
       the "_" operator --

       x = strx_x;
       y = strx_y;

   1.3 Volume Naming Conventions

       Like structures, volumes need to be backwards compatible with AgSTAR, and
       that means that volume names need to obey two conventions:

       1) The name of the volume should begin with the same letter as the name of
          the module.  i.e.  ECAL, ESMD, EMOD, etc... in ecalgeo6.

       2) The name of the volume is limited to four characters.

       Finally, every volume which is implemented in the geometry module must be
       declared within the <Content> block.

--------------------------------------------------------------------------------------------
2. Language Elements
--------------------------------------------------------------------------------------------

   AgML is a xml-based language for the description of detector geometries.  It utilizes
   XML tags to define the data structures (variables, structures, material, shapes,
   etc...) used in the creation of the geometry.  It also uses XML tags to define looping
   and branching statements (i.e. For-loops and If-statements).  Arithmetic and logical
   expressions are implented as a subset of the c-programming language, which we
   refere to as LessC.


   Printing Information
   --------------------
   
   The Info tag is the only support I/O mechanism in AgML.  The syntax is

   <Info format="... {A.Bx} ..."> var1, val1, ... </Info>.

   The format attribute is a string which contains the text to be output and
   one format descriptor for every variable and/or value to be printed.  The
   format descriptors take the form

   {A.Bx}

   Where A is the width of the field, B is an (optional) number of places past
   the decimal point and x is one of f, d or i for floats, doubles and integers.
   Each format descriptor is wrapped in brackets.
   

   Variables and Structures:
   -------------------------

   There are three types of variables in AgML:  ints, floats and chars.  Ints and
   floats have the same representation as ints and floats in C/C++.  Chars are
   strings consisting of four characters (*).  Variables may be declared as
   arrays of either one or two dimensions (**).

   Declaration of variables is done using the varlist tag:

   <varlist type="int"> i, j, k </varlist>

   Variables in a varlist may be initialized by placing the intial value between
   two slashes in the varlist

   <varlist type="float">PI/3.14159265359/, e/2.7182818285/, a</varlist>

   Variables may be referenced in either code blocks or XML tags, e.g.

   <If expr"i.eq.5">
       a = 2.0 * cos(35.0*PI/180.0)
   </If>


   AgML provides support for structures.  There are several advantatages which
   structures have over simple variables, and detector developers are strongly
   encouraged to utilize structures for storing detector data, and using variables
   only for loop indexes, and intermediate calculations.  Structures in AgML resemble
   c-structures, in that they allow one to declare a grouping of variables and arrays:

   <Structure name="XYZT">
      <var name="version" type="float" />
      <var name="x" type="float" />
      <var name="y" type="float" />
      <var name="z" type="float" />
      <var name="flag1(3)" type="int" />
      <var name="flag2(3,2)" type="int" />
   </Structure>

   This is where the similarity ends, however.  The above declaration is not a typedef...
   it creates an instance of a structure called XYZT.  Values are assigned to the
   members of this structure in Fill blocks:

   <Fill struct="XYZT" comment="Structure containing XYZ information">
      <var name="version" value="1.0"     comment="XYZT version" />
      <var name="x"       value="0.0"     comment="X-position" />
      <var name="y"       value="0.0"     comment="Y-position" />
      <var name="z"       value="10.0"    comment="Z-position" />
      <var name="flag1"   value="{1,1,2}" comment="A 1D array" />
      <var name="flag2"   value="{1,1,2;
                                  2,2,3}" comment="A 2D array with a ; separating dimensions" />
   </Fill>

   Multiple fill blocks may be specified for the same AgML structure:

   <Fill struct="XYZT" comment="Structure containing XYZ information, version 2">
      <var name="version" value="2.0"     comment="XYZT version" />
      <var name="x"       value="0.13"    comment="X-position" />
      <var name="y"       value="0.18"    comment="Y-position" />
      <var name="z"       value="9.95"    comment="Z-position" />
      <var name="flag1"   value="{1,1,2}" comment="A 1D array" />
      <var name="flag2"   value="{1,1,2;
                                  2,2,3}" comment="A 2D array with a ; separating dimensions" />
   </Fill>

   This is particularly useful for the development of a geometry.  As more precise
   values for the dimensions, material and position of a detector volume become available,
   different versions of that volume may be provided in a single geometry module
   with a simple mechanism for switching between.

   Members of the structures are accessed using the "_" operator, and may appear
   in either XML tags or code declarations:

   <Use struct="XYZT" select="version" value="2.0" />
   <If expr="XYZT_flag1[0]==1">

       <Position block="VOLA" x="XYZT_x" y="XYZT_y" z="XYZT_z" />
       ztotal += XYZT_z
   
   </If>
   
   The Use operator provides a mechanism to select between different versions
   of a given structure.  One specifies which variable is being used to select
   the instance of the structure and the value which that variable has.  The
   structure will be filled with the values specified in the fill block which
   contains the variable with the specified value.  For example, for the XYZT
   structure declared above

   <Use struct="XYZT" select="x" value="0.13" />

   would result in XYZT being filled with the values in version 2 of the fill
   statement,

   <Use struct="XYZT" select="y" value="0.0" />

   would result in XYZT being filled with the values in version 1 of the fill
   statement, and

   <Use struct="XYZT" select="z" value="0.0" />

   would result in an exception being raised, since there are no versions which
   have z=0.

   ------   

(*) The four-character limit on the char variable is imposed to ensure backwards
    compatibility with AgSTAR.

(**) This is another limitation imposed for backwards compatabilit

   CASE SenSitivity: 
   -----------------

   AgML is both case sensitive and case insensitive.  AgML tags like <Volume>,
   <Material>, <For>, etc... are case sensitive.  Variables, structures and
   arithmetic expressions are case insensitive.

   We strongly encourage the following conventions for readabiltiy of the code

   (1) Capitalize all volume names
   (2) Capitalize all structure names
   (3) Lowercase all variable names
   (4) Avoid the use of the "_" in variable names, to distinguish from structure members
   (5) Keep variable names short but meaningful
   
   Looping:
   --------

   For loops are implemented using XML syntax.  

   <For var="variable-name" from="lower-bound" to="upper-bound" step="step-size">
   ...
   </For>

   where variable-name references a variable declared in the module, lower-bound,
   upper-bound and step-size are simple arithmetic expressions involving numbers,
   variables and/or structure members.

   Branching:
   ----------

   The If statement takes the form:

   <If expr="logical-expression-0">
       <Then> ... </Then>
       <Elif expr="logical-expression-1"> ... </Elif>
       <Elif expr="logical-expression-2"> ... </Elif>
       ...
       <Else> ... </Else>
   </If>

   Logical expressions borrow the logical operators from fortran:
      .and.     -- logical and operator
      .or.      -- logcial or  operator
      .not.     -- logical negation operator
      .ne.      -- not-equal-to
      .eq.      -- equal-to
      .gt.      -- greater-than
      .lt.      -- less-than
      .leq.     -- less-than-or-equal-to
      .geq.     -- greater-than-or-equal-to
      .ge.      -- greater-than-or-equal-to
      .le.      -- less-than-or-equal-to

   Or one can use the XML equivalents: &gt; &lt; etc...

   The <Then> block is optional, but makes the code more readable.

   Arithmetic Expressions:
   -----------------------

   Simple arithmetic expressions involving variables and arrays are supported.

   e.g.
   
      y = a * x + b
      array[5] = (a*a + 2.0*b/3)*array[7]
      x = 3

   They must be complete on a single line.  A trailing ';' is optional, and can be used
   to separate multiple statements on a single line, e.g.

      x=1; y=2; z=x+y;
   
   Logical Expressions:
   --------------------

   The logical operators in AgML are borrowed from Fortran.

      .and.     -- logical and operator
      .or.      -- logcial or  operator
      .not.     -- logical negation operator
      .ne.      -- not-equal-to
      .eq.      -- equal-to
      .gt.      -- greater-than
      .lt.      -- less-than
      .leq.     -- less-than-or-equal-to
      .geq.     -- greater-than-or-equal-to
      .ge.      -- greater-than-or-equal-to
      .le.      -- less-than-or-equal-to

   There is no intrinsic boolean type in AgML, but integers can be used to
   store the results of a boolean expression.  e.g.

   <varlist type="int"> flag </varlist>

   flag = (x .eq. 7) .or. (y .ne. 5);

   String Assignment:
   ------------------
   
   Strings are either initialized in Fill statements or in code, e.g.

   <varlist type="char"> name </varlist>

   name = "ABCD";

   Note that the "char" data type is very limited... only four characters are
   permitted.  This limitation is imposed for backwards compatibility with
   AgSTAR and will be lifted in future releases.

   Array Assignment:
   -----------------

   An array assignment operator has been defined to simplifiy the assignment of
   data to arrays.  The format is

   <Assign var="ARRAY_NAME" value="{ comma-separated-list-of-values }"

   where ARRAY_NAME is the name of the array, and the value attribute takes a
   comma-separated list of values enclosed in braces {}.  for two dimensional
   arrays, the rows in the array are separated by a semi-colon (;).

--------------------------------------------------------------------------------------------
3. Volumes
--------------------------------------------------------------------------------------------

   3.1 Volume Declaration

   Volumes describe the shape, material, physical interactions with particles,
   visualization attributes and instrumentation characteristics of the various
   active and passive elements of a detector geometry.  Volumes are declared at
   the end of the module.  No code can be interleaved between volume declarations
   or after volumes.

   The generaral form of a volume declaration looks like:

   <Volume name="volume-name" comment="Required Documentation String">

      <Material name="material-name" optional-parameters />

      or

      <Mixture name="mixture-name" density="density in g/cm3">
         <Component name="string" a="atomic-mass" z="atomic number" w="fraction or natoms">
         ...
         <Component name="string" a="atomic-mass" z="atomic number" w="fraction or natoms">
      </Mixture

      <Medium name="medium-name" optional-parameters />

      <Attribute for="volume-name" optioanl-parameters />

      <Shape type="shape-type" shape-parameters />

      STEERING CODE TO CREATE AND POSITION DAUGHTER VOLUMES
   
   </Volume>

   Optional declarations:
   ----------------------

      Material  Mixture  Medium  Attribute

      The Material/Mixture, Medium and Attribute operators are optional in AgML.
      When they are left unspecified, the properties will be inhertited from the
      volume which created the volume.  This will usually (but not alwasys) be
      the mother volume.

      New materials can be created in two ways.  One way is to declare a Mixture.
      The Mixture tag will create a new material from the specified components,
      with the density specified in the attribute list.  Once a mixture has been
      created, it can be referenced by name in a Material operator.

      The other way is to specify all of the properties of the material using
      the material operator.  This will be described in detail below.  But care
      should be exercised at this point, as an incomplete declaration of a
      material coupled with the inheritance rules can result in unintended behavior.

   Required declaration:
   ---------------------

      Shape

      The shape declaration is required and must appear after the material,
      medium and attribute operators if they are present.  The type of the
      shape must be specified, but the shape parameters may be left unset.
      If parameters are unset, then the volume will obtain them in order
      of the following precedence

         (1) Use any parameters specified in the <Create ...> operator
         (2) Inherit from the volume which created it



   Interleaving code and properties:
   ---------------------------------

      Steering code may be interleaved with the volume property declarations.
      For example:

      <For var="i" from="1" to="10">

         <Create    block="VolA" />
         <Placement block="VolA" z="2.0*i" />

      </For>

      ...

      <Volume name="VolA" comment="A block with variable shape and material">

         <If expr="i .le. 5">
         <Then>
                 <Material name="Iron" />
                 <Shape type="Box" dx="10" dy="10" dz="100" />
         </Then>
         <Else>
                 <Material name="Air" />
                 <Shape type="Tube" rmin="0" rmax="10" dz="100" />
         </Else>
         </If>
      
      </Volume>


      The Material, Medium, Attribute and Shape operators support a conditional
      attribute.  The above code can be reduced to

      <Material name="Iron"      if="i.le.5" />
      <Material name="Air"       if="i.gt.5" />
      <Shape    type="Box"       if="i.le.5"     dx="10" dy="10" dz="100" />
      <Shape    type="Tube"      if="i.gt.5"     rmin="0" rmax="10" dz="100" />


   3.2 Volume Creation

   The Volume block is essentially a constructor for the volume.  In order to instantiate
   a new volume one uses the Create operator:

   <Create block="volume-name" optional-shape-arguements />

   where volume-name is the name of the volume declared above.  Shape arguements may
   be provided if the corresponding arguement in the volume's shape declaration is
   unset.  If you provide shape arguements for the creation of one instance of a
   volume, we recommend providing shape arguements for the creation of all instances.

   When the Create operator is called for a given volume, the AgML library will first
   search for a concrete instance of a volume with the same (generic) name and shape
   as specified in the Volume block.  If it finds one, the volume constructor will
   return after the <Shape> operator and the existing concrete volume will be used.
   If it does not find a matching volume, a new volume will be created and the code
   following the shape operator executed to place daughter volumes.


   3.3 Volume Placement

   Volumes are positioned using the placement operator.  The operator takes several forms.
   For pure translations of the volume one can use

   <Placement block="volume-name" in="mother-name" translation-parameters shape-parameters />

   where volume-name is the name of the volume being positioned and mother-name is the
   name of the mother volume into which the volume is being positioned.  If ommitted,
   the mother volume will be the volume which is placing the volume.

   translation parameters are optional and take the form x="x-position", etc...
   where the arguement is in units of cm.

   Shape parameters may be specified only for volumes with parameterized shapes.

   When rotations are involved, the order matters.  In that case, one should use
   the container form of the placement operator. 

   <Placement block="volume-name" in="mother-volume" translation-parameters optional-shape-parameters>
      
      <Rotation  .../>
      <Rotation  .../>
      
   </Placement>

   Translations will be executed first.  Followed by the rotations, evaluated in the
   order in which they are specified.

   3.4 Sensitive Volumes

   Sensitive volumes are declared in two ways.  First, in either the medium or material
   declaration the isvol=1 flag is set.  Second, an instrumentation block is declared
   as follows...

   <Volume name="VOLA" ... >
      <Material ... />
      <Medium   ... />
      <Shape    ... />

      <Instrument block="VOLA" >
          <Hit meas="x"     bins="0.1" opts="S" />
          <Hit meas="y"     bins="0.1"          />
          <Hit meas="z"     bins="0.1"          />
          <Hit meas="cx"    nbits="10" />
          <Hit meas="cy"    nbits="10" />
          <Hit meas="cz"    nbits="10" />
          <Hit meas="step"  bins="0.1" />
          <Hit meas="slen"  bins="0.1" min="0" max="600" />
          <Hit meas="ptot"  nbits="16" min="0" max="100" />
          <Hit meas="tof"   nbits="18" min="0" max="1.0E-6" />
          <Hit meas="eloss" nbits="16" min="0" max="0.01" />
      </Instrument>
      
   </Volume>

   Inside each instrumentation block, one or more hits are declared.  The Hit
   declaration specifies what quantity is to be stored and with what resolution
   and / or binning.

   The "meas" attribute specifies which quantity is to be stored.  Possible
   values are

   x,  y,  z  -- the local coordinates w/in the detector volume
   xx, yy, zz -- the global coordinates relative to the top volume
   cx, cy, cz -- the cosines of the angles of the track

   step       -- the stepsize of the track in the current tracking step
   slen       -- the pathlength of the track through the volume in the current
                 tracking step
   ptot       -- the total momentum of the track
   tof        -- the time of flight of the track to the current point
   eloss      -- the energy lost by the track in the current tracking step
   birk       -- the energy lost corrected by Birk's formula (relevant for
                 scintillators)
   lgam       -- log(gamma)

   The "opts" flag specifies one of three options.  The definition is copied from
   the AGI manual:

   R - Rounding   -  local  (for this measurement only):
       calculated bin size is rounded to 2 decimal digits
   S - SingleStep -  global (for all measurements, bit 1 in IOPTION)
       Even if Geant breakes path in a sensetive volume into several
       steps, hit is regestered as if it was a single step from the
       entry to the exit point.
   C - Calorimery -  global (for all measurements, bit 4 in IOPTION)
       1 is saved in hits ITRACK instead of actual track number.
       This allows to sum up energies of all particles in the same
       event in the cumulative part. pn: coded 28.05.95

   The "bins" flag specifies the width of the binning used to store the hit.
   When specified, the min and max of the binning may be inferred from the type
   of hit declared.  These may also be specified using the "min" and "max"
   attributes.

   The "bits" flag specifies the number of bits in which to pack the result,
   with "min" and "max" specifying the range of the value to pack.  If
   nbits="0", then the value will be packed as a floating point value.
   

--------------------------------------------------------------------------------------------
4. Materials
--------------------------------------------------------------------------------------------

   Referencing materials:
   ----------------------

   <Material name="material-name" />

      This indicates that the volume will use the named material.  In actuality,
      a copy will be made and used.  This ensures that changes made to a material
      in one module will not affect materials in other modules.

   Copying materials:
   -----------------
   
   <Material name="material-name" />
   <Material name="new-material-name" isvol="0" />  <!-- or isvol="1" -->

      This creates a copy of the named material with the new name specified.  It
      is supported for backwards compatability only.  If an acceptable material
      exists (e.g. Air), please use the previous signature.
      
   Modifying materials:
   --------------------

   When none of the existing materials are suitable, it is possible to copy the
   material and modify it using the following signature.
   
   <Material name="material-name" />
   <Material name="new-material-name" new-material-parameters />

      where new-material-parameters lists the parametes which differ from the
      original material, and may be one or more of

      new-material-parameters one of:

      a="atomic mass"
      z="atomic number"
      dens="density in g/cm^3"
      radl="radiation length in cm"
      absl="hadronic absorption length in cm"
   

   Defining materials:
   -------------------

   <Material name="materia-name" material-parameters />

      when defining materials in this way you must make certain to specify at least
      the a, z and density.  And you are strongly encouraged to specify the radiation
      length and absorption length, otherwise the inheritance rules may result in
      unintended behavior.
      
      material-parameters all of:

      a="atomic mass"
      z="atomic number"
      dens="density in g/cm^3"

      material-parameters optional:
      
      radl="radiation length in cm"
      absl="hadronic absorption length in cm"

   Conditional Material
   --------------------

   In the event that you wish to define several related volumes with different
   materials, you may make the Material declaration subject to a conditional
   expression.  The material command then becomes

   <Material name="material-name" material-parameters if="expression" />

   where the expression is any logical expression involving variables
   declared in the module.  When defining a volume with a changing material,
   care must be taken to ensure that multiple versions of the volume will
   be created.  AgML only detects changes to shape for generating a new
   volume.  In cases where the shape remains constant, the serial option
   should be used to ensure new volumes are created...

   e.g. <Attribute for="VOLA" serial="unique-index" />


--------------------------------------------------------------------------------------------
5. Media
--------------------------------------------------------------------------------------------

   Referencing media:
   ------------------

   <Medium name="Standard" />

      Tuning of the Monte Carlo tracking medium parameters for a geometry is usually not
      needed.  You can't go too far wrong by using the "Standard" parameters defined for
      STAR.

   Declaring a sensitive volume:
   -----------------------------

   <Medium name="Standard" isvol="1" />

      The one routine exception is the sensitive volume flag.  Any volume which is you
      wish to be instrumented for readout (i.e. provide hits for Monte Carlo or
      reconstruction) should have isvol set to 1.

   Setting tracking parameters:
   ----------------------------

   <Medium name="Standard" medium-parameters />

      medium-parameters one or more of:

      isvol  = 0 or 1  
      ifield =      1  
      fieldm=      20  
      tmaxfd=      20
      stemax=      10
      deemax=       0
      epsil=     0.01
      stmin=        0

      One should consult the geant manual (routine gstmed) for meaning

   Conditional Medium
   ------------------

   In the event that you wish to define several related volumes with different
   mediums, you may make the Medium declaration subject to a conditional
   expression.  The medium command then becomes

   <Medium name="medium-name" medium-parameters if="expression" />

   where the expression is any logical expression involving variables
   declared in the module.  When defining a volume with a changing medium,
   care must be taken to ensure that multiple versions of the volume will
   be created.  AgML only detects changes to shape for generating a new
   volume.  In cases where the shape remains constant, the serial option
   should be used to ensure new volumes are created...

   e.g. <Attribute for="VOLA" serial="unique-index" />


--------------------------------------------------------------------------------------------
6. Shapes
--------------------------------------------------------------------------------------------

   AgML supports the 16 basic shapes in Geant 3, plus divisions of those shapes.  The
   available shapes are listed below along with the arguements which they accept. The
   general form of the shape declaration is

   <Shape type="type-specifier" shape-arguements />

   Specifying shape arguements:
   ----------------------------

   In the event that one or more of the shape arguements  is ommitted from the list,
   then the following behavior will result:
   
      1) A default value will be used (i.e. rmin=0 for a tube)
      2) The value specified in the shape of the mother volume will be
         inherited
      3) The value specified as an arguement to the Create operator
         will be used.

      with item 3 taking precedence over 2, taking precedence over 1.

   If shape arguements for a volume are passed using the Create operator, we highly
   recommend that this be done for all versions of that volume.  i.e. do not mix
   inheritance and Create arguements.


   Parameterized volumes:
   ----------------------

   In the event that all shape arguements are set to zero, this indicates that the
   shape parameters will be provided when the volume is positioned.

   Example:
   
   <Create block="ABCD" />

   <!-- Position block ABCD a +40cm and -60cm, with half-widths of 20 and 30cm -->
   length=20.0
   <Position block="ABCD" z="-2.0*length" dx="10" dy="10" dz="length" />

   length=30.0
   <Position block="ABCD" z="+2.0*length" dx="10" dy="10" dz="length" />
      
   <Volume block="ABCD" comment="A parameterized volume box" />
   <Shape type="box" dx="0" dy="0" dz="0" />
   </Volume>

   The type of the shape may not be parameterized.  However, one can make the
   shape subject to a conditional arguement using the form

   <Shape type="type-specifier" shape-parameters if="expression" />

   Where the expression is a logical expression involving any variables 
   in the module.

   ==================
   Shape Declarations
   ==================

   Arbitrary Trapezoid Declaration:
   --------------------------------
   <Shape type="arb8" arb8-arguements />

   arb8-arguements:
      dz -- half-length along z axis
      x0,y0 -- x,y position of 1st point at -dz plane
      x1,y1 -- x,y position of 2nd point at -dz plane
      x2,y2 -- x,y position of 3rd point at -dz plane
      x3,y3 -- x,y position of 4th point at -dz plane
      x4,y4 -- x,y position of 4th point at +dz plane
      x5,y5 -- x,y position of 5th point at +dz plane
      x6,y6 -- x,y position of 6th point at +dz plane
      x7,y7 -- x,y position of 7th point at +dz plane


   Box Declaration:
   ----------------
   <Shape type="box" box-arguements />

   box-arguements:
      dx -- half-length along x axis
      dy -- half-length along y axis
      dz -- half-length along z axis


   Cone and Cone Segment Declaration:
   ----------------------------------
   <Shape type="cone" cone-arguements />
   <Shape type="cons" cone-arguements phi-arguements />
   
   cone-arguements:
      dz -- half-length along z-axis
      rmn1 -- inner radius for -dz plane [default 0]
      rmx1 -- outer radius for -dz plane
      rmn2 -- inner radius for +dz plane [default 0]
      rmx2 -- outer radius for +dz plane

   phi-arguements:
      phi1 -- start-phi in degrees
      phi2 -- end-phi in degrees


   Eliptical Tube Declaration:
   ---------------------------   
   <Shape type="eltu" eltu-arguements />

   eltu-arguements:
      dz -- half-length in z
      p1 -- radius 1
      p2 -- radius 2
   

   Generalized Trapezoid Declaration:
   ----------------------------------
   gtra -- a generalized trapezoid

     dz    -- half-width along z
     thet  --
     phi   --
     twist --
     h1    --
     bl1   --
     tl1   --
     alp1  --
     h2    --
     bl2   --
     tl2   --
     alp2  --


   Hyperboloid Declaration:
   ------------------------
   <Shape type="hype" hype-arguements />

   hype-arguements:
      dz    -- half-width along Z
      rin   -- 
      stin  --
      rout  --
      stout --


   Parabaloid Declaration:
   -----------------------
   <Shape type="para" para-arguements />

   para-arguements:
      dx   -- half-width along x
      dy   -- half-width along y
      dz   -- half-width along z
      alph -- angle about Y of the Z bases
      thet -- inclination of para axis about Z
      phi  -- phi angle of para axis


   PolyCone and PolyGone Declaration:
   ----------------------------------
   <Shape type="pcon" pcon-arguements />
   <Shape type="pgon" number-of-phi-divisions pcon-arguements />

   pcon-arguements:
      nz   -- number of slices in z
      phi1 -- start phi in degrees [default 0]
      dphi -- delta phi in degrees [default 360]
      zi   -- array of z positions
      rmn  -- array of inner radii at each z
      rmx  -- array of outer radii at each z

   number-of-phi-divisions:
      npdiv -- number of edges for a polygone
      

   Sphere Declaration:
   -------------------
   <Shape type="sphe" sphere-arguements />

   sphere-arguements:

      rmax -- outer radius
      rmin -- inner radius [default 0]
      phi1 -- start phi    [default 0]
      phi2 -- end phi      [default 360]
      the1 -- start theta  [default 0]
      the2 -- end theta    [default 180]


   Torus Declaration:
   ------------------
   <Shape type="torus" torus-arguements />

   r -- axial radius
   rmin -- inner radius
   rmax -- outer radius

   phi1 -- phi start [default 0]
   phi2 -- phi end   [default 360]


   Trd1 Declaration:
   -----------------
   <Shape type="trd1" trd1-parameters />

   trd1-parameters:
      dx1 -- half-length in X at lower surface -dz
      dx2 -- half-length in X at upper surface +dz
      dy  -- half-length in Y at both surfaces
      dz  -- half-length in Z


   Trd2 Declaration:
   -----------------
   <Shape type="trd2" trd2-parameters />

   trd2-parameters:
      dx1 -- half-length in X at lower surface -dz
      dx2 -- half-length in X at upper surface +dz
      dy1 -- half-length in Y at lower surface -dz
      dy2 -- half-length in Y at upper surface +dz  
      dz  -- half-length in Z


   Tube and Tube Segment Declaration:
   ----------------------------------
   <Shape type="tube" tube-arguements />
   <Shape type="tubs" tube-arguements phi-arguements />

   tube-arguements:
      dz -- half-width in z
      rmin -- inner radius [default 0]
      rmax -- outer radius
   
   phi-arguuements:
      phi1 -- start-phi in degrees
      phi2 -- end-phi in degrees   

   Divisions:
   ----------
   division -- indicates that the specified volume is a division of the mother volume




--------------------------------------------------------------------------------------------
Obscurata
--------------------------------------------------------------------------------------------

There are a few places where we wish to inject code only into the Mortran
description of the detector, or the ROOT description.  For those cases an
Export tag is provided.

<Export language="Mortran|AgROOT|AgML|AgDL">
   [code]
</Export>


"""

from Dyson.Export.Handler import Handler
import Dyson.Utils.Shapes
#import sys
#print sys.path
#import pyparsing 

from pyparsing import * 

export_comments = False

_agstar_attribute_list = ['seen','colo','ltyp','serial','fill','lsty','lwid'];
_structures = {}
_struct_sep = '_'
_agml_sep = ':'
_soft_limit = 80 # applies a soft limit on the line length
_depth = 0
_prepend = '    '

# locator object which implements getLineNumber() and possibly other methods
# to be set by the syntax handler when the language is defined
locator = None

class Sanitize:
    """
    Helper function to strip illegal characters (in XML) from
    the output stream and replace them with their XML equivalent.
    """
    def __init__(self):
        self.table = {       # Replacement table
            '>' : '&gt;',
            '<' : '&lt;',
            '&' : '&amp;'
            }
    def __call__(self,line):
        out = ''
        for c in line:
            try:
                out += self.table[c]
            except KeyError:
                out += c
        return out

sanitize = Sanitize()
           
class Operator(Handler):

    def __init__(self,firstKey=None,keylist=[]):
        self.firstKey = firstKey
        self.keylist  = keylist

    def startElement(self,tag,attr):
        out = ''
        if ( self.firstKey != None ):
            try:
                firstValue = attr.pop(self.firstKey)
                if firstValue != None: out += '%s="%s" '%(self.firstKey,firstValue)
            except KeyError:
                pass
        for key in self.keylist:
            try:
                value = attr.pop(key)
                if value != None: out += '%s="%s" '%( key, value )
            except KeyError:
                pass
        for key,value in attr.iteritems():
            if value != None: out += '%s="%s" '%( key, value )
        out = sanitize(out)
        out = '<%s '%( tag ) + out + ' />'
        form( out )

class Container(Handler):

    def __init__(self,firstKey=None,keylist=[]):
        self.firstKey = firstKey
        self.keylist  = keylist
        
    def startElement(self,tag,attr):
        global _depth
        #out='<%s '%( tag )
        
        out = ''
        if ( self.firstKey != None ):

            try:
                firstValue = attr.pop(self.firstKey)
                if firstValue != None: out += '%s="%s" '%(self.firstKey,firstValue)
            except KeyError:
                pass

        for key in self.keylist:

            try:
                value = attr.pop(key,None)
                if ( value != None ): out += '%s="%s" '%( key, value )
            except KeyError:
                pass
        for key,value in attr.iteritems():
            if value != None: out += '%s="%s" '%( key, value )
        out = '<%s '%tag + sanitize(out) + ' >'
        #out += '>'
        form( out )
        _depth += 1             

    def characters(self,contents):
        global _depth
        #_depth+=1
        contents = sanitize(contents)
        if ( len(contents.strip()) > 0 ):
            form( contents.lstrip() )
        #_depth-=1

    def endElement(self,tag):
        global _depth
        _depth -= 1                             
        form( '</%s>' % tag )
        form( '' )


# ====================================================================================================
class NotYet( Exception ):
    def __init__(self):
        pass
    def __str__(self):
        return repr(self)+": Not yet implemented"

# ====================================================================================================


class SimplePrint:
    def __init__(self):
        pass
    def __call__(self,line):
        global _depth, _prepend        
        output = ''
        i=0
        while ( i<_depth ):
            output += _prepend
            i+=1
        output += line
        print output


# ====================================================================================================
#form = PrettyPrint()
form = SimplePrint()


# ====================================================================================================
# Document syntax
class Document( Container ):
    """
    Every AgML module must be wrapped by a <Document> ... </Document> tag.
    """
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Detector( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Setup( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Modules( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Init( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class StarGeometry( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Tag( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Geometry( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Construct( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

# ====================================================================================================

def sanitizeComments( line ):
    """
    Replaces all occurances of '--' with '++' in comments
    """
    length = len(line)
    output = ''
    i=0
    while i<length:
        c=line[i]
        if ( c == '{' or c == '}' ):
            c = ' '
            
        if ( i+1 < length ):
            d = line[i+1]
            if c+d == '--':
                output += '++'
                i=i+1
            else:
                output += c
        i+=1

    return output
                    
class Comment( Handler ):
    """
    Comments in AgML are simply the standard XML comments.  Use

    <!-- to start a comment, and
         to end a comment -->

    NOTE: XML does not permit "--" inside of comments (or elsewhere).
    """
    def __init__(self):
        Handler.__init__(self)
        self.content = ''
    def setParent(self,p):
        self.parent = p
    def startElement(self,tag,attr):
        pass
    def characters(self,content):
        self.content = content
    def endElement(self,tag):
        global export_comments
        if ( export_comments ):
            content = sanitizeComments(self.content)            
            form ( '<!-- ' + content + ' -->' )
     
class Module ( Container ):
    """
    Modules define the top-level geometry of a major STAR subsystem.
    They contain the data structures, material and volume definitions,
    and steering code to create and place the various pieces of the
    detector.  A typical module will be layed out as follows:

    <Module name="module_name" comment="documentation_string">

       <Author  name="Alan A. Aardvark" />
       <Created date="MM/DD/YY" />

       DATA AND VARIABLE DECLARATIONS

       STEERING CODE

       <Volume name="volume_name_1" comment="documentation_string">

           MATERIAL, MEDIUM and/or ATTRIBUTE declartion
           SHAPE declaration

           STEERING CODE
       
       </Volume>

       ...

       <Volume name="volume_name_2" comment="documentation_string">

           MATERIAL, MEDIUM and/or ATTRIBUTE declartion
           SHAPE declaration

           STEERING CODE
       
       </Volume>
    
    </Module>
    
    module_name is the name of the module.  The STAR convention for
    module names is a four-letter acronym for the detector followed
    by "geo".  e.g. bbcmgeo, tpcegeo, ftpcgeo, etc...

    volume_name is the name of a volume in the detector module.  It
    is limited to four characters for backwards compatability with
    starsim.

    In both module and volume declarations, the comment is required.

    Data structures are declared using the <Struct> tag and initialized
    using the <Fill> tag.  Variables are declared using <varlist>.
    Data structures and variables are always in scope.    
    """
    def __init__(self):
        self. name    = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Container.__init__(self,firstKey='name')
    def setParent(self,p): self.parent = p


class Volume ( Container ):
    """
    Volumes are declared in AgML using the <Volume> tag.  Each
    volume consists of an optional material, medium and attribute
    definition, and a mandatory declaration of the shape.  When
    a material and/or medium declaration is ommitted, the volume
    will inherit the material and medium definitions specified
    in the volume which created it (usually, but not always its
    parent.)

    Usage:
    <Volume name="NAME" comment="COMMENT" >

       <Material name="NAME" key="VALUE" ...  /> or <Mixture name="NAME" key="VALUE"> ... </Mixture>
       <Medium   name="NAME" key="VALUE" ...  />
       <Attributes for="NAME" key="VALUE" ... />
       <Shape type="TYPE" key="VALUE" ... />

       STEERING CODE
       
    </Volume>

    The STEERING CODE consists of any lessc statements, <For> loops,
    <If> statements, etc... and <Create> and <Placement> tags to
    create and place daughter volumes.    
    """
    def __init__(self):
        self. name = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Container.__init__(self,firstKey='name')    
    def setParent(self,p): self.parent = p

class Block( Volume ):
    def __init__(self): Volume.__init__(self)

class Group( Operator ):
    """
    """
    def __init__(self):
        self.name = ""
        self.comment = ""
        Operator.__init__(self,firstKey="name")
    def setParent(self,p): self.parent=p

class Export ( Container ):
    """
    The export tag is a feature implemented for backwards compatability
    with AgSTAR.  There are some features and behaviors of the AgSTAR
    language which are difficult to translate directly into c++ and
    vice versa.  For these cases one can wrap code in an export block.
    Multiple languages can be exported to, as in the following example.
        
    <Export language="Mortran">

       [code which should only appear in an AgSTAR file e.g....]
       write (*,*) 'This is a fortran write statement... probably shouldn't put in a c++ class'
    
    </Export>
    <Export language="AgROOT|AgDL">

       [code which should only be exported to c++ or AgDL]
       printf("This is a c-style print statement... best not to export to mortran")
    
    </Export>    
    """
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p


class Import( Operator ):
    """
    Includes the specified file in the XML source at the line where the
    import statement is executed.  By default the <Import> tag will pass
    the contents of the file to the XML parser.

    (** n.b. At present this is not implemented and will result in an error **).

    To include a file verbatim, specify verbtaim="true" as below.

    (** n.b. At present, this is the only functionality implemented **)
    
    <Import file="path/to/file" verbatim="true" />
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p
    

class Subroutine ( Container ):
    """
    It is possible to define subroutines, but their use is discouraged.
    """
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        self.name = attr.get('name')
        self.args = attr.get('args',[])
        Container.startElement(self,tag,attr)

class Assign ( Container ):
    """
    The assign tag provides a simplified mechanism for initializing
    the values of an array.

    <varlist type="int"> array1D(5), array2(5,2) </varlist>

    ...

    One dimensional arrays are assigned by specifying a comma-separated
    list of values enclosed within braces:

    <Assign var="array1D" value="{01,02,03,04,05}" />

    Two dimensional arrays are assigned similary, with the 2nd dimension
    separated by a semi-colon:

    <Assign var="array2D" value="{11,12,13,14,15;
                                  21,22,23,24,25}" />         
    """
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p

class Keep ( Container ):
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p
    
class Content ( Container ):
    """
    Each module which contains volumes should declare those volumes
    in the content tag.  Omitting a volume will result in a run-
    time error.  Declaring a volume in the content tag without
    implementing a corresponding volume tag will result in a
    warning.

    Usage:
    <Content>
       Comma-separated list of volume names
    </Content>
    """
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self. content = False
        Container.__init__(self)

class Include ( Operator ):
    def __init__(self):
        Operator.__init__(self)
    def setParent(self,p): self.parent = p    

class Cde ( Container ):
    """
    The CDE tag indicates which geant3 common blocks should be included
    in an AgSTAR export file.
    Usage:
    <CDE>
       AGECOM
       GCONST
       GCUNIT
    </CDE>
    """
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p    

class Inline( Container ):
    """
    Inline declares a helper function in AgML.  The format is
    
    <Inline name="method_name" type="return_type">

       <Arguement name="arg1" type="arg1_type" />
       <Arguement name="arg2" type="arg2_type" />
       ...
       <Arguement name="argN" type="argN_type" />
       
       <Return value="expression" />
       
    </Inline>

    This creates a function with the specified name which returns
    a value of the specified return type.  Arguements are
    declared in the order in which they are expected in the method
    call, e.g.

    y = method_name(arg1,arg2,...,argN);

    The <Return ...> tag specifies the expression used to compute
    the return value of the function, and no other code may be
    inserted into the inline declartion. 

    The expression may use any variables and/or structures which
    are declared in the module.
    """
    def __init__(self): Container.__init__(self)
    def setParent(self,p): 
        self.parent = p

class External( Operator ):
    """
    For backwards compatability with fortran, this declares any and all
    external routines needed by the geometry file.
    """
    def __init__(self):
        Operator.__init__(self)

class Varlist( Container ):
    """
    The varlist tag declares a list of variables.

    Usage:
    <varlist type="variable_type">
        COMMA SEPARATED LIST OF VARIABLES
    </varlist>

    where variable_type is one of: float, int, char.

    AgSTAR Limitations:
    Variables of type char are limited to four characters.
    """
    def setParent(self,p):
        self.parent = p
    def __init__(self):
        Container.__init__(self,firstKey='type')
        
class Var ( Operator ):
    """
    The <var> tag is used to declare structure members and fill them.
    See the <Struct> and <Fill> tags respectively.    
    """
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self.type    = None
        self.content = False
        Operator.__init__(self,keylist=['name','value','type','comment'])                

class Data( Operator ):
    """
    The <Data> tag is used for backwards compatability with AgSTAR.
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p
    
class Parameter( Operator ):
    """
    Class to represent PARAMETER (name=value) statements.  Limitation: only one
    name,value pair allowed per parameter statement.
    """
    def __init__(self):
        Operator.__init__(self)
    def setParent(self,p):
        self.parent = p
class Enum( Operator ):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p    
# ====================================================================================================
class Struct( Container ):
    """
    AgML structures are similar to c-structures.  They contain
    several different members of varying types.  AgML structures
    are limited to three types: float, int and char.  When a
    type specification is ommitted, float is assumed.  All char
    variables are strings of arbitrary length.  However, for
    compatability with AgSTAR, you should only use strings which
    are four characters in length.

    Arrays of ints and floats are supported up to two dimensions.

    Usage:
    <Struct name="struct_name">
       <var name="var_1" type="TYPE" />
       ...
       <var name="var_N" type="TYPE" />
    </Struct>

    Members of data structures may be accessesd in user code
    using the name of the structure and variable, e.g.

    y = stuct_name.var_1
    
    As with variables, arrays have limited support up to two
    dimensions.  They are declared similarly:

    <Struct name="struct_name">
       <var name="array_1(dim)" type="TYPE" />
       ...
       <var name="array_N(dim1,dim2)" type="TYPE" />
    </Struct>

    And accessed in the usual way

    First element:
    y = struct_name.array_1[0];       (or struct_name.array(1))

    Data structures are initialized using the Fill tag.
    """
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self,firstKey='name')

class Fill( Container ):
    """
    The fill statement is used to set the variables within an AgML data structure.
    The format is similar to the format of the Struct statement:

    <Fill struct="NAME" comment="A mandatory documentation string">
       <var name="NAME" value="VALUE" comment="A mandatory documentation string" />
       ...
       <var name="NAME" value="VALUE" comment="A mandatory documentation string" />
    </Fill>

    A single structure may be filled multiple times.  Different versions of a structure
    may be accessed by application of the Use operator:

    <Use struct="STRUCT" select="VAR" value="VALUE" />

    This loads into the structure specified by STRUCT the list of values from the
    fill statement which set VAR to VALUE.
    """
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self,keylist=['name','comment'])

class Filling(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self,keylist=['name','comment'])


class Use(Operator):
    """
    The <Use> operator selects different versions of an AgML data structure.
    The syntax is:

    <Use struct="struct_name" />
    <Use struct="struct_name" select="selector" value="selector_value" />

    The first form will load into the structure struct_name the values
    entered in the first <Fill> block.

    The second form will search through the different versions of struct_name
    in the order which they were defined by the <Fill> blocks.  When the
    member variable named "selector" is equal to the specified "selector_value",    
    the values of the structure "struct_name" will be copied from that
    fill statement.

    Note that searches are only allowed for member variables of type int, float
    and char.    
    """
    def __init__(self): Operator.__init__(self,keylist=['struct','select','value'])
    def setParent(self,p): self.parent = p    

class Using(Operator):

    def __init__(self): Operator.__init__(self,keylist=['struct','select','value'])
    def setParent(self,p): self.parent = p    

def ag_variable( line, classname ):
    """
    Parses the given line and trys to identify variables of
    the form ag_BLAH.  When it finds ag_BLAH it will replace
    with classname:BLAH.  This enables the emulation of the
    %BLAH and AG_BLAH variable names in AgSTAR.
    """
    AG      = CaselessLiteral( "AG_" )|Literal("%")

    AG_DENS = AG + CaselessKeyword("dens") ('var')
    AG_A    = AG + CaselessKeyword("a")    ('var')
    AG_Z    = AG + CaselessKeyword("z")    ('var')
    AG_RADL = AG + CaselessKeyword("radl") ('var')
    AG_ABSL = AG + CaselessKeyword("absl") ('var')

    RULE    = ( AG_DENS|AG_A|AG_Z|AG_RADL|AG_ABSL ) + restOfLine('rest')

    try:
        result = RULE.parseString(line)
        agvar  = result.get('var',None)
        rest   = result.get('rest',None)

        if rest:
            return '%s::%s '%(classname,agvar) + rest
        else:
            return '%s::%s '%(classname,agvar)

    except ParseException:        
        return line
            
    
class Material(Operator):
    """
    The material tag is used to declare the physical material used in a given
    volume, and/or to define new materials.

    Usage:

    To set the material used by a volume to an already existing material,
    simply provide the name of the material as an arguement to the
    material operator.
    
       <Material name="Air" />

    To define a new material, we specify the properties of the material
    fully, using

       <Material name="MyAir"
                 a="14.7" z="7.4"
                 density="0.00135"
                 radlen="0.305E+5"
                 abslen="0.675E+5" />

    Alternatively we can abuse inheritance to copy parameters of an existing
    material while redefining one of its parameters:

       <Material name="Air" />   <!-- copies Air -->
       <Material name="MyAir" density="0.00145" /> <!-- defines new density -->

    In this case, a new material MyAir is created with the properties
    of Air, but with a new density of 0.00145 g/cm^3.  One should take
    care when using this last option, as a declaration such as

       <Material name="Air" isvol="0" />

    will copy its parameters from the volume which created it.  If the
    material used in the mother volume is something other than "Air",
    such as Iron, then your detector description will not be as you
    expect.
    """
    def __init__(self):
        Operator.__init__(self)
        self.name = ""
        self.opts=[]
    def setParent(self,p): self.parent = p    

    def startElement(self,tag,attr):
        #
        # Perform %var --> material:var and ag_var --> material:var substitution
        #
        myattr = {}
        for key,value in attr.iteritems():
            myattr[key] = ag_variable( attr[key], 'material' )
        Operator.startElement(self,tag,myattr)

class Mixture(Container):
    """
    Mixtures provide another method for defining new materials in AgML
    in terms of a mixture of a number of components of specified
    atomic weight and number.  The mixture requires the specification
    of its name and density.  All other properties (radiation length,
    effective A and Z, etc...) will be computed from the mixture.

    Usage:
    <Mixture name="mixture_name" density="mixture_density">
       <Component name="c_name_1" a="atomic_weight_1" z="atomic_number_1" w="fraction or natoms" />
       ...
       <Component name="c_name_N" a="atomic_weight_N" z="atomic_number_N" w="fraction or natoms" />
    </Mixture>

    When specifying components, the use of a non-integer  atomic_number
    will result in a warning.

    The "weight" of the material may be specified using either the
    fractional weight (0 < w <= 1) or the number of atoms ( w>=1 ).    
    """
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self.name = ""        
        self.opts = []
        self.comps = []        
        Container.__init__(self,firstKey='name')                    

class Component(Operator):
    def __init__(self): Operator.__init__(self,firstKey='name')
    def setParent(self,p): self.parent = p    

class Medium(Operator):
    """
    The medium tag is used to impose some constraints on the simulator's
    propagation of particles through the volume.
    """
    def __init__(self):
        self.name = 0
        self.opts = []        
        Operator.__init__(self)
    def setParent(self,p): self.parent = p    


class Attribute(Operator):
    """
    The primary purpose of the attribute operator is to
    specifiy the visualization attributes of the volume.  Its
    format is

    <Attributes for="volume_name" seen="0/1" colo="volume_color" ... />

    Additionally, a serial="expression" flag may be provided to force
    the creation of a new volume when the syntax rules for the shape
    would otherwise reuse an existing volume.
    """
    def __init__(self):
        self.style = []
        self.name=""
        Operator.__init__(self,firstKey='for')
    def setParent(self,p): self.parent = p    
class Shape(Operator):
    """
    Specifies the shape of the volume.  The general form of the
    short form of the shape operator is given by

    <Shape type="TYPE" key="value" ... />

    The long form of the shape operator is given by

    <Shape type="TYPE" key="value" ...>
       <Section z="Z1" rmin="RMIN1" rmax="RMAX1" />
       ...
       <Section z="Zn" rmin="RMINn" rmax="RMAXn" />
    </Shape>

    TYPE is one of the basic geant3 shapes.  Once starsim is retired we
    will expand support to include all TGeo shapes.

    key=value pairs are used to specifiy the parameters of the shape.
    """              
    def __init__(self):
        Operator.__init__(self,firstKey='type')
        self.arglist = []
    def setParent(self,p):
        self.parent = p    

class Create(Operator):
    def __init__(self):
        Operator.__init__(self,firstKey='block')
    def setParent(self,p):
        self.parent = p        

class Position(Operator):
    def __init__(self):
        self.pos  = []
        self.into = None
        self.block = None
        Operator.__init__(self,keylist=['block','in','x','y','z'])        
    def setParent(self,p): self.parent = p

class Create_and_Position(Position):
    def setParent(self,p):
        self.parent = p    
    def __init__(self):
        self.pos  = []
        self.into = None
        self.block = None
        Position.__init__(self)

class For(Container):
    """
    For loops in AgML take the form

    <For var="i" from="lower" to="upper" step="step">

       CODE
     
    </For>
 
    where upper and lower are the inclusive bounds of
    the loop and step is the (optional ) stepsize.  For
    example
    
    <For var="i" from="1" to="10">
        <Print level="0" fmt="%i"> i </Print>
    </For>

    Will output the numbers 1 ... 10 inclusive.
    """
    
    def setParent(self,p): 
    	self.parent = p
    def __init__(self): 
    	Container.__init__(self,keylist=['var','from','to'])                    
    	self.var = ''

class While(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self): Container.__init__(self)                    

class Foreach(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self): Container.__init__(self)

class Call(Operator):
    def setParent(self,p): self.parent = p
    def __init__(self):Operator.__init__(self)

# ====================================================================================================
class If(Container):
    """
    The general form of the IF statement is given by
    
    <If expr="expression">
      <Then>                               <!-- Then tag is optional -->
         CODE
      </Then>
      <Elif expr="expression">
         CODE      
      </Elif>
      <Elif expr="expression">
         CODE
      </Elif>
      ...
      <Else>
         CODE      
      </Else>
    </If>
    """
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self)
        self.expr=''
class Then(Container):
    def setParent(self,p): self.parent = p
    def __init__(self): Container.__init__(self)    
class Elif(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self)
class Else(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self): Container.__init__(self)

class Return(Operator):
    def setParent(self,p): self.parent = p    
    def __init__(self): Operator.__init__(self)                    

class Check(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p

class Author(Operator):
    """
    Documents the author of the module.
    <Author name="NAME" />
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
class Created(Operator):
    """
    Documents the date on which the module was created.
    <Created date="DATE" />
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
class Translator(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        

class Par(Operator):
    """
    Sets a medium or material parameter for this volume.  The
    general form of the operator is 
    <Par name="PAR" value="VALUE" />

    It is the interface to the geant3 GSTPAR subroutine.

    Possible parameters are:

    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
class Cut(Operator):
    """
    Defines the track propogation cuts.  The operator's syntax is given
    by
    
    <Cut name="CUT" value="VALUE" />

    where CUT can be:

         CUTELE
         CUTGAM
         CUTHAD
         CUTMUO
         DCUTE
         BCUTE
         ...

    The VALUE is the energy at which the track will be stopped, in units
    of GeV.

    == POSSIBLE UPGRADE: implement units keV, MeV, GeV ==
    
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        

# ===> Slated for deprecation <===
class Hits(Operator):
    """
    Defines the HITS 
    """
    def __init__(self):
        Operator.__init__(self,firstKey='for')
        self.hit_list = []
        self.arg_list = []
    def setParent(self,p): self.parent = p        
# ===> Slated for deprecation <===

class Instrument(Container):
    """
    Defines how a volume is to be instrumented
    """
    def __init__(self):
        Container.__init__(self)
        self.hit_list = []
        self.arg_list = []
    def setParent(self,p): self.parent = p

class Hit(Operator):
    """
    Individual measurement performed by a volume, as specified
    in the Instrumentation block
    """
    def __init__(self):
        Operator.__init__(self, keylist=['meas','nbits','bin','opts','min','max'])
    def setParent(self,p): self.parent = p
        

class Gsckov(Operator):
    """
    Something to do with cherenkov generation...
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
    pass

class Print(Container):
    """
    Prints an exoression to standard out.  The form of the print statement
    is
    <Print level="L">
       expression
    </Print>

    The expression is any legitimate Mortran/LessC statement.  The level
    "L" specifies the frequency with which the statement line will be
    printed.

    Print
    Level
    0        Always
    1        Warn
    2        Error
    3        Fatal
    
    """
    def __init__(self):
        self.level  = 0
        self.format = 0
        self.args   = []        
        Container.__init__(self,firstKey='level')
    def setParent(self,p): self.parent = p

class Info(Container):
    """
    The Info tag is the only support I/O mechanism in AgML.  The syntax is

    <Info format="... {A.Bx} ..."> var1, val1, ... </Info>.

    The format attribute is a string which contains the text to be output and
    one format descriptor for every variable and/or value to be printed.  The
    format descriptors take the form

    {A.Bx}

    Where A is the width of the field, B is an (optional) number of places past
    the decimal point and x is one of f, d or i for floats, doubles and integers.
    Each format descriptor is wrapped in brackets.
    """
    def __init__(self):
        Container.__init__(self)
    def setParent(self,p): self.parent=p

class Replace(Container):
    """
    Defines a replacement macro.
    """
    def __init__(self):
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Placement(Container):
    """
    Places a volume within a mother volume.  The syntax of the placement
    statement is

    <Placement block="BLOCK" in="MOTHER" x="X" y="Y" z="Z" konly="KONLY" ncopy="NCOPY" if="expr" >
       ... rotation commands ...
    </Placement>

    or if no rotations are needed ...

    <Placement block="BLOCK" in="MOTHER" x="X" y="Y" z="Z" konly="KONLY" ncopy="NCOPY" if="expr" /> 

    required:

      BLOCK  -- specifes the block being placed

    optional:

      MOTHER -- specifies the mother volume into which the daughter will
                be placed.  If ommitted, the mother volume will default
                to the volume which created the daughter block, or the
                CAVE if the volume was created within the scope of the
                module.
                
      X,Y,Z  -- specifies the position of the volume within the mother's
                coordinate system.  The translation will be carried out
                before any rotations.
  
      KONLY  -- Possible values are ONLY and MANY.  Default is ONLY, and
                indicates that the volume does not overlap any other
                volumes.  The MANY option should be used when volumes are
                allowed to overlap.
                
      if     -- Specifies an expression which, if true, will allow the volume
                to be placed.  If false, the volume will not be placed.

                e.g.

                <Do var="i" from="1" to="10">
                <Placement block="ABCD" y="+10.0" z="i*2.0" if="i<=5" />
                <Placement block="ABCD" y="-10.0" z="i*3.0" if="i>=5" />
                </Do>
                

    """
    def __init(self):
        Container.__init__(self, firstKey='block' )
    def setParent(self,p):
        self.parent=p

class Translation(Operator):
    """
    The translation operator modifies the placement of a volume.  It
    should be contained within the placement element.
    <Translation x="X" y="Y" z="Z" />
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent=p

class Rotation(Operator):
    """
    """
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent=p

class Matrix(Container):
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent=p



class Function(Container):
    def __init__(self):
        self.parent      = None
        self.name        = ""
        self.arg_list    = []
        self.arg_intent  = {}
        self.arg_type    = {}
        self.arg_default = {}        
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

class Arguement(Operator):
    def __init__(self):
        self.parent = None
        Operator.__init__(self)
    def setParent(self,p): 
        self.parent = p

class Fatal(Container):
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p        

if __name__ == '__main__':

    if ( 0 ):

        s = Subroutine()
        s.characters("test")

