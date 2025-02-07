# =======================================================================================================
#
#   Shape.py defines how the XML shape syntax results in the
#   creation of the corresponding TGeo class
#
#   <shape type="TYPE" arg1="ARG1" arg2="ARG2" ... />
#
# TODO:
#
# + Add capability to handle divisions
#
# =======================================================================================================

# Specifies the target pointer for the output
#
_target = "_shape"

# Mapping between TYPE and TGeo class name
#
_shape_types = {
    'arb8'       : 'Arb8',
    'box'        : 'BBox',
    'bbox'       : 'BBox',
    'cone'       : 'Cone',
    'cons'       : 'ConeSeg',
    'eltu'       : 'Eltu',
    'gtra'       : 'Gtra',
    'hype'       : 'Hype',
    'para'       : 'Para',
    'paraboloid' : 'Paraboloid',
    'pcon'       : 'Pcon',
    'pgon'       : 'Pgon',
    'sphere'     : 'Sphere',
    'torus'      : 'Torus',
    'trd1'       : 'Trd1',
    'trd2'       : 'Trd2',
    'tube'       : 'Tube',
    'tubs'       : 'TubeSeg'
    }

# List specifying the shape of the arguement list in the TGeo constructor
#
_shape_arglist = {
    'arb8'       : ['dz','x0','y0','x1','y1','x2','y2','x3','y3','x4','y4','x5','y5','x6','y6','x7','y7'],
    'box'        : ['dx','dy','dz'], 
    'bbox'       : ['dx','dy','dz'], 
    'cone'       : ['dz','rmn1','rmx1','rmn2','rmx2'],
    'coneseg'    : ['dz','rmn1','rmx1','rmn2','rmx2','phi1','phi2'],
    'cons'       : ['dz','rmn1','rmx1','rmn2','rmx2','phi1','phi2'],    
    'eltu'       : ['p1','p2','dz'],
#    'gtra'       : ['dz','thet','phi','twist','h1','bl1','tl1','alp1','h2','bl2','tl2','alp2' ],
    'gtra'       : ['dz','thet','phi','twis','h1','bl1','tl1','alp1','h2','bl2','tl2','alp2' ],
    'trap'       : ['dz','thet','phi','h1','bl1','tl1','alp1','h2','bl2','tl2','alp2'],

    'hype'       : ['rin','stin','rout','stout','dz'], 
    'para'       : ['dx','dy','dz','alph','thet','phi'], 
    'paraboloid' : ['rlo','rhi','dz'], 
    'pcon'       : ['phi1','dphi','nz', 'zi', 'rmn', 'rmx'],  
    'pgon'       : ['phi1','dphi','npdiv','nz', 'zi', 'rmn', 'rmx'],   # npdiv and npdv are equivalent
    'sphe'       : ['rmin','rmax','the1','the2','phi1','phi2'], 
    'sphere'     : ['rmin','rmax','the1','the2','phi1','phi2'], 
    'torus'      : ['r','rmin','rmax','phi1','dphi'], 
    'trd1'       : ['dx1','dx2','dy','dz'], 
    'trd2'       : ['dx1','dx2','dy1','dy2','dz'], 
    'tube'       : ['rmin','rmax','dz'],
    'tubs'       : ['rmin','rmax','phi1','phi2','dz'],
    'ctub'       : ['rmin','rmax','phi1','phi2','dz','hx','hy','hz','lx','ly','lz'],
    'division'   : ['ndiv','iaxis','c0']
    }

shape_params = [
    'dx', 'dy', 'dz',
    'dx1','dx2','dy1','dy2',
    'rmin','rmax','r',
    'rmn1','rmx1','rmn2','rmx2',
    'phi1','phi2','dphi',
    'thet','phi','alph',
    'nz','npdif','zi','rmn','rmx',
    'p1','p2',
    'twist','h1','h2','bl1','bl2','tl1','tl2','alp1','alp2',    
    'x0','y0','x1','y1','x2','y2','x3','y3','x4','y4','x5','y5','x6','y6','x7','y7'
    ]



#_all_shape_args = []
#_tmp = {}
#for shape,args in _shape_arglist.iteritems():
#    for a in args:
#        _tmp[a]=1
#for a in _tmp:
#    global _all_shape_args
#    _all_shape_args.append(a)


# Given the type of shape, return list of possible parameters
#
def arglist( type ):
    try:
        return _shape_arglist[ type ]
    except KeyError:
        print "=="
        print "=================================================================="
        print "Could not find shape with type %s"%type
        print "=================================================================="
        print "=="
        raise KeyError

def listOfShapes():
    return _shape_types.keys()
