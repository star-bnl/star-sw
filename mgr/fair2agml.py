#!/usr/bin/env python

import cmd
import ROOT

from xml.sax import saxutils
from xml.sax import make_parser
from xml.sax.handler import feature_namespaces

from optparse import OptionParser

import Dyson.Utils.Shapes  

parser = 0

import sys
sys.path.append("$STAR/")
sys.path.append("./")

# Do not export bytecode to pyc files
sys.dont_write_bytecode = True

#import Dyson.Syntax.SyntaxHandler as SyntaxHandler
from Dyson.Syntax.SyntaxHandler import SyntaxHandler
syntax = None
mode   = None 

def agmlarray( a ): return str(a).replace('[','{').replace(']','}')

def material( mat ):
    # TODO: add properties...
    if mat == None:
        return
    name = mat.GetName()
    A    = mat.GetA()
    Z    = mat.GetZ()
    DENS = mat.GetDensity()
    RADL = mat.GetRadLen()
    INTL = mat.GetIntLen()
    pars = {'name':name, 'a':str(A), 'z':str(Z), 'dens':str(DENS) }
    syntax.startElement( 'Material', pars )
    syntax.endElement  ( 'Material' )

def medium( med ):
    # TODO: add properties...
    if med == None:
        return
    name = med.GetName()
    #mylist    = ['ifield','fieldm','tmaxfd','epsil','stemax','deemax','stmin','isvol']

    pars = { 'name':name }

    for iflag,flag in enumerate(['isvol','ifield','fieldm','tmaxfd','stemax','deemax','epsil','stmin']):
        value = med.GetParam(iflag)
        if flag=='stemax' and float(value) > 1.0E9: continue
        if flag=='isvol'  and int(value)  == 0:     continue
        pars[flag] = str(value)
    
    syntax.startElement( 'Medium', pars )
    syntax.endElement  ( 'Medium' )
#______________________________________________________________________________________________
def phirange(shape):
    phi1 = shape.GetPhi1()
    phi2 = shape.GetPhi2()
    return { 'phi1':str(phi1), 'phi2':str(phi2) }
#______________________________________________________________________________________________
def bbox( shape ):
    dx = shape.GetDX()
    dy = shape.GetDY()
    dz = shape.GetDZ()
    return { 'dx':str(dx), 'dy':str(dy), 'dz':str(dz) }
def tube(shape):
    dz = shape.GetDz()
    rmin = shape.GetRmin()
    rmax = shape.GetRmax()
    return { 'dz':str(dz), 'rmin':str(rmin), 'rmax':str(rmax) }
def tubeseg(shape):
    dz = shape.GetDz()
    rmin = shape.GetRmin()
    rmax = shape.GetRmax()
    phi1 = shape.GetPhi1()
    phi2 = shape.GetPhi2()
    return { 'dz':str(dz), 'rmin':str(rmin), 'rmax':str(rmax), 'phi1':str(phi1), 'phi2':str(phi2) }
def ctub(shape):
    low = shape.GetNlow()
    top = shape.GetNhigh()
    pars = {
        'lx' : str(low[0]),
        'ly' : str(low[1]),
        'lz' : str(low[2]),
        'tx' : str(top[0]),
        'ty' : str(top[1]),
        'tz' : str(top[2]) }
    return pars.update( tube(shape) )

def cone(shape):
    dz=shape.GetDz()
    rmin1=shape.GetRmin1()
    rmin2=shape.GetRmin2()
    rmax1=shape.GetRmax1()
    rmax2=shape.GetRmax2()
    return { 'dz':str(dz), 'rmn1':str(rmin1), 'rmn2':str(rmin2), 'rmx2':str(rmax2), 'rmx1':str(rmax1) }
def coneseg(shape):
    dz=shape.GetDz()
    rmin1=shape.GetRmin1()
    rmin2=shape.GetRmin2()
    rmax1=shape.GetRmax1()
    rmax2=shape.GetRmax2()
    phi1 = shape.GetPhi1()
    phi2 = shape.GetPhi2()
    return { 'dz':str(dz), 'rmn1':str(rmin1), 'rmn2':str(rmin2), 'rmx2':str(rmax2), 'rmx1':str(rmax1), 'phi1':str(phi1), 'phi2':str(phi2) }
def eltu(shape):
    pars = { 'a': str(shape.GetA()),             'b': str(shape.GetB()),             'dz': str(shape.GetDz()) }
    return pars
def trd1(shape):
    dx1 = shape.GetDx1()
    dx2 = shape.GetDx2()
    dy  = shape.GetDy()
    dz  = shape.GetDz()
    return { 'dz':str(dz), 'dx1':str(dx1), 'dx2':str(dx2), 'dy':str(dy) }
def trd2(shape):
    dx1 = shape.GetDx1()
    dx2 = shape.GetDx2()
    dy1 = shape.GetDy1()
    dy2 = shape.GetDy1()
    dz  = shape.GetDz()
    return { 'dz':str(dz), 'dx1':str(dx1), 'dx2':str(dx2), 'dy1':str(dy1), 'dy2':str(dy2) }
def pcon(shape):
    nz = shape.GetNz()
    phi1 = shape.GetPhi1()
    dphi = shape.GetDphi()
    rmax = []
    rmin = []
    zsli = []
    for iz in range(0,nz):
        rmax.append( shape.GetRmax()[iz] )
        rmin.append( shape.GetRmin()[iz] )
        zsli.append( shape.GetZ   ()[iz] )                
    rmax = str(rmax)
    rmin = str(rmin)
    zsli = str(zsli)
    rmax = rmax.replace('[','{').replace(']','}')
    rmin = rmin.replace('[','{').replace(']','}')
    zsli = zsli.replace('[','{').replace(']','}')
    pars = { 'nz':str(nz), 'phi1':str(phi1), 'dphi':str(dphi), 'rmn':rmin, 'rmx':rmax, 'zi':zsli }
    return pars
def pgon(shape):
    nz = shape.GetNz()
    phi1 = shape.GetPhi1()
    dphi = shape.GetDphi()
    npdiv = shape.GetNedges()    
    rmax = []
    rmin = []
    zsli = []
    for iz in range(0,nz):
        rmax.append( shape.GetRmax()[iz] )
        rmin.append( shape.GetRmin()[iz] )
        zsli.append( shape.GetZ   ()[iz] )                
    rmax = str(rmax)
    rmin = str(rmin)
    zsli = str(zsli)
    rmax = rmax.replace('[','{').replace(']','}')
    rmin = rmin.replace('[','{').replace(']','}')
    zsli = zsli.replace('[','{').replace(']','}')
    pars = { 'npdiv':npdiv, 'nz':str(nz), 'phi1':str(phi1), 'dphi':str(dphi), 'rmn':rmin, 'rmx':rmax, 'zi':zsli }
    return pars
def trap(itsa):
    dz = itsa.GetDz()
    theta = itsa.GetTheta()
    phi = itsa.GetPhi()
    h1 = itsa.GetH1()
    bl1 =itsa.GetBl1()
    tl1 =itsa.GetTl1()
    alpha1 =itsa.GetAlpha1()
    h2 = itsa.GetH2()
    bl2 =itsa.GetBl2()
    tl2 =itsa.GetTl2()
    alpha2 =itsa.GetAlpha2()
    pars = {
        'dz': str(dz),
        'thet': str(theta),
        'phi'  : str(phi),
        'h1'   : str(h1),
        'bl1'  : str(bl1),
        'tl1'  : str(tl1),
        'alp1':str(alpha1),
        'h2'   : str(h2),
        'bl2'  : str(bl2),
        'tl2'  : str(tl2),
        'alp2':str(alpha2) }
    return pars
def gtra(itsa):
    dz = itsa.GetDz()
    theta = itsa.GetTheta()
    phi = itsa.GetPhi()
    h1 = itsa.GetH1()
    bl1 =itsa.GetBl1()
    tl1 =itsa.GetTl1()
    alpha1 =itsa.GetAlpha1()
    h2 = itsa.GetH2()
    bl2 =itsa.GetBl2()
    tl2 =itsa.GetTl2()
    alpha2 =itsa.GetAlpha2()
    twist = itsa.GetTwistAngle()
    pars = {
        'dz': str(dz),
        'thet': str(theta),
        'phi'  : str(phi),
        'h1'   : str(h1),
        'bl1'  : str(bl1),
        'tl1'  : str(tl1),
        'alp1':str(alpha1),
        'h2'   : str(h2),
        'bl2'  : str(bl2),
        'tl2'  : str(tl2),
        'alp2':str(alpha2), 'twis':str(twist) }
    return pars
#   #['dz','thet','phi','twis','h1','bl1','tl1','alp1','h2','bl2','tl2','alp2' ],
#   pars = { 'twis' : str(shape.GetTwistAngle()) }
#   return pars.update( trap(shape) )
def para(shape):
    dx = shape.GetX()
    dy = shape.GetY()
    dz = shape.GetZ()
    alpha = shape.GetAlpha()
    theta = shape.GetTheta()
    phi   = shape.GetPhi()
    pars = {
        'dx':str(dx),
        'dy':str(dy),
        'dz':str(dz),
        'alph':str(alpha),
        'thet':str(theta),
        'phi':str(phi)
        }
    return pars
def sphere(shape):
    rmin=shape.GetRmin()
    rmax=shape.GetRmax()
    the1=shape.GetTheta1()
    the2=shape.GetTheta2()
    phi1=shape.GetPhi1()
    phi2=shape.GetPhi2()
    pars = {
        'rmin': str(rmin),
        'rmax': str(rmax),
        'phi1': str(phi1),
        'phi2': str(phi2),
        'the1': str(the1),
        'the2': str(the2)
        }
    return pars
def hype(shape):
    stin=shape.GetStIn()
    stout=shape.GetStOut()
    rin=shape.GetRmin()
    rout=shape.GetRmax()
    dz=shape.GetDz()
    pars = {
        'stin'  : str(stin),
        'stout' : str(stout),
        'rin'   : str(rin),
        'rout'  : str(rout),
        'dz'    : str(dz)
        }
    return pars
def xtru(shape):
    if mode == "G3":
        shape.ComputeBBox()
	dx=shape.GetDX()
    	dy=shape.GetDY()
    	dz=shape.GetDZ()
    	pars = { 'dx':str(dx), 'dy':str(dy), 'dz':str(dz), 'comment':'Xtru not supported, convert to bounding box' }
    else:
    	nz = shape.GetNz()    # number of z slices
	x0 = [] # x origin
	y0 = [] # y origin
	z0 = [] # z of plane
	s0 = [] # scale of polygon
	for i in range(0,nz):
	    x0 .append (shape.GetXOffset(i) )
	    y0 .append (shape.GetYOffset(i) )
	    z0 .append (shape.GetZ(i)       )
	    s0 .append (shape.GetScale(i)   ) 
	nv = shape.GetNvert() # number of vertices
	xv  = [] # x vertices
	yv  = [] # y vertices
	for i in range(0,nv):
	    xv .append (shape.GetX(i))
	    yv .append (shape.GetY(i)) 
	pars = {
	    'nv':str(nv), 'xv':agmlarray(xv), 'yv':agmlarray(yv),
	    'nz':str(nz), 'x0':agmlarray(x0), 'y0':agmlarray(y0), 'z0':agmlarray(z0), 's0':agmlarray(s0)
	    } 
	return pars 

    return pars 
def compositeshape(shape):
    pass


_shape_parameters = {
    'bbox'   : bbox,    'box'  : bbox,      # box and aliases
    'tube'   : tube,
    'tubs'   : tubeseg, 'tubeseg':tubeseg,
    'ctub'   : ctub, 
    'cone'   : cone,
    'cons'   : coneseg,
    'coneseg':coneseg,
    'eltu'   : eltu,
    'trd1'   : trd1,
    'trd2'   : trd2, 
    'pcon'   : pcon,
    'pgon'   : pgon,
    'trap'   : trap, 'gtra' : gtra,
    'para'   : para,
    'sphere' :sphere, 'sphe': sphere,
    'hype'   : hype,
    'xtru'   : xtru
    }

def rotation( matrix ):
    rotm  = [1,0,0,0,1,0,0,0,1]
    if hasattr(matrix,"GetRotationMatrix"):
        for i in range(0,9):
            rotm[i] = matrix.GetRotationMatrix()[i]
        rotm = str(rotm).replace('[','{').replace(']','}')
    
    syntax.startElement( 'Rotation', { 'matrix': rotm } )
    syntax.endElement  ( 'Rotation' )

def translation(matrix):
    x = matrix.GetTranslation()[0]
    y = matrix.GetTranslation()[1]
    z = matrix.GetTranslation()[2]
    pars = { 'x':str(x), 'y':str('y'), 'z':str(z) }
    syntax.startElement( 'Translation', pars )
    syntax.endElement  ('Translation' )


	


def shape_parameters( sha ):

    # Use introspection to discover wtf I have here
    name = sha.IsA().GetName()

    name = name.lower()
    name = name.replace('tgeo','')
    
    try:
        pars = _shape_parameters[ name ]( sha )
        return pars
    except KeyError:
        print "<!-- failed on Shape %s -->"%name        
        return {}

def composite( sha ):
    boolean = sha.GetBoolNode()
    isa = boolean.IsA().GetName()
    isa.replace('TGeo','')
    isa = isa.lower() 
    pars = { 'name': sha.GetName(), 'operation':isa }
    syntax.startElement( 'Composite', pars )
    left_shape  = boolean.GetLeftShape()
    left_matrix = boolean.GetLeftMatrix()
    if 1:
        syntax.startElement( 'PlacedShape', { 'name':left_shape.GetName() } )
        shape(left_shape)
        rotation(left_matrix)
        translation(left_matrix)
        syntax.endElement( 'PlacedShape' )
    
    right_shape = boolean.GetRightShape()
    right_matrix= boolean.GetRightMatrix()

    if 1:
        syntax.startElement( 'PlacedShape', { 'name':right_shape.GetName() } )
        shape(right_shape)
        rotation(right_matrix)
        translation(right_matrix) 
        syntax.endElement( 'PlacedShape' )
    
    syntax.endElement  ( 'Composite' )

def shape( sha, usename=False ):
    if sha == None:
        return

    if sha.IsComposite():
    	composite(sha)
	return

    # Use introspection to discover wtf I have here
    name = sha.IsA().GetName()
    
    name = name.replace( 'TGeo', '' ) # simplify

    # replace root shape names with g3 equiv
    repl = { 'tubeseg': 'tubs',
             'coneseg': 'cons',
             'sphere' : 'sphe' }
    try:
        name = repl[name.lower()]
    except KeyError:
        pass

    pars = { 'type': name }
    spars =  shape_parameters(sha)
    if spars != None:
        pars.update( spars )

    if usename: pars.update( {'name':sha.GetName()} )
    
    syntax.startElement( 'Shape', pars )
    syntax.endElement( 'Shape' )    
    

def attributes( vol ):
    vis = 1
    if vol.IsVisible() == False: vis = 0
    par = { 
        'for' :vol.GetName(),
	'seen':str(vis),         
	'colo':vol.GetLineColor(),
	'lwid':vol.GetLineWidth()
    }
    syntax.startElement( 'Attribute', par )
    syntax.endElement  ( 'Attribute' )

def volume( vol ):
    
    name = vol.GetName()
    comm = vol.GetTitle()

    vtype = 'Volume'
    if vol.IsAssembly() and mode != 'G3': vtype = 'Assembly' 

    syntax.startElement( vtype, { 'name':name, 'comment':comm } )
    
    if vtype == 'Volume':
        material( vol.GetMaterial() )
    	medium  ( vol.GetMedium()   )
        attributes( vol )
    	shape   ( vol.GetShape()    )

    # iterate over daughters and create a list of volume names
    list_of_daughter_nodes = vol.GetNodes()
    dict_of_daughter_nodes = {}

    # Make sure we have something to do
    if list_of_daughter_nodes != None:

        for node in list_of_daughter_nodes:
            daughter = node.GetVolume()
            dict_of_daughter_nodes[ daughter ] = node
        list_of_daughter_volumes = dict_of_daughter_nodes.keys()

        # iterate over volume names and call create operator
        for daughter in list_of_daughter_volumes:
            syntax.startElement( 'Create', {'block':daughter.GetName()} )
            syntax.endElement  ( 'Create' )
        for node in list_of_daughter_nodes:
            daughter = node.GetVolume().GetName()
            konly = 'ONLY'
            if node.IsOverlapping(): konly = 'MANY'
            target = node.GetMotherVolume().GetName()
            matrix = node.GetMatrix()
            tran  = matrix.GetTranslation()
            rotm  = [1,0,0,0,1,0,0,0,1]
            if hasattr(matrix,"GetRotationMatrix"):
                for i in range(0,9):
                    rotm[i] = matrix.GetRotationMatrix()[i]
            rotm = str(rotm).replace('[','{').replace(']','}')

            pars = { 'block' : daughter,
                     'in'    : target,
                     'konly' : konly,
                     'x'     : str(tran[0]),
                     'y'     : str(tran[1]),
                     'z'     : str(tran[2]) }
            
            syntax.startElement( 'Placement', pars )
            syntax.startElement( 'Rotation',  { 'matrix': rotm } )
            syntax.endElement  ( 'Rotation' )            
            syntax.endElement  ( 'Placement' )

    syntax.endElement  ( vtype )


def main():

    global syntax, mode

    optparser = OptionParser()
    optparser.add_option("--file",   help="Selects the geometry file to be parsed.", dest="filename", default="NONE")
    optparser.add_option("--export", help="Selects the export language (Mortran,AgML,...)", dest="language",default="Mortran" )
    optparser.add_option("--module", help="Defines the name of the module for export",      dest="module_name", default="testgeo" )
    optparser.add_option("--path",   help="Defines the path to export the files",           dest="path_name",   default='./')
    optparser.add_option("--mode",   help="G3|ROOT",                                        dest="mode",        default="G3" )
    optparser.add_option("--topvolume", help="Top volume to export",          dest="topvolume",   default="TOP" )
    
    (opts, args) = optparser.parse_args()

    # Get the syntax handler
    mode = opts.mode
    if mode == 'ROOT':
        Dyson.Syntax.SyntaxHandler.rootmode = True

    syntax = SyntaxHandler( opts )
    syntax_type = opts.language

    gfile     = ROOT.TFile.Open( "%s"%opts.filename )  # open geometry file
    topvolume = gfile.Get( "%s"%opts.topvolume )       # get the top volume


    
    geom = ROOT.gGeoManager

    agml_document_name = opts.filename
    agml_document_name.replace('.root','Geo.agml')

    agml_module_name   = opts.module_name
    
#   list_of_volumes = geom.GetListOfVolumes()

    list_of_volumes      = []
    list_of_volume_names = [] # in order to populate the content block
    dict_of_volume_names = {} # to ensure no duplicates emitted

    iter = ROOT.TGeoIterator( topvolume )

    while (True):
        node = iter.Next()
        if ( node == None ): break
        vol = node.GetVolume()
        name   = vol.GetName()
        try:
            check = dict_of_volume_names[name]
            continue
        except KeyError:
            list_of_volumes.append(vol)
            list_of_volume_names.append( name )
            dict_of_volume_names[name] = vol       

    # Emit the document header
    syntax.startElement( 'Document', { "file" : "%s"%agml_document_name } )
    syntax.startElement( 'Module',   { "name" : "%s"%agml_module_name, "comment" : "Imported from %s"%opts.filename } )
    syntax.startElement( 'Author',   { "name" : "root2agml" } )
    syntax.endElement  ( 'Author' )
    syntax.startElement( 'Created',  { "date" : "now" } )
    syntax.endElement  ( 'Created' )
    syntax.startElement( 'CDE', {} )
    syntax.characters  ( 'AGECOM,GCONST,GCUNIT' )
    syntax.endElement  ( 'CDE' )
    syntax.startElement( 'Content', {} )
    syntax.characters  ( ','.join( list_of_volume_names ) )               
    syntax.endElement  ( 'Content')

    # Emit volume blocks
    for volname in list_of_volume_names:
        vol = dict_of_volume_names[volname]
        volume( vol )
    # Close module and document
    syntax.endElement  ( 'Module'   )
    syntax.endElement  ( 'Document' )
     
if __name__ == '__main__':
    main()
 
