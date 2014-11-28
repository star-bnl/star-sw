from ROOT import TFile
from ROOT import TH2F, TH1F, TH1
from ROOT import gStyle
from ROOT import TLegend
from ROOT import TStyle
from ROOT import TColor
from ROOT import TCanvas
from ROOT import TPad
from ROOT import TExec
from ROOT import TPython
from ROOT import TGaxis
from ROOT import TLine
from ROOT import TMath

from ROOT import TGeoManager
from ROOT import   TGeoVolume
from ROOT import   TGeoNode
from ROOT import   TGeoShape
from ROOT import   TGeoMedium
from ROOT import TPaveText

from Canvas import *
from array import *

gStyle.SetPalette(1)
gStyle.SetOptStat(0)

_palette = {}

_files = {}
_current_file = 0
_normalized = {}

_file_path = "./"

# ---------------------------------------------------------------------------------------------------------------    

def get_geom_file(name):

    """
    Trys to retrieve the geometry histogram file from the dictionary of files.
    If it does not exist, it creates a new file and adds to the dictionary.
    """

    path = _file_path + "/" + name
    
    try:
        file = _files[ path ]
        return file
    except KeyError:
        file = TFile( path + ".root" )
        _files[ path ] = file
        _current_file  = file
        return file

# ---------------------------------------------------------------------------------------------------------------    

def set_palette(name="default", ncontours=999, id=1 ):

    """Set a color palette from a given RGB list
    stops, red, green and blue should all be lists of the same length
    see set_decent_colors for an example"""

    if name == "gray" or name == "grayscale":         # simple grayscale
        _palette[ name ] = id
        stops = [0.00, 0.34, 0.61, 0.84, 1.00]
        red   = [1.00, 0.84, 0.61, 0.34, 0.00]
        green = [1.00, 0.84, 0.61, 0.34, 0.00]
        blue  = [1.00, 0.84, 0.61, 0.34, 0.00]
    elif name == "diffs":                           # grayscale in abs(z)
        _palette[ name ] = id
        stops = [0.00, 0.08, 0.195,0.33, 0.50-0.0001,0.500, 0.500+0.0001, 0.670,0.805,0.920,1.000]
        red   = [0.00/1.5, 0.34/1.5, 0.61/1.5, 0.84/1.5, 1.00,       1.000, 1.00,         0.84/1.5, 0.61/1.5, 0.34/1.5, 0.00] 
#       green = [0.00/1.5, 0.34/1.5, 0.61/1.5, 0.84/1.5, 1.00,       1.000, 1.00,         0.84/1.5, 0.61/1.5, 0.34/1.5, 0.00] 
        green = [0.00/1.5, 0.00/1.5, 0.00/1.5, 0.00/1.5, 1.00,       1.000, 1.00,         0.00/1.5, 0.00/1.5, 0.00/1.5, 0.00] 
#       blue  = [0.00/1.5, 0.34/1.5, 0.61/1.5, 0.84/1.5, 0.60,       0.600, 0.60,         0.84/1.5, 0.61/1.5, 0.34/1.5, 0.00] 
        blue  = [0.00/1.5, 0.00/1.5, 0.00/1.5, 0.00/1.5, 0.60,       0.600, 0.60,         0.00/1.5, 0.00/1.5, 0.00/1.5, 0.00] 

    elif name == "bool":                               # simple 1 or 0
        _palette[ name ] = id
        stops = [0.00,0.000001,1.00000]
        red   = [0.00,         1.0,1.0]
        green = red
        blue  = red
    else:                                              # default smooth color gradient
        _palette[ name ] = id
        stops = [0.00, 0.34, 0.61, 0.84, 1.00]
        red   = [0.00, 0.00, 0.87, 1.00, 0.51]
        green = [0.00, 0.81, 1.00, 0.20, 0.00]
        blue  = [0.51, 1.00, 0.12, 0.00, 0.00]

    s = array('d', stops)
    r = array('d', red)
    g = array('d', green)
    b = array('d', blue)

    npoints = len(s)
    TColor.CreateGradientColorTable(npoints, s, r, g, b, ncontours)
    gStyle.SetNumberContours(ncontours)

def set_color():
    set_palette(name="color")
def set_diffs():
    set_palette(name="diffs")

# ---------------------------------------------------------------------------------------------------------------    

class Sum1D:

    def __init__(self,geom="y2006g", volumes=['TPCE'], stat="radlen", 
                 xmin=0., xmax=-1., 
                 ymin=0., ymax=-1.,
                 rebin=0, pad=0 ):

        self.file = TFile( geom + ".root")                                  # connect the file
        self.file.ls();
        self.histos = []
        i = 0
        while ( i < len(volumes) ):
#           print volumes[i]
            hname = "h_"+stat+"_"+volumes[i]+"_eta"
            cname = "h_counts_"  +volumes[i]+"_eta"
            nname = "H_"+stat+"_"+volumes[i]+"_eta"
            histo = self.file.Get( hname )                                  # retrieve histogram from the file
            count = self.file.Get( cname )                                  # retrieve the corresponding number of geantinos
            if ( rebin ):
                histo.Rebin(rebin)
                count.Rebin(rebin)
            histo.Divide( count )                                           # normalize by the number of geantinos            
            self.histos.append( histo.Clone(nname) )                        # clone the histogram and store locally
            i+=1
        if ( pad == 0 ):
            self.c1 = TCanvas( geom+"::"+volumes[0], geom, 700,375)             # create a new canvas and prepare to draw
        else:
            self.c1 = pad

        self.c1.SetGridx()
        self.c1.SetGridy()
        i = 0
        while ( i < len(volumes) ):
            opt = ""
            if ( i ):
                opt = "same"
            else:
                self.histos[i].SetLineWidth(2)
            print "["+str(i)+"]"+volumes[i]+" "+str(self.histos[i].GetEntries())+" "+str(self.histos[i].Integral())
            if ( i>0 ):
                self.histos[i].SetFillColor(i+1)
            if ( i>1 ):
                self.histos[i].Add( self.histos[i-1], 1.0 )
            self.histos[i].Draw( opt )
            i+=1

        self.legend = TLegend(0.8,0.54,0.985,0.985,volumes[0]+" in "+geom)
        if ( xmax > xmin ):
            self.histos[0].GetXaxis().SetRangeUser(xmin,xmax)
        if ( ymax > ymin ):
            self.histos[0].GetYaxis().SetRangeUser(ymin,ymax)
        self.histos[0].Draw()
        i = len(volumes)-1
        while ( i>0 ):
            self.histos[i].Draw("same")
            self.legend.AddEntry( self.histos[i], volumes[i] )
            i-=1
        self.legend.Draw()
        
    def Print(self,name="sum1d.png"):
        self.c1.Print(name)
    

# -------------------------------------------------------------------------------------------------------
def stat_histo(stat,volume,file=_current_file):

    """    
    Retrieve the specified statistic histogram from the specified file (defaults
    to the current file) and normalizes histogram to the number of counts.
    """
           
    hname = "h_"+stat+"_"+volume+"_eta" 

    hstats = file.Get(hname)
    hname = "h_counts_"+volume+"_eta"                         # retrive the histograms from the files
    hcount = file.Get(hname)

    if hstats==None:
        print "Warning: Could not find histogram "+hname+" in file "+file.GetName()
        print "... probably going to throw an error here..."
        return
    
    hstats.Divide( hcount )

    # Make hcount unity so that subsequent calls
    # does not change hstats  (means only one stat per run)
    nbinx=hcount.GetNbinsX()
    i=1
    while ( i<=nbinx ):
        hcount.SetBinContent(i,1)
        i+=1
    
    return hstats

# -------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------        
def auto_min( histogram, nbins=20 ):
    """Find the first nonzero entry in the histogam and return a minimum
    """

    nbinx=histogram.GetNbinsX()
    i     = 1
    first = 1
    last  = nbinx

    if ( histogram.Integral() == 0. ):
        return histogram.GetXaxis().GetXmin()

    content = histogram.GetBinContent(i)
    while ( i<nbinx and content == 0 ):
        content=histogram.GetBinContent(i)
        first=i
        i+=1

    xmin=histogram.GetXaxis().GetBinCenter(first)
    xwid=histogram.GetXaxis().GetBinWidth(nbinx/2)
    xmin-=nbins*xwid

    xxmin = histogram.GetXaxis().GetXmin()
    
    return TMath.Max(xmin,xxmin)

def auto_max( histogram, nbins=20 ):
    """Find the first nonzero entry in the histogam and return a maximum
    """

    nbinx=histogram.GetNbinsX()
    i     = nbinx
    first = 1
    last  = nbinx

    if ( histogram.Integral() == 0. ):
        return histogram.GetXaxis().GetXmax()

    content = histogram.GetBinContent(i)
    while ( i>1 and content == 0 ):
        content=histogram.GetBinContent(i)
        last=i
        i-=1

    xmax=histogram.GetXaxis().GetBinCenter(last)
    xwid=histogram.GetXaxis().GetBinWidth(nbinx/2)
    xmax+=nbins*xwid

    xxmax = histogram.GetXaxis().GetXmax()

    return TMath.Min(xmax,xxmax)


class Differential1D:

    """ Base class for displaying 1D differential plots.  

    Produces a plot comparing the baseline geometry (base) to the geometry specified by
    the geom arguement.  The quantity plotted is given by the "stat" option, and is
    limited to the different statistics created by the StarBASE application.  At present,
    these include:

    stat="radlen"  :: plots number of radiation lengths of material encountered
                      ... need to add more stats

    The plot will show a solid histogram for the baseline geometry, with the comparison
    geometry shown with red hashes.  The fractional difference (baseline-comp)/baseline
    is shown at the bottom in blue, scaled according to the alternate axis at the right.

    base:       selects the baseline geometry.                     [Mandatory]
    geom:       selects the comparison geometry.                   [Mandatory]
    volume:     selects the volume to be compared.                 [Mandatory]
    geomvolume: selects the volume in the comparison geometry.     [Default: same]
    stat:       selects the statistic to compare.                  [Default: radlen]
    xmin, xmax: x-axis range.                                      [Optional]
    ymin, ymax: y-axis range.                                      [Optional]
    """

    def __init__(self, 
                 base,                   geom,
                 volume="EMSS",          geomvolume="same",
                 stat="radlen",                    
                 xmin=+0.,                xmax=-1.,
                 ymin=+0.,                ymax=-1.,
                 canvas=0,
                 legend=False
                 ):

        self.name = volume
        self.base = base
        self.geom = geom

        baseFile = get_geom_file(base)                                       # get the files
        compFile = get_geom_file(geom)

        hname = "h_"+stat+"_"+volume+"_eta"                             # retrive the histograms from the files
        print "Get histo: " + str(hname)


        self.histo_base = stat_histo( stat, volume, file=baseFile )
        if ( geomvolume == "same" ):
            self.histo_comp = stat_histo( stat, volume, compFile )
        else:
            self.histo_comp = stat_histo( stat, geomvolume, compFile )

        self.histo_diff = self.histo_base.Clone( hname + "_diff" )      # difference the histograms
        self.histo_diff.Add( self.histo_comp, -1.0 )
        self.histo_diff.Divide(self.histo_base)                         # diff will be (old-new)/old * 100%

        if ( canvas == 0 ):
            canvas = TCanvas("temp"+base+geom+volume+geomvolume,"Differential 1D: baseline="+base+" compare="+geom,500,400);
            self.canvas = canvas

        # Detect and apply optional x-axis range
        if ( xmax > xmin ):
            self.histo_base.GetXaxis().SetRangeUser(xmin,xmax)
            self.histo_comp.GetXaxis().SetRangeUser(xmin,xmax)
        # ... or zoom in on an appropriate scale
        else:
#            auto_range( self.histo_base )
#            auto_range( self.histo_comp )
            xmin = auto_min( self.histo_base )
            xmax = auto_max( self.histo_base )
            self.histo_base.GetXaxis().SetRangeUser(xmin,xmax)
            self.histo_comp.GetXaxis().SetRangeUser(xmin,xmax)

#        xmin = TMath.Min( self.histo_base.GetXaxis().GetXmin(),self.histo_comp.GetXaxis().GetXmin() )
#        xmax = TMath.Max( self.histo_base.GetXaxis().GetXmax(),self.histo_comp.GetXaxis().GetXmax() )

#        print "xmin="+str(xmin)
#        print "xmax="+str(xmax)

        # Detect and apply optional y-axis range
        if ( ymax > ymin ):
            self.histo_base.GetYaxis().SetRangeUser(ymin,ymax)
        else:
            ymin = self.histo_base.GetMinimum()
            ymax = TMath.Max( self.histo_base.GetMaximum(),
                              self.histo_comp.GetMaximum())
            ymax *= 1.05

        # Current range in y-axis extends from 0 to ymax.  We want
        # to expand this to go from -0.2*ymax to ymax.
        ymin = -0.2 * ymax
        self.histo_base.GetYaxis().SetRangeUser(ymin,ymax)

        # Draw the baseline and comparison histograms
        self.histo_base.SetLineWidth(2)
        self.histo_base.SetFillStyle(1001)
        self.histo_base.SetFillColor(22)
        self.histo_base.Draw()

        self.histo_comp.SetLineColor(2)
        self.histo_comp.SetLineStyle(2)
        self.histo_comp.SetFillStyle(3345)
        self.histo_comp.SetFillColor(2)
        self.histo_comp.Draw("same")

        # Rescale difference histogram so that it is in percent
        self.histo_diff.Scale(100.0)

        # This is the maximum of the histogram.
        yfull_max = self.histo_diff.GetMaximum()
        yfull_min = self.histo_diff.GetMinimum()
        yfull = TMath.Max( yfull_max, TMath.Abs( yfull_min ) )
        self.max_differential = yfull

        yfull *= 1.3

        if ( yfull == 0. ):
            yfull = 1.0

        # We need to rescale the histogram so that it fits w/in 10% of ymax
        self.histo_diff.Scale( 0.10 * ymax / yfull )

        # Next we shift the histogram down by 0.1 * ymax
        nbinx=self.histo_diff.GetNbinsX();
        i=1
        while ( i<=nbinx ):
            self.histo_diff[i] -= 0.1 * ymax
            i+=1
        
        # Reset the line color and draw on the same plot
        self.histo_diff.SetLineColor(4)
        self.histo_diff.Draw("same")

        self.line = TLine(xmin, -0.1 * ymax, xmax, -0.1*ymax )
        self.line.SetLineStyle(3)
        self.line.Draw()

        # And superimpose an axis on the  new plot
        xa = xmax

        self.axis = TGaxis( xa, -0.2*ymax, xa, 0.0, -yfull, +yfull, 50510, "-+L" )
        self.axis.SetLabelSize(0.03)
        self.axis.SetLabelOffset(-0.02)
        self.axis.SetLineColor(4)
        self.axis.SetTextColor(4)
        self.axis.SetLabelColor(4)
        self.axis.SetNdivisions(4)
#       self.axis.SetTitle("(base-comp)/base [%]")
        self.axis.SetTitleSize(0.0175)
        self.axis.SetTitleOffset(-0.5)
        self.axis.Draw();

        # Add the legend if requested
        if ( legend ):
            self.legend = TLegend( 0.78, 0.80, 0.98, 0.98 )
            self.legend.AddEntry( self.histo_base, base )
            self.legend.AddEntry( self.histo_comp, geom )
            self.legend.AddEntry( self.histo_diff, "#frac{"+base+"-"+geom+"}{"+base+"} [%]" )
            self.legend.Draw()





# -------------------------------------------------------------------------------------------------------
            
# -------------------------------------------------------------------------------------------------------
class Differential:

    """
    base:       selects the baseline geometry file                 [Mandatory]
    comp:       selects the comparison geometry file               [Mandatory]
    basegeo:    selects the geometry name, if different than base  [Optional]
    compgeo:    selects the geometry name, if different than comp  [Optional]
    top:        selects the top level geometry                     [Default: CAVE]
    xmin,xmax:  selects the x range                                [Optional]
    canvas:     An iterable python canvas.                         [Optional]
    """

    def __init__(self,base,comp,basegeo="same",compgeo="same", top="CAVE", xmin=0., xmax=-1.,canvas=0, stat="radlen" ):

        # Baseline and comparison geometries
        baseFile = get_geom_file(base)
        compFile = get_geom_file(comp)

        if ( basegeo == "same" ):
            basegeo = base

        if ( compgeo == "same" ):
            compgeo = comp
            
        #
        # Create a histogram to store the maximal deviations
        #
        self.hmax = TH1F("hmax","Maximum differential;;(base-comp)/base [%]",1,0.,1.);         self.hmax.SetBit( TH1.kCanRebin )
        self.hmax.SetFillColor(44)
        self.hmax.SetFillStyle(1)

        #
        # Get the list of toplevel volumes from baseline geometry
        # and generate a differential plot for each of them
        #
        baseFile.cd()

        baseGeometry = baseFile.Get(basegeo)
        self.top_volume = baseGeometry.FindVolumeFast( top )

        self.volumes       = {}
        self.missing       = {}
        self.differentials = []

        self.volumes_base  = {}
        self.volumes_comp  = {}

        #print "========================================================================="
        mytitle = "Geometry differential base="+base+" comparison="+comp+" for volume="+top
        #print mytitle

        if ( canvas == 0 ):
            canvas = CanvasPDF(name="geometry_differential_"+top,title=mytitle, thumbnail=True )

        #
        # Front page is a single histogram of the top volume
        #
        #print "Page 1"
        print "Page 1: statistic = " + stat
        d = Differential1D( base=base,
                            geom=comp,
                            volume=top,
                            xmin=xmin,
                            xmax=xmax,
                            canvas=canvas.next(),
                            legend=True,
                            stat=stat
                            )
                          
        
        self.hmax.Fill( d.name, d.max_differential )
        self.differentials.append(d)

        #
        # Second page we print summary informtion about the 2 geometries
        #
        if ( 0 ):
            #print "Page 2"
            sbase = Summary( base, volume=top )
            scomp = Summary( comp, volume=top )
            canvas.divide(1,2)
            sbase.Canvas( canvas.next() )
            scomp.Canvas( canvas.next() )
                            
        canvas.divide(2,2)

        #
        # Next series of pages we iterate through the nodes one level below
        # the top node requested by the user.  Because TGeoManager is a
        # braindead piece of crap with every object in global scope, we
        # have to read it back in from the file (and hope that it behaves
        # as expected).
        #

        baseGeometry = baseFile.Get(basegeo)
        self.top_volume = baseGeometry.FindVolumeFast( top )

        #print "Page 3"

        for node in self.top_volume.GetNodes():

            volume = node.GetVolume()
            name   = volume.GetName()

            #print "  adding volume="+name

            try:
                lookup = self.volumes[ name ]

            except KeyError:

                self.volumes[ name ] = name
                self.volumes_base[ name ] = name      # builds list of volumes in baseline geometry

                #print "++ adding "+name+" to baseline list"


        # Reset to comparison geometry
        compGeometry = compFile.Get(compgeo)
        if compGeometry==None:
            print "Warning: could not find geometry "+compgeo+" in file "+compFile.GetName()
            print "... probably going to throw an error now"

        self.top_volume = compGeometry.FindVolumeFast( top )

        for node in self.top_volume.GetNodes():

            volume = node.GetVolume()
            name   = volume.GetName()

            try:
                lookup = self.volumes_comp[ name ]

            except KeyError:
                self.volumes[ name ]      = name
                self.volumes_comp[ name ] = name       # builds list of volumes in comparison geometry

                #print "++ adding "+name+" to comparison list"

        mystat=stat

        #
        # For every volume in the base geometry we check to see if it is present
        # in the comparison geometry.  If it is not, we add it to a dictionary
        # of missing volume names.  Then we check to see if the volume is in
        # the missing list.  If it is not, we create the differential.
        #
        for name,stat in self.volumes_base.iteritems():

            try:
                volume = compGeometry.FindVolumeFast( name ).GetName()

            except AttributeError:

                print "Volume "+name+" is missing in "+compgeo
                self.missing[ name ] = name

            try:
                lookup = self.missing[ name ]

            except KeyError:

                d = Differential1D( base=base,
                                    geom=comp,
                                    volume=name,
                                    canvas=canvas.next(),
                                    xmin=xmin,
                                    xmax=xmax,
                                    stat=mystat)
                
                self.hmax.Fill( d.name, d.max_differential )
                self.differentials.append(d)


        # At this point, we have three dictionaries:
        # self.volumes      contains the union of volumes in comp and base
        # self.volumes_base contains just the baseline volumes
        # self.volumes_comp contains just the comparison volumes

        canvas.divide(1,2)

        ipad = canvas.next()
        ipad.cd()
        
        self.pave   = TPaveText(0.05,0.05,0.95,0.95)
        self.pave.SetTextFont(82)
        self.pave.SetTextAlign(11)
                        
        self.pave.AddText( "VOLUME\t"+base+"\t"+comp )
        self.pave.AddText( "==================================================" )
        for name,stat in self.volumes.iteritems():

            base_stat = "X"
            comp_stat = "X"

            try:
                lookup = self.volumes_base[ name ]
            except KeyError:
                base_stat = " "

            try:
                lookup = self.volumes_comp[ name ]
            except KeyError:
                comp_stat = " "

            self.pave.AddText( name + "\t    " + base_stat + "\t    " + comp_stat )

        self.pave.AddText("") # pad the bottom
        self.pave.AddText("")
        self.pave.Draw()
                        
                
#        print self.missing

        if ( 1 ):
            canvas.next()
            set_hmax_range( self.hmax )
            self.hmax.Draw()

        #print "Reset canvas"
        canvas.divide(1,1) # 

        #print "========================================================================="


# -------------------------------------------------------------------------------------------------------
def set_hmax_range( hmax ):

    axis = hmax.GetXaxis()
    bin = 1
    xmax = 0.0
    while ( bin < axis.GetNbins() ):
        xbin  = axis.GetBinCenter(bin)
        label = axis.GetBinLabel(bin)
        if ( label != '' ):
            #print label+" "+str(xbin)
            xmax = xbin+0.5
        bin += 1
    if ( xmax > 0. ):
        hmax.GetXaxis().SetRangeUser( 0., xmax )
    
# -------------------------------------------------------------------------------------------------------


class Diff1D:
    
    def __init__(self,geom1="y2006g",geom2="y2006h",
                 vol1="ECAL",vol2="ECAL",stat="radlen",
                 xmin=0.,xmax=-1.,
                 ymin=0.,ymax=-1.,
                 opt="", color=0, marker=0, offset=0.0
                 ):

        # Attach the files
        self.file1 = TFile( geom1 + ".root" )
        self.file2 = TFile( geom2 + ".root" )
        file1=self.file1
        file2=self.file2
        file1.ls()

        self.hA = file1.Get("h_"+stat+"_"+vol1+"_eta");
        counts = file1.Get("h_counts_"  +vol1+"_eta");
        self.hA.Divide(counts)

        self.hB = file2.Get("h_"+stat+"_"+vol2+"_eta");
        counts = file2.Get("h_counts_"  +vol2+"_eta");
        self.hB.Divide(counts);

        vg1 = geom1+"::"+vol1
        vg2 = geom2+"::"+vol2
        title = "Radlen differential "+vg1+" - "+vg2

        if ( opt != "SAME" and opt != "same" ):
            self.c3 = TCanvas( "canvas_"+vg1+"_diff_"+vg2, title,700,375)        

        self.hD=self.hA.Clone("h_"+stat+"_"+vol1+"_eta_diff")

        if ( offset != 0.0 ):
            nbin = self.hD.GetNbinsX()
            ibin = 1
            while ( ibin <= nbin ):
                y = self.hD.GetBinContent(ibin)
                y += offset
                self.hD.SetBinContent(ibin,y)
                ibin+=1

        self.hD.Add(self.hB,-1.0)
        if ( xmax > xmin ):
            self.hD.GetXaxis().SetRangeUser(xmin,xmax)
        if ( ymax > ymin ):
            self.hD.GetYaxis().SetRangeUser(ymin,ymax)
        self.hD.SetTitle( title )

        if ( color ):
            self.hD.SetLineColor( color )

        if ( marker ):
            self.hD.SetMarkerStyle( marker )

        if ( opt != "SAME" and opt != "same" ):        
            self.hD.Draw()
        else:
            if ( marker ):
                self.hD.Draw("p,same")
            else:
                self.hD.Draw("same")

    def Print(self,name="sum1d.png"):
        self.c3.Print(name)

    def integral(self):
        y = self.hD.Integral(0,9999)
        print "Integral = " + str(y)
        
# -------------------------------------------------------------------------------------------------------

class Diff2D:

    def __init__(self,geom1="y2006g",geom2="y2006h",stat="radlen", xmin=0.,xmax=-1.,ymin=0.,ymax=-1.,canvas=0):

        # Attach the files
        self.file1 = TFile( geom1 + ".root" )
        self.file2 = TFile( geom2 + ".root" )
        file1=self.file1
        file2=self.file2
        file1.ls()

        # Retrieve histogram and normalize
        self.hA = file1.Get("h_" + stat + "_rz")
        counts  = file1.Get("h_ncount_rz")
        self.hA.Divide(counts)

        # Retrieve historagm and normalize
        self.hC = file2.Get("h_" + stat + "_rz")
        counts  = file2.Get("h_ncount_rz")
        self.hC.Divide(counts)

        # Construct the difference histogram
        self.hDiff = self.hA.Clone("h_diff_" + stat + "_rz" );
        self.hDiff.Add(self.hC,-1.0);

        if ( xmax > xmin ):
            self.hA.GetXaxis().SetRangeUser( xmin, xmax )
            self.hC.GetXaxis().SetRangeUser( xmin, xmax )
            self.hDiff.GetXaxis().SetRangeUser( xmin, xmax )

        if ( ymax > ymin ):
            self.hA.GetYaxis().SetRangeUser( ymin, ymax )
            self.hC.GetYaxis().SetRangeUser( ymin, ymax )
            self.hDiff.GetYaxis().SetRangeUser( ymin, ymax )

        # Create canvases and draw histograms if appropriate
        if ( canvas == 0 ):

            set_palette("color", id=1)    
            self.c1 = TCanvas( geom1+"_"+stat,geom1,700,375)
            self.hA.Draw("colz")
            self.c1.Print( geom1+"_"+stat+".png" );
            self.c2 = TCanvas( geom2+"_"+stat,geom2,700,375)
            self.hC.Draw("colz")
            self.c2.Print( geom2+"_"+stat+".png" );

            self.c3 = TCanvas( "diff_"+stat,geom1+" minus "+geom2,700,375)
            set_palette("diffs", id=2)
            max = self.hDiff.GetMaximum()
            max = max * 1.05
            self.hDiff.GetZaxis().SetRangeUser(-1.0*max,+1.0*max)
            self.hDiff.Draw("colz")
            set_palette("diffs", id=2)
            self.c3.Update()
            self.c3.Print( geom1+"_"+geom2+"_"+stat+"_diff.png");

        # Assume external canvas
        else:

            # We need to trick root into displaying histograms
            # with different palettes.  This is done through
            # a complete hack to hide ROOT's poor oo design...
            # Use TExec to "continually update" the three pads
            # in the master canvas with our desired palette.

            pal1 = TExec("pal1","TPython::Exec(\"from diffs import *; set_color()\")")             
            self.pad1 = canvas.cd(1)  # assume divided into 3 pads
            self.hA.UseCurrentStyle()
            self.hA.Draw("col")
            pal1.Draw("same")
            self.hA.Draw("colz same")
            self.palette1 = pal1


            pal2 = TExec("pal2","TPython::Exec(\"from diffs import *; set_color()\")")
            self.pad2 = canvas.cd(2)
            self.hC.Draw("col")
            pal2.Draw("same")
            self.hC.Draw("colz same")
            self.palette2 = pal2  # make it persistent


            pal3 = TExec("pal3","TPython::Exec(\"from diffs import *; set_diffs()\")")
            self.pad3 = canvas.cd(3)
            max = 1.05 * self.hDiff.GetMaximum()
            self.hDiff.GetZaxis().SetRangeUser(-1.0*max,+1.0*max)
            self.hDiff.Draw("col")
            pal3.Draw("same")
            self.hDiff.Draw("colz same")
            self.palette3 = pal3

# -------------------------------------------------------------------------------------------------------
def mass( value ):

    if ( value >= 100.0 ):
        return '%(value)0.2f tons' % { 'value' : value/1000.0 }

    if ( value < 100.0 and value > 0.1 ):
        return '%(value)0.2f kg' % { 'value' : value }

    if ( value <= 0.1 ):
        return '%(value)0.2f g' % { 'value' : value*1000.0 }

class Summary:

    def __init__( self, geom, name="same", volume="CAVE" ):

        if ( name == "same" ):
            name = geom

        file    = get_geom_file( geom )
        manager = file.Get(name)

        top = manager.FindVolumeFast(volume)

        self.geom     = geom
        self.name     = volume
        self.weightA  = top.Weight( 0.005, "a" )
        self.weightS  = top.Weight( 0.005, "s" )
        self.shape    = top.GetShape()
        self.material = top.GetMaterial()
        self.medium   = top.GetMedium()
        self.daughters = {}

        for node in top.GetNodes():
            volu  = node.GetVolume()
            vname = volu.GetName()
            try:
                self.daughters[ vname ] += 1
            except KeyError:
                self.daughters[ vname ] = 1

        self.lines = []
        self.lines.append( "geometry:         "+self.geom )
        self.lines.append( "==============================================================" )
        self.lines.append( "volume:           "+self.name )
        self.lines.append( "shape:            "+self.shape.Class().GetName() )
        self.lines.append( "material:         "+self.material.GetName() )
        self.lines.append( "medium:           "+self.medium.GetName() )
        if ( 0 ):
            self.lines.append( "mass analytic:    "+mass(self.weightA) )
            self.lines.append( "mass sampled:     "+mass(self.weightS) )
        self.lines.append( "" )
        self.lines.append( "daughter volumes: " )
        self.lines.append( " volume     count" )

        for kid,number in self.daughters.iteritems():
            daughter  = manager.FindVolumeFast(kid)
            dshape    = daughter.GetShape().Class().GetName()
            dmaterial = daughter.GetMaterial().GetName()
            dweightA  = daughter.Weight(0.005, "a" )
            dweightS  = daughter.Weight(0.005, "s" )
#            self.lines.append( " + "+kid+"      "+str(number)+" "+dshape+" "+dmaterial+"weight="+mass(dweightA)+" or "+mass(dweightS)+" kg" )

            mydict = { 'kid':      kid,
                       'shape':    daughter.GetShape().Class().GetName(),
                       'material': daughter.GetMaterial().GetName(),
                       'massa':    mass( daughter.Weight(0.005, "a") ),
                       'masss':    mass( daughter.Weight(0.005, "s") ) }

            format = '%(kid)-8s %(shape)-12s %(material)-14s %(massa)-9s %(masss)-9s'

            self.lines.append( format % mydict )





    def Print(self,filename=0,opts='w'):

        if ( filename == 0 ):
            for line in self.lines:
                print line

        else:
            file = open(filename, opts)
            for line in self.lines:
                file.write(line+'\n')

    def Canvas(self,canvas=0):

        if ( canvas == 0 ):
            self.canvas = TCanvas(self.geom+"_"+self.name+"_summary","",850*2/3,1100*2/3)
        else:
            self.canvas = canvas

        self.canvas.cd()
        self.pave   = TPaveText(0.05,0.05,0.95,0.95)
        self.pave.SetTextFont(82)
        self.pave.SetTextAlign(11)
        for line in self.lines:
            self.pave.AddText( line )
        self.pave.AddText("") # pad the bottom
        self.pave.AddText("")
        self.pave.Draw()

# -------------------------------------------------------------------------------------------------------



if __name__ == '__main__':

#    ecal = Differential1D( base="y2009_baseline1", geom="y2009a_baseline1", volume="ECAL", xmin=1.086-0.2, xmax=2.2 )
#    calb = Differential1D( base="y2009_baseline1", geom="y2009a_baseline1", volume="CALB", xmin=-1.2,xmax=+1.2 )
#    tpce = Differential1D( base="y2009_baseline1", geom="y2009a_baseline1", volume="TPCE" )
#    help(Differential1D)


#    iter = CanvasHydra(title="Geometry Differentials",nx=2,ny=2)
#    cave = Differential1D(base="y2009_baseline1",geom="y2009a_baseline1",volume="CAVE",xmin=-5.0,xmax=+5.0,canvas=iter.next() )
#    magp = Differential1D(base="y2009_baseline1",geom="y2009a_baseline1",volume="MAGP",xmin=-2.5,xmax=+2.5,canvas=iter.next() )
#    tpce = Differential1D(base="y2009_baseline1",geom="y2009a_baseline1",volume="TPCE",xmin=-1.5,xmax=+1.5,canvas=iter.next() )
#    bbcm = Differential1D(base="y2009_baseline1",geom="y2009a_baseline1",volume="BBCM",xmin=-5.0,xmax=+5.0,canvas=iter.next() )
#    ecal = Differential1D(base="y2009_baseline1",geom="y2009a_baseline1",volume="ECAL",xmin=+0.9,xmax=+2.2,canvas=iter.next() )
#    calb = Differential1D(base="y2009_baseline1",geom="y2009a_baseline1",volume="CALB",xmin=-1.1,xmax=+1.1,canvas=iter.next() )
#    zcal = Differential1D(base="y2009_baseline1",geom="y2009a_baseline1",volume="ZCAL",xmin=-6.0,xmax=+6.0,canvas=iter.next() )



    if ( 1 ):
        cave = Differential( base="y2009_baseline1",  basegeo="y2009",
                             comp="y2009a_baseline1", compgeo="y2009a", 
                             top="CAVE")

    if ( 0 ):
        emss = Differential( base="y2009_baseline1",  basegeo="y2009",
                             comp="y2009a_baseline1", compgeo="y2009a", 
                             top="EMSS", xmin=0.9,xmax=2.25 )

    if ( 0 ):
        tpce = Differential( base="y2009_baseline1",  basegeo="y2009",
                             comp="y2009a_baseline1", compgeo="y2009a", 
                             top="TPCE", xmin=-3.0,xmax=+3.00 )

    if ( 0 ):
        calb = Differential( base="y2009_baseline1",  basegeo="y2009",
                             comp="y2009a_baseline1", compgeo="y2009a", 
                             top="CALB", xmin=-1.5, xmax=+1.5 )

    if ( 0 ):
        chlv = Differential( base="y2009_baseline1",  basegeo="y2009",
                             comp="y2009a_baseline1", compgeo="y2009a", 
                             top="CHLV", xmin=-1.5, xmax=+1.5 )

    if ( 0 ):
        cphi = Differential( base="y2009_baseline1",  basegeo="y2009",
                             comp="y2009a_baseline1", compgeo="y2009a", 
                             top="CPHI", xmin=-1.5, xmax=+1.5 )


#    tswh = Differential( base="y2009_baseline1",  basegeo="y2009",
#                         comp="y2009a_baseline1", compgeo="y2009a", top="TSWH", xmin=-3.0,xmax=+3.00 )
























