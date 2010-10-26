from ROOT import TCanvas
from ROOT import TPad
from ROOT import TPaveText
from ROOT import TStyle, gStyle
from ROOT import TColor, kWhite
from ROOT import TPDF, TPostScript

# ----------------------------------------------------------------------------------------------------
class CanvasIter(TCanvas):
    """A root canvas with title and active pads.  The active pads may be iterated.
    """

    def __init__(self,name,title,wx=850*2/3,wy=1100*2/3,nx=0,ny=1):
        
        TCanvas.__init__(self,name,title,wx,wy)

        self.name     = name
        self.title    = title
        self.wx       = wx
        self.wy       = wy
        self.nx       = nx
        self.ny       = ny

        # Divide this canvase into a 'title' and 'active' area
        self.Divide(1,2)
        self.title_pad = self.cd(1)
        self.active_pad = self.cd(2)
        self.title_pad.SetPad(0.00,0.93,1.00,1.00)
        self.active_pad.SetPad(0.00,0.00,1.00,0.93)

        # In the title pad, draw a TPaveText with the title
        self.title_pad.cd()
        self.pave_title = TPaveText( 0.015, 0.075, 0.985, 0.925, "r" )
        self.pave_title.AddText(0.5,0.85, title)
        self.pave_title.Draw()

        # Divide the active pad, if nx>0
        self.active_pad.cd()
        if ( nx ):
            self.active_pad.Divide(nx,ny)

        self.current = 0         # currently active pad


    def divide(self,nx,ny):
        
        self.active_pad.Clear()
        self.active_pad.Divide(nx,ny)
        self.nx = nx
        self.ny = ny
        self.current = 0

    def __iter__(self):
        return self

    def next(self):
        
        pad = self.active_pad
        if ( self.nx ):
            self.current += 1
            if ( self.current > self.nx*self.ny ):
                raise StopIteration
                return 0
            pad = pad.cd( self.current )

        return pad

# ----------------------------------------------------------------------------------------------------
class CanvasHydra:

    def __init__(self,name="hydra",title="",wx=850*2/3,wy=1100*2/3,nx=0,ny=1,maxheads=10):

        self.list = []
        self.list.append( CanvasIter(name,title,wx,wy,nx,ny) )
        self.canvas = self.list[ len(self.list) - 1 ]
        
        self.name  = name
        self.title = title
        self.wx    = wx
        self.wy    = wy
        self.nx    = nx
        self.ny    = ny

        self.maxheads=maxheads

    def next(self):

        # Get the current canvas
        current = self.list[ len(self.list) - 1 ]

        # Get the next pad from the current canvas
        try:
            pad = current.next()
            return pad

        # If we run into a stop iteration we spawn a
        # new canvas and return the next(first) pad
        # within it
        except StopIteration:

            myname  = self.name + "_" + str(len(self.list))
            mytitle = self.title
            self.list.append( CanvasIter(myname,mytitle,self.wx,self.wy,self.nx,self.ny) )

            return self.next()

    def divide(self,nx,ny):
        self.canvas.divide(nx,ny)
        self.nx = nx
        self.ny = ny

# ----------------------------------------------------------------------------------------------------
class CanvasLoop:

    def __init__(self,name="looper",title="",wx=850*2/3,wy=1100*2/3,nx=0,ny=1,maxheads=10):

        self.canvas = CanvasIter(name,title,wx,wy,nx,ny)
        
        self.name  = name
        self.title = title
        self.wx    = wx
        self.wy    = wy
        self.nx    = nx
        self.ny    = ny

    def next(self):

        # Return pointer to the current pad
        try:
            pad = self.canvas.next()
            return pad

        # When we're at the end, clear all pads,
        # reset and try again
        except StopIteration:
            self.canvas.current=0
            for pad in self.canvas:
                pad.Clear()
            self.canvas.current=0
            return self.next()

    def divide(self,nx,ny):
        self.canvas.divide(nx,ny)


# ----------------------------------------------------------------------------------------------------
class CanvasPDF:

    def __init__(self,name="canvaspdf",title="",wx=850*2/3,wy=1100*2/3,nx=0,ny=1,maxheads=10,thumbnail=False,thumbnailsize=False):

        self.canvas = CanvasIter(name,title,wx,wy,nx,ny)
        
        self.name  = name
        self.title = title
        self.wx    = wx
        self.wy    = wy
        self.nx    = nx
        self.ny    = ny

        self.count = 0
        self.post  = "("     # append to the ps name
        self.thumbnail = thumbnail
        self.thumbnailsize = thumbnailsize

    def __del__(self):

        # On deletion, finish off the printing
        self.canvas.Update()                                           # may need if multiple canvases are present
        self.canvas.Print( "/tmp/" + self.name + ".ps)" )              # export canvas to a .ps file
        import os                                                      
        os.system("ps2pdf /tmp/"+self.name+".ps "+self.name+".pdf")    # and convert to pdf file
        os.system("rm /tmp/*.ps")

    def next(self):

        # Return pointer to the current pad
        try:
            pad = self.canvas.next()
            return pad

        # When we're at the end
        except StopIteration:

            # 1) Add the current version of the canavs to the PDF
            self.canvas.Print( "/tmp/"+self.name + ".ps" + self.post )
            self.post = ""  # future pages are added, not overwritten
            self.count += 1

            # 2) If user selected thumbnail, then on first call we
            #    create a .png file and set thumbnail = False
            if ( self.thumbnail ):

                print "Creating thumbnail"
                
                # Resize canvas to 1/2 normal
                self.canvas.SetCanvasSize( self.wx*2/3, self.wy*2/3 )
                self.canvas.Print(self.name+".png")
                self.thumbnail = False
                # Reset to default size
                self.canvas.SetCanvasSize( self.wx, self.wy )

            

            # 2) Clear the canvas for the next iteration
            self.canvas.current=0
            for pad in self.canvas:
                pad.Clear()
            self.canvas.current=0

            # 3) Return the first pad in the reset canvas
            return self.next()


    def divide(self,nx,ny):

        # 1) If user selected thumbnail, then on first call we
        #    create a .png file and set thumbnail = False
        if ( self.thumbnail ):

            print "Creating thumbnail"
            
            # Resize canvas to 1/2 normal
            self.canvas.SetCanvasSize( self.wx*2/3, self.wy*2/3 )
            self.canvas.Print(self.name+".png")
            self.thumbnail = False
            # Reset to default size
            self.canvas.SetCanvasSize( self.wx, self.wy )

        # 2) Add the canvas to the .ps file
        self.canvas.Print( "/tmp/"+self.name+".ps" + self.post )
        self.post = ""              
        self.canvas.divide(nx,ny)
            
# ----------------------------------------------------------------------------------------------------

if __name__ == '__main__':

    gStyle.SetCanvasBorderMode(0); 
    gStyle.SetFrameBorderMode(0); 
    gStyle.SetCanvasColor(kWhite);

    if ( 0 ):
        canvas = CanvasIter(name="canvas",title="title",nx=2,ny=2)
        for pad in canvas:
            pad.cd()
            print "got a pad"

    if ( 0 ) :
        h = CanvasHydra(name="hydra",title="a canvas hydra",nx=2,ny=2)
        p = h.next()
        i = 0
        while ( i < 10 ):
            p = h.next()
            i += 1

    if ( 1 ):

        from ROOT import TH1F

        l = CanvasPDF(title="a canvas PDF",nx=2,ny=2)
        histos = []
        for i in [0,1,2,3,4,5,6,7,8,9,20,40,60]:
            histo = TH1F("h"+str(i),"h"+str(i),10,0.,1.)
            histos.append( histo )
            l.next()
            histo.Draw()

