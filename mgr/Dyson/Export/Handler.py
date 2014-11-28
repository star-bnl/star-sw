class Handler:

    # Reference to the handler which contains this one
    parent = None

    def __init__(self):
        self.parent = None

    def startElement(self, tag, attr ):
        pass

    def characters( self, content ):
        pass

    def endElement( self, tag ):
        pass

    def setParent( self, p ):
        self.parent = p
