<!DOCTYPE CW><CW>
<customwidgets>
    <customwidget>
        <class>TQtGLViewerWidget</class>
        <header location="global">TQtGLViewerWidget.h</header>
        <sizehint>
            <width>400</width>
            <height>300</height>
        </sizehint>
        <container>0</container>
        <sizepolicy>
            <hordata>7</hordata>
            <verdata>7</verdata>
        </sizepolicy>
        <pixmap>
            <data format="PNG" length="807">89504e470d0a1a0a0000000d494844520000000f000000100806000000c9562504000002ee49444154789c75935d6893671886aff7cb57f3675263da343392b5b6ebfa67b5b3365f253aff05713026db18839d0c87a84c264ecf76b0a3e1c1041171a3634cb4e089c51f90d1ea8694f86ad475f527b623b6d4c5b5a62d6d4dd3a649be7707a10575bb8fef8be781e77a442412e1ff52575dadba4efd44456d2d4dbb7721a5144628a4525d1d589696a2bd0a3437af5581b272d5b4ba4975b59fe7eeb90bf45fea2437330b403ad2c9f87707c95cbf58800dc350a190a100623d4fb87826c2f367c3346e584febf66d6853b3e4335900ccc40045c904dae0e302fce0e1df4c4ea6686d6d557ff526b1db3d385d0eaa56adc4ebf3e3b03bd18a7400ac6501ec8bac583269b4c5de66f5eb6ffde8ba203b9723f1244765ed32dc9e62542ecfdce8142e7f19bacd0a8096cf532404bad58136322ed814aec4e9b433149f623ca9a86a2841d717919998627a28c9d2ea15dc8ade16008c0da39b26165f007d6bc8423aedc334059d1d4f09bee5c1ebb321a5146fe66c6a6e224dc9ea1a524c631886cadf388f2634b4ca7a34054819a3edc46d7aa249d66d7f0304845a5ad4d0b528767f099e9af2c229529310fb03e1f620eade4193528abed8086da76ed2142e6679851d5329c61e0df0b4bb97c0c6355897b800507dbda8780c2a6a1055f5e800e1f50d905b4c7da34926334bf6c50c7fb65d46d8ac04771805395ad6aadcb1c398d32fb0185bc1ed29c0a97442ec3d1856ed277f61e4511f33f78748dd8bb3eec087b8837e78368819eb217bfd32a23440d1c6f790528a05c3a4bc255c2e1757be3f47bcfb3ec1cd6b78fb837791528a50439d4aff7c9c6c72186df3fb8815b50085c9f309efdcc4e8c341661209c2873ee1de835e116a5ca9267e3cc6dcef57b155d4a07fb407198d0ae065b7fbe2fde2d36ff7e328c9d073f6078263ffa8816fbe64f4ec69723627d63d47d1caab17fae2bfbeca31f65cdd387280e5e62ca564f1967a59b6f76bdc1f7f8e8cde11f33dfd3512a8dfb683ecbeaf4876b4e3f4fbf07ff605ee2dbb16d69dcfbfa4a425ef358fc3310000000049454e44ae426082</data>
        </pixmap>
        <signal> animateNeeded() </signal>
        <signal> axisIsDrawnChanged(bool) </signal>
        <signal> cameraIsEditedChanged(bool) </signal>
        <signal> drawFinished(bool) </signal>
        <signal> drawNeeded() </signal>
        <signal> FPSIsDisplayedChanged(bool) </signal>
        <signal> gridIsDrawnChanged(bool) </signal>
        <signal> helpRequired() </signal>
        <signal> mouseGrabberChanged(qglviewer::MouseGrabber *mouseGrabber) </signal>
        <signal> pointSelected(QMouseEvent *e) </signal>
        <signal> stereoChanged(bool) </signal>
        <signal> textIsEnabledChanged(bool) </signal>
        <signal> viewerInitialized() </signal>
        <signal> zBufferIsDisplayedChanged(bool) </signal>
        <slot access="public"> aboutQGLViewer() </slot>
        <slot access="public"> animate() </slot>
        <slot access="public"> copyBufferToTexture(GLint, GLenum format) </slot>
        <slot access="public"> help() </slot>
        <slot access="public"> initFromDOMElement(QDomElement &amp;element) </slot>
        <slot access="public"> openSnapshotFormatDialog() </slot>
        <slot access="public"> resize(int, int) </slot>
        <slot access="public"> restoreStateFromFile() </slot>
        <slot access="public"> saveSnapshot(bool, bool) </slot>
        <slot access="public"> saveSnapshot(QString, bool) </slot>
        <slot access="public"> saveStateToFile() </slot>
        <slot access="public"> select(QMouseEvent *event) </slot>
        <slot access="public"> select(QPoint) </slot>
        <slot access="public"> setAddKeyFrameStateKey(int) </slot>
        <slot access="public"> setAnimationPeriod(int) </slot>
        <slot access="public"> setAxisIsDrawn(bool) </slot>
        <slot access="public"> setBackgroundColor(QColor &amp;color) </slot>
        <slot access="public"> setCameraIsEdited(bool) </slot>
        <slot access="public"> setCamera(qglviewer::Camera *camera) </slot>
        <slot access="public"> setForegroundColor(QColor &amp;color) </slot>
        <slot access="public"> setFPSIsDisplayed(bool) </slot>
        <slot access="public"> setFullScreen(bool) </slot>
        <slot access="public"> setGridIsDrawn(bool) </slot>
        <slot access="public"> setHandlerStateKey(MouseHandler handler, int) </slot>
        <slot access="public"> setKeyDescription(int, QString) </slot>
        <slot access="public"> setManipulatedFrame(qglviewer::ManipulatedFrame *frame) </slot>
        <slot access="public"> setMouseBindingDescription(int, QString, bool) </slot>
        <slot access="public"> setMouseBinding(int, ClickAction action, bool, int) </slot>
        <slot access="public"> setMouseBinding(int, MouseHandler handler, MouseAction action, bool) </slot>
        <slot access="public"> setMouseGrabber(qglviewer::MouseGrabber *mouseGrabber) </slot>
        <slot access="public"> setMouseTracking(bool) </slot>
        <slot access="public"> setPathKey(int, int) </slot>
        <slot access="public"> setPlayPathStateKey(int) </slot>
        <slot access="public"> setSceneBoundingBox(qglviewer::Vec &amp;min, qglviewer::Vec &amp;max) </slot>
        <slot access="public"> setSceneCenter(qglviewer::Vec &amp;center) </slot>
        <slot access="public"> setSceneRadius(float) </slot>
        <slot access="public"> setSelectBufferSize(int) </slot>
        <slot access="public"> setSelectedName(int) </slot>
        <slot access="public"> setSelectRegionHeight(int) </slot>
        <slot access="public"> setSelectRegionWidth(int) </slot>
        <slot access="public"> setShortcut(KeyboardAction action, int) </slot>
        <slot access="public"> setSnapshotCounter(int) </slot>
        <slot access="public"> setSnapshotFilename(QString) </slot>
        <slot access="public"> setSnapshotFormat(QString) </slot>
        <slot access="public"> setSnapshotQuality(int) </slot>
        <slot access="public"> setStateFileName(QString) </slot>
        <slot access="public"> setStereoDisplay(bool) </slot>
        <slot access="public"> setTextIsEnabled(bool) </slot>
        <slot access="public"> setWheelBinding(int, MouseHandler handler, MouseAction action, bool) </slot>
        <slot access="public"> setZBufferIsDisplayed(bool) </slot>
        <slot access="public"> showEntireScene() </slot>
        <slot access="public"> startAnimation() </slot>
        <slot access="public"> stopAnimation() </slot>
        <slot access="public"> toggleAnimation() </slot>
        <slot access="public"> toggleAxisIsDrawn() </slot>
        <slot access="public"> toggleCameraIsEdited() </slot>
        <slot access="public"> toggleCameraMode() </slot>
        <slot access="public"> toggleFPSIsDisplayed() </slot>
        <slot access="public"> toggleFullScreen() </slot>
        <slot access="public"> toggleGridIsDrawn() </slot>
        <slot access="public"> toggleStereoDisplay() </slot>
        <slot access="public"> toggleTextIsEnabled() </slot>
        <slot access="public"> toggleZBufferIsDisplayed() </slot>
        <slot access="protected"> setAutoBufferSwap(bool on) </slot>
    </customwidget>
</customwidgets>
</CW>
