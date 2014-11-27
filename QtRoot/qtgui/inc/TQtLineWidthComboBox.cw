<!DOCTYPE CW><CW>
<customwidgets>
    <customwidget>
        <class>TQtGLViewerWidget</class>
        <header location="global">$ROTOTSYS/include/TQtGLViewerWidget.h</header>
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
    <customwidget>
        <class>TQtPatternSelectButton</class>
        <header location="local">TQtPatternSelectButton.h</header>
        <sizehint>
            <width>-1</width>
            <height>24</height>
        </sizehint>
        <container>0</container>
        <sizepolicy>
            <hordata>1</hordata>
            <verdata>0</verdata>
        </sizepolicy>
        <pixmap>
            <data format="XPM.GZ" length="1280">789c75d24b6fa25018c6f1bd9f82c8ce4c6ce56e26b368eba5dadaba9c64320b388252112b2a6a9bf9eec3ff059d48a6ef73bcfcf208e79870d3d27e4e275aeba6b1ddf9bb58696ae1675a6bb65fad4ebf7efff86c340d432b9665699de6b746b3ad294dbf95813368283332143c42b343e04868588e69c050e859a1e9c14818dab619c203b464602eb4091c0b5dc7b65cb81746043e41db700d5b367a13ba6ed7961f2fa1e378a1e3c02d741d02d742cff75c39d53bf42c0257c280c005ec9a04ee842e818150066e84330213e89b044e851e8173a122f0150606812f4297c0895011f80c5587405d681298092d021f850e813da10cbc87b35b023f841681a9d02170200c081c0a15817d6144600c4383c007a14de09db04ba012ca401f461e8127a18c3c93ff1d6974ff3241f9a1578d9a85e51de66aa1e402553571f8b69449e2559c144512574d1a2dd73241fa9ea6413b48d3aad964995ecdb678ad747d5735ed7d9e1f0e87fd21cff952bc1dcfcd492657ffe6ba3970c6727dd49abbe28ce5ba2f9bd3b97928ce58ae5eade9f70783620d87c3c75a33188d9fca7d9eeb4db5cfd372526b7ae526e3e5fae5bac97bd9795e6bd764e53ea3309ad71afdf27fa6b5bb4d2eff27b96e8ef364b3295631c1b9f9ea39f8a2f9f3bdf117251c4f40</data>
        </pixmap>
        <signal>brushSelected(const TQtBrush &amp;)</signal>
        <slot access="public">SetBrush(unsigned int)</slot>
        <slot access="public">SetBrush(const TQtBrush&amp;)</slot>
        <slot access="public">SetStyle(Style_t)</slot>
    </customwidget>
    <customwidget>
        <class>TQtColorSelectButton</class>
        <header location="local">TQtColorSelectButton.h</header>
        <sizehint>
            <width>-1</width>
            <height>24</height>
        </sizehint>
        <container>0</container>
        <sizepolicy>
            <hordata>1</hordata>
            <verdata>0</verdata>
        </sizepolicy>
        <pixmap>
            <data format="XPM.GZ" length="1280">789c75d24b6fa25018c6f1bd9f82c8ce4c6ce56e26b368eba5dadaba9c64320b388252112b2a6a9bf9eec3ff059d48a6ef73bcfcf208e79870d3d27e4e275aeba6b1ddf9bb58696ae1675a6bb65fad4ebf7efff86c340d432b9665699de6b746b3ad294dbf95813368283332143c42b343e04868588e69c050e859a1e9c14818dab619c203b464602eb4091c0b5dc7b65cb81746043e41db700d5b367a13ba6ed7961f2fa1e378a1e3c02d741d02d742cff75c39d53bf42c0257c280c005ec9a04ee842e818150066e84330213e89b044e851e8173a122f0150606812f4297c0895011f80c5587405d681298092d021f850e813da10cbc87b35b023f841681a9d02170200c081c0a15817d6144600c4383c007a14de09db04ba012ca401f461e8127a18c3c93ff1d6974ff3241f9a1578d9a85e51de66aa1e402553571f8b69449e2559c144512574d1a2dd73241fa9ea6413b48d3aad964995ecdb678ad747d5735ed7d9e1f0e87fd21cff952bc1dcfcd492657ffe6ba3970c6727dd49abbe28ce5ba2f9bd3b97928ce58ae5eade9f70783620d87c3c75a33188d9fca7d9eeb4db5cfd372526b7ae526e3e5fae5bac97bd9795e6bd764e53ea3309ad71afdf27fa6b5bb4d2eff27b96e8ef364b3295631c1b9f9ea39f8a2f9f3bdf117251c4f40</data>
        </pixmap>
        <signal>colorSelected(const QColor&amp;)</signal>
        <signal>destroyed(QObject*)</signal>
        <slot access="protected">languageChange()</slot>
        <slot access="public">PopupDialog()</slot>
        <property type="Color">fColor</property>
    </customwidget>
    <customwidget>
        <class>TQtFloatSpinBox</class>
        <header location="local">TQtFloatSpinBox.h</header>
        <sizehint>
            <width>-1</width>
            <height>24</height>
        </sizehint>
        <container>0</container>
        <sizepolicy>
            <hordata>1</hordata>
            <verdata>0</verdata>
        </sizepolicy>
        <pixmap>
            <data format="PNG" length="605">89504e470d0a1a0a0000000d4948445200000016000000160806000000c4b46c3b0000022449444154789ccd943d8bea4014869f48dc45dbd88bb0c6bf2081fc0c6dfd3dfe8814692cacb7d462611102162ab8b0606b3729fc221f7ecc2d640663b22e72b9705f3864e69d9937e7cc39670c40f6fb7d2e970b4208f6fb3dcf62b3d9b0582cf8fafad29c0970b95ce8743a341a8da74515269309aeebea7909400841a3d1e07c3e3f654110e8b1699a991f9500f6fb3de7f399d3e9a4ed16b7bc5a0b8200c7710882000029655e58218aa28cd86eb763b7db65d6a228623a9deab05dd7d59e170a2b4fa228d2760fc5398e93e15dd7cd4559ba3d341e8fb12c0bcbb2188fc74829f9fefe36dedfdf33bc1082300c09c310210442889c13991bef76bb7a53ad56d3e36eb74b18864829351fc731afafaf002449429aa6c5c2524a7cdfa756ab01e0fb3e499200b05eafb12c4b1f52fce170b88a9826c7e3b1583849127abd1eabd58a72b94cbd5e67b55a01d7fbbfe5d55d97cb65bdfe30799ee7f1f6f646bd5e67381cf2f1f181e33872341a69def33c5dbb711c6b535114de71bbdd66b95ceaf966b3310079cfb75a2dee311c0e8b3d8ee398e3f198b16ab52a67b3598e1f0c061991c160f073b9dd77d723b36d1bdff7816b926ddbfe397940ae64140cc3c8b5ac6ddb789e47b3d9244dd3c70da2a032acbe52ca5cd6a594d8b65de888f678bbdd6684e05a7ea6691672a7d349bf66aa39d49b92119ecfe77c7e7e52a9543242699af2f2f2f2a357b7d14ea7d30c6700b278fbdfa1f4fb96ff4d78329918ff42f80f3effcb44bb7727b50000000049454e44ae426082</data>
        </pixmap>
        <signal>valueChanged(const QString&amp;)</signal>
        <slot access="public">SetMinValue(float)</slot>
        <slot access="public">SetMinValue(const QString&amp;)</slot>
        <slot access="public">SetMaxValue(float)</slot>
        <slot access="public">SetMaxValue(const QString&amp;)</slot>
        <slot access="public">SetValue(float)</slot>
        <slot access="public">SetValue(const QString&amp;)</slot>
        <slot access="public">SetDigit(int)</slot>
        <property type="String">minValue</property>
        <property type="String">maxValue</property>
        <property type="String">Value</property>
        <property type="ButtonSymbols">buttonSymbols</property>
        <property type="String">prefix</property>
        <property type="String">suffix</property>
    </customwidget>
    <customwidget>
        <class>TQtWidget</class>
        <header location="local">TQtWidget.h</header>
        <sizehint>
            <width>-1</width>
            <height>-1</height>
        </sizehint>
        <container>0</container>
        <sizepolicy>
            <hordata>5</hordata>
            <verdata>5</verdata>
        </sizepolicy>
        <pixmap>
            <data format="PNG" length="333">89504e470d0a1a0a0000000d4948445200000020000000200806000000737a7af40000011449444154789ced974d1283200c8543c7f150d934474f3739941bba7052417e8c544a17be9563087c401ea2131118a907000022faa100352dcb5284abc52c7113406fdd0037c0a40fd66a272260e66cec2837d43ccf2e02d017b90e34b69e171e881c88880b6347b925356e81bfecf01a5e0366005dfe4d1e88e87700bd341d376993d6c8d1d7f6940d995fd17ba27c5e685544f4a16d552d36f444cf6247aacdaa1b70cd8ac36bc004903a60153347e741a9dd570056abb50c6e0258d5efc676810d3d20baa6d94700351beeed97c61800f26db44ef65634d9501373f6d3c14bb17dbc64c5620db416d559fde739609d7d6df96395ef0f09c099a5af15a7152202e8bfef2984139121ff8622e23e002335dc056fb62eaaecb151bc8f0000000049454e44ae426082</data>
        </pixmap>
        <signal>CanvasPainted()</signal>
        <signal>Saved(bool)</signal>
        <signal>RootEventProcessed(TObject *object, UInt_t event, TCanvas *RootCanvas)</signal>
        <slot access="public">cd(int)</slot>
        <slot access="public">Disconnect()</slot>
        <slot access="public">cd()</slot>
        <slot access="public">Refresh()</slot>
        <slot access="public">slot()</slot>
        <slot access="public">Save(const char *fileName) const</slot>
        <slot access="public">Save(QString&amp;fileName) const</slot>
        <slot access="public">Save(QString&amp;fileName, const char*format, int quality) const</slot>
        <slot access="public">Save(const char *fileName, const char*format, int quality) const</slot>
    </customwidget>
    <customwidget>
        <class>TQtLineWidthComboBox</class>
        <header location="local">/home/fine/public/work/ROOT/ROOTCVS/root/include/TQtStyleComboBox.h</header>
        <sizehint>
            <width>-1</width>
            <height>-1</height>
        </sizehint>
        <container>0</container>
        <sizepolicy>
            <hordata>5</hordata>
            <verdata>5</verdata>
        </sizepolicy>
        <pixmap>
            <data format="PNG" length="1005">89504e470d0a1a0a0000000d4948445200000016000000160806000000c4b46c3b000003b449444154789cad954f4c1c551cc73fefcd303bcb2e8575cbb288588a42aa26550e9543638d420f26d28007e3b18d9e6d8c5762d26393a6dea951b968e8c9b4bdd83fd618486c6208d602ee621121cb02c2c2eeec2ebbb333ef7960215d03c6a47c4f6f32f33eeffbfb7d7f7923b4d6ec4a08c161486b8dd8050f0f0fd3dddd4d2e97d3e9741ac771fe172497cb914824989c9cc475dd3d67e6aed3d1d1515ada2c1deb2871fc64918a5f44088190609a20e5ce1abde3a85a22be522cfc69a1bf3078701f0d883d70f5641deb28b1b279878c3387e76f13082afc8acddab24166bd0cdae299a3753c7bbc4c28ec51a9681002330ccd71a3a6923d703a9da6f364918c334745e7b08341e61fd571ef569ed95fb78162f5cb101ddd3667871ae87dd340e90268b083727fb0e338b87e11cf2f6007833cb80f6323ab8024d460d2d915435a82f4a2cb42729b6b970bac2c4679efbc85a68c527a7ff06eaf0341987f643236b20228fa065ae81f3088c404a6ed532a363271db656ce46f6e7e9ba625de49ff908d94851a708d7f21c1afd8dcbde1001e7defc678ffa33a9ae2054c3bc7e69acdbd9b45ce0ed673fe623360f2dd37eb381b12c3a81dd51ac7a6096b2983e46f658e1c3942ff391325b7300d0367ab91afaee69999cab0b1eaf2e127cd8cdf0e919cce929c8d60d490fee5584ac1c646194589632fd8449a05a66950c84618b9ec303395217ab49ede371a08359638f16afd4ef04b15843c20bc9d1e835016a09096c2b07d36d7227cfd799ed9290750040226aded75987611c3dad9aeb489346ac1b54f0222cd0208915aaa50deae67e2079799a90c4d4df5c4e34d2ca7b6b832bccce3a908a9c7252492b6e72da450073bd60ada3a3c8ebd18e4af3f8a8c7fef72ee8310d98d08af9f0e136f0b70e5338fe5852d2e7dbc885b2cd0180df0728f269daa05d73ad69a50d8e79dc13000d7afad3171a7c4858b514ebdedd3f95a964f2f75108d3591dfdac4f53cfa06a3b4777968fe638e11824a057acf182c2f45b83196e2cbab69c6ef8638f18a8d610952f32b381917aa61fdf25381e4400cb3ee80f072b91cbe5220349ec83374c126d6dac5adebab241f66493edc04142089446dfa069fe3e71f1d9ca28b5b56e4b307f4389148b0b0606184050a89c2a36f5072ea4c9cb9df5d5617151a496bbbc54b3d9af66ec55b032d54ca609a0ed393e5dae2b5d60821b02c4bf79c36688e1b048212ed0b84a130a4c4a85e9b521a08a1aafd5498a6249f83e9c912eb2b1e6b69af3a5b4f8077e3e3e9547bd1eff7e26925e1893fc2216a6f460e13aeb5e61fb1529294cbb917520000000049454e44ae426082</data>
        </pixmap>
    </customwidget>
</customwidgets>
</CW>
