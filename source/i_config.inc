{------------------------------------------------------------------------------
 Settings for fm_main unit
 ------------------------------------------------------------------------------}

{$IfDef inc_fm_main}
{$UnDef inc_fm_main}

Settings.Add(acShowBorder,                     @cfg.com.border);
Settings.Add('_com.splitter',        stDouble, @cfg.com.splitter,        '0.5');
Settings.Add('_com.update.lastTime', stInt64,  @cfg.com.update.lastTime, '0');

Settings.Add(acShowLineCounts,             @cfg.editor.lineCounts);
Settings.Add(acShowHEX,                    @cfg.editor.hex.enable);

Settings.Add(cbBaudrate,                   @cfg.connect.baudId);
Settings.Add(seBaudRateCustom,             @cfg.connect.baudrate);
Settings.Add(acPortDB5,                    @cfg.connect.dataBits.db5);
Settings.Add(acPortDB6,                    @cfg.connect.dataBits.db6);
Settings.Add(acPortDB7,                    @cfg.connect.dataBits.db7);
Settings.Add(acPortDB8,                    @cfg.connect.dataBits.db8);
Settings.Add(acPortPBE,                    @cfg.connect.parityBits.e);
Settings.Add(acPortPBM,                    @cfg.connect.parityBits.m);
Settings.Add(acPortPBN,                    @cfg.connect.parityBits.n);
Settings.Add(acPortPBO,                    @cfg.connect.parityBits.o);
Settings.Add(acPortPBS,                    @cfg.connect.parityBits.s);
Settings.Add(acPortSB1,                    @cfg.connect.stopBits.sb1);
Settings.Add(acPortSB1h,                   @cfg.connect.stopBits.sb1h);
Settings.Add(acPortSB2,                    @cfg.connect.stopBits.sb2);
Settings.Add(acShowSignals,                @cfg.connect.signals);
Settings.Add('_connect.opened',  stBool,   @cfg.connect.opened,      '0');
Settings.Add('_connect.port',    stString, @cfg.connect.port,        '---');

Settings.Add(cbRxType,                     @cfg.rx.view);   
Settings.Add(cbRxEncoding,                 @cfg.rx.encoding);
Settings.Add(acRxEnable,                   @cfg.rx.enable);   
Settings.Add(acRxScrollToEnd,              @cfg.rx.scrollEnd);
Settings.Add(acAutoAnswerEnable,           @cfg.rx.answer.enable);
Settings.Add(acShowRxBox,                  @cfg.rx.showBox);
Settings.Add('_rx.buffer',       stString, @cfg.rx.buffer,        '');
Settings.Add('_rx.answer.in',    stString, @cfg.rx.answer.input,  '');
Settings.Add('_rx.answer.out',   stString, @cfg.rx.answer.output, '');
                                                          
Settings.Add(cbTxType,                     @cfg.tx.view);    
Settings.Add(cbTxEncoding,                 @cfg.tx.encoding);
Settings.Add(acTxCommandMode,              @cfg.tx.cmdEnable);
Settings.Add(acTxSequences,                @cfg.tx.seqEnable);
Settings.Add(acShowTxBox,                  @cfg.tx.showBox);
Settings.Add('_tx.sequences',    stString, @cfg.tx.sequences, '');
Settings.Add('_tx.buffer',       stString, @cfg.tx.buffer,    '');

Settings.Add(acPlotterShow,                @cfg.plt.enable);
Settings.Add(acPlotterSettings,            @cfg.plt.settings);
Settings.Add(acPlotterQTracker,            @cfg.plt.tracker);
Settings.Add(acPlotterQLiveMode,           @cfg.plt.live);
Settings.Add(cbPlotterProtocol,            @cfg.plt.protocol);
Settings.Add(cbPlotterView,                @cfg.plt.viewmode);
Settings.Add(sePlotterViewport,            @cfg.plt.viewport);
Settings.Add(sePlotterPenSize,             @cfg.plt.pen.thickness);
Settings.Add(cbPlotterPenStyle,            @cfg.plt.pen.style);
Settings.Add(sePlotterPoints,              @cfg.plt.pen.pointSize);

Settings.Add(cbPlotterRegExp,              @cfg.regexp.selected);

Settings.Add(acSearch,                     @cfg.search.enable);
Settings.Add(cbSearchCaseSens,             @cfg.search.caseSense);
Settings.Add(cbSearchRegex,                @cfg.search.regex);
Settings.Add(cbSearchReplace,              @cfg.search.replace);
Settings.Add(cbSearchReplaceAll,           @cfg.search.replaceAll);
                                             
Settings.Add(acShowTBMain,                 @cfg.toolbar.main);
Settings.Add(acShowTBPort,                 @cfg.toolbar.port);
Settings.Add(acShowTBRx,                   @cfg.toolbar.rx);
Settings.Add(acShowTBTx,                   @cfg.toolbar.tx);

{$EndIf}


{------------------------------------------------------------------------------
 Settings for fm_settings unit
 ------------------------------------------------------------------------------}

{$IfDef inc_fm_settings}
{$UnDef inc_fm_settings}

Settings.Add(cbLanguage,                   @cfg.com.langIndex);
Settings.Add(cbPanelsLayout,               @cfg.com.layoutIndex);
Settings.Add(seIconsRes,                   @cfg.com.iconsRes);
Settings.Add(seFontSize,                   @cfg.com.fontSize);
Settings.Add(cbGluedWindow,                @cfg.com.glued);
Settings.Add(cbShowIndicators,             @cfg.com.leds);
Settings.Add(cbMinimizeToTray,             @cfg.com.tray);
Settings.Add(cbShowMenu,                   @cfg.com.menu);
Settings.Add(cbShowEncoding,               @cfg.com.encoding);
Settings.Add(cbShowRS232Captions,          @cfg.com.RS232);
Settings.Add(cbShowStatusBar,              @cfg.com.status);
Settings.Add(cbAppTheme,                   @cfg.com.theme);

Settings.Add(cbAppUpdateWay,               @cfg.com.update.wayIndex);
Settings.Add(cbAppUpdateFreq,              @cfg.com.update.freqIndex);

Settings.Add(seTabSize,                    @cfg.editor.tab);
Settings.Add(cbFontQuality,                @cfg.editor.quality);
Settings.Add(cbShowRightEdge,              @cfg.editor.right.enable);
Settings.Add(seRightEdge,                  @cfg.editor.right.pos);
Settings.Add(cbShowSizes,                  @cfg.editor.view.size);
Settings.Add(cbSizesInBytes,               @cfg.editor.view.inBytes);
Settings.Add(cbShowPosAndSel,              @cfg.editor.view.pos);
Settings.Add(seHEXBlockBytes,              @cfg.editor.hex.block);
Settings.Add(seHEXLineBytes,               @cfg.editor.hex.line);
Settings.Add(cbLineBreakStyle,             @cfg.editor.linebreakIndex);

Settings.Add(cbAutoconnect,                @cfg.connect.auto);
Settings.Add(cbCheckPort,                  @cfg.connect.check);
Settings.Add(cbHardflow,                   @cfg.connect.hardflow);

Settings.Add(cbTxRestore,                  @cfg.tx.restore);
Settings.Add(cbFileLoadWarning,            @cfg.tx.loadWarn);
Settings.Add(cbFileDataAdd,                @cfg.tx.addition);
Settings.Add(seTxBreakTime,                @cfg.tx.breakTime);
Settings.Add(seTxDeadlockTO,               @cfg.tx.timeout);
Settings.Add(cbFontTxName,                 @cfg.theme.light.font.tx.index);
Settings.Add(seFontTxSize,                 @cfg.theme.light.font.tx.size);
Settings.Add(colbFontTx,                   @cfg.theme.light.font.tx.color);
Settings.Add(cbFontTxNameD,                @cfg.theme.dark.font.tx.index);
Settings.Add(seFontTxSizeD,                @cfg.theme.dark.font.tx.size);
Settings.Add(colbFontTxD,                  @cfg.theme.dark.font.tx.color);

Settings.Add(cbRxRestore,                  @cfg.rx.restore);
Settings.Add(seRxSizeLimit,                @cfg.rx.limit, 1024);
Settings.Add(cbRxTimestamp,                @cfg.rx.timestamp.enable);
Settings.Add(edRxTSBefore,                 @cfg.rx.timestamp.before);
Settings.Add(edRxTSAfter,                  @cfg.rx.timestamp.after);
Settings.Add(seRxTimeout,                  @cfg.rx.timestamp.timeout);
Settings.Add(cbFontRxName,                 @cfg.theme.light.font.rx.index);
Settings.Add(seFontRxSize,                 @cfg.theme.light.font.rx.size);
Settings.Add(colbFontRx,                   @cfg.theme.light.font.rx.color);
Settings.Add(cbFontRxNameD,                @cfg.theme.dark.font.rx.index);
Settings.Add(seFontRxSizeD,                @cfg.theme.dark.font.rx.size);
Settings.Add(colbFontRxD,                  @cfg.theme.dark.font.rx.color);

Settings.Add(cbCSVDelimiter,               @cfg.csv.delimiterIndex);
Settings.Add(cbCSVDecDelimiter,            @cfg.csv.decimalIndex);
Settings.Add(cbCSVQuotes,                  @cfg.csv.quotesIndex);
Settings.Add(cbCSVLineBreak,               @cfg.csv.linebreakIndex);

Settings.Add(seImageWidth,                 @cfg.png.w);
Settings.Add(seImageHeight,                @cfg.png.h);
Settings.Add(cbImageDialogOne,             @cfg.png.silent);
Settings.Add(cbImageCustomSize,            @cfg.png.custom);
Settings.Add(cbImageFontProp,              @cfg.png.font.prop);
Settings.Add(seImageFontSize,              @cfg.png.font.size);

Settings.Add(cbPlotLineRecolor,            @cfg.plt.recolor);
Settings.Add(cbPlotLineReactivate,         @cfg.plt.reactivate);
Settings.Add(cbPlotCopyRx,                 @cfg.plt.copyRx);
Settings.Add(cbPlotClearRx,                @cfg.plt.clearRx);
Settings.Add(cbPlotAllowCommands,          @cfg.plt.commands);
Settings.Add(cbPlotSmooth,                 @cfg.plt.smooth);
Settings.Add(cbPlotSize,                   @cfg.plt.size);
Settings.Add(cbPlotMinimap,                @cfg.plt.view.minimap);
Settings.Add(cbPlotPanelOnMain,            @cfg.plt.view.panelOnMain);
Settings.Add(colbPlotterBG,                @cfg.theme.light.plotter.bg);
Settings.Add(colbPlotterBGW,               @cfg.theme.light.plotter.bgwork);
Settings.Add(colbPlotterText,              @cfg.theme.light.plotter.txt);
Settings.Add(colbPlotterBGd,               @cfg.theme.dark.plotter.bg);
Settings.Add(colbPlotterBGWd,              @cfg.theme.dark.plotter.bgwork);
Settings.Add(colbPlotterTextd,             @cfg.theme.dark.plotter.txt);

Settings.Add(cbPlotGridX,                  @cfg.ax.grid);
Settings.Add(cbPlotMarksX,                 @cfg.ax.marks);
Settings.Add(cbPlotLabelX,                 @cfg.ax.labels);
Settings.Add(cbPlotSampleCount,            @cfg.ax.counter);
Settings.Add(sePlotSamples,                @cfg.ax.samples);
Settings.Add(sePlotWinClear,               @cfg.ax.space);
Settings.Add(fsePlotCtrlFactorX,           @cfg.ax.ctrl.factor);
Settings.Add(cbPlotXBar,                   @cfg.ax.ctrl.bar);
Settings.Add(cbPlotCtrlX,                  @cfg.ax.ctrl.methodIndex);
Settings.Add(colbPlotterGX,                @cfg.theme.light.plotter.ax);
Settings.Add(colbPlotterGXd,               @cfg.theme.dark.plotter.ax);

Settings.Add(cbPlotGridY,                  @cfg.ay.grid);
Settings.Add(cbPlotMarksY,                 @cfg.ay.marks);
Settings.Add(fsePlotCtrlFactorY,           @cfg.ay.ctrl.factor);
Settings.Add(cbPlotCtrlY,                  @cfg.ay.ctrl.methodIndex);
Settings.Add(sePlotOffsetYTop,             @cfg.ay.offset.t);
Settings.Add(sePlotOffsetYBot,             @cfg.ay.offset.b);
Settings.Add(colbPlotterGY,                @cfg.theme.light.plotter.ay);
Settings.Add(colbPlotterGYd,               @cfg.theme.dark.plotter.ay);

Settings.Add(cbPlotLegendShow,             @cfg.legend.enable);
Settings.Add(cbPlotShowUntitled,           @cfg.legend.untitled);
Settings.Add(cbPlotLegendColored,          @cfg.legend.colored);
Settings.Add(cbPlotLegendColoredB,         @cfg.legend.coloredframe);
Settings.Add(cbPlotLegendActive,           @cfg.legend.interactive);
Settings.Add(cbPlotLegendFrame,            @cfg.legend.frame);
Settings.Add(cbPlotLegendIndex,            @cfg.legend.index);
Settings.Add(cbPlotLegendStyle,            @cfg.legend.style);

Settings.Add(cbRegExpCaseCare,             @cfg.regexp.casecare);
Settings.Add('_regexp.listData', stString, @cfg.regexp.listData,  '');

{$EndIf}

